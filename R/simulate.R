# ── Private: map model-matrix column name back to variable name ───────────────

.re_colname_to_varname <- function(colname, within_vars) {
  if (colname == "(Intercept)") return("(Intercept)")
  for (v in within_vars) {
    if (identical(v$type, "continuous") && identical(colname, v$name))
      return(v$name)
    if (v$type %in% c("binary", "categorical")) {
      for (lv in names(v$categories)) {
        if (identical(colname, paste0(v$name, v$categories[[lv]])))
          return(v$name)
      }
    }
  }
  colname
}


# ── Private: build Sigma_u from a random_effects spec ─────────────────────────
#
# Accepts either:
#   - A bare list (array) of {vars, value} entries  →  treated as type "custom"
#   - A named list (object) with $type in
#       "custom"           : uses $components (same {vars,value} format)
#       "sphericity"       : derives Sigma_u from within categorical vars;
#                            requires $variance (> 0); optional $components
#       "compound_symmetry": same but also requires $covariance (numeric);
#                            optional $components
#
# Returns a named numeric matrix (dimnames = model-matrix column names).
# Throws an error if the result is not positive semi-definite.
#
.build_re_matrix <- function(random_effects, within_vars) {
  if (is.null(random_effects))
    return(matrix(0, 0, 0, dimnames = list(character(0L), character(0L))))
  re_type    <- as.character(random_effects$type %||% "")
  components <- random_effects$components %||% list()

  .term_key <- function(ref) {
    nm <- ref$name %||% ""
    if (nm == "(Intercept)") return("(Intercept)")
    spec <- Find(function(x) identical(x$name, nm), within_vars)
    if (is.null(spec) || identical(spec$type, "continuous")) return(nm)
    lv <- as.character(ref$level %||% "")
    paste0(nm, spec$categories[[lv]])
  }

  # ── structured path: sphericity / compound_symmetry ──────────────────────
  within_cat <- Filter(function(v) v$type %in% c("binary", "categorical"), within_vars)

  if (re_type %in% c("sphericity", "compound_symmetry")) {
    if (length(within_cat) == 0L) {
      sigma_u <- matrix(0, 0, 0, dimnames = list(character(0L), character(0L)))
    } else {
      factor_list <- lapply(within_cat, function(v) {
        keys      <- sort(as.integer(names(v$categories)))
        labels    <- vapply(as.character(keys), function(k) v$categories[[k]], character(1L))
        ref_label <- v$categories[[as.character(v$reference)]]
        factor(labels, levels = c(ref_label, setdiff(labels, ref_label)))
      })
      names(factor_list) <- vapply(within_cat, `[[`, character(1L), "name")
      idata <- do.call(expand.grid, factor_list)

      fml   <- stats::as.formula(paste0("~ ", paste(names(factor_list), collapse = " * ")))
      M     <- stats::model.matrix(fml, data = idata)
      C_inv <- solve(M)
      k     <- nrow(idata)

      if (re_type == "sphericity") {
        sigma_x <- diag(as.numeric(random_effects$variance), k)
      } else {
        variance  <- as.numeric(random_effects$variance)
        covariance <- as.numeric(random_effects$covariance)
        sigma_x   <- matrix(covariance, k, k)
        diag(sigma_x) <- variance
      }

      sigma_u <- C_inv %*% sigma_x %*% t(C_inv)
      dimnames(sigma_u) <- list(colnames(M), colnames(M))
    }

    # apply components: overwrite existing cells or expand for new terms
    for (entry in components) {
      val  <- as.numeric(entry$value)
      keys <- vapply(entry$vars, .term_key, character(1L))
      for (k in keys) {
        if (!k %in% rownames(sigma_u)) {
          n       <- nrow(sigma_u)
          new_mat <- matrix(0, n + 1L, n + 1L)
          if (n > 0L) new_mat[seq_len(n), seq_len(n)] <- sigma_u
          dimnames(new_mat) <- list(c(rownames(sigma_u), k), c(colnames(sigma_u), k))
          sigma_u <- new_mat
        }
      }
      if (length(entry$vars) == 1L) {
        sigma_u[keys[1L], keys[1L]] <- val
      } else {
        sigma_u[keys[1L], keys[2L]] <- val
        sigma_u[keys[2L], keys[1L]] <- val
      }
    }

  } else if (re_type == "custom") {
    if (length(components) == 0L)
      return(matrix(0, 0, 0, dimnames = list(character(0L), character(0L))))

    terms <- character(0L)
    for (entry in components) {
      for (ref in entry$vars) {
        tk <- .term_key(ref)
        if (!tk %in% terms) terms <- c(terms, tk)
      }
    }
    n       <- length(terms)
    sigma_u <- matrix(0, n, n, dimnames = list(terms, terms))
    for (entry in components) {
      val <- as.numeric(entry$value)
      if (length(entry$vars) == 1L) {
        sigma_u[.term_key(entry$vars[[1L]]), .term_key(entry$vars[[1L]])] <- val
      } else {
        ka <- .term_key(entry$vars[[1L]])
        kb <- .term_key(entry$vars[[2L]])
        sigma_u[ka, kb] <- sigma_u[kb, ka] <- val
      }
    }
  } else {
    stop(paste0("random_effects: unknown type '", re_type,
                "'. Must be 'sphericity', 'compound_symmetry', or 'custom'."))
  }

  # ── PSD check ──────────────────────────────────────────────────────────────
  if (nrow(sigma_u) > 0L) {
    eigs <- eigen(sigma_u, symmetric = TRUE, only.values = TRUE)$values
    tol  <- -1e-8 * max(abs(eigs), 1e-10)
    if (any(eigs < tol))
      stop(paste0("random_effects: Sigma_u is not positive semi-definite. ",
                  "Minimum eigenvalue: ", round(min(eigs), 6)))
  }

  sigma_u
}


# ── Private: fit the appropriate model ───────────────────────────────────────

.fit_model <- function(dep_name, rhs, df, family, use_mixed,
                       n_reps, residual_sd,
                       active_terms,
                       within_continuous, within_random_cat,
                       between_vars, within_vars,
                       subject_id_name) {
  dep_form <- stats::as.formula(paste(dep_name, "~", rhs))

  if (!use_mixed) {
    if (family == "identity+normal") {
      return(stats::lm(dep_form, data = df))
    } else if (family == "logit+binomial") {
      return(stats::glm(dep_form, data = df, family = stats::binomial))
    } else {
      return(stats::glm(dep_form, data = df, family = stats::poisson))
    }
  }

  # Derive slope variable names from active model-matrix column names.
  # Filter to main-effect terms only (no ':' = no interactions); lmerTest
  # does not support interaction random slopes via standard formula syntax.
  .slope_vars <- function() {
    main <- active_terms[!grepl(":", active_terms, fixed = TRUE)]
    unique(Filter(
      function(x) x != "(Intercept)",
      vapply(main, .re_colname_to_varname, character(1L), within_vars = within_vars)
    ))
  }

  if (family == "identity+normal") {
    # LMM is unidentified when n_reps == 1 (residual confounded with RE) or
    # residual_sd == 0. afex::aov_ez() uses subject x condition as error term
    # and is always identified for balanced factorial within designs.
    use_afex <- (n_reps == 1L || residual_sd == 0) &&
      length(within_continuous) == 0L &&
      length(within_random_cat) == 0L
    if (use_afex) {
      between_cat  <- vapply(
        Filter(function(v) v$type %in% c("binary", "categorical"), between_vars),
        `[[`, character(1L), "name")
      between_cont <- vapply(
        Filter(function(v) v$type == "continuous", between_vars),
        `[[`, character(1L), "name")
      within_cat   <- vapply(
        Filter(function(v) v$type %in% c("binary", "categorical"), within_vars),
        `[[`, character(1L), "name")
      df[[subject_id_name]] <- factor(df[[subject_id_name]])
      return(suppressMessages(afex::aov_ez(
        id          = subject_id_name,
        dv          = dep_name,
        data        = df,
        between     = if (length(between_cat)  > 0L) between_cat  else NULL,
        within      = if (length(within_cat)   > 0L) within_cat   else NULL,
        covariate   = if (length(between_cont) > 0L) between_cont else NULL,
        type        = 3L,
        include_aov = FALSE
      )))
    }
    slope_var_names <- .slope_vars()
    re_part <- if (length(slope_var_names) == 0L) {
      paste0("(1 | ", subject_id_name, ")")
    } else {
      paste0("(1 + ", paste(slope_var_names, collapse = " + "), " || ", subject_id_name, ")")
    }
    mixed_form            <- stats::as.formula(paste(dep_name, "~", rhs, "+", re_part))
    df[[subject_id_name]] <- factor(df[[subject_id_name]])
    return(lmerTest::lmer(mixed_form, data = df, REML = FALSE))
  }

  # Non-Gaussian families: glmer (family determines dispersion; n_reps / residual_sd
  # conditions do not affect identifiability the same way)
  slope_var_names <- .slope_vars()
  re_part <- if (length(slope_var_names) == 0L) {
    paste0("(1 | ", subject_id_name, ")")
  } else {
    paste0("(1 + ", paste(slope_var_names, collapse = " + "), " || ", subject_id_name, ")")
  }
  mixed_form            <- stats::as.formula(paste(dep_name, "~", rhs, "+", re_part))
  df[[subject_id_name]] <- factor(df[[subject_id_name]])
  if (family == "logit+binomial") {
    lme4::glmer(mixed_form, data = df, family = stats::binomial)
  } else {
    lme4::glmer(mixed_form, data = df, family = stats::poisson)
  }
}


# ── Private: capture model summary as text ───────────────────────────────────

.capture_summary <- function(model) {
  if (inherits(model, "afex_aov")) {
    paste(utils::capture.output(print(model)), collapse = "\n")
  } else {
    paste(utils::capture.output(summary(model)), collapse = "\n")
  }
}


# ── Private: compute odds ratio table for logistic models ────────────────────

.compute_or_table <- function(model, use_mixed) {
  if (use_mixed) {
    fe  <- lme4::fixef(model)
    se  <- sqrt(diag(as.matrix(stats::vcov(model))))
    tbl <- cbind(OR = exp(fe), `2.5 %` = exp(fe - 1.96 * se),
                 `97.5 %` = exp(fe + 1.96 * se))
  } else {
    tbl <- exp(cbind(OR = stats::coef(model), stats::confint(model)))
  }
  paste(utils::capture.output(print(tbl)), collapse = "\n")
}


# ── Public: generate simulated data from a study design ──────────────────────
#
#' Simulate a dataset from a study design (schema v2).
#'
#' Continuous predictors are drawn with their designed mean and SD; binary and
#' categorical ones as latent normal scores cut at the proportions. The
#' outcome uses mean-centred continuous predictors, so the intercept is the
#' expected outcome at the predictor means.
#' @param study_design Study design list (see `dev/schemas/study_design.md`).
#' @param output_csv Path of the CSV file the data are written to.
#' @return `list(df, csv_path)`.
#' @keywords internal
simulate <- function(study_design, output_csv = tempfile(fileext = ".csv")) {
  family          <- study_design$family
  n_subjects      <- as.integer(study_design$n_subjects)
  n_reps          <- as.integer(study_design$n_reps)
  subject_id_name <- study_design$subject_id_name
  dep             <- study_design$variables$dependent

  between_vars   <- study_design$variables$between %||% list()
  within_vars    <- study_design$variables$within  %||% list()
  all_indep      <- c(between_vars, within_vars)
  var_names      <- vapply(all_indep, `[[`, character(1L), "name")
  correlations   <- study_design$correlations   %||% list()
  coefficients   <- study_design$coefficients
  residual_sd    <- as.numeric(study_design$residual_sd %||% 0)
  random_effects <- study_design$random_effects

  within_balanced_cat <- Filter(function(v)
    v$type %in% c("binary", "categorical") &&
      identical(as.character(v$assignment %||% "balanced"), "balanced"),
    within_vars)
  within_random_cat <- Filter(function(v)
    v$type %in% c("binary", "categorical") &&
      identical(as.character(v$assignment %||% "balanced"), "random"),
    within_vars)
  within_continuous <- Filter(function(v) identical(v$type, "continuous"), within_vars)

  .vars_to_colname <- function(vars_list) {
    parts <- vapply(vars_list, function(ref) {
      nm <- ref$name %||% ""
      if (nm == "(Intercept)") return("(Intercept)")
      spec <- Find(function(x) identical(x$name, nm), all_indep)
      if (is.null(spec) || identical(spec$type, "continuous")) return(nm)
      lv <- as.character(ref$level %||% "")
      paste0(nm, spec$categories[[lv]])
    }, character(1L))
    paste(parts, collapse = ":")
  }

  # ── Correlation matrix ─────────────────────────────────────────────────────
  eligible_vars <- c(between_vars, within_continuous)
  elig_names    <- vapply(eligible_vars, `[[`, character(1L), "name")
  n_elig        <- length(elig_names)
  C_mat         <- if (n_elig > 0L) diag(n_elig) else matrix(0, 0, 0)
  for (entry in correlations) {
    v1 <- as.character(entry$vars[[1]])
    v2 <- as.character(entry$vars[[2]])
    i  <- which(elig_names == v1)
    j  <- which(elig_names == v2)
    if (length(i) && length(j)) C_mat[i, j] <- C_mat[j, i] <- as.numeric(entry$value)
  }
  n_b  <- length(between_vars)
  n_wc <- length(within_continuous)
  between_C <- if (n_b  > 0L) C_mat[seq_len(n_b), seq_len(n_b), drop = FALSE] else matrix(0, 0, 0)
  wc_C      <- if (n_wc > 0L) C_mat[n_b + seq_len(n_wc), n_b + seq_len(n_wc), drop = FALSE] else matrix(0, 0, 0)

  # ── Step 1: Draw between-subjects variables ────────────────────────────────
  between_names <- vapply(between_vars, `[[`, character(1L), "name")
  X_between <- NULL
  if (n_b > 0L) {
    b_mu  <- vapply(between_vars, function(v)
      if (v$type == "continuous") as.numeric(v$distribution$mean) else 0, numeric(1L))
    b_sds <- vapply(between_vars, function(v)
      if (v$type == "continuous") as.numeric(v$distribution$sd) else 1,  numeric(1L))
    D_b       <- diag(b_sds, nrow = n_b)
    Sigma_b   <- D_b %*% between_C %*% D_b
    X_between <- as.data.frame(MASS::mvrnorm(n = n_subjects, mu = b_mu, Sigma = Sigma_b))
    names(X_between) <- between_names
    for (v in between_vars) {
      if (v$type %in% c("binary", "categorical")) {
        props_raw    <- v$proportion
        keys_sorted  <- sort(as.integer(names(props_raw)))
        props_sorted <- vapply(as.character(keys_sorted),
                               function(k) as.numeric(props_raw[[k]]), numeric(1L))
        breaks_inner  <- stats::qnorm(cumsum(props_sorted[-length(props_sorted)]))
        labels_sorted <- vapply(as.character(keys_sorted),
                                function(k) v$categories[[k]], character(1L))
        x_cut <- cut(X_between[[v$name]],
                     breaks = c(-Inf, breaks_inner, Inf), labels = labels_sorted)
        ref_label            <- v$categories[[as.character(v$reference)]]
        X_between[[v$name]] <- stats::relevel(x_cut, ref = ref_label)
      }
    }
  }

  # ── Step 2: Balanced within-subjects skeleton ──────────────────────────────
  within_skel   <- NULL
  n_within_rows <- n_reps
  if (length(within_balanced_cat) > 0L) {
    cat_levels_list <- lapply(within_balanced_cat, function(v) {
      keys_sorted <- sort(as.integer(names(v$categories)))
      vapply(as.character(keys_sorted), function(k) v$categories[[k]], character(1L))
    })
    names(cat_levels_list) <- vapply(within_balanced_cat, `[[`, character(1L), "name")
    within_skel <- do.call(expand.grid, c(cat_levels_list, list(stringsAsFactors = FALSE)))
    within_skel <- within_skel[rep(seq_len(nrow(within_skel)), n_reps), , drop = FALSE]
    for (v in within_balanced_cat) {
      ref_label             <- v$categories[[as.character(v$reference)]]
      within_skel[[v$name]] <- stats::relevel(factor(within_skel[[v$name]]), ref = ref_label)
    }
    rownames(within_skel) <- NULL
    n_within_rows <- nrow(within_skel)
  }

  n_obs       <- n_subjects * n_within_rows
  subject_ids <- rep(seq_len(n_subjects), each = n_within_rows)

  if (!is.null(X_between)) {
    df <- X_between[subject_ids, , drop = FALSE]
    rownames(df) <- NULL
  } else {
    df <- data.frame(matrix(nrow = n_obs, ncol = 0L))
  }
  if (!is.null(within_skel)) {
    wcs_rep           <- within_skel[rep(seq_len(nrow(within_skel)), n_subjects), , drop = FALSE]
    rownames(wcs_rep) <- NULL
    df                <- cbind(df, wcs_rep)
  }
  df[[subject_id_name]] <- subject_ids

  # ── Step 3: Randomly-assigned within-subjects variables ───────────────────
  for (v in within_random_cat) {
    props_raw <- v$proportion
    keys_sorted <- sort(as.integer(names(v$categories)))
    if (is.null(props_raw)) {
      n_cats       <- length(v$categories)
      props_sorted <- rep(1 / n_cats, n_cats)
    } else {
      props_sorted <- vapply(as.character(keys_sorted),
                             function(k) as.numeric(props_raw[[k]]), numeric(1L))
      props_sorted <- props_sorted / sum(props_sorted)
    }
    breaks_inner  <- stats::qnorm(cumsum(props_sorted[-length(props_sorted)]))
    labels_sorted <- vapply(as.character(keys_sorted),
                            function(k) v$categories[[k]], character(1L))
    z_draw       <- stats::rnorm(n_obs)
    x_cut        <- cut(z_draw, breaks = c(-Inf, breaks_inner, Inf), labels = labels_sorted)
    ref_label    <- v$categories[[as.character(v$reference)]]
    df[[v$name]] <- stats::relevel(x_cut, ref = ref_label)
  }

  # ── Step 4: Within-continuous variables ───────────────────────────────────
  if (n_wc > 0L) {
    wc_mu    <- vapply(within_continuous, function(v) as.numeric(v$distribution$mean), numeric(1L))
    wc_sds   <- vapply(within_continuous, function(v) as.numeric(v$distribution$sd),  numeric(1L))
    D_wc     <- diag(wc_sds, nrow = n_wc)
    Sigma_wc <- D_wc %*% wc_C %*% D_wc
    X_wc     <- as.data.frame(MASS::mvrnorm(n = n_obs, mu = wc_mu, Sigma = Sigma_wc))
    wc_names <- vapply(within_continuous, `[[`, character(1L), "name")
    names(X_wc) <- wc_names
    df <- cbind(df, X_wc)
  }

  pred_cols <- var_names[var_names %in% names(df)]
  df        <- df[, c(pred_cols, subject_id_name), drop = FALSE]

  # ── Step 5: Model matrix and fixed-effects linear predictor ───────────────
  df_mm      <- df[, pred_cols, drop = FALSE]
  # Continuous predictors are centred only for the linear predictor (so the
  # intercept is the outcome at the predictor means); df keeps them as drawn.
  for (v in all_indep) {
    if (v$type == "continuous")
      df_mm[[v$name]] <- df_mm[[v$name]] - mean(df_mm[[v$name]])
  }
  rhs   <- paste(var_names, collapse = " + ")
  X_mat <- stats::model.matrix(stats::as.formula(paste("~", rhs)), data = df_mm)

  coef_vec <- stats::setNames(rep(0, ncol(X_mat)), colnames(X_mat))
  for (entry in coefficients) {
    col <- .vars_to_colname(entry$vars)
    if (col %in% names(coef_vec)) coef_vec[col] <- as.numeric(entry$value)
  }
  eta <- as.vector(X_mat %*% coef_vec)

  # ── Step 6: Draw random effects ────────────────────────────────────────────
  sigma_u    <- .build_re_matrix(random_effects, within_vars)
  active_idx <- which(diag(sigma_u) > 0)
  if (length(active_idx) > 0L) {
    active_terms <- colnames(sigma_u)[active_idx]
    Sigma_u_act  <- sigma_u[active_idx, active_idx, drop = FALSE]
    n_re_act     <- length(active_idx)
    U_act        <- MASS::mvrnorm(n = n_subjects, mu = rep(0, n_re_act), Sigma = Sigma_u_act)
    if (is.null(dim(U_act))) dim(U_act) <- c(n_subjects, n_re_act)
    Z_act <- matrix(0, nrow = n_obs, ncol = n_re_act)
    for (k in seq_len(n_re_act)) {
      tk <- active_terms[k]
      if (tk == "(Intercept)") {
        Z_act[, k] <- 1
      } else if (tk %in% colnames(X_mat)) {
        Z_act[, k] <- X_mat[, tk]
      }
    }
    eta <- eta + rowSums(Z_act * U_act[subject_ids, , drop = FALSE])
  }

  # ── Step 7: Sample outcome ────────────────────────────────────────────────
  if (family == "identity+normal") {
    y <- if (residual_sd > 0) eta + stats::rnorm(n_obs, 0, residual_sd) else eta
  } else if (family == "logit+binomial") {
    y <- stats::rbinom(n_obs, 1, stats::plogis(eta))
  } else {
    y <- stats::rpois(n_obs, exp(eta))
  }

  df[[dep$name]] <- y

  utils::write.csv(df, output_csv, row.names = FALSE)
  list(df = df, csv_path = output_csv)
}


# ── Public: fit a model to data using a study design specification ────────────
#
#' Fit the appropriate model to df given a study design.
#' @param study_design Study design list.
#' @param df Data frame from [simulate()].
#' @return `list(summary, or_table)`.
#' @keywords internal
estimate <- function(study_design, df) {
  family          <- study_design$family
  n_reps          <- as.integer(study_design$n_reps)
  subject_id_name <- study_design$subject_id_name
  dep             <- study_design$variables$dependent
  residual_sd     <- as.numeric(study_design$residual_sd %||% 0)

  between_vars   <- study_design$variables$between %||% list()
  within_vars    <- study_design$variables$within  %||% list()
  all_indep      <- c(between_vars, within_vars)
  var_names      <- vapply(all_indep, `[[`, character(1L), "name")
  random_effects <- study_design$random_effects

  within_continuous <- Filter(function(v) identical(v$type, "continuous"), within_vars)
  within_random_cat <- Filter(function(v)
    v$type %in% c("binary", "categorical") &&
      identical(as.character(v$assignment %||% "balanced"), "random"),
    within_vars)

  rhs        <- paste(var_names, collapse = " + ")
  sigma_u    <- .build_re_matrix(random_effects, within_vars)
  active_idx <- which(diag(sigma_u) > 0)
  use_mixed  <- length(active_idx) > 0L

  model <- .fit_model(
    dep_name          = dep$name,
    rhs               = rhs,
    df                = df,
    family            = family,
    use_mixed         = use_mixed,
    n_reps            = n_reps,
    residual_sd       = residual_sd,
    active_terms      = colnames(sigma_u)[active_idx],
    within_continuous = within_continuous,
    within_random_cat = within_random_cat,
    between_vars      = between_vars,
    within_vars       = within_vars,
    subject_id_name   = subject_id_name
  )

  or_table <- NULL
  if (family == "logit+binomial")
    or_table <- .compute_or_table(model, use_mixed)

  list(summary = .capture_summary(model), or_table = or_table)
}


# ── Public: simulate data and fit model ──────────────────────────────────────
#
#' Simulate a dataset from a study design and fit a regression model.
#' @inheritParams simulate
#' @return `list(summary, or_table, csv_path)`.
#' @keywords internal
simulate_estimate <- function(study_design,
                              output_csv = tempfile(fileext = ".csv")) {
  sim <- simulate(study_design, output_csv)
  est <- estimate(study_design, sim$df)
  list(summary = est$summary, or_table = est$or_table, csv_path = sim$csv_path)
}
