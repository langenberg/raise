# Validates a random_effects spec against a within_vars list and a
# between_names character vector. Returns NULL on success or an error string.
# Also calls .build_re_matrix() for the PSD check.
.validate_random_effects <- function(random_effects, within_vars, between_names) {
  if (!is.list(random_effects) || is.null(random_effects$type))
    return("Error: random_effects must be an object with a 'type' field ('sphericity', 'compound_symmetry', or 'custom').")

  re_type <- as.character(random_effects$type)

  if (!re_type %in% c("custom", "sphericity", "compound_symmetry"))
    return(paste0("Error: random_effects type must be one of: 'custom', 'sphericity', ",
                  "'compound_symmetry'. Got: '", re_type, "'"))

  if (re_type == "sphericity") {
    var_val <- suppressWarnings(as.numeric(random_effects$variance))
    if (is.null(random_effects$variance) || is.na(var_val) || var_val <= 0)
      return("Error: random_effects$variance must be a positive number for type 'sphericity'.")
  }
  if (re_type == "compound_symmetry") {
    var_val <- suppressWarnings(as.numeric(random_effects$variance))
    cov_val <- suppressWarnings(as.numeric(random_effects$covariance))
    if (is.null(random_effects$variance) || is.na(var_val) || var_val <= 0)
      return("Error: random_effects$variance must be a positive number for type 'compound_symmetry'.")
    if (is.null(random_effects$covariance) || is.na(cov_val))
      return("Error: random_effects$covariance must be a numeric value for type 'compound_symmetry'.")
  }

  components <- random_effects$components %||% list()

  .valid_re_term <- function(ref) {
    nm <- ref$name %||% ""
    if (nm == "(Intercept)") return(TRUE)
    spec <- Find(function(x) identical(x$name, nm), within_vars)
    if (is.null(spec)) return(FALSE)
    if (identical(spec$type, "continuous")) return(TRUE)
    lv <- as.character(ref$level %||% "")
    if (nchar(lv) == 0L) return(FALSE)
    !is.null(spec$categories[[lv]])
  }

  for (i in seq_along(components)) {
    entry  <- components[[i]]
    vars_i <- entry$vars
    if (is.null(vars_i) || !length(vars_i) %in% 1:2)
      return(paste0("Error: random_effects[[", i,
                    "]] must have 1 or 2 entries in 'vars'."))
    for (j in seq_along(vars_i)) {
      nm <- vars_i[[j]]$name %||% ""
      if (nm != "(Intercept)" && nm %in% between_names)
        return(paste0("Error: random_effects[[", i, "]] references between-subjects variable '",
                      nm, "'. Random slopes for between-subjects variables are not identified."))
      if (!.valid_re_term(vars_i[[j]]))
        return(paste0("Error: random_effects[[", i, "]] part ", j,
                      " references unknown variable/level '", nm, "'."))
    }
    val <- suppressWarnings(as.numeric(entry$value))
    if (is.na(val))
      return(paste0("Error: random_effects[[", i,
                    "]] value must be numeric. Got: ", entry$value %||% "NULL"))
    if (length(vars_i) == 1L && val < -1e-8)
      return(paste0("Error: random_effects[[", i,
                    "]] variance must be >= 0. Got: ", val))
  }

  # PSD check via the shared construction function
  mat_check <- tryCatch(
    .build_re_matrix(random_effects, within_vars),
    error = function(e) e
  )
  if (inherits(mat_check, "error"))
    return(paste0("Error: ", conditionMessage(mat_check)))

  NULL
}

.make_study_id <- function(title) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(title))
  slug <- gsub("^_+|_+$", "", slug)
  slug <- substr(slug, 1L, 40L)
  if (nchar(slug) == 0L) slug <- "study"
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  paste0(slug, "_", ts)
}

# Validates one variable spec; returns an error string or NULL.
# Used by save_study, save_description.
.check_var <- function(v, is_within) {
  nm    <- v$name %||% ""
  vtype <- v$type %||% ""
  if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", nm))
    return(paste0("Error: variable name '", nm, "' is not a valid R identifier."))
  if (nchar(nm) > 10L)
    return(paste0("Error: variable name '", nm, "' exceeds 10 characters."))
  if (!isTRUE(vtype %in% c("continuous", "binary", "categorical")))
    return(paste0("Error: variable '", nm, "' type must be 'continuous', ",
                  "'binary', or 'categorical'. Got: '", vtype, "'"))
  if (identical(vtype, "continuous")) {
    dist <- v$distribution
    if (is.null(dist) || is.null(dist$mean) || is.null(dist$sd))
      return(paste0("Error: continuous variable '", nm,
                    "' must have distribution with 'mean' and 'sd'."))
    sd_val <- suppressWarnings(as.numeric(dist$sd))
    if (is.na(sd_val) || sd_val <= 0)
      return(paste0("Error: variable '", nm,
                    "' distribution$sd must be positive. Got: ", dist$sd))
    if (is_within && !is.null(v$assignment))
      return(paste0("Error: within-continuous variable '", nm,
                    "' must not have 'assignment'."))
  }
  if (identical(vtype, "binary")) {
    cats <- v$categories
    if (is.null(cats) || length(cats) != 2L)
      return(paste0("Error: binary variable '", nm,
                    "' must have exactly 2 categories."))
    cat_keys_int <- suppressWarnings(as.integer(names(cats)))
    if (any(is.na(cat_keys_int)))
      return(paste0("Error: binary variable '", nm,
                    "' category keys must be integers."))
    ref <- as.character(v$reference %||% "")
    if (!ref %in% names(cats))
      return(paste0("Error: binary variable '", nm,
                    "' reference '", ref, "' is not a category key."))
    if (!identical(ref, as.character(min(cat_keys_int))))
      return(paste0("Error: binary variable '", nm,
                    "' reference must be the lower key ('",
                    min(cat_keys_int), "'). Got: '", ref, "'"))
    if (!is_within) {
      prop <- v$proportion
      if (is.null(prop))
        return(paste0("Error: between-subjects binary variable '", nm,
                      "' must include 'proportion'."))
      prop_vals <- suppressWarnings(as.numeric(unlist(prop)))
      if (any(is.na(prop_vals)) || abs(sum(prop_vals) - 1.0) > 0.01)
        return(paste0("Error: variable '", nm,
                      "' proportions must sum to 1.0. Got: ",
                      round(sum(prop_vals), 4)))
    } else {
      asn <- as.character(v$assignment %||% "balanced")
      if (!isTRUE(asn %in% c("balanced", "random")))
        return(paste0("Error: within-subjects variable '", nm,
                      "' assignment must be 'balanced' or 'random'. Got: '", asn, "'"))
      if (identical(asn, "balanced") && !is.null(v$proportion))
        return(paste0("Error: within-subjects balanced variable '", nm,
                      "' must not have 'proportion'."))
    }
  }
  if (identical(vtype, "categorical")) {
    cats <- v$categories
    if (is.null(cats) || length(cats) < 3L)
      return(paste0("Error: categorical variable '", nm,
                    "' must have at least 3 categories."))
    cat_keys      <- sort(names(cats))
    expected_keys <- sort(as.character(seq_len(length(cats))))
    if (!identical(cat_keys, expected_keys))
      return(paste0("Error: categorical variable '", nm,
                    "' category keys must be '1', '2', ... Got: ",
                    paste(cat_keys, collapse = ", ")))
    ref <- as.character(v$reference %||% "")
    if (!identical(ref, "1"))
      return(paste0("Error: categorical variable '", nm,
                    "' reference must be '1'. Got: '", ref, "'"))
    if (!is_within) {
      prop <- v$proportion
      if (is.null(prop))
        return(paste0("Error: between-subjects categorical variable '", nm,
                      "' must include 'proportion'."))
      prop_vals <- suppressWarnings(as.numeric(unlist(prop)))
      if (any(is.na(prop_vals)) || abs(sum(prop_vals) - 1.0) > 0.01)
        return(paste0("Error: variable '", nm,
                      "' proportions must sum to 1.0. Got: ",
                      round(sum(prop_vals), 4)))
    } else {
      asn <- as.character(v$assignment %||% "balanced")
      if (!isTRUE(asn %in% c("balanced", "random")))
        return(paste0("Error: within-subjects variable '", nm,
                      "' assignment must be 'balanced' or 'random'. Got: '", asn, "'"))
      if (identical(asn, "balanced") && !is.null(v$proportion))
        return(paste0("Error: within-subjects balanced variable '", nm,
                      "' must not have 'proportion'."))
    }
  }
  NULL
}

# Resolves a {name, level?} variable reference to its model.matrix column name.
# Returns a string starting with "__error__:" on failure.
# Used by save_study and save_coefficients.
.resolve_varref <- function(ref, all_vars_list) {
  nm <- ref$name %||% ""
  if (nm == "(Intercept)") return("(Intercept)")
  spec <- Find(function(x) identical(x$name, nm), all_vars_list)
  if (is.null(spec))
    return(paste0("__error__: variable '", nm, "' not found."))
  if (identical(spec$type, "continuous")) return(nm)
  lv <- as.character(ref$level %||% "")
  if (nchar(lv) == 0L)
    return(paste0("__error__: variable '", nm, "' is ", spec$type,
                  " but no 'level' specified."))
  if (is.null(spec$categories[[lv]]))
    return(paste0("__error__: variable '", nm, "' has no category key '", lv, "'."))
  paste0(nm, spec$categories[[lv]])
}

#' Save the complete study design to disk (schema v2).
#'
#' Validates the full design and writes \code{study_design.json}. Calling again
#' with the same \code{study_id} overwrites the design and deletes any prior
#' simulation artefacts (\code{data.csv}, \code{regression.txt},
#' \code{odds_ratios.txt}).
#'
#' @param session_path Path to the session folder.
#' @param study_id Study ID in the format \code{<slug>_<YYYYMMDD_HHMMSS>}.
#' @param title Study title, 5--10 words.
#' @param context Context paragraph, 3--5 sentences.
#' @param family One of \code{"identity+normal"}, \code{"logit+binomial"},
#'   \code{"log+poisson"}.
#' @param n_subjects Number of subjects (integer >= 2).
#' @param n_reps Repetitions per balanced within-subject condition combination
#'   (integer >= 1).
#' @param subject_id_name Column name for the subject ID (valid R identifier).
#' @param variables Named list with \code{dependent}, \code{between}, and
#'   \code{within}.
#' @param correlations List of \code{list(vars=c("A","B"), value=r)} pairs for
#'   between-subjects and within-continuous variables.
#' @param coefficients List of \code{list(vars=list(...), value=b)} entries;
#'   one entry per required fixed-effect term plus optional interactions.
#' @param residual_sd Residual SD (numeric >= 0); required for
#'   \code{"identity+normal"}, \code{NULL} otherwise.
#' @param random_effects List of \code{list(vars=list(...), value=v)} entries
#'   for variances (1-entry \code{vars}) and covariances (2-entry \code{vars});
#'   \code{list()} for fixed-effects-only designs.
#' @return A success string (possibly with non-fatal warnings) or an error
#'   string starting with \code{"Error:"}.
#' @export
save_study <- function(session_path, study_id, title, context, family,
                       n_subjects, n_reps, subject_id_name, variables,
                       correlations, coefficients, residual_sd = NULL,
                       random_effects) {

  # -- Top-level string checks --

  if (!is.character(title) || length(title) != 1L || !nzchar(trimws(title)))
    return("Error: title must be a non-empty string.")

  if (!is.character(context) || length(context) != 1L || !nzchar(trimws(context)))
    return("Error: context must be a non-empty string.")

  if (!is.character(study_id) || length(study_id) != 1L ||
      !grepl("^[a-z0-9_]+_[0-9]{8}_[0-9]{6}$", study_id))
    return(paste0("Error: study_id must match <slug>_<YYYYMMDD_HHMMSS>. Got: '",
                  study_id %||% "NULL", "'"))

  # -- Structural checks --

  if (!isTRUE(family %in% c("identity+normal", "logit+binomial", "log+poisson")))
    return(paste0("Error: family must be one of: 'identity+normal', ",
                  "'logit+binomial', 'log+poisson'. Got: '", family %||% "NULL", "'"))

  n_subjects <- suppressWarnings(as.integer(n_subjects))
  if (is.na(n_subjects) || n_subjects < 2L)
    return("Error: n_subjects must be an integer >= 2.")

  n_reps <- suppressWarnings(as.integer(n_reps))
  if (is.na(n_reps) || n_reps < 1L)
    return("Error: n_reps must be an integer >= 1.")

  if (!is.character(subject_id_name) || length(subject_id_name) != 1L ||
      !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", subject_id_name))
    return(paste0("Error: subject_id_name must be a valid R identifier. Got: '",
                  subject_id_name %||% "NULL", "'"))

  # -- Dependent variable --

  dep <- variables$dependent
  if (is.null(dep))
    return("Error: variables$dependent is missing.")
  for (field in c("name", "label", "type")) {
    if (is.null(dep[[field]]))
      return(paste0("Error: variables$dependent is missing field '", field, "'."))
  }
  if (!isTRUE(dep$type %in% c("continuous", "binary")))
    return(paste0("Error: dependent variable type must be 'continuous' or 'binary'. ",
                  "Got: '", dep$type, "'"))
  if (identical(family, "logit+binomial") && !identical(dep$type, "binary"))
    return("Error: family 'logit+binomial' requires a binary dependent variable.")
  if (family %in% c("identity+normal", "log+poisson") && !identical(dep$type, "continuous"))
    return(paste0("Error: family '", family, "' requires a continuous dependent variable."))

  # -- Independent variables --

  between_vars <- if (!is.null(variables$between)) variables$between else list()
  within_vars  <- if (!is.null(variables$within))  variables$within  else list()
  all_indep    <- c(between_vars, within_vars)

  if (length(all_indep) == 0L)
    return("Error: at least one independent variable is required.")
  if (length(all_indep) > 10L)
    return(paste0("Error: at most 10 independent variables allowed. Got: ",
                  length(all_indep), "."))

  for (v in between_vars) {
    err <- .check_var(v, is_within = FALSE)
    if (!is.null(err)) return(err)
  }
  for (v in within_vars) {
    err <- .check_var(v, is_within = TRUE)
    if (!is.null(err)) return(err)
  }

  all_names <- vapply(all_indep, `[[`, character(1), "name")
  if (anyDuplicated(all_names) > 0L)
    return(paste0("Error: duplicate variable names: ",
                  paste(unique(all_names[duplicated(all_names)]), collapse = ", ")))
  if (subject_id_name %in% all_names)
    return(paste0("Error: subject_id_name '", subject_id_name,
                  "' conflicts with a variable name."))

  # -- correlations --

  between_names    <- vapply(between_vars, `[[`, character(1), "name")
  within_cont      <- Filter(function(v) identical(v$type, "continuous"), within_vars)
  within_cont_names <- vapply(within_cont, `[[`, character(1), "name")
  eligible_names   <- c(between_names, within_cont_names)

  if (!is.list(correlations))
    return("Error: correlations must be a list.")

  seen_pairs <- character(0)
  for (i in seq_along(correlations)) {
    entry <- correlations[[i]]
    vars2 <- entry$vars
    if (is.null(vars2) || length(vars2) != 2L)
      return(paste0("Error: correlations[[", i,
                    "]] must have exactly 2 variable names in 'vars'."))
    v1 <- as.character(vars2[[1]])
    v2 <- as.character(vars2[[2]])
    if (!isTRUE(v1 %in% eligible_names))
      return(paste0("Error: correlations[[", i, "]] variable '", v1,
                    "' is not a between-subjects or within-continuous variable."))
    if (!isTRUE(v2 %in% eligible_names))
      return(paste0("Error: correlations[[", i, "]] variable '", v2,
                    "' is not a between-subjects or within-continuous variable."))
    pair_key <- paste(sort(c(v1, v2)), collapse = "||")
    if (pair_key %in% seen_pairs)
      return(paste0("Error: duplicate correlation pair (", v1, ", ", v2, ")."))
    seen_pairs <- c(seen_pairs, pair_key)
    val <- suppressWarnings(as.numeric(entry$value))
    if (is.na(val) || val <= -1 || val >= 1)
      return(paste0("Error: correlations[[", i,
                    "]] value must be in (-1, 1). Got: ", entry$value %||% "NULL"))
  }

  if (length(correlations) > 0L && length(eligible_names) >= 2L) {
    n_elig <- length(eligible_names)
    C_mat  <- diag(n_elig)
    for (entry in correlations) {
      i <- which(eligible_names == as.character(entry$vars[[1]]))
      j <- which(eligible_names == as.character(entry$vars[[2]]))
      C_mat[i, j] <- C_mat[j, i] <- as.numeric(entry$value)
    }
    chol_res <- tryCatch(chol(C_mat), error = function(e) e)
    if (inherits(chol_res, "error"))
      return(paste0("Error: correlations matrix is not positive definite: ",
                    conditionMessage(chol_res)))
  }

  # -- coefficients --

  if (!is.list(coefficients) || length(coefficients) == 0L)
    return("Error: coefficients must be a non-empty list.")

  expected_main <- "(Intercept)"
  for (v in all_indep) {
    nm    <- v$name
    vtype <- v$type
    if (identical(vtype, "continuous")) {
      expected_main <- c(expected_main, nm)
    } else if (identical(vtype, "binary")) {
      non_ref <- as.character(max(as.integer(names(v$categories))))
      expected_main <- c(expected_main, paste0(nm, v$categories[[non_ref]]))
    } else {
      ref <- as.character(v$reference)
      for (k in names(v$categories)) {
        if (k != ref) expected_main <- c(expected_main, paste0(nm, v$categories[[k]]))
      }
    }
  }

  seen_cols <- character(0)
  for (i in seq_along(coefficients)) {
    entry  <- coefficients[[i]]
    vars_i <- entry$vars
    if (is.null(vars_i) || length(vars_i) == 0L)
      return(paste0("Error: coefficients[[", i, "]] has empty 'vars'."))
    val <- suppressWarnings(as.numeric(entry$value))
    if (is.na(val))
      return(paste0("Error: coefficients[[", i, "]] value must be numeric. Got: ",
                    entry$value %||% "NULL"))
    if (length(vars_i) == 1L) {
      col <- .resolve_varref(vars_i[[1]], all_indep)
      if (startsWith(col, "__error__"))
        return(paste0("Error: coefficients[[", i, "]] ",
                      sub("^__error__: ", "", col)))
      nm_ref <- vars_i[[1]]$name %||% ""
      if (nm_ref != "(Intercept)") {
        spec_ref <- Find(function(x) identical(x$name, nm_ref), all_indep)
        if (!is.null(spec_ref) && spec_ref$type %in% c("binary", "categorical")) {
          lv_ref <- as.character(vars_i[[1]]$level %||% "")
          if (identical(lv_ref, as.character(spec_ref$reference %||% "")))
            return(paste0("Error: coefficients[[", i, "]] references reference ",
                          "category '", lv_ref, "' of variable '", nm_ref, "'."))
        }
      }
      if (col %in% seen_cols)
        return(paste0("Error: duplicate coefficient entry for '", col, "'."))
      seen_cols <- c(seen_cols, col)
    } else {
      parts <- character(length(vars_i))
      for (j in seq_along(vars_i)) {
        p_col <- .resolve_varref(vars_i[[j]], all_indep)
        if (startsWith(p_col, "__error__"))
          return(paste0("Error: coefficients[[", i, "]] interaction part ", j,
                        ": ", sub("^__error__: ", "", p_col)))
        parts[j] <- p_col
      }
      col <- paste(parts, collapse = ":")
      if (col %in% seen_cols)
        return(paste0("Error: duplicate coefficient entry for '", col, "'."))
      seen_cols <- c(seen_cols, col)
    }
  }

  missing_main <- setdiff(expected_main, seen_cols)
  if (length(missing_main) > 0L)
    return(paste0("Error: coefficients missing required main-effect entries: ",
                  paste(missing_main, collapse = ", ")))

  # -- residual_sd --

  if (identical(family, "identity+normal")) {
    if (is.null(residual_sd))
      return("Error: residual_sd is required for family 'identity+normal'.")
    rsd <- suppressWarnings(as.numeric(residual_sd))
    if (is.na(rsd) || rsd < 0)
      return(paste0("Error: residual_sd must be >= 0. Got: ", residual_sd))
    residual_sd <- rsd
  } else {
    if (!is.null(residual_sd))
      return(paste0("Error: residual_sd must be absent for family '", family, "'."))
  }

  # -- random_effects --

  re_err <- .validate_random_effects(random_effects, within_vars, between_names)
  if (!is.null(re_err)) return(re_err)

  # -- Non-fatal warnings --

  warns <- character(0)

  if (identical(family, "identity+normal") && length(within_vars) > 0L &&
      !is.null(residual_sd) && as.numeric(residual_sd) > 0)
    warns <- c(warns, paste0(
      "Warning: Consider setting residual_sd = 0 for designs with within-subjects ",
      "variables. The MANOVA approach captures all variance in random_effects."))

  # -- Write to disk --

  study_dir <- file.path(session_path, "studies", study_id)
  dir.create(study_dir, recursive = TRUE, showWarnings = FALSE)

  for (fname in c("data.csv", "regression.txt", "odds_ratios.txt")) {
    path <- file.path(study_dir, fname)
    if (file.exists(path)) file.remove(path)
  }

  design <- list(
    study_id        = study_id,
    title           = title,
    context         = context,
    family          = family,
    n_subjects      = n_subjects,
    n_reps          = n_reps,
    subject_id_name = subject_id_name,
    variables       = variables,
    correlations    = correlations,
    coefficients    = coefficients,
    residual_sd     = residual_sd,
    random_effects  = random_effects,
    status          = "design_saved"
  )
  jsonlite::write_json(design, file.path(study_dir, "study_design.json"),
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)

  msg <- paste0("Study design saved: ", study_id)
  if (length(warns) > 0L)
    msg <- paste(c(msg, warns), collapse = "\n")
  msg
}

#' Save study description and variables (Step 2 of the step-by-step workflow).
#'
#' Validates the core design fields and writes a partial \code{study_design.json}.
#' Subsequent calls to \code{save_correlations}, \code{save_coefficients},
#' \code{save_random_effects}, and \code{simulate_study} extend this file.
#'
#' @param title Study title string.
#' @param context Study context paragraph.
#' @param family One of \code{"identity+normal"}, \code{"logit+binomial"},
#'   \code{"log+poisson"}.
#' @param n_subjects Number of subjects (integer >= 2).
#' @param n_reps Repetitions per balanced within-subject condition (integer >= 1).
#' @param subject_id_name Column name for the subject ID (valid R identifier).
#' @param variables Named list with \code{dependent}, \code{between}, and
#'   \code{within} entries.
#' @param session_path Path to the session folder.
#' @param study_id Existing study ID to overwrite; \code{NULL} creates a new study.
#' @return The \code{study_id} string, or an error string starting with
#'   \code{"Error:"}.
#' @export
save_description <- function(title, context, family, n_subjects, n_reps = 1L,
                              subject_id_name, variables, session_path,
                              study_id = NULL) {

  if (!is.character(title) || length(title) != 1L || !nzchar(trimws(title)))
    return("Error: title must be a non-empty string.")
  if (!is.character(context) || length(context) != 1L || !nzchar(trimws(context)))
    return("Error: context must be a non-empty string.")
  if (!isTRUE(family %in% c("identity+normal", "logit+binomial", "log+poisson")))
    return(paste0("Error: family must be one of: 'identity+normal', ",
                  "'logit+binomial', 'log+poisson'. Got: '", family %||% "NULL", "'"))

  n_subjects <- suppressWarnings(as.integer(n_subjects))
  if (is.na(n_subjects) || n_subjects < 2L)
    return("Error: n_subjects must be an integer >= 2.")
  n_reps <- suppressWarnings(as.integer(n_reps))
  if (is.na(n_reps) || n_reps < 1L)
    return("Error: n_reps must be an integer >= 1.")
  if (!is.character(subject_id_name) || length(subject_id_name) != 1L ||
      !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", subject_id_name))
    return(paste0("Error: subject_id_name must be a valid R identifier. Got: '",
                  subject_id_name %||% "NULL", "'"))

  dep <- variables$dependent
  if (is.null(dep))
    return("Error: variables$dependent is missing.")
  for (field in c("name", "label", "type")) {
    if (is.null(dep[[field]]))
      return(paste0("Error: variables$dependent is missing field '", field, "'."))
  }
  if (!isTRUE(dep$type %in% c("continuous", "binary")))
    return(paste0("Error: dependent variable type must be 'continuous' or 'binary'. ",
                  "Got: '", dep$type, "'"))
  if (identical(family, "logit+binomial") && !identical(dep$type, "binary"))
    return("Error: family 'logit+binomial' requires a binary dependent variable.")
  if (family %in% c("identity+normal", "log+poisson") && !identical(dep$type, "continuous"))
    return(paste0("Error: family '", family, "' requires a continuous dependent variable."))

  between_vars <- if (!is.null(variables$between)) variables$between else list()
  within_vars  <- if (!is.null(variables$within))  variables$within  else list()
  all_indep    <- c(between_vars, within_vars)

  if (length(all_indep) == 0L)
    return("Error: at least one independent variable is required.")
  if (length(all_indep) > 10L)
    return(paste0("Error: at most 10 independent variables allowed. Got: ",
                  length(all_indep), "."))

  for (v in between_vars) {
    err <- .check_var(v, is_within = FALSE)
    if (!is.null(err)) return(err)
  }
  for (v in within_vars) {
    err <- .check_var(v, is_within = TRUE)
    if (!is.null(err)) return(err)
  }

  all_names <- vapply(all_indep, `[[`, character(1), "name")
  if (anyDuplicated(all_names) > 0L)
    return(paste0("Error: duplicate variable names: ",
                  paste(unique(all_names[duplicated(all_names)]), collapse = ", ")))
  if (subject_id_name %in% all_names)
    return(paste0("Error: subject_id_name '", subject_id_name,
                  "' conflicts with a variable name."))

  if (is.null(study_id)) study_id <- .make_study_id(title)

  study_dir <- file.path(session_path, "studies", study_id)
  dir.create(study_dir, recursive = TRUE, showWarnings = FALSE)

  for (fname in c("data.csv", "regression.txt", "odds_ratios.txt")) {
    path <- file.path(study_dir, fname)
    if (file.exists(path)) file.remove(path)
  }

  design <- list(
    study_id        = study_id,
    title           = title,
    context         = context,
    family          = family,
    n_subjects      = n_subjects,
    n_reps          = n_reps,
    subject_id_name = subject_id_name,
    variables       = variables,
    status          = "description_saved"
  )
  jsonlite::write_json(design, file.path(study_dir, "study_design.json"),
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)
  study_id
}

#' Save the predictor correlations for a study (Step 3 of the step-by-step workflow).
#'
#' Accepts correlations as a named-pair list \code{list(list(vars=c("A","B"), value=r), ...)}.
#' Between-subjects variables and within-continuous variables are eligible;
#' within-binary and within-categorical must not appear.
#'
#' @param study_id Study identifier string.
#' @param correlations List of \code{list(vars=list("A","B"), value=r)} entries.
#'   Use \code{list()} when there are no off-diagonal correlations.
#' @param session_path Path to the session folder.
#' @export
save_correlations <- function(study_id, correlations, session_path) {
  study_dir   <- file.path(session_path, "studies", study_id)
  design_file <- file.path(study_dir, "study_design.json")
  if (!file.exists(design_file))
    return(paste0("Error: study not found: ", study_id))

  design <- tryCatch(
    jsonlite::read_json(design_file),
    error = function(e) return(paste0("Error reading study_design.json: ", conditionMessage(e)))
  )
  if (is.character(design)) return(design)

  valid_statuses <- c("description_saved", "correlations_set", "coefficients_set", "simulated")
  if (!isTRUE(design$status %in% valid_statuses))
    return(paste0("Error: study '", study_id, "' has unexpected status '",
                  design$status %||% "unknown", "'. Save study description first."))

  if (!is.list(correlations))
    return("Error: correlations must be a list.")

  between_vars      <- design$variables$between %||% list()
  within_vars       <- design$variables$within  %||% list()
  within_cont       <- Filter(function(v) identical(v$type, "continuous"), within_vars)
  between_names     <- vapply(between_vars, `[[`, character(1), "name")
  within_cont_names <- if (length(within_cont) > 0L)
    vapply(within_cont, `[[`, character(1), "name") else character(0)
  eligible_names    <- c(between_names, within_cont_names)

  seen_pairs <- character(0)
  for (i in seq_along(correlations)) {
    entry <- correlations[[i]]
    vars2 <- entry$vars
    if (is.null(vars2) || length(vars2) != 2L)
      return(paste0("Error: correlations[[", i,
                    "]] must have exactly 2 variable names in 'vars'."))
    v1 <- as.character(vars2[[1]])
    v2 <- as.character(vars2[[2]])
    if (!isTRUE(v1 %in% eligible_names))
      return(paste0("Error: correlations[[", i, "]] variable '", v1,
                    "' is not a between-subjects or within-continuous variable."))
    if (!isTRUE(v2 %in% eligible_names))
      return(paste0("Error: correlations[[", i, "]] variable '", v2,
                    "' is not a between-subjects or within-continuous variable."))
    pair_key <- paste(sort(c(v1, v2)), collapse = "||")
    if (pair_key %in% seen_pairs)
      return(paste0("Error: duplicate correlation pair (", v1, ", ", v2, ")."))
    seen_pairs <- c(seen_pairs, pair_key)
    val <- suppressWarnings(as.numeric(entry$value))
    if (is.na(val) || val <= -1 || val >= 1)
      return(paste0("Error: correlations[[", i,
                    "]] value must be in (-1, 1). Got: ", entry$value %||% "NULL"))
  }

  nudged <- FALSE
  if (length(correlations) > 0L && length(eligible_names) >= 2L) {
    n_elig <- length(eligible_names)
    C_mat  <- diag(n_elig)
    for (entry in correlations) {
      i <- which(eligible_names == as.character(entry$vars[[1]]))
      j <- which(eligible_names == as.character(entry$vars[[2]]))
      C_mat[i, j] <- C_mat[j, i] <- as.numeric(entry$value)
    }
    min_eig <- min(eigen(C_mat, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eig < 0) {
      C_mat  <- C_mat + (abs(min_eig) + 0.01) * diag(n_elig)
      d      <- sqrt(diag(C_mat))
      C_mat  <- C_mat / outer(d, d)
      nudged <- TRUE
      new_corr <- list()
      for (entry in correlations) {
        i <- which(eligible_names == as.character(entry$vars[[1]]))
        j <- which(eligible_names == as.character(entry$vars[[2]]))
        new_corr <- c(new_corr, list(list(vars = entry$vars, value = C_mat[i, j])))
      }
      correlations <- new_corr
    }
  }

  design$coefficients <- NULL
  design$correlations <- correlations
  design$status       <- "correlations_set"
  jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  for (fname in c("data.csv", "regression.txt", "odds_ratios.txt")) {
    path <- file.path(study_dir, fname)
    if (file.exists(path)) file.remove(path)
  }

  nudge_note <- if (nudged) " (nudged to positive-definite)" else ""
  paste0("Correlations saved for study '", study_id, "'", nudge_note, ".")
}

#' Save regression coefficients and residual SD for a study (Step 4).
#'
#' Coefficients use the v2 named format: a list of
#' \code{list(vars=list(list(name, level?)), value)} entries, one per
#' fixed-effects term. \code{residual_sd} is a separate parameter.
#'
#' @param study_id Study identifier string.
#' @param coefficients List of \code{list(vars=list(...), value=b)} entries.
#' @param residual_sd Residual SD (>= 0) for \code{"identity+normal"};
#'   \code{NULL} for other families.
#' @param session_path Path to the session folder.
#' @export
save_coefficients <- function(study_id, coefficients, residual_sd = NULL,
                               session_path) {
  study_dir   <- file.path(session_path, "studies", study_id)
  design_file <- file.path(study_dir, "study_design.json")
  if (!file.exists(design_file))
    return(paste0("Error: study not found: ", study_id))

  design <- tryCatch(
    jsonlite::read_json(design_file),
    error = function(e) return(paste0("Error reading study_design.json: ", conditionMessage(e)))
  )
  if (is.character(design)) return(design)

  valid_statuses <- c("correlations_set", "coefficients_set", "simulated")
  if (!isTRUE(design$status %in% valid_statuses))
    return(paste0("Error: study '", study_id, "' has status '",
                  design$status %||% "unknown", "'. Save correlations first."))

  family <- design$family %||% ""
  if (!isTRUE(family %in% c("identity+normal", "logit+binomial", "log+poisson")))
    return(paste0("Error: unrecognized family '", family,
                  "'. Save study description first."))

  between_vars <- design$variables$between %||% list()
  within_vars  <- design$variables$within  %||% list()
  all_indep    <- c(between_vars, within_vars)

  if (!is.list(coefficients) || length(coefficients) == 0L)
    return("Error: coefficients must be a non-empty list.")

  expected_main <- "(Intercept)"
  for (v in all_indep) {
    nm    <- v$name
    vtype <- v$type
    if (identical(vtype, "continuous")) {
      expected_main <- c(expected_main, nm)
    } else if (identical(vtype, "binary")) {
      non_ref <- as.character(max(as.integer(names(v$categories))))
      expected_main <- c(expected_main, paste0(nm, v$categories[[non_ref]]))
    } else {
      ref <- as.character(v$reference)
      for (k in names(v$categories)) {
        if (k != ref) expected_main <- c(expected_main, paste0(nm, v$categories[[k]]))
      }
    }
  }

  seen_cols <- character(0)
  for (i in seq_along(coefficients)) {
    entry  <- coefficients[[i]]
    vars_i <- entry$vars
    if (is.null(vars_i) || length(vars_i) == 0L)
      return(paste0("Error: coefficients[[", i, "]] has empty 'vars'."))
    val <- suppressWarnings(as.numeric(entry$value))
    if (is.na(val))
      return(paste0("Error: coefficients[[", i, "]] value must be numeric. Got: ",
                    entry$value %||% "NULL"))
    if (length(vars_i) == 1L) {
      col <- .resolve_varref(vars_i[[1]], all_indep)
      if (startsWith(col, "__error__"))
        return(paste0("Error: coefficients[[", i, "]] ",
                      sub("^__error__: ", "", col)))
      nm_ref <- vars_i[[1]]$name %||% ""
      if (nm_ref != "(Intercept)") {
        spec_ref <- Find(function(x) identical(x$name, nm_ref), all_indep)
        if (!is.null(spec_ref) && spec_ref$type %in% c("binary", "categorical")) {
          lv_ref <- as.character(vars_i[[1]]$level %||% "")
          if (identical(lv_ref, as.character(spec_ref$reference %||% "")))
            return(paste0("Error: coefficients[[", i, "]] references reference ",
                          "category '", lv_ref, "' of variable '", nm_ref, "'."))
        }
      }
      if (col %in% seen_cols)
        return(paste0("Error: duplicate coefficient entry for '", col, "'."))
      seen_cols <- c(seen_cols, col)
    } else {
      parts <- character(length(vars_i))
      for (j in seq_along(vars_i)) {
        p_col <- .resolve_varref(vars_i[[j]], all_indep)
        if (startsWith(p_col, "__error__"))
          return(paste0("Error: coefficients[[", i, "]] interaction part ", j,
                        ": ", sub("^__error__: ", "", p_col)))
        parts[j] <- p_col
      }
      col <- paste(parts, collapse = ":")
      if (col %in% seen_cols)
        return(paste0("Error: duplicate coefficient entry for '", col, "'."))
      seen_cols <- c(seen_cols, col)
    }
  }

  missing_main <- setdiff(expected_main, seen_cols)
  if (length(missing_main) > 0L)
    return(paste0("Error: coefficients missing required main-effect entries: ",
                  paste(missing_main, collapse = ", ")))

  if (identical(family, "identity+normal")) {
    if (is.null(residual_sd))
      return("Error: residual_sd is required for family 'identity+normal'.")
    rsd <- suppressWarnings(as.numeric(residual_sd))
    if (is.na(rsd) || rsd < 0)
      return(paste0("Error: residual_sd must be >= 0. Got: ", residual_sd))
    residual_sd <- rsd
  } else {
    if (!is.null(residual_sd))
      return(paste0("Error: residual_sd must be absent for family '", family, "'."))
  }

  design$coefficients <- coefficients
  design$residual_sd  <- residual_sd
  design$status       <- "coefficients_set"
  jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  for (fname in c("data.csv", "regression.txt", "odds_ratios.txt")) {
    path <- file.path(study_dir, fname)
    if (file.exists(path)) file.remove(path)
  }

  paste0("Coefficients saved for study '", study_id, "'.")
}

#' Save random-effects variances and covariances for a study (Step 5).
#'
#' Stores a list of \code{\{vars, value\}} entries into \code{study_design.json}
#' and advances the status to \code{"random_effects_set"}.
#' Skip for purely between-subjects designs (call \code{simulate_study} directly
#' after \code{save_coefficients}).
#'
#' @param random_effects List of \code{list(vars=list(...), value=v)} entries.
#'   Each entry specifies a variance (one-element \code{vars}) or covariance
#'   (two-element \code{vars}).  Use \code{list()} for fixed-effects-only designs.
#' @param session_path Path to the session folder.
#' @param study_id Study identifier string.
#' @export
save_random_effects <- function(random_effects, session_path, study_id) {
  study_dir   <- file.path(session_path, "studies", study_id)
  design_file <- file.path(study_dir, "study_design.json")
  if (!file.exists(design_file))
    return(paste0("Error: study not found: ", study_id))

  design <- tryCatch(
    jsonlite::read_json(design_file),
    error = function(e) return(paste0("Error reading study_design.json: ", conditionMessage(e)))
  )
  if (is.character(design)) return(design)

  valid_statuses <- c("coefficients_set", "random_effects_set", "simulated")
  if (!isTRUE(design$status %in% valid_statuses))
    return(paste0("Error: study '", study_id, "' has status '",
                  design$status %||% "unknown", "'. Save coefficients first."))

  within_vars   <- design$variables$within  %||% list()
  between_names <- vapply(design$variables$between %||% list(), `[[`, character(1L), "name")

  re_err <- .validate_random_effects(random_effects, within_vars, between_names)
  if (!is.null(re_err)) return(re_err)

  design$random_effects <- random_effects
  design$status         <- "random_effects_set"
  jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  for (fname in c("data.csv", "regression.txt", "odds_ratios.txt")) {
    path <- file.path(study_dir, fname)
    if (file.exists(path)) file.remove(path)
  }

  paste0("Random effects saved for study '", study_id, "'.")
}

#' Run the R simulation for a study and save regression output artifacts.
#' @param study_id Study identifier string.
#' @param session_path Path to the session folder.
#' @export
simulate_study <- function(study_id, session_path) {
  study_dir   <- file.path(session_path, "studies", study_id)
  design_file <- file.path(study_dir, "study_design.json")
  if (!file.exists(design_file))
    return(paste0("Error: study not found: ", study_id))

  design <- tryCatch(
    jsonlite::read_json(design_file),
    error = function(e) return(paste0("Error reading study_design.json: ", conditionMessage(e)))
  )
  if (is.character(design)) return(design)

  ready_statuses <- c("design_saved", "coefficients_set", "random_effects_set", "simulated")
  if (!isTRUE(design$status %in% ready_statuses))
    return(paste0("Error: study '", study_id, "' has status '",
                  design$status %||% "unknown",
                  "'. Complete the study design (through save_coefficients or",
                  " save_random_effects) before simulating."))

  prev_status   <- design$status
  design$status <- "simulated"
  jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  csv_tmp <- tempfile(fileext = ".csv")
  result <- tryCatch(
    simulate_estimate(design, output_csv = csv_tmp),
    error = function(e) {
      design$status <- prev_status
      jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)
      list(error = conditionMessage(e))
    }
  )

  if (!is.null(result$error))
    return(paste0("Error: simulation failed:\n", result$error))

  writeLines(result$summary, file.path(study_dir, "regression.txt"))

  or_path <- file.path(study_dir, "odds_ratios.txt")
  if (!is.null(result$or_table)) {
    writeLines(result$or_table, or_path)
  } else if (file.exists(or_path)) {
    file.remove(or_path)
  }

  if (file.exists(csv_tmp)) {
    file.copy(csv_tmp, file.path(study_dir, "data.csv"), overwrite = TRUE)
    file.remove(csv_tmp)
  }

  parts <- c(
    paste0("Study saved as: ", study_id), "",
    "=== Regression Output ===", result$summary
  )
  if (!is.null(result$or_table))
    parts <- c(parts, "", "=== Odds Ratio Table ===", result$or_table)
  paste(parts, collapse = "\n")
}

#' Delete a study folder and all its simulation artifacts.
#' @param study_id Study identifier string.
#' @param session_path Path to the session folder.
#' @export
delete_study <- function(study_id, session_path) {
  study_dir <- file.path(session_path, "studies", study_id)
  if (!dir.exists(study_dir))
    return(paste0("Error: study not found: ", study_id))

  qset_dir <- file.path(session_path, "question_sets")
  linked   <- character(0)
  if (dir.exists(qset_dir)) {
    for (f in list.files(qset_dir, pattern = "\\.json$", full.names = TRUE)) {
      data <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
      if (!is.null(data) && identical(data$study_id, study_id))
        linked <- c(linked, tools::file_path_sans_ext(basename(f)))
    }
  }

  unlink(study_dir, recursive = TRUE)

  msg <- paste0("Study '", study_id, "' deleted.")
  if (length(linked) > 0L)
    msg <- paste0(msg, " Note: the following question sets still reference this study: ",
                  paste(linked, collapse = ", "), ".")
  msg
}

# Null-coalescing helper (base R doesn't have one)
`%||%` <- function(a, b) if (!is.null(a)) a else b
