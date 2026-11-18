library(RAISE)

# ── Fixtures (schema v2) ──────────────────────────────────────────────────────

linear_between <- list(
  family = "identity+normal", n_subjects = 150L, n_reps = 1L,
  subject_id_name = "student_id",
  variables = list(
    dependent = list(name = "score", type = "continuous"),
    between   = list(
      list(name = "study_hrs", type = "continuous",
           distribution = list(mean = 5, sd = 2)),
      list(name = "gender", type = "binary",
           categories = list("0" = "Male", "1" = "Female"),
           reference = "0",
           proportion = list("0" = 0.5, "1" = 0.5))
    ),
    within = list()
  ),
  correlations = list(
    list(vars = list("study_hrs", "gender"), value = 0.1)
  ),
  coefficients = list(
    list(vars = list(list(name = "(Intercept)")),           value = 50),
    list(vars = list(list(name = "study_hrs")),             value =  3),
    list(vars = list(list(name = "gender", level = "1")),   value =  2)
  ),
  residual_sd    = 10,
  random_effects = list(type = "custom", components = list())
)

logistic_between <- list(
  family = "logit+binomial", n_subjects = 200L, n_reps = 1L,
  subject_id_name = "participant_id",
  variables = list(
    dependent = list(name = "pass", type = "binary"),
    between   = list(
      list(name = "study_hrs", type = "continuous",
           distribution = list(mean = 5, sd = 2))
    ),
    within = list()
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")), value =  0),
    list(vars = list(list(name = "study_hrs")),   value =  0.4)
  ),
  residual_sd    = NULL,
  random_effects = list(type = "custom", components = list())
)

poisson_between <- list(
  family = "log+poisson", n_subjects = 200L, n_reps = 1L,
  subject_id_name = "student_id",
  variables = list(
    dependent = list(name = "errors", type = "continuous"),
    between   = list(
      list(name = "study_hrs", type = "continuous",
           distribution = list(mean = 5, sd = 2))
    ),
    within = list()
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")), value =  1.5),
    list(vars = list(list(name = "study_hrs")),   value = -0.2)
  ),
  residual_sd    = NULL,
  random_effects = list(type = "custom", components = list())
)

# 60 subjects x 2 conditions (Single/Dual) = 120 obs
linear_within <- list(
  family = "identity+normal", n_subjects = 60L, n_reps = 1L,
  subject_id_name = "participant_id",
  variables = list(
    dependent = list(name = "rt_ms", type = "continuous"),
    between   = list(
      list(name = "sleep_hrs", type = "continuous",
           distribution = list(mean = 6.5, sd = 1.2))
    ),
    within = list(
      list(name = "condition", type = "binary",
           categories = list("0" = "Single", "1" = "Dual"),
           reference  = "0")
    )
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")),              value = 320),
    list(vars = list(list(name = "sleep_hrs")),                value = -12),
    list(vars = list(list(name = "condition", level = "1")),   value =  55)
  ),
  residual_sd    = 40,
  random_effects = list(type = "custom", components = list(
    list(vars = list(list(name = "(Intercept)")), value = 900)
  ))
)

# 60 subjects x 2 conditions (Placebo/Drug) = 120 obs
logistic_within <- list(
  family = "logit+binomial", n_subjects = 60L, n_reps = 1L,
  subject_id_name = "patient_id",
  variables = list(
    dependent = list(name = "response", type = "binary"),
    between   = list(),
    within    = list(
      list(name = "treatment", type = "binary",
           categories = list("0" = "Placebo", "1" = "Drug"),
           reference  = "0")
    )
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")),              value = -0.5),
    list(vars = list(list(name = "treatment", level = "1")),   value =  1.0)
  ),
  residual_sd    = NULL,
  random_effects = list(type = "custom", components = list(
    list(vars = list(list(name = "(Intercept)")), value = 0.5)
  ))
)

# 3-level categorical within: 40 subjects x 3 time points = 120 obs
linear_within_cat3 <- list(
  family = "identity+normal", n_subjects = 40L, n_reps = 1L,
  subject_id_name = "subj_id",
  variables = list(
    dependent = list(name = "score", type = "continuous"),
    between   = list(),
    within    = list(
      list(name = "time", type = "categorical",
           categories = list("1" = "T1", "2" = "T2", "3" = "T3"),
           reference  = "1")
    )
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")),           value = 50),
    list(vars = list(list(name = "time", level = "2")),     value =  5),
    list(vars = list(list(name = "time", level = "3")),     value =  9)
  ),
  residual_sd    = 0,
  random_effects = list(type = "custom", components = list(
    list(vars = list(list(name = "(Intercept)")), value = 100)
  ))
)

# Random-assignment within-subjects: 100 subjects x n_reps = 100 obs (no expand.grid)
linear_within_random <- list(
  family = "identity+normal", n_subjects = 100L, n_reps = 1L,
  subject_id_name = "subj_id",
  variables = list(
    dependent = list(name = "score", type = "continuous"),
    between   = list(
      list(name = "age", type = "continuous",
           distribution = list(mean = 35, sd = 8))
    ),
    within    = list(
      list(name = "trial_type", type = "binary",
           categories = list("0" = "Easy", "1" = "Hard"),
           reference  = "0",
           assignment = "random",
           proportion = list("0" = 0.5, "1" = 0.5))
    )
  ),
  correlations   = list(),
  coefficients   = list(
    list(vars = list(list(name = "(Intercept)")),                  value = 70),
    list(vars = list(list(name = "age")),                          value = -0.5),
    list(vars = list(list(name = "trial_type", level = "1")),      value = -5)
  ),
  residual_sd    = 8,
  random_effects = list(type = "custom", components = list())
)


# ── simulate_estimate() — between-subjects ────────────────────────────────

test_that("simulate_estimate returns expected list structure for linear between", {
  withr::with_tempdir({
    set.seed(42)
    result <- simulate_estimate(linear_between, output_csv = "sim.csv")
    expect_type(result, "list")
    expect_named(result, c("summary", "or_table", "csv_path"))
    expect_type(result$summary, "character")
    expect_null(result$or_table)
    expect_true(file.exists(result$csv_path))
  })
})

test_that("simulate_estimate CSV has correct columns and row count for linear between", {
  withr::with_tempdir({
    set.seed(1)
    result <- simulate_estimate(linear_between, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_true("score"      %in% names(df))
    expect_true("study_hrs"  %in% names(df))
    expect_true("gender"     %in% names(df))
    expect_true("student_id" %in% names(df))
    expect_equal(nrow(df), 150L)
  })
})

test_that("simulate_estimate summary contains coefficient names for linear between", {
  withr::with_tempdir({
    set.seed(3)
    result <- simulate_estimate(linear_between, output_csv = "sim.csv")
    expect_true(grepl("study_hrs",    result$summary))
    expect_true(grepl("genderFemale", result$summary))
  })
})

test_that("simulate_estimate returns or_table for logistic between", {
  withr::with_tempdir({
    set.seed(7)
    result <- simulate_estimate(logistic_between, output_csv = "sim.csv")
    expect_false(is.null(result$or_table))
    expect_true(grepl("OR", result$or_table))
  })
})

test_that("simulate_estimate logistic between: n_obs = n_subjects", {
  withr::with_tempdir({
    set.seed(8)
    result <- simulate_estimate(logistic_between, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 200L)
    expect_true("participant_id" %in% names(df))
  })
})

test_that("simulate_estimate returns correct structure for Poisson between", {
  withr::with_tempdir({
    set.seed(9)
    result <- simulate_estimate(poisson_between, output_csv = "sim.csv")
    expect_type(result, "list")
    expect_named(result, c("summary", "or_table", "csv_path"))
    expect_null(result$or_table)
    df <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 200L)
    expect_true(all(df$errors >= 0))
  })
})


# ── simulate_estimate() — within-subjects ────────────────────────────────────

test_that("simulate_estimate afex/lmerTest: n_obs = n_subjects x n_conditions", {
  withr::with_tempdir({
    set.seed(11)
    result <- simulate_estimate(linear_within, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 120L)
  })
})

test_that("simulate_estimate afex/lmerTest: subject_id in CSV", {
  withr::with_tempdir({
    set.seed(12)
    result <- simulate_estimate(linear_within, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_true("participant_id" %in% names(df))
    expect_equal(length(unique(df$participant_id)), 60L)
  })
})

test_that("simulate_estimate afex: summary contains Anova Table for within identity+normal n_reps=1", {
  withr::with_tempdir({
    set.seed(13)
    result <- simulate_estimate(linear_within, output_csv = "sim.csv")
    expect_true(grepl("Anova Table", result$summary))
  })
})

test_that("simulate_estimate afex/lmerTest: or_table is NULL for identity+normal", {
  withr::with_tempdir({
    set.seed(14)
    result <- suppressWarnings(simulate_estimate(linear_within, output_csv = "sim.csv"))
    expect_null(result$or_table)
  })
})

test_that("simulate_estimate glmer: or_table not NULL for logit+binomial", {
  withr::with_tempdir({
    set.seed(15)
    result <- suppressWarnings(simulate_estimate(logistic_within, output_csv = "sim.csv"))
    expect_false(is.null(result$or_table))
    expect_true(grepl("OR", result$or_table))
  })
})

test_that("simulate_estimate glmer: summary contains random effects output", {
  withr::with_tempdir({
    set.seed(16)
    result <- suppressWarnings(simulate_estimate(logistic_within, output_csv = "sim.csv"))
    expect_true(grepl("Random effects", result$summary))
  })
})

test_that("simulate_estimate glmer: n_obs = n_subjects x n_conditions", {
  withr::with_tempdir({
    set.seed(17)
    result <- suppressWarnings(simulate_estimate(logistic_within, output_csv = "sim.csv"))
    df     <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 120L)
  })
})


# ── simulate_estimate() — v2-specific features ───────────────────────────

test_that("3-level within-subjects categorical: n_obs and Anova Table output correct", {
  withr::with_tempdir({
    set.seed(20)
    result <- simulate_estimate(linear_within_cat3, output_csv = "sim.csv")
    df <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 120L)   # 40 subjects x 3 time points
    expect_true("time"    %in% names(df))
    expect_true("score"   %in% names(df))
    expect_true("subj_id" %in% names(df))
    expect_true(grepl("Anova Table", result$summary))
    expect_true(grepl("time",        result$summary))
  })
})

test_that("randomly-assigned within variable: n_obs = n_subjects x n_reps", {
  withr::with_tempdir({
    set.seed(21)
    result <- simulate_estimate(linear_within_random, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 100L)   # NOT 200 — random assignment, no expand.grid
    expect_true("trial_type" %in% names(df))
    expect_true("age"        %in% names(df))
  })
})

test_that("randomly-assigned within variable: both levels appear in data", {
  withr::with_tempdir({
    set.seed(22)
    result <- simulate_estimate(linear_within_random, output_csv = "sim.csv")
    df     <- utils::read.csv(result$csv_path)
    expect_true(all(c("Easy", "Hard") %in% df$trial_type))
  })
})

test_that("lmerTest: n_reps>1 residual_sd>0 dispatches to lmerTest with p-values", {
  design_with_slope <- linear_within
  design_with_slope$n_reps <- 2L   # 60 subjects x 2 conditions x 2 reps = 240 obs
  design_with_slope$random_effects <- list(
    type = "custom",
    components = list(
      list(vars = list(list(name = "(Intercept)")),             value = 400),
      list(vars = list(list(name = "condition", level = "1")), value = 100)
    )
  )
  withr::with_tempdir({
    set.seed(23)
    result <- suppressWarnings(
      simulate_estimate(design_with_slope, output_csv = "sim.csv")
    )
    expect_true(grepl("Random effects",    result$summary))
    expect_true(grepl("conditionDual",     result$summary))
    expect_true(grepl("Pr(>|t|)", result$summary, fixed = TRUE))
  })
})

test_that("coefficients v2 mapped correctly: model coef names match expected", {
  withr::with_tempdir({
    set.seed(24)
    result <- simulate_estimate(linear_between, output_csv = "sim.csv")
    expect_true(grepl("genderFemale", result$summary))
    expect_true(grepl("study_hrs",    result$summary))
  })
})

test_that("residual_sd = 0 for within-subjects: dispatches to afex (Anova Table)", {
  design_zero_rsd <- linear_within
  design_zero_rsd$residual_sd    <- 0
  design_zero_rsd$random_effects <- list(
    type = "custom",
    components = list(list(vars = list(list(name = "(Intercept)")), value = 900))
  )
  withr::with_tempdir({
    set.seed(25)
    result <- simulate_estimate(design_zero_rsd, output_csv = "sim.csv")
    expect_false(startsWith(result$summary, "Error"))
    expect_true(grepl("Anova Table", result$summary))
  })
})

test_that("empty between list (pure within): n_obs = n_subjects x n_conditions", {
  withr::with_tempdir({
    set.seed(26)
    result <- suppressWarnings(simulate_estimate(logistic_within, output_csv = "sim.csv"))
    df     <- utils::read.csv(result$csv_path)
    expect_equal(nrow(df), 120L)
    expect_false("between_var" %in% names(df))
  })
})


# ── simulate_study() integration (end-to-end with v2 schema) ─────────────────

test_that("simulate_study produces regression.txt and data.csv from v2 design", {
  withr::with_tempdir({
    for (d in c("studies", "materials", "question_sets"))
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    id <- "sleep_study_20260101_000000"
    save_study(
      session_path    = ".",
      study_id        = id,
      title           = "Sleep and Reaction Time",
      context         = "Subjects completed tasks under two conditions.",
      family          = "identity+normal",
      n_subjects      = 40L,
      n_reps          = 1L,
      subject_id_name = "subj_id",
      variables = list(
        dependent = list(name = "rt_ms", label = "RT", type = "continuous"),
        between   = list(
          list(name = "sleep_hrs", label = "Sleep", type = "continuous",
               distribution = list(mean = 7, sd = 1))
        ),
        within = list(
          list(name = "condition", label = "Condition", type = "binary",
               categories = list("0" = "Control", "1" = "Task"),
               reference  = "0")
        )
      ),
      correlations   = list(),
      coefficients   = list(
        list(vars = list(list(name = "(Intercept)")),              value = 300),
        list(vars = list(list(name = "sleep_hrs")),                value = -10),
        list(vars = list(list(name = "condition", level = "1")),   value =  40)
      ),
      residual_sd    = 0,
      random_effects = list(type = "custom", components = list(
        list(vars = list(list(name = "(Intercept)")), value = 625)
      ))
    )
    set.seed(99)
    msg       <- suppressWarnings(simulate_study(id, "."))
    study_dir <- file.path(".", "studies", id)
    expect_true(file.exists(file.path(study_dir, "regression.txt")))
    expect_true(file.exists(file.path(study_dir, "data.csv")))
    expect_true(grepl("=== Regression Output ===", msg))
    design <- jsonlite::read_json(file.path(study_dir, "study_design.json"))
    expect_equal(design$status, "simulated")
  })
})


# ── estimate() ───────────────────────────────────────────────────────────────

test_that("estimate returns correct structure for identity+normal between", {
  withr::with_tempdir({
    set.seed(20)
    sim    <- simulate(linear_between, output_csv = "data.csv")
    result <- estimate(linear_between, sim$df)
    expect_named(result, c("summary", "or_table"))
    expect_type(result$summary, "character")
    expect_null(result$or_table)
  })
})

test_that("estimate returns or_table for logit+binomial between", {
  withr::with_tempdir({
    set.seed(21)
    sim    <- simulate(logistic_between, output_csv = "data.csv")
    result <- estimate(logistic_between, sim$df)
    expect_false(is.null(result$or_table))
    expect_true(grepl("OR", result$or_table))
  })
})


# .build_re_matrix — new random_effects schema (R38) -------------------------

binary_within_vars <- list(
  list(name = "condition", type = "binary",
       categories = list("0" = "Single", "1" = "Dual"), reference = "0")
)

cat3_within_vars <- list(
  list(name = "time", type = "categorical",
       categories = list("1" = "T1", "2" = "T2", "3" = "T3"), reference = "1")
)

test_that(".build_re_matrix sphericity 1 binary factor: 2x2, Cov(y) ~ σ²I", {
  sigma_u <- RAISE:::.build_re_matrix(
    list(type = "sphericity", variance = 900),
    binary_within_vars
  )
  expect_equal(dim(sigma_u), c(2L, 2L))
  M   <- model.matrix(~ condition,
                      data = expand.grid(condition = factor(c("Single","Dual"),
                                                            levels = c("Single","Dual"))))
  cov_y <- M %*% sigma_u %*% t(M)
  expect_equal(unname(cov_y), diag(900, 2), tolerance = 1e-10)
})

test_that(".build_re_matrix compound_symmetry 3-level factor: Cov(y) is CS", {
  sigma_u <- RAISE:::.build_re_matrix(
    list(type = "compound_symmetry", variance = 400, covariance = 100),
    cat3_within_vars
  )
  expect_equal(dim(sigma_u), c(3L, 3L))
  M <- model.matrix(~ time,
                    data = expand.grid(time = factor(c("T1","T2","T3"),
                                                     levels = c("T1","T2","T3"))))
  cov_y <- unname(M %*% sigma_u %*% t(M))
  expect_equal(diag(cov_y), rep(400, 3), tolerance = 1e-10)
  expect_equal(cov_y[1,2], 100, tolerance = 1e-10)
  expect_equal(cov_y[1,3], 100, tolerance = 1e-10)
})

test_that(".build_re_matrix sphericity + continuous slope via components: (k+1)x(k+1)", {
  sigma_u <- RAISE:::.build_re_matrix(
    list(type = "sphericity", variance = 900,
         components = list(list(vars = list(list(name = "x")), value = 25))),
    c(binary_within_vars,
      list(list(name = "x", type = "continuous",
                distribution = list(mean = 0, sd = 1))))
  )
  expect_equal(dim(sigma_u), c(3L, 3L))
  expect_equal(sigma_u["x", "x"], 25)
})

test_that(".build_re_matrix throws for non-PSD matrix (custom)", {
  expect_error(
    RAISE:::.build_re_matrix(
      list(type = "custom", components = list(
        list(vars = list(list(name = "(Intercept)")), value = 1),
        list(vars = list(list(name = "condition", level = "1")), value = 1),
        list(vars = list(list(name = "(Intercept)"),
                         list(name = "condition", level = "1")), value = 10)
      )),
      binary_within_vars
    ),
    "semi-definite"
  )
})


test_that(".build_re_matrix sphericity smoke: simulate_estimate completes without error", {
  withr::with_tempdir({
    set.seed(99)
    design <- linear_within
    design$random_effects <- list(type = "sphericity", variance = 900)
    sim    <- simulate(design, output_csv = "data.csv")
    result <- estimate(design, sim$df)
    expect_type(result$summary, "character")
  })
})

test_that(".build_re_matrix compound_symmetry smoke: simulate_estimate completes without error", {
  withr::with_tempdir({
    set.seed(100)
    design <- linear_within
    design$random_effects <- list(type = "compound_symmetry", variance = 900, covariance = 200)
    sim    <- simulate(design, output_csv = "data.csv")
    result <- estimate(design, sim$df)
    expect_type(result$summary, "character")
  })
})

test_that("continuous predictors keep their designed mean and SD; intercept is at the means", {
  withr::with_tempdir({
    set.seed(11)
    d <- linear_between
    d$n_subjects <- 4000L
    d$variables$between[[1]]$distribution <- list(mean = 15, sd = 5)
    result <- simulate_estimate(d, output_csv = "sim.csv")
    df <- utils::read.csv(result$csv_path)
    expect_equal(mean(df$study_hrs), 15, tolerance = 0.02)
    expect_equal(sd(df$study_hrs), 5, tolerance = 0.03)
    # outcome at the mean of study_hrs (gender = Male) is the designed intercept
    df$gender <- factor(df$gender, levels = c("Male", "Female"))
    fit <- stats::lm(score ~ I(study_hrs - 15) + gender, data = df)
    expect_equal(unname(coef(fit)[1]), 50, tolerance = 0.02)
    expect_equal(unname(coef(fit)[2]), 3, tolerance = 0.03)
  })
})
