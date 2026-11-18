library(RAISE)

# Helpers ----------------------------------------------------------------------

make_session <- function(path) {
  for (d in c("studies", "materials", "question_sets"))
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
}

base_design <- function(session_path,
                        study_id = "test_study_20260101_000000") {
  list(
    session_path    = session_path,
    study_id        = study_id,
    title           = "Sleep and Cognition",
    context         = "A study of sleep effects on test scores.",
    family          = "identity+normal",
    n_subjects      = 50L,
    n_reps          = 1L,
    subject_id_name = "participant_id",
    variables = list(
      dependent = list(name = "score", label = "Test score", type = "continuous"),
      between = list(
        list(name = "sleep_hrs", label = "Hours of sleep", type = "continuous",
             distribution = list(mean = 7, sd = 1.5)),
        list(name = "caffeine", label = "Caffeine use", type = "binary",
             categories = list("0" = "No", "1" = "Yes"),
             reference  = "0",
             proportion = list("0" = 0.6, "1" = 0.4))
      ),
      within = list()
    ),
    correlations = list(
      list(vars = list("sleep_hrs", "caffeine"), value = 0.2)
    ),
    coefficients = list(
      list(vars = list(list(name = "(Intercept)")),           value = 50),
      list(vars = list(list(name = "sleep_hrs")),             value =  3),
      list(vars = list(list(name = "caffeine", level = "1")), value =  2)
    ),
    residual_sd    = 10,
    random_effects = list(type = "custom", components = list())
  )
}

within_design <- function(session_path,
                          study_id = "within_study_20260101_000000") {
  list(
    session_path    = session_path,
    study_id        = study_id,
    title           = "Condition Effects Study",
    context         = "Subjects completed two task conditions.",
    family          = "identity+normal",
    n_subjects      = 30L,
    n_reps          = 1L,
    subject_id_name = "subj_id",
    variables = list(
      dependent = list(name = "rt_ms", label = "Reaction time", type = "continuous"),
      between = list(
        list(name = "sleep_hrs", label = "Hours of sleep", type = "continuous",
             distribution = list(mean = 7, sd = 1.5))
      ),
      within = list(
        list(name = "condition", label = "Task condition", type = "binary",
             categories = list("0" = "Single", "1" = "Dual"),
             reference = "0")
      )
    ),
    correlations = list(),
    coefficients = list(
      list(vars = list(list(name = "(Intercept)")),              value = 320),
      list(vars = list(list(name = "sleep_hrs")),                value = -12),
      list(vars = list(list(name = "condition", level = "1")),   value =  55)
    ),
    residual_sd    = 0,
    random_effects = list(type = "custom", components = list(
      list(vars = list(list(name = "(Intercept)")), value = 900)
    ))
  )
}


# save_study — happy path -----------------------------------------------

test_that("save_study creates study_design.json with status design_saved", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    do.call(save_study, args)
    path <- file.path(".", "studies", args$study_id, "study_design.json")
    expect_true(file.exists(path))
    design <- jsonlite::read_json(path)
    expect_equal(design$status, "design_saved")
    expect_equal(design$title,  "Sleep and Cognition")
    expect_equal(design$family, "identity+normal")
  })
})

test_that("save_study success message contains study_id", {
  withr::with_tempdir({
    make_session(".")
    msg <- do.call(save_study, base_design("."))
    expect_false(startsWith(msg, "Error"))
    expect_true(grepl("test_study_20260101_000000", msg))
  })
})

test_that("save_study redo clears prior simulation artifacts", {
  withr::with_tempdir({
    make_session(".")
    args      <- base_design(".")
    do.call(save_study, args)
    study_dir <- file.path(".", "studies", args$study_id)
    writeLines("dummy", file.path(study_dir, "data.csv"))
    writeLines("dummy", file.path(study_dir, "regression.txt"))
    do.call(save_study, args)
    expect_false(file.exists(file.path(study_dir, "data.csv")))
    expect_false(file.exists(file.path(study_dir, "regression.txt")))
  })
})

test_that("save_study accepts logit+binomial with binary outcome", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$family                       <- "logit+binomial"
    args$variables$dependent$type     <- "binary"
    args$residual_sd                  <- NULL
    args$coefficients[[1]]$value      <- 0
    args$coefficients[[2]]$value      <- 0.5
    args$coefficients[[3]]$value      <- 0.3
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})

test_that("save_study accepts within-subjects balanced binary (no proportion)", {
  withr::with_tempdir({
    make_session(".")
    msg <- do.call(save_study, within_design("."))
    expect_false(startsWith(msg, "Error"))
  })
})

test_that("save_study accepts within-subjects random binary with proportion", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$variables$within[[1]]$assignment <- "random"
    args$variables$within[[1]]$proportion <- list("0" = 0.5, "1" = 0.5)
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})

test_that("save_study accepts interaction term in coefficients", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$coefficients <- c(args$coefficients, list(
      list(vars = list(list(name = "sleep_hrs"),
                       list(name = "condition", level = "1")),
           value = 3)
    ))
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})

test_that("save_study accepts random slope for within-subjects variable", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$random_effects <- list(
      type = "custom",
      components = list(
        list(vars = list(list(name = "(Intercept)")),             value = 900),
        list(vars = list(list(name = "condition", level = "1")), value =  25),
        list(vars = list(list(name = "(Intercept)"),
                         list(name = "condition", level = "1")), value = -15)
      )
    )
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})


# save_study — structural errors ----------------------------------------

test_that("save_study rejects invalid family", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$family <- "linear"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("family", msg))
  })
})

test_that("save_study rejects n_subjects < 2", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$n_subjects <- 1L
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("n_subjects", msg))
  })
})

test_that("save_study rejects n_reps < 1", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$n_reps <- 0L
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("n_reps", msg))
  })
})

test_that("save_study rejects invalid subject_id_name", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$subject_id_name <- "1participant"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("subject_id_name", msg))
  })
})

test_that("save_study rejects empty independent variable lists", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between <- list()
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
  })
})

test_that("save_study rejects logit+binomial with continuous dependent", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$family <- "logit+binomial"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("binary dependent", msg))
  })
})


# save_study — variable errors ------------------------------------------

test_that("save_study rejects variable name > 10 characters", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[1]]$name <- "sleep_hours_long"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("10 characters", msg))
  })
})

test_that("save_study rejects duplicate variable names", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[2]]$name <- "sleep_hrs"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("duplicate", msg, ignore.case = TRUE))
  })
})

test_that("save_study rejects subject_id_name conflicting with variable name", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$subject_id_name <- "sleep_hrs"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("conflicts", msg))
  })
})

test_that("save_study rejects within-continuous variable with assignment field", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$variables$within <- list(
      list(name = "rt_within", label = "RT per trial", type = "continuous",
           distribution = list(mean = 300, sd = 50),
           assignment = "balanced")
    )
    args$coefficients <- list(
      list(vars = list(list(name = "(Intercept)")),  value = 320),
      list(vars = list(list(name = "sleep_hrs")),    value = -12),
      list(vars = list(list(name = "rt_within")),    value =   1)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("assignment", msg))
  })
})

test_that("save_study rejects within-balanced binary with proportion", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$variables$within[[1]]$proportion <- list("0" = 0.5, "1" = 0.5)
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("proportion", msg))
  })
})

test_that("save_study rejects between-subjects binary without proportion", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[2]]$proportion <- NULL
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("proportion", msg))
  })
})

test_that("save_study rejects categorical variable with fewer than 3 categories", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[2]] <- list(
      name = "grp", label = "Group", type = "categorical",
      categories = list("1" = "A", "2" = "B"),
      reference = "1",
      proportion = list("1" = 0.5, "2" = 0.5)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("3 categor", msg))
  })
})

test_that("save_study rejects continuous variable missing distribution", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[1]]$distribution <- NULL
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("distribution", msg))
  })
})

test_that("save_study rejects continuous variable with sd <= 0", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[1]]$distribution$sd <- 0
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("sd", msg))
  })
})

test_that("save_study rejects binary proportions not summing to 1", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[2]]$proportion <- list("0" = 0.5, "1" = 0.3)
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("1.0", msg))
  })
})

test_that("save_study accepts binary variable with 1/2 keys", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between[[2]] <- list(
      name = "caffeine", label = "Caffeine", type = "binary",
      categories = list("1" = "No", "2" = "Yes"),
      reference = "1",
      proportion = list("1" = 0.6, "2" = 0.4)
    )
    args$coefficients[[3]] <- list(
      vars = list(list(name = "caffeine", level = "2")), value = 2
    )
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})


# save_study — correlations errors --------------------------------------

test_that("save_study rejects correlation referencing within-binary variable", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$correlations <- list(
      list(vars = list("sleep_hrs", "condition"), value = 0.3)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("between-subjects or within-continuous", msg))
  })
})

test_that("save_study rejects duplicate correlation pair", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$correlations <- list(
      list(vars = list("sleep_hrs", "caffeine"), value =  0.2),
      list(vars = list("caffeine",  "sleep_hrs"), value = -0.1)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("duplicate", msg, ignore.case = TRUE))
  })
})

test_that("save_study rejects correlation value outside (-1, 1)", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$correlations[[1]]$value <- 1.0
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("\\(-1, 1\\)", msg))
  })
})

test_that("save_study rejects non-positive-definite correlations matrix", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$variables$between <- c(args$variables$between, list(
      list(name = "anxious", label = "Anxiety", type = "continuous",
           distribution = list(mean = 3, sd = 1))
    ))
    args$coefficients <- c(args$coefficients, list(
      list(vars = list(list(name = "anxious")), value = -1)
    ))
    args$correlations <- list(
      list(vars = list("sleep_hrs", "caffeine"),  value = -0.6),
      list(vars = list("sleep_hrs", "anxious"),   value = -0.6),
      list(vars = list("caffeine",  "anxious"),   value = -0.6)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("positive definite", msg, ignore.case = TRUE))
  })
})

test_that("save_study rejects unknown variable in correlations", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$correlations <- list(
      list(vars = list("sleep_hrs", "no_such_var"), value = 0.2)
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
  })
})


# save_study — coefficients errors --------------------------------------

test_that("save_study rejects missing (Intercept) in coefficients", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients <- args$coefficients[-1]
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("(Intercept)", msg, fixed = TRUE))
  })
})

test_that("save_study rejects missing required main-effect entry", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients <- args$coefficients[-2]
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("missing required", msg))
  })
})

test_that("save_study rejects reference category level in coefficients", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients[[3]] <- list(
      vars = list(list(name = "caffeine", level = "0")), value = 2
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("reference category", msg))
  })
})

test_that("save_study rejects non-numeric coefficient value", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients[[2]]$value <- "big"
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("numeric", msg))
  })
})

test_that("save_study rejects unknown variable name in coefficients", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients[[2]] <- list(
      vars = list(list(name = "no_such_var")), value = 3
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("not found", msg))
  })
})

test_that("save_study rejects unknown category key in coefficients", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients[[3]] <- list(
      vars = list(list(name = "caffeine", level = "9")), value = 2
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("no category key", msg))
  })
})

test_that("save_study rejects duplicate coefficient entry", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$coefficients <- c(args$coefficients, list(
      list(vars = list(list(name = "sleep_hrs")), value = 99)
    ))
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("duplicate", msg, ignore.case = TRUE))
  })
})

test_that("save_study rejects missing residual_sd for identity+normal", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$residual_sd <- NULL
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("residual_sd", msg))
  })
})

test_that("save_study rejects residual_sd < 0", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$residual_sd <- -1
    msg  <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("residual_sd", msg))
  })
})

test_that("save_study rejects residual_sd present for logit+binomial", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$family                   <- "logit+binomial"
    args$variables$dependent$type <- "binary"
    args$coefficients[[1]]$value  <- 0
    args$coefficients[[2]]$value  <- 0.5
    args$coefficients[[3]]$value  <- 0.3
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("residual_sd", msg))
  })
})


# save_study — random_effects errors ------------------------------------

test_that("save_study rejects between-subjects variable in random_effects", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    args$random_effects <- list(
      type = "custom",
      components = list(list(vars = list(list(name = "sleep_hrs")), value = 10))
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("between-subjects variable", msg))
  })
})

test_that("save_study rejects unknown variable in random_effects", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$random_effects <- list(
      type = "custom",
      components = list(list(vars = list(list(name = "no_such_var")), value = 10))
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("unknown variable", msg))
  })
})

test_that("save_study rejects negative variance in random_effects", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$random_effects <- list(
      type = "custom",
      components = list(list(vars = list(list(name = "(Intercept)")), value = -1))
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl(">= 0", msg))
  })
})

test_that("save_study rejects non-PSD random_effects matrix", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$random_effects <- list(
      type = "custom",
      components = list(
        list(vars = list(list(name = "(Intercept)")),             value =   1),
        list(vars = list(list(name = "condition", level = "1")), value =   1),
        list(vars = list(list(name = "(Intercept)"),
                         list(name = "condition", level = "1")), value =  10)
      )
    )
    msg <- do.call(save_study, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("semi-definite", msg))
  })
})


# save_study — non-fatal warnings ---------------------------------------

test_that("save_study warns when residual_sd > 0 with within-subjects variables", {
  withr::with_tempdir({
    make_session(".")
    args <- within_design(".")
    args$residual_sd <- 10
    msg  <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
    expect_true(grepl("Warning", msg))
    expect_true(grepl("residual_sd = 0", msg))
    path <- file.path(".", "studies", args$study_id, "study_design.json")
    expect_true(file.exists(path))
  })
})


# simulate_study ---------------------------------------------------------------

test_that("simulate_study produces regression output and sets status to simulated", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    id   <- args$study_id
    do.call(save_study, args)
    set.seed(77)
    msg <- simulate_study(id, ".")
    study_dir   <- file.path(".", "studies", id)
    design_path <- file.path(study_dir, "study_design.json")
    expect_true(file.exists(file.path(study_dir, "regression.txt")))
    expect_true(file.exists(file.path(study_dir, "data.csv")))
    expect_true(grepl("=== Regression Output ===", msg))
    design <- jsonlite::read_json(design_path)
    expect_equal(design$status, "simulated")
  })
})

test_that("simulate_study returns error for unrecognised status", {
  withr::with_tempdir({
    make_session(".")
    args        <- base_design(".")
    do.call(save_study, args)
    design_path <- file.path(".", "studies", args$study_id, "study_design.json")
    design      <- jsonlite::read_json(design_path)
    design$status <- "incomplete"
    jsonlite::write_json(design, design_path, pretty = TRUE, auto_unbox = TRUE)
    msg <- simulate_study(args$study_id, ".")
    expect_true(grepl("^Error", msg))
  })
})


# delete_study -----------------------------------------------------------------

test_that("delete_study removes the study folder", {
  withr::with_tempdir({
    make_session(".")
    args      <- base_design(".")
    do.call(save_study, args)
    study_dir <- file.path(".", "studies", args$study_id)
    expect_true(dir.exists(study_dir))
    msg <- delete_study(args$study_id, ".")
    expect_match(msg, "deleted")
    expect_false(dir.exists(study_dir))
  })
})

test_that("delete_study returns error for missing study", {
  withr::with_tempdir({
    make_session(".")
    msg <- delete_study("no_such_study_20260101_000000", ".")
    expect_match(msg, "Error")
  })
})

# save_description (v2) -------------------------------------------------------

# Helper for save_description tests
desc_args <- function(session_path, study_id = NULL) {
  args <- list(
    title           = "Effect of Sleep on Scores",
    context         = "A study on sleep and test performance.",
    family          = "identity+normal",
    n_subjects      = 50L,
    n_reps          = 1L,
    subject_id_name = "student_id",
    variables       = list(
      dependent = list(name = "score", label = "Test score", type = "continuous"),
      between   = list(
        list(name = "sleep_hrs", label = "Sleep hours", type = "continuous",
             distribution = list(mean = 7, sd = 1.5))
      ),
      within    = list()
    ),
    session_path    = session_path
  )
  if (!is.null(study_id)) args$study_id <- study_id
  args
}

test_that("save_description creates study_design.json with v2 fields and status description_saved", {
  withr::with_tempdir({
    make_session(".")
    id   <- do.call(save_description, desc_args("."))
    path <- file.path(".", "studies", id, "study_design.json")
    expect_true(file.exists(path))
    design <- jsonlite::read_json(path)
    expect_equal(design$status,          "description_saved")
    expect_equal(design$family,          "identity+normal")
    expect_equal(design$n_subjects,      50L)
    expect_equal(design$n_reps,          1L)
    expect_equal(design$subject_id_name, "student_id")
    expect_equal(design$title,           "Effect of Sleep on Scores")
    expect_null(design$correlations)
    expect_null(design$coefficients)
  })
})

test_that("save_description returns a study_id string containing the title slug", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    expect_true(is.character(id))
    expect_true(grepl("effect_of_sleep", id))
  })
})

test_that("save_description uses an explicit study_id when provided", {
  withr::with_tempdir({
    make_session(".")
    fixed_id    <- "my_study_20260101_000000"
    returned_id <- do.call(save_description, desc_args(".", study_id = fixed_id))
    expect_equal(returned_id, fixed_id)
    expect_true(file.exists(file.path(".", "studies", fixed_id, "study_design.json")))
  })
})

test_that("save_description redo clears prior simulation artifacts", {
  withr::with_tempdir({
    make_session(".")
    id        <- do.call(save_description, desc_args("."))
    study_dir <- file.path(".", "studies", id)
    writeLines("dummy", file.path(study_dir, "data.csv"))
    writeLines("dummy", file.path(study_dir, "regression.txt"))
    do.call(save_description, desc_args(".", study_id = id))
    expect_false(file.exists(file.path(study_dir, "data.csv")))
    expect_false(file.exists(file.path(study_dir, "regression.txt")))
  })
})

test_that("save_description rejects invalid family", {
  withr::with_tempdir({
    make_session(".")
    args         <- desc_args(".")
    args$family  <- "linear"
    msg          <- do.call(save_description, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("family", msg))
  })
})

test_that("save_description rejects n_subjects < 2", {
  withr::with_tempdir({
    make_session(".")
    args             <- desc_args(".")
    args$n_subjects  <- 1L
    msg              <- do.call(save_description, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("n_subjects", msg))
  })
})

test_that("save_description rejects invalid subject_id_name", {
  withr::with_tempdir({
    make_session(".")
    args                  <- desc_args(".")
    args$subject_id_name  <- "1bad"
    msg                   <- do.call(save_description, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("subject_id_name", msg))
  })
})

test_that("save_description rejects empty variable lists", {
  withr::with_tempdir({
    make_session(".")
    args                    <- desc_args(".")
    args$variables$between  <- list()
    msg                     <- do.call(save_description, args)
    expect_true(startsWith(msg, "Error"))
  })
})

test_that("save_description rejects logit+binomial with continuous dependent", {
  withr::with_tempdir({
    make_session(".")
    args         <- desc_args(".")
    args$family  <- "logit+binomial"
    msg          <- do.call(save_description, args)
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("binary dependent", msg))
  })
})


# save_correlations (v2 named-pair) --------------------------------------------

test_that("save_correlations (v2) saves named-pair correlations and advances status", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    msg <- save_correlations(
      study_id     = id,
      correlations = list(),
      session_path = "."
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$status, "correlations_set")
  })
})

test_that("save_correlations (v2) stores a named pair and clears coefficients", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    # Manually inject a stale coefficients field to confirm it gets cleared
    path   <- file.path(".", "studies", id, "study_design.json")
    design <- jsonlite::read_json(path)
    design$coefficients <- list(list(vars = list(list(name = "(Intercept)")), value = 1))
    jsonlite::write_json(design, path, pretty = TRUE, auto_unbox = TRUE)

    save_correlations(
      study_id     = id,
      correlations = list(list(vars = list("sleep_hrs", "sleep_hrs"), value = 0.3)),
      session_path = "."
    )
    design2 <- jsonlite::read_json(path)
    expect_null(design2$coefficients)
  })
})

test_that("save_correlations (v2) rejects within-binary variable", {
  withr::with_tempdir({
    make_session(".")
    # within design has a binary within variable
    id <- save_description(
      title = "Within Test", context = "Context.",
      family = "identity+normal", n_subjects = 30L, n_reps = 1L,
      subject_id_name = "subj_id",
      variables = list(
        dependent = list(name = "rt", label = "RT", type = "continuous"),
        between   = list(list(name = "age", label = "Age", type = "continuous",
                              distribution = list(mean = 25, sd = 5))),
        within    = list(list(name = "cond", label = "Condition", type = "binary",
                              categories = list("0" = "A", "1" = "B"),
                              reference = "0"))
      ),
      session_path = "."
    )
    msg <- save_correlations(
      study_id     = id,
      correlations = list(list(vars = list("age", "cond"), value = 0.2)),
      session_path = "."
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("between-subjects or within-continuous", msg))
  })
})


# save_coefficients (v2) -------------------------------------------------------

test_that("save_coefficients (v2) saves coefficients and residual_sd, advances status", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    save_correlations(id, list(), ".")
    msg <- save_coefficients(
      study_id     = id,
      coefficients = list(
        list(vars = list(list(name = "(Intercept)")),  value = 50),
        list(vars = list(list(name = "sleep_hrs")),    value =  3)
      ),
      residual_sd  = 10,
      session_path = "."
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$status,      "coefficients_set")
    expect_equal(design$residual_sd, 10)
    expect_length(design$coefficients, 2L)
  })
})

test_that("save_coefficients (v2) rejects missing (Intercept)", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    save_correlations(id, list(), ".")
    msg <- save_coefficients(
      study_id     = id,
      coefficients = list(list(vars = list(list(name = "sleep_hrs")), value = 3)),
      residual_sd  = 10,
      session_path = "."
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("(Intercept)", msg, fixed = TRUE))
  })
})

test_that("save_coefficients (v2) rejects residual_sd for logit+binomial", {
  withr::with_tempdir({
    make_session(".")
    id <- save_description(
      title = "Logistic Study", context = "Context.",
      family = "logit+binomial", n_subjects = 100L, n_reps = 1L,
      subject_id_name = "participant_id",
      variables = list(
        dependent = list(name = "pass", label = "Pass", type = "binary",
                         categories = list("0" = "No", "1" = "Yes"),
                         reference = "0"),
        between   = list(list(name = "study_hrs", label = "Study hours",
                              type = "continuous",
                              distribution = list(mean = 5, sd = 2))),
        within    = list()
      ),
      session_path = "."
    )
    save_correlations(id, list(), ".")
    msg <- save_coefficients(
      study_id     = id,
      coefficients = list(
        list(vars = list(list(name = "(Intercept)")), value = -1),
        list(vars = list(list(name = "study_hrs")),  value =  0.5)
      ),
      residual_sd  = 1,
      session_path = "."
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("residual_sd", msg))
  })
})


# simulate_study via step-by-step path ----------------------------------------

test_that("simulate_study succeeds from coefficients_set status (between-subjects)", {
  withr::with_tempdir({
    make_session(".")
    id <- do.call(save_description, desc_args("."))
    save_correlations(id, list(), ".")
    save_coefficients(
      study_id     = id,
      coefficients = list(
        list(vars = list(list(name = "(Intercept)")), value = 50),
        list(vars = list(list(name = "sleep_hrs")),   value =  3)
      ),
      residual_sd  = 10,
      session_path = "."
    )
    set.seed(42)
    msg <- simulate_study(id, ".")
    expect_false(grepl("^Error", msg))
    expect_true(grepl("=== Regression Output ===", msg))
    study_dir <- file.path(".", "studies", id)
    expect_true(file.exists(file.path(study_dir, "data.csv")))
    expect_true(file.exists(file.path(study_dir, "regression.txt")))
    design <- jsonlite::read_json(file.path(study_dir, "study_design.json"))
    expect_equal(design$status, "simulated")
  })
})


# save_random_effects ----------------------------------------------------------

# Helper: write a study_design.json at "coefficients_set" status
make_study_at_coefficients_set <- function(path, id = "re_study_20260101_000000") {
  study_dir <- file.path(path, "studies", id)
  dir.create(study_dir, recursive = TRUE, showWarnings = FALSE)
  design <- list(
    study_id  = id,
    title     = "RE Test Study",
    context   = "Context.",
    status    = "coefficients_set",
    variables = list(
      dependent = list(name = "score", label = "Score", type = "continuous"),
      between   = list(),
      within    = list(
        list(name = "cond", label = "Condition", type = "binary",
             categories = list("0" = "Control", "1" = "Treatment"),
             reference  = "0")
      )
    )
  )
  jsonlite::write_json(design, file.path(study_dir, "study_design.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  id
}

test_that("save_random_effects writes random_effects and sets status random_effects_set", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "sphericity", variance = 900),
      session_path   = ".",
      study_id       = id
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$status, "random_effects_set")
    expect_equal(design$random_effects$type, "sphericity")
  })
})

test_that("save_random_effects accepts fixed-effects-only design", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "custom", components = list()),
      session_path   = ".",
      study_id       = id
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$status, "random_effects_set")
  })
})

test_that("save_random_effects accepts custom type with variance + covariance forming a PSD matrix", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(
        type = "custom",
        components = list(
          list(vars = list(list(name = "(Intercept)")),              value = 900),
          list(vars = list(list(name = "cond", level = "1")),        value =  25),
          list(vars = list(list(name = "(Intercept)"),
                           list(name = "cond", level = "1")),        value = -10)
        )
      ),
      session_path = ".",
      study_id     = id
    )
    expect_false(startsWith(msg, "Error"))
  })
})

test_that("save_random_effects clears simulation artifacts", {
  withr::with_tempdir({
    make_session(".")
    id        <- make_study_at_coefficients_set(".")
    study_dir <- file.path(".", "studies", id)
    writeLines("dummy", file.path(study_dir, "data.csv"))
    writeLines("dummy", file.path(study_dir, "regression.txt"))
    save_random_effects(
      random_effects = list(type = "custom", components = list()),
      session_path   = ".",
      study_id       = id
    )
    expect_false(file.exists(file.path(study_dir, "data.csv")))
    expect_false(file.exists(file.path(study_dir, "regression.txt")))
  })
})

test_that("save_random_effects returns error for missing study", {
  withr::with_tempdir({
    make_session(".")
    msg <- save_random_effects(
      random_effects = list(type = "custom", components = list()),
      session_path   = ".",
      study_id       = "no_such_study_20260101_000000"
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("not found", msg))
  })
})

test_that("save_random_effects returns error when status is wrong", {
  withr::with_tempdir({
    make_session(".")
    id        <- "early_study_20260101_000000"
    study_dir <- file.path(".", "studies", id)
    dir.create(study_dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      list(study_id = id, status = "description_saved"),
      file.path(study_dir, "study_design.json"),
      pretty = TRUE, auto_unbox = TRUE
    )
    msg <- save_random_effects(
      random_effects = list(type = "custom", components = list()),
      session_path   = ".",
      study_id       = id
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("coefficients first", msg))
  })
})

test_that("save_random_effects rejects negative variance", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(
        type = "custom",
        components = list(list(vars = list(list(name = "(Intercept)")), value = -1))
      ),
      session_path = ".",
      study_id     = id
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl(">= 0", msg))
  })
})

test_that("save_random_effects rejects non-PSD random effects matrix", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(
        type = "custom",
        components = list(
          list(vars = list(list(name = "(Intercept)")),              value =  1),
          list(vars = list(list(name = "cond", level = "1")),        value =  1),
          list(vars = list(list(name = "(Intercept)"),
                           list(name = "cond", level = "1")),        value = 10)
        )
      ),
      session_path = ".",
      study_id     = id
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("semi-definite", msg))
  })
})


test_that("save_random_effects accepts sphericity dict form", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "sphericity", variance = 900),
      session_path   = ".",
      study_id       = id
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$status, "random_effects_set")
    expect_equal(design$random_effects$type, "sphericity")
  })
})

test_that("save_random_effects accepts compound_symmetry dict form", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "compound_symmetry", variance = 900, covariance = 200),
      session_path   = ".",
      study_id       = id
    )
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(file.path(".", "studies", id, "study_design.json"))
    expect_equal(design$random_effects$type, "compound_symmetry")
  })
})

test_that("save_random_effects accepts custom dict form", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(
        type       = "custom",
        components = list(list(vars = list(list(name = "(Intercept)")), value = 900))
      ),
      session_path = ".",
      study_id     = id
    )
    expect_false(startsWith(msg, "Error"))
  })
})


test_that("save_random_effects rejects invalid type", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "diagonal", variance = 900),
      session_path   = ".",
      study_id       = id
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("type", msg))
  })
})

test_that("save_random_effects rejects compound_symmetry violating covariance constraint", {
  withr::with_tempdir({
    make_session(".")
    id  <- make_study_at_coefficients_set(".")
    msg <- save_random_effects(
      random_effects = list(type = "compound_symmetry", variance = 100, covariance = 200),
      session_path   = ".",
      study_id       = id
    )
    expect_true(startsWith(msg, "Error"))
    expect_true(grepl("semi-definite", msg))
  })
})

test_that("save_study accepts sphericity dict for within design", {
  withr::with_tempdir({
    make_session(".")
    args                 <- within_design(".")
    args$random_effects  <- list(type = "sphericity", variance = 900)
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
    design <- jsonlite::read_json(
      file.path(".", "studies", args$study_id, "study_design.json"))
    expect_equal(design$random_effects$type, "sphericity")
  })
})

test_that("save_study accepts compound_symmetry dict for within design", {
  withr::with_tempdir({
    make_session(".")
    args                <- within_design(".")
    args$random_effects <- list(type = "compound_symmetry", variance = 900, covariance = 300)
    msg <- do.call(save_study, args)
    expect_false(startsWith(msg, "Error"))
  })
})


# delete_study (continued) -----------------------------------------------------

test_that("delete_study reports linked question sets", {
  withr::with_tempdir({
    make_session(".")
    args <- base_design(".")
    id   <- args$study_id
    do.call(save_study, args)
    qset <- list(
      id = "qs_linked", topic = "T", study_id = id,
      source_materials = list(),
      questions = list(list(type = "numeric", question = "Q?",
                            explanation = "E.", correct_answer = 1.0,
                            tolerance = 0.05)),
      model = "test", created_at = "2026-01-01T00:00:00Z"
    )
    jsonlite::write_json(qset,
                         file.path(".", "question_sets", "qs_linked.json"),
                         pretty = TRUE, auto_unbox = TRUE)
    msg <- delete_study(id, ".")
    expect_match(msg, "qs_linked")
  })
})
