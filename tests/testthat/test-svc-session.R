library(RAISE)

make_session <- function(path) {
  for (d in c("studies", "materials", "question_sets"))
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
}

make_study <- function(session_path, title = "Test Study",
                       family = "identity+normal", status = "design_saved") {
  slug   <- gsub("[^a-z0-9]+", "_", tolower(title))
  id     <- paste0(slug, "_20260101_000000")
  dep_type <- if (identical(family, "logit+binomial")) "binary" else "continuous"
  coefs  <- list(
    list(vars = list(list(name = "(Intercept)")), value = 0),
    list(vars = list(list(name = "x")),           value = 1)
  )
  rsd    <- if (identical(family, "identity+normal")) 1 else NULL
  save_study(
    session_path    = session_path,
    study_id        = id,
    title           = title,
    context         = "Some context.",
    family          = family,
    n_subjects      = 50L,
    n_reps          = 1L,
    subject_id_name = "subject_id",
    variables = list(
      dependent = list(name = "y", label = "Y", type = dep_type),
      between   = list(list(name = "x", label = "X", type = "continuous",
                            distribution = list(mean = 0, sd = 1))),
      within    = list()
    ),
    correlations   = list(),
    coefficients   = coefs,
    residual_sd    = rsd,
    random_effects = list(type = "custom", components = list())
  )
  if (!identical(status, "design_saved")) {
    design_file <- file.path(session_path, "studies", id, "study_design.json")
    design <- jsonlite::read_json(design_file)
    design$status <- status
    jsonlite::write_json(design, design_file, pretty = TRUE, auto_unbox = TRUE)
  }
  id
}

# init_session -----------------------------------------------------------------

test_that("init_session returns NULL for NULL or blank input", {
  expect_null(init_session(NULL))
  expect_null(init_session(""))
  expect_null(init_session("   "))
})

test_that("init_session creates all four subdirectories", {
  withr::with_tempdir({
    sess_path <- init_session(".")
    for (d in c("materials", "studies", "question_sets", "exports"))
      expect_true(dir.exists(file.path(sess_path, d)))
  })
})

test_that("init_session creates nested path recursively", {
  withr::with_tempdir({
    sess_path <- init_session("nested/deep/session")
    expect_true(dir.exists(sess_path))
    expect_true(dir.exists(file.path(sess_path, "studies")))
  })
})

test_that("init_session returns normalised absolute path", {
  withr::with_tempdir({
    sess_path <- init_session(".")
    expect_true(is.character(sess_path))
    expect_false(startsWith(sess_path, "."))
  })
})

test_that("init_session is idempotent", {
  withr::with_tempdir({
    init_session(".")
    expect_silent(init_session("."))
    expect_true(dir.exists(file.path(normalizePath("."), "studies")))
  })
})

# list_studies -----------------------------------------------------------------

test_that("list_studies returns [] when folder absent", {
  withr::with_tempdir({
    result <- jsonlite::fromJSON(list_studies("."))
    expect_length(result, 0L)
  })
})

test_that("list_studies lists one study with correct fields", {
  withr::with_tempdir({
    make_session(".")
    id     <- make_study(".", title = "Alpha Study")
    result <- jsonlite::fromJSON(list_studies("."), simplifyVector = FALSE)
    expect_length(result, 1L)
    expect_equal(result[[1]]$study_id, id)
    expect_equal(result[[1]]$title, "Alpha Study")
    expect_equal(result[[1]]$status, "design_saved")
    expect_equal(result[[1]]$family, "identity+normal")
  })
})

test_that("list_studies lists multiple studies", {
  withr::with_tempdir({
    make_session(".")
    make_study(".", title = "A")
    make_study(".", title = "B")
    result <- jsonlite::fromJSON(list_studies("."), simplifyVector = FALSE)
    expect_length(result, 2L)
  })
})

# list_materials ---------------------------------------------------------------

test_that("list_materials returns [] when folder absent", {
  withr::with_tempdir({
    result <- jsonlite::fromJSON(list_materials("."))
    expect_length(result, 0L)
  })
})

test_that("list_materials lists uploaded files", {
  withr::with_tempdir({
    make_session(".")
    writeLines("content", file.path(".", "materials", "lecture.pdf"))
    writeLines("content", file.path(".", "materials", "notes.md"))
    result <- jsonlite::fromJSON(list_materials("."))
    expect_true("lecture.pdf" %in% result)
    expect_true("notes.md"    %in% result)
  })
})

test_that("list_materials excludes hidden files", {
  withr::with_tempdir({
    make_session(".")
    writeLines("", file.path(".", "materials", ".DS_Store"))
    writeLines("x", file.path(".", "materials", "visible.txt"))
    result <- jsonlite::fromJSON(list_materials("."))
    expect_false(".DS_Store" %in% result)
    expect_true("visible.txt" %in% result)
  })
})

# load_study_context -----------------------------------------------------------

test_that("load_study_context returns error for missing study", {
  withr::with_tempdir({
    make_session(".")
    result <- load_study_context("no_such_study", ".")
    expect_true(grepl("not found", result))
  })
})

test_that("load_study_context returns JSON with null regression_output when not yet simulated", {
  withr::with_tempdir({
    make_session(".")
    id     <- make_study(".")
    result <- load_study_context(id, ".")
    parsed <- jsonlite::fromJSON(result)
    expect_true(is.null(parsed$regression_output))
  })
})

test_that("load_study_context returns full JSON after simulation", {
  withr::with_tempdir({
    make_session(".")
    id        <- make_study(".", title = "Memory Study")
    study_dir <- file.path(".", "studies", id)
    writeLines("Coefficients: (Intercept) 10, x 2", file.path(study_dir, "regression.txt"))
    result <- load_study_context(id, ".")
    parsed <- jsonlite::fromJSON(result)
    expect_equal(parsed$title, "Memory Study")
    expect_true(grepl("Intercept", parsed$regression_output))
    expect_null(parsed$odds_ratios_output)
  })
})

# list_question_sets / load_question_set ---------------------------------------

make_qset_file <- function(session_path, id = "q1") {
  qset <- list(
    id               = id,
    topic            = "Regression basics",
    study_id         = "study_abc",
    source_materials = list(),
    questions        = list(list(
      type           = "numeric",
      question       = "Q?",
      explanation    = "E.",
      correct_answer = 1.5,
      tolerance      = 0.08
    )),
    model      = "test",
    created_at = "2026-01-01T00:00:00Z"
  )
  qset_dir <- file.path(session_path, "question_sets")
  dir.create(qset_dir, showWarnings = FALSE)
  jsonlite::write_json(qset, file.path(qset_dir, paste0(id, ".json")),
                       pretty = TRUE, auto_unbox = TRUE)
  id
}

test_that("list_question_sets returns [] when folder absent", {
  withr::with_tempdir({
    result <- jsonlite::fromJSON(list_question_sets("."))
    expect_length(result, 0L)
  })
})

test_that("list_question_sets summarises existing sets", {
  withr::with_tempdir({
    make_session(".")
    make_qset_file(".", "qs1")
    result <- jsonlite::fromJSON(list_question_sets("."), simplifyVector = FALSE)
    expect_length(result, 1L)
    expect_equal(result[[1]]$id, "qs1")
    expect_equal(result[[1]]$n_questions, 1L)
  })
})

test_that("load_question_set returns error for missing set", {
  withr::with_tempdir({
    make_session(".")
    result <- load_question_set("no_such", ".")
    expect_true(grepl("not found", result))
  })
})

test_that("load_question_set returns formatted string", {
  withr::with_tempdir({
    make_session(".")
    make_qset_file(".", "qs2")
    result <- load_question_set("qs2", ".")
    expect_true(grepl("Regression basics", result))
    expect_true(grepl("Question 1", result))
  })
})

# delete_material --------------------------------------------------------------

test_that("delete_material removes the file and returns confirmation", {
  withr::with_tempdir({
    make_session(".")
    writeLines("content", file.path(".", "materials", "lecture.pdf"))
    msg <- delete_material("lecture.pdf", ".")
    expect_match(msg, "deleted")
    expect_false(file.exists(file.path(".", "materials", "lecture.pdf")))
  })
})

test_that("delete_material returns error for missing file", {
  withr::with_tempdir({
    make_session(".")
    msg <- delete_material("ghost.pdf", ".")
    expect_match(msg, "Error")
  })
})

# list_exports -----------------------------------------------------------------

make_export <- function(session_path, folder_name, destination = "pdf",
                        n_questions = 3L, label = "Test Export") {
  exp_dir <- file.path(session_path, "exports", folder_name)
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("", file.path(exp_dir, "output.pdf"))
  jsonlite::write_json(
    list(label = label, destination = destination, n_questions = n_questions,
         created_at = "2026-01-01T00:00:00"),
    file.path(exp_dir, "meta.json"), auto_unbox = TRUE
  )
  folder_name
}

test_that("list_exports returns [] when folder absent", {
  withr::with_tempdir({
    result <- jsonlite::fromJSON(list_exports("."))
    expect_length(result, 0L)
  })
})

test_that("list_exports lists exports with correct fields", {
  withr::with_tempdir({
    make_export(".", "test_export_20260101_120000", label = "Test Export")
    result <- jsonlite::fromJSON(list_exports("."), simplifyVector = FALSE)
    expect_length(result, 1L)
    expect_equal(result[[1]]$export_id, "test_export_20260101_120000")
    expect_equal(result[[1]]$label, "Test Export")
    expect_equal(result[[1]]$destination, "pdf")
    expect_equal(result[[1]]$n_questions, 3L)
    expect_true(result[[1]]$has_artifact)
  })
})

test_that("list_exports returns newest first", {
  withr::with_tempdir({
    make_export(".", "combined_20260101_090000")
    make_export(".", "combined_20260101_120000")
    result <- jsonlite::fromJSON(list_exports("."), simplifyVector = FALSE)
    expect_equal(result[[1]]$export_id, "combined_20260101_120000")
  })
})
