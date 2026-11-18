library(RAISE)

# ── Mock chat object ─────────────────────────────────────────────────────────

make_mock_chat <- function() {
  env <- new.env(parent = emptyenv())
  env$tools <- list()
  env$register_tool <- function(tool_def) {
    env$tools <- c(env$tools, list(tool_def))
    invisible(env)
  }
  env
}

# Wrap make_chat with a mocked chat_anthropic that never calls the API
with_mock_chat <- function(session_path, code) {
  mock <- make_mock_chat()
  local_mocked_bindings(
    chat_anthropic = function(...) mock,
    .package = "ellmer"
  )
  session_env <- new.env(parent = emptyenv())
  session_env$path <- session_path
  chat <- make_chat(session_env, "test prompt")
  force(code)
  list(chat = chat, mock = mock)
}

# ── Tool registration ─────────────────────────────────────────────────────────

test_that("make_chat registers exactly 31 tools", {
  withr::with_tempdir({
    dir.create("s/studies",       recursive = TRUE, showWarnings = FALSE)
    dir.create("s/materials",     showWarnings = FALSE)
    dir.create("s/question_sets", showWarnings = FALSE)
    res <- with_mock_chat("s", NULL)
    expect_length(res$mock$tools, 31L)
  })
})

test_that("make_chat registers tools with correct names", {
  withr::with_tempdir({
    dir.create("s/studies",       recursive = TRUE, showWarnings = FALSE)
    dir.create("s/materials",     showWarnings = FALSE)
    dir.create("s/question_sets", showWarnings = FALSE)
    res <- with_mock_chat("s", NULL)
    tool_names <- vapply(res$mock$tools, function(t) attr(t, "name"), character(1))
    expected <- c(
      "save_description", "save_correlations", "save_coefficients",
      "save_random_effects", "save_study", "simulate_study",
      "load_material", "search_materials", "list_rags", "set_rag_active", "delete_rags",
      "load_study_context", "save_question_set", "list_materials", "list_studies",
      "list_question_sets", "load_question_set", "remove_questions", "update_questions",
      "add_questions", "delete_question_sets", "delete_materials", "delete_studies",
      "list_exports", "delete_exports", "export_question_sets",
      "list_figures", "view_figure", "create_figure", "replace_figure", "delete_figures"
    )
    expect_setequal(tool_names, expected)
  })
})

# ── Session guard ─────────────────────────────────────────────────────────────

# Helper: extract a registered tool by name from the mock
get_tool <- function(mock, name) {
  for (t in mock$tools) {
    if (identical(attr(t, "name"), name)) return(t)
  }
  stop("tool not found: ", name)
}

NO_SESSION_MSG <- "No session folder is active. Please open a session folder first."

test_that("tools return no-session warning when session_path is NULL", {
  withr::with_tempdir({
    res <- with_mock_chat(NULL, NULL)
    t   <- get_tool(res$mock, "list_studies")
    expect_equal(t(), NO_SESSION_MSG)
  })
})

test_that("tools return no-session warning when session directory does not exist", {
  withr::with_tempdir({
    res <- with_mock_chat("/nonexistent/path/99", NULL)
    t   <- get_tool(res$mock, "list_materials")
    expect_equal(t(), NO_SESSION_MSG)
  })
})

test_that("list_studies tool returns JSON when session is valid", {
  withr::with_tempdir({
    dir.create("s/studies",       recursive = TRUE, showWarnings = FALSE)
    dir.create("s/materials",     showWarnings = FALSE)
    dir.create("s/question_sets", showWarnings = FALSE)
    res <- with_mock_chat("s", NULL)
    t   <- get_tool(res$mock, "list_studies")
    result <- t()
    expect_false(identical(result, NO_SESSION_MSG))
    parsed <- jsonlite::fromJSON(result)
    expect_length(parsed, 0L)
  })
})

test_that("list_materials tool returns JSON when session is valid", {
  withr::with_tempdir({
    dir.create("s/studies",       recursive = TRUE, showWarnings = FALSE)
    dir.create("s/materials",     showWarnings = FALSE)
    dir.create("s/question_sets", showWarnings = FALSE)
    writeLines("content", "s/materials/notes.md")
    res <- with_mock_chat("s", NULL)
    t   <- get_tool(res$mock, "list_materials")
    result  <- t()
    parsed  <- jsonlite::fromJSON(result)
    expect_true("notes.md" %in% parsed)
  })
})

test_that("save_study tool creates study when session is valid", {
  withr::with_tempdir({
    dir.create("s/studies",       recursive = TRUE, showWarnings = FALSE)
    dir.create("s/materials",     showWarnings = FALSE)
    dir.create("s/question_sets", showWarnings = FALSE)
    res <- with_mock_chat("s", NULL)
    t   <- get_tool(res$mock, "save_study")
    result <- t(
      study_id        = "test_study_20260519_120000",
      title           = "Test Study",
      context         = "A test study examining the effect of x on y.",
      family          = "identity+normal",
      n_subjects      = 10L,
      n_reps          = 1L,
      subject_id_name = "subject_id",
      variables = list(
        dependent = list(name = "y", label = "Outcome", type = "continuous"),
        between   = list(list(name = "x", label = "Predictor", type = "continuous",
                              distribution = list(mean = 0, sd = 1))),
        within    = list()
      ),
      correlations   = list(),
      coefficients   = list(
        list(vars = list(list(name = "(Intercept)")), value = 0),
        list(vars = list(list(name = "x")),           value = 1)
      ),
      residual_sd    = 1,
      random_effects = list(type = "custom", components = list())
    )
    expect_false(startsWith(result, "Error:"))
    expect_true(file.exists(
      file.path("s", "studies", "test_study_20260519_120000", "study_design.json")
    ))
  })
})
