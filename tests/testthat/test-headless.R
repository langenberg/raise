library(RAISE)

make_mock_chat <- function() {
  env <- new.env(parent = emptyenv())
  env$tools <- list()
  env$register_tool <- function(tool_def) {
    env$tools <- c(env$tools, list(tool_def))
    invisible(env)
  }
  env$chat <- function(...) "OK"
  env
}

test_that("raise_chat creates the session and registers all tools", {
  withr::with_tempdir({
    mock <- make_mock_chat()
    seen <- new.env()
    local_mocked_bindings(
      chat_anthropic = function(system_prompt, model, ...) {
        seen$model <- model
        seen$system <- system_prompt
        mock
      },
      .package = "ellmer"
    )
    chat <- raise_chat("sess", model = "claude-test")
    expect_identical(chat, mock)
    expect_length(mock$tools, 31L)
    expect_equal(seen$model, "claude-test")
    expect_true(nzchar(seen$system))
    for (d in c("materials", "studies", "question_sets", "exports"))
      expect_true(dir.exists(file.path("sess", d)))
  })
})

test_that("raise_chat rejects an empty session path", {
  expect_error(raise_chat(""), "non-empty")
})

test_that("check_api_key stops when the key is not set", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  expect_error(check_api_key(), "ANTHROPIC_API_KEY is not set")
})

test_that("check_api_key succeeds when the request succeeds", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")
  local_mocked_bindings(chat_anthropic = function(...) make_mock_chat(),
                        .package = "ellmer")
  expect_message(expect_true(check_api_key()), "API works \\(anthropic \\(claude-sonnet-4-6\\)")
})

test_that("check_api_key works for other providers without an Anthropic key", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  seen <- new.env()
  local_mocked_bindings(chat_openai = function(system_prompt, model = NULL, params = NULL, ...) {
    seen$model <- model; seen$params <- params; make_mock_chat()
  }, .package = "ellmer")
  expect_message(expect_true(check_api_key("openai", model = "gpt-4.1")), "API works \\(openai \\(gpt-4.1\\)")
  expect_equal(seen$model, "gpt-4.1")
  expect_false(is.null(seen$params))
})

test_that("check_api_key reports API errors", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")
  failing <- make_mock_chat()
  failing$chat <- function(...) stop("HTTP 401 Unauthorized")
  local_mocked_bindings(chat_anthropic = function(...) failing,
                        .package = "ellmer")
  expect_error(check_api_key(), "API request failed: HTTP 401")
})
