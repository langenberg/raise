test_that("new_llm_chat: Anthropic is the default, with the RAISE default model", {
  seen <- new.env()
  local_mocked_bindings(
    chat_anthropic = function(system_prompt, model, ...) {
      seen$model <- model; seen$prompt <- system_prompt; seen$dots <- list(...); "chat"
    },
    .package = "ellmer"
  )
  expect_equal(new_llm_chat(system_prompt = "S"), "chat")
  expect_equal(seen$model, raise_default_model)
  expect_equal(seen$prompt, "S")
  new_llm_chat("anthropic", model = "claude-x", system_prompt = "S", base_url = "u")
  expect_equal(seen$model, "claude-x")
  expect_equal(seen$dots$base_url, "u")
})

test_that("new_llm_chat: other providers by name and as provider/model", {
  seen <- new.env()
  local_mocked_bindings(
    chat_openai = function(system_prompt, model = NULL, ...) {
      seen$model <- if (missing(model)) "<ellmer default>" else model; "openai chat"
    },
    .package = "ellmer"
  )
  expect_equal(new_llm_chat("openai", system_prompt = "S"), "openai chat")
  expect_equal(seen$model, "<ellmer default>")
  new_llm_chat("openai/gpt-4.1", system_prompt = "S")
  expect_equal(seen$model, "gpt-4.1")
  expect_error(new_llm_chat("nope"), "unknown provider 'nope'.*anthropic")
  expect_error(new_llm_chat(42), "provider name")
})

test_that("new_llm_chat: a function or an ellmer Chat object as provider", {
  f <- function(system_prompt) paste("built with", system_prompt)
  expect_equal(new_llm_chat(f, system_prompt = "S"), "built with S")
  ch <- ellmer::chat_anthropic(model = "claude-x", credentials = function() "k",
                               system_prompt = "old")
  ch$set_turns(list(ellmer::Turn("user", "hi"), ellmer::Turn("assistant", "hello")))
  out <- new_llm_chat(ch, system_prompt = "new")
  expect_equal(out$get_system_prompt(), "new")
  expect_length(out$get_turns(), 0L)
  expect_equal(ch$get_system_prompt(), "old")   # the original is untouched
  expect_length(ch$get_turns(), 2L)
})

test_that("embed_function: bm25 default, ragnar providers by name", {
  expect_null(embed_function())
  expect_true(is.function(embed_function("openai")))
  expect_true(is.function(embed_function("ollama", "all-minilm")))
  expect_error(embed_function("nope"), "unknown embedding provider")
  expect_equal(default_embed_model("ollama"), "nomic-embed-text")
  expect_equal(default_embed_model("google_gemini"), "")
})

test_that("run_raise rejects unknown providers before starting", {
  expect_error(run_raise(provider = "nope"), "unknown provider")
  expect_error(run_raise(embed_provider = "nope"), "unknown embedding provider")
})
