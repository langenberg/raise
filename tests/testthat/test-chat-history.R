library(RAISE)

# save_chat_history ------------------------------------------------------------

test_that("save_chat_history writes chat.rds to the session path", {
  withr::with_tempdir({
    turns <- list(
      ellmer::Turn("user",      list(ellmer::ContentText("hello"))),
      ellmer::Turn("assistant", list(ellmer::ContentText("hi there")))
    )
    save_chat_history(turns, ".")
    expect_true(file.exists("chat.rds"))
  })
})

test_that("save_chat_history preserves tool request and result objects", {
  withr::with_tempdir({
    req <- ellmer::ContentToolRequest("call_1", "save_study", list(title = "Test"))
    res <- ellmer::ContentToolResult(request = req, value = "Study saved.", error = NULL)
    turns <- list(
      ellmer::Turn("assistant", list(ellmer::ContentText("Let me save."), req)),
      ellmer::Turn("user",      list(res))
    )
    save_chat_history(turns, ".")
    loaded <- readRDS("chat.rds")
    expect_length(loaded, 2L)
    expect_true(inherits(loaded[[1]]@contents[[2]], "ellmer::ContentToolRequest"))
    expect_true(inherits(loaded[[2]]@contents[[1]], "ellmer::ContentToolResult"))
  })
})

# load_chat_history ------------------------------------------------------------

test_that("load_chat_history returns list() when chat.rds is absent", {
  withr::with_tempdir({
    expect_equal(load_chat_history("."), list())
  })
})

test_that("load_chat_history round-trips ContentText turns", {
  withr::with_tempdir({
    turns <- list(
      ellmer::Turn("user",      list(ellmer::ContentText("question?"))),
      ellmer::Turn("assistant", list(ellmer::ContentText("answer!")))
    )
    save_chat_history(turns, ".")
    loaded <- load_chat_history(".")
    expect_length(loaded, 2L)
    expect_true(inherits(loaded[[1]], "ellmer::Turn"))
    expect_equal(loaded[[1]]@role, "user")
    expect_equal(loaded[[1]]@contents[[1]]@text, "question?")
    expect_equal(loaded[[2]]@role, "assistant")
    expect_equal(loaded[[2]]@contents[[1]]@text, "answer!")
  })
})

test_that("load_chat_history round-trips tool request and result turns", {
  withr::with_tempdir({
    req <- ellmer::ContentToolRequest("id_42", "list_studies", list())
    res <- ellmer::ContentToolResult(request = req, value = "[]", error = NULL)
    turns <- list(
      ellmer::Turn("assistant", list(req)),
      ellmer::Turn("user",      list(res))
    )
    save_chat_history(turns, ".")
    loaded <- load_chat_history(".")
    expect_length(loaded, 2L)
    loaded_req <- loaded[[1]]@contents[[1]]
    expect_true(inherits(loaded_req, "ellmer::ContentToolRequest"))
    expect_equal(loaded_req@id,   "id_42")
    expect_equal(loaded_req@name, "list_studies")
    loaded_res <- loaded[[2]]@contents[[1]]
    expect_true(inherits(loaded_res, "ellmer::ContentToolResult"))
    expect_equal(loaded_res@value,        "[]")
    expect_equal(loaded_res@request@id,   "id_42")
    expect_equal(loaded_res@request@name, "list_studies")
  })
})

test_that("chat_token_totals sums assistant turns, also after injected user turns", {
  ch <- suppressWarnings(ellmer::chat_anthropic(model = "claude-sonnet-4-6",
                                                credentials = function() "x"))
  U <- function(t) ellmer::Turn("user", t)
  A <- function(t, tok) ellmer::Turn("assistant", t, tokens = tok)
  expect_null(chat_token_totals(ch))
  ch$set_turns(list(U("hi"), A("hello", c(100, 20, 0)), U("[UI Notification] x"),
                    U("q2"), A("ok", c(150, 30, 50))))
  expect_equal(chat_token_totals(ch), c(input = 250, output = 50, cached = 50))
})
