#' Create a RAISE agent chat for scripted use, without the Shiny app.
#'
#' Opens (and if needed creates) the session folder and returns an ellmer chat
#' object with the RAISE system prompt and all agent tools registered. Talk to
#' it with `chat$chat("...")`; tool calls read and write the session folder.
#'
#' @param session_path Path to the session folder.
#' @inheritParams new_llm_chat
#' @return An ellmer `Chat` object.
#' @export
raise_chat <- function(session_path, provider = raise_default_provider, model = NULL, ...) {
  session_env <- new.env(parent = emptyenv())
  session_env$path <- init_session(session_path)
  if (is.null(session_env$path))
    stop("raise_chat: 'session_path' must be a non-empty path")
  make_chat(session_env, load_system_prompt(), provider = provider, model = model, ...)
}

#' Check that the LLM provider works.
#'
#' Sends one minimal request. Stops with the provider's error message if the
#' credentials are missing or invalid, or the model is not available.
#'
#' @inheritParams new_llm_chat
#' @return `TRUE`, invisibly.
#' @export
check_api_key <- function(provider = raise_default_provider, model = NULL, ...) {
  if (identical(provider, "anthropic") && !nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
    stop("ANTHROPIC_API_KEY is not set. Add a line ANTHROPIC_API_KEY=<key> ",
         "to ~/.Renviron and restart R.")
  args <- list(...)
  if (is.character(provider) && is.null(args$params))
    args$params <- ellmer::params(max_tokens = 5)
  chat <- tryCatch(
    do.call(new_llm_chat, c(list(provider, model = model,
                                 system_prompt = "Reply with the single word OK."), args)),
    error = function(e) stop("Could not create the chat: ", conditionMessage(e), call. = FALSE)
  )
  reply <- tryCatch(
    chat$chat("ping", echo = "none"),
    error = function(e) stop("API request failed: ", conditionMessage(e), call. = FALSE)
  )
  message("API works (", .provider_label(provider, model), ", reply: ", trimws(reply), ").")
  invisible(TRUE)
}
