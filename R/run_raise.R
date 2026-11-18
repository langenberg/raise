#' Launch the RAISE Shiny application.
#' @param session_path Optional path to a session folder to open automatically
#'   on startup. Relative paths are resolved against the working directory.
#' @inheritParams new_llm_chat
#' @param embed_provider Default backend in the "Create RAG store" dialog:
#'   `"bm25"` (default, keyword search) or a ragnar embedding provider such as
#'   `"openai"`, `"ollama"`, `"google_gemini"` (see [embed_function()]).
#' @param embed_model Default embedding model for `embed_provider`; `NULL`
#'   uses the RAISE / ragnar default.
#' @param ... Further arguments for the ellmer `chat_*()` function.
#' @examples
#' \dontrun{
#' run_raise("my_session")                                   # Anthropic, claude-sonnet-4-6
#' run_raise("my_session", provider = "openai", model = "gpt-4.1")
#' run_raise("my_session", provider = "ollama/qwen3:14b", embed_provider = "ollama")
#' }
#' @export
run_raise <- function(session_path = NULL, provider = raise_default_provider, model = NULL,
                      embed_provider = raise_default_embed_provider, embed_model = NULL, ...) {
  if (is.character(provider)) {
    p <- sub("/.*$", "", provider)
    if (!paste0("chat_", p) %in% getNamespaceExports("ellmer"))
      stop("unknown provider '", p, "'")
  }
  embed_function(embed_provider, embed_model)  # fails early for an unknown backend
  chat_args <- list(...)
  if (utils::packageVersion("shinychat") >= "0.4.0")
    warning("RAISE is built for shinychat 0.3.x (installed: ",
            utils::packageVersion("shinychat"), "). shinychat 0.4+ renders the chat ",
            "differently; math in the chat is not protected and the app is untested. ",
            "Install 0.3.0 from the CRAN archive to be safe.", call. = FALSE)
  shiny::addResourcePath("docs", system.file("doc", package = "RAISE"))
  initial <- session_path
  shiny::shinyApp(
    ui     = raise_ui,
    server = function(input, output, session) {
      raise_server(input, output, session, initial_session_path = initial,
                   provider = provider, model = model,
                   embed_provider = embed_provider, embed_model = embed_model,
                   chat_args = chat_args)
    }
  )
}
