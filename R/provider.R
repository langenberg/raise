# LLM and embedding providers. RAISE talks to models only through ellmer
# (chat) and ragnar (embeddings), so any provider they support can be used.

#' Default LLM provider and model
#'
#' `raise_default_provider` is `"anthropic"`; `raise_default_model` is the
#' model used when the provider is Anthropic and no model is given. For other
#' providers, `model = NULL` means the ellmer default for that provider.
#' @export
raise_default_provider <- "anthropic"

#' @rdname raise_default_provider
#' @export
raise_default_model <- "claude-sonnet-4-6"

#' Default embedding provider for RAG stores
#'
#' `"bm25"`: keyword search, no embeddings and no setup. Any ragnar embedding
#' provider (`"openai"`, `"ollama"`, `"google_gemini"`, ...) can be used instead.
#' @export
raise_default_embed_provider <- "bm25"

# Default embedding models where RAISE has a preference; other providers use
# the ragnar default.
.default_embed_models <- c(ollama = "nomic-embed-text", openai = "text-embedding-3-small")

#' Create an ellmer chat for a provider
#'
#' @param provider Which LLM to use. One of:
#'   - a provider name: the suffix of an ellmer `chat_*()` function, e.g.
#'     `"anthropic"` (default), `"openai"`, `"google_gemini"`, `"ollama"`,
#'     `"azure_openai"`, `"aws_bedrock"`, `"openrouter"`;
#'   - `"provider/model"`, e.g. `"openai/gpt-4.1"`;
#'   - a function taking `system_prompt` and returning an ellmer `Chat`, for
#'     full control (`function(system_prompt) ellmer::chat_openai(system_prompt = system_prompt, base_url = ...)`);
#'   - an ellmer `Chat` object, which is cloned (its turns are cleared and its
#'     system prompt replaced).
#' @param model Model name. `NULL` uses [raise_default_model] for Anthropic and
#'   the ellmer default otherwise. Ignored when `provider` is a function or Chat.
#' @param system_prompt System prompt.
#' @param ... Further arguments for the ellmer `chat_*()` function (e.g.
#'   `base_url`, `params`, `credentials`).
#' @return An ellmer `Chat` object.
#' @export
new_llm_chat <- function(provider = raise_default_provider, model = NULL,
                         system_prompt = NULL, ...) {
  if (is.function(provider)) return(provider(system_prompt = system_prompt))
  if (inherits(provider, "Chat")) {
    chat <- provider$clone(deep = TRUE)
    chat$set_turns(list())
    chat$set_system_prompt(system_prompt)
    return(chat)
  }
  if (!is.character(provider) || length(provider) != 1L || !nzchar(provider))
    stop("'provider' must be a provider name, a function or an ellmer Chat object")
  if (grepl("/", provider, fixed = TRUE)) {
    model    <- model %||% sub("^[^/]*/", "", provider)
    provider <- sub("/.*$", "", provider)
  }
  fn_name <- paste0("chat_", provider)
  if (!fn_name %in% getNamespaceExports("ellmer")) {
    known <- sub("^chat_", "", grep("^chat_", getNamespaceExports("ellmer"), value = TRUE))
    stop("unknown provider '", provider, "'. ellmer supports: ",
         paste(sort(known), collapse = ", "))
  }
  if (is.null(model) && identical(provider, "anthropic")) model <- raise_default_model
  args <- c(list(system_prompt = system_prompt), list(...))
  if (!is.null(model)) args$model <- model
  do.call(getExportedValue("ellmer", fn_name), args)
}

# Model name of a provider spec, for messages.
.provider_label <- function(provider, model = NULL) {
  if (is.function(provider)) return("custom provider function")
  if (inherits(provider, "Chat")) return("custom Chat object")
  if (is.null(model) && identical(provider, "anthropic")) model <- raise_default_model
  paste0(provider, if (!is.null(model)) paste0(" (", model, ")") else "")
}

#' Embedding function for a RAG backend
#'
#' @param provider `"bm25"` (no embeddings, returns `NULL`) or the suffix of a
#'   ragnar `embed_*()` function, e.g. `"openai"`, `"ollama"`, `"google_gemini"`.
#' @param model Embedding model; `NULL` uses RAISE's default for Ollama
#'   (`nomic-embed-text`) and OpenAI (`text-embedding-3-small`), otherwise the
#'   ragnar default.
#' @return A function that embeds text, or `NULL` for BM25.
#' @export
embed_function <- function(provider = raise_default_embed_provider, model = NULL) {
  if (identical(provider, "bm25")) return(NULL)
  fn_name <- paste0("embed_", provider)
  if (!fn_name %in% getNamespaceExports("ragnar")) {
    known <- sub("^embed_", "", grep("^embed_", getNamespaceExports("ragnar"), value = TRUE))
    stop("unknown embedding provider '", provider, "'. Use 'bm25' or one of: ",
         paste(sort(known), collapse = ", "))
  }
  model <- model %||% unname(.default_embed_models[provider])
  if (length(model) == 1L && is.na(model)) model <- NULL
  fn <- getExportedValue("ragnar", fn_name)
  if (is.null(model)) fn() else fn(model = model)
}

default_embed_model <- function(provider) {
  m <- unname(.default_embed_models[provider])
  if (length(m) == 1L && !is.na(m)) m else ""
}
