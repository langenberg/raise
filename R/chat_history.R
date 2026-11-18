#' Save chat history to disk
#'
#' Saves the current ellmer turn list to `<session_path>/chat.rds`.
#'
#' @param turns List of ellmer `Turn` objects from `chat$get_turns()`.
#' @param session_path Path to the session folder.
#' @export
save_chat_history <- function(turns, session_path) {
  saveRDS(turns, file.path(session_path, "chat.rds"))
}

#' Load chat history from disk
#'
#' Reads `<session_path>/chat.rds` and returns the turn list.
#' Returns an empty list if the file does not exist.
#'
#' @param session_path Path to the session folder.
#' @return A list of ellmer `Turn` objects, or `list()` if no history exists.
#' @export
load_chat_history <- function(session_path) {
  rds_path <- file.path(session_path, "chat.rds")
  if (!file.exists(rds_path)) return(list())
  readRDS(rds_path)
}
