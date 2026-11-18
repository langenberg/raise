# Fields copied from a legacy question; everything else is dropped.
.legacy_question_fields <- c("type", "question", "exhibit", "choices",
                             "correct_answer", "explanation")

# Runs in a callr subprocess: the legacy objects are R6 instances of the old
# `raise` package, so loading them in the user's session would mix their
# environments (and the API key held in `private$cache`) into it.
.read_legacy_rdata <- function(rdata_path) {
  e <- new.env()
  objs <- suppressWarnings(load(rdata_path, envir = e))
  quizzes <- Filter(function(o) {
    x <- get(o, envir = e)
    inherits(x, "R6") && inherits(x, "Quiz") &&
      is.list(x$.__enclos_env__$private$questions)
  }, objs)
  if (length(quizzes) == 0L)
    return(list(error = paste0("no legacy Quiz object found (objects: ",
                               paste(objs, collapse = ", "), ")")))
  if (length(quizzes) > 1L)
    return(list(error = paste0("more than one legacy Quiz object found: ",
                               paste(quizzes, collapse = ", "))))
  priv <- get(quizzes, envir = e)$.__enclos_env__$private
  list(
    object    = quizzes,
    title     = as.character(priv$title %||% ""),
    questions = priv$questions
  )
}

.normalise_legacy_question <- function(q) {
  q <- q[intersect(names(q), .legacy_question_fields)]
  if (!is.null(q[["choices"]])) {
    q[["choices"]] <- lapply(q[["choices"]], function(ch) list(
      choice  = as.character(ch[["choice"]]),
      correct = isTRUE(as.logical(ch[["correct"]]))
    ))
  }
  if (is.character(q[["correct_answer"]])) {
    num <- suppressWarnings(as.numeric(q[["correct_answer"]]))
    if (!is.na(num)) q[["correct_answer"]] <- num
  }
  q
}

#' Import a quiz saved by the legacy `raise` package into a session.
#'
#' The legacy `.RData` file holds an R6 `Quiz` / `QuizSlides` object. It is
#' loaded in a separate R process; only the questions are copied out (no chat
#' history, thread or vector store IDs, or cached API keys). The questions are
#' validated and saved as `question_sets/<qset_id>.json`, with
#' `imported_from` recording the source file.
#'
#' @param rdata_path Path to the legacy `.RData` file.
#' @param session_path Path to the session folder.
#' @param qset_id Identifier of the new question set (filename stem).
#' @param topic Human-readable label for the question set.
#' @param source_materials Optional character vector of files in
#'   `materials/` the questions are based on.
#' @return Path to the written question set JSON.
#' @export
import_legacy_quiz <- function(rdata_path, session_path, qset_id, topic,
                               source_materials = NULL) {
  if (!requireNamespace("callr", quietly = TRUE))
    stop("import_legacy_quiz: the 'callr' package is required")
  if (!file.exists(rdata_path))
    stop("import_legacy_quiz: file not found: ", rdata_path)

  legacy <- callr::r(.read_legacy_rdata,
                     args = list(rdata_path = normalizePath(rdata_path)))
  if (!is.null(legacy[["error"]]))
    stop("import_legacy_quiz: ", legacy[["error"]], " in ", rdata_path)

  init_session(session_path)
  save_question_set(
    list(
      id               = qset_id,
      topic            = topic,
      source_materials = as.list(source_materials),
      imported_from    = rdata_path,
      questions        = lapply(legacy[["questions"]], .normalise_legacy_question),
      created_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    ),
    session_path
  )
}
