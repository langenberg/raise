#' Collect questions from question sets in a structured, renderable form.
#'
#' Questions of all sets are numbered consecutively. Regression output of the
#' linked study is prepended as with the other exports, numeric answers are
#' rounded as the precision note says, and math delimiters are normalised
#' (`normalise_math()`), so the result renders in pandoc and Jupyter alike.
#'
#' @param qset_ids Character vector of question set IDs.
#' @param session_path Path to the session folder.
#' @param precision_mode Numeric precision: `"fixed"` or `"auto"`.
#' @param n_digits Number of decimal places when `precision_mode = "fixed"`.
#' @param include_precision_note Whether numeric questions carry the rounding note.
#' @param choice_style `"dash"` (plain list) or `"letter"` (a), b), ... labels).
#' @return A list with one element per question: `number`, `qset_id`, `id`,
#'   `type`, `question`, `exhibit`, `choices` (each with `label`, `text`,
#'   `correct`), `precision_note`, `answer` (the correct choice or the rounded
#'   number, as text), `unrounded` (the exact number, only when rounding changed
#'   it) and `explanation`.
#' @export
format_questions <- function(qset_ids, session_path, precision_mode = "fixed",
                             n_digits = 2L, include_precision_note = TRUE,
                             choice_style = c("dash", "letter")) {
  choice_style <- match.arg(choice_style)
  loaded <- load_export_questions(qset_ids, session_path)
  md     <- function(x) if (!is.character(x)) NULL else normalise_math(x, "markdown")

  Map(function(q, origin, number) {
    item <- list(
      number      = number,
      qset_id     = origin$qset_id,
      id          = q[["id"]],
      type        = q[["type"]],
      question    = md(q[["question"]]),
      exhibit     = md(q[["exhibit"]]),
      choices     = NULL,
      precision_note = NULL,
      answer      = NULL,
      explanation = md(q[["explanation"]])
    )
    if (identical(q[["type"]], "multiple_choice")) {
      item$choices <- Map(function(ch, k) list(
        label   = if (choice_style == "letter") paste0(letters[k], ")") else NULL,
        text    = md(ch[["choice"]]),
        correct = isTRUE(ch[["correct"]])
      ), q[["choices"]], seq_along(q[["choices"]]))
      correct <- Filter(function(ch) ch$correct, item$choices)
      item$answer <- paste(vapply(correct, function(ch)
        paste(c(ch$label, ch$text), collapse = " "), ""), collapse = "; ")
    } else {
      prec <- compute_precision(q[["correct_answer"]], precision_mode, n_digits)
      if (include_precision_note) item$precision_note <- prec$format_note
      item$answer <- formatC(q[["correct_answer"]], format = "f",
                             digits = prec$decimal_places)
      if (as.numeric(item$answer) != q[["correct_answer"]])
        item$unrounded <- format(q[["correct_answer"]], digits = 15)
    }
    item
  }, loaded$questions, loaded$origins, seq_along(loaded$questions))
}

#' Render questions from question sets as Markdown.
#'
#' One heading per question, the question text, the choices as a list and,
#' for numeric questions, the rounding note. With `answers = TRUE`, each
#' question is followed by its correct answer and explanation.
#'
#' @inheritParams format_questions
#' @param answers Whether to include correct answers and explanations.
#' @param heading_level Markdown heading level of each question heading.
#' @param file Optional path; when given, the Markdown is also written there.
#' @param figures How figures (`![alt](figures/<file>)`) are handled:
#'   `"copy"` keeps the relative links and, when `file` is given, copies the
#'   figures to a `figures/` folder next to it; `"embed"` puts each image into
#'   the Markdown as a base64 data URI (one self-contained file).
#' @return The Markdown as a single string (invisibly when `file` is given).
#' @export
questions_to_markdown <- function(qset_ids, session_path, answers = TRUE,
                                  heading_level = 2L,
                                  choice_style = c("dash", "letter"),
                                  precision_mode = "fixed", n_digits = 2L,
                                  include_precision_note = TRUE, file = NULL,
                                  figures = c("copy", "embed")) {
  figures <- match.arg(figures)
  items <- format_questions(qset_ids, session_path, precision_mode, n_digits,
                            include_precision_note, match.arg(choice_style))
  md    <- render_markdown(items, answers, heading_level)
  if (figures == "embed") md <- embed_figures(md, session_path)
  if (is.null(file)) return(md)
  writeLines(md, file, sep = "", useBytes = TRUE)
  if (figures == "copy") copy_figures(md, session_path, dirname(file))
  invisible(md)
}

#' Render questions from question sets as JSON, one Markdown entry per question.
#'
#' The same Markdown as [questions_to_markdown()], split per question, for
#' tools that place single questions somewhere else (e.g. each question next to
#' the exercise it belongs to in a notebook). Each entry has `number` (position
#' across all sets, as in the other exports), `qset_id`, `id`, `type`,
#' `question` (text, exhibit, choices, rounding note; no heading) and, with
#' `answers = TRUE`, `answer` (correct answer and explanation).
#'
#' @inheritParams questions_to_markdown
#' @param file Optional path; when given, the JSON is also written there.
#' @param figures How figures (`![alt](figures/<file>)`) are handled:
#'   `"embed"` (default) puts each image into the Markdown as a base64 data
#'   URI (one self-contained file); `"copy"` keeps the relative links and, when
#'   `file` is given, copies the figures to a `figures/` folder next to it.
#' @return The JSON as a single string (invisibly when `file` is given):
#'   an object with one field, `questions`, an array of the entries above.
#' @export
questions_to_json <- function(qset_ids, session_path, answers = TRUE,
                              choice_style = c("dash", "letter"),
                              precision_mode = "fixed", n_digits = 2L,
                              include_precision_note = TRUE, file = NULL,
                              figures = c("embed", "copy")) {
  figures <- match.arg(figures)
  items <- format_questions(qset_ids, session_path, precision_mode, n_digits,
                            include_precision_note, match.arg(choice_style))
  md <- character(0)
  entries <- lapply(items, function(it) {
    entry <- list(number = it$number, qset_id = it$qset_id, id = it$id,
                  type = it$type, question = render_question_md(it))
    if (answers) entry$answer <- render_answer_md(it)
    md <<- c(md, entry$question, entry$answer)
    if (figures == "embed") {
      entry$question <- embed_figures(entry$question, session_path)
      if (answers) entry$answer <- embed_figures(entry$answer, session_path)
    }
    entry
  })
  json <- as.character(jsonlite::toJSON(list(questions = entries), pretty = TRUE,
                                        auto_unbox = TRUE, digits = NA, null = "null"))
  if (is.null(file)) return(json)
  writeLines(json, file, useBytes = TRUE)
  if (figures == "copy") copy_figures(paste(md, collapse = "\n"), session_path, dirname(file))
  invisible(json)
}

# Figure references as base64 data URIs.
embed_figures <- function(text, session_path) {
  for (f in figure_files_in_all(text)) {
    mime <- if (grepl("\\.png$", f, ignore.case = TRUE)) "image/png" else "image/jpeg"
    uri  <- paste0("data:", mime, ";base64,",
                   base64enc::base64encode(file.path(figures_dir(session_path), f)))
    text <- gsub(paste0("](figures/", f, ")"), paste0("](", uri, ")"), text, fixed = TRUE)
  }
  text
}

# Copies the figures a text references to <dir>/figures/.
copy_figures <- function(text, session_path, dir) {
  files <- figure_files_in_all(text)
  if (length(files) == 0L) return(invisible())
  dest <- file.path(dir, "figures")
  dir.create(dest, showWarnings = FALSE)
  file.copy(file.path(figures_dir(session_path), files), dest, overwrite = TRUE)
  invisible()
}

# Figure files referenced anywhere in a (multi-reference) text.
figure_files_in_all <- function(text) {
  refs <- regmatches(text, gregexpr(.figure_ref_pattern, text, perl = TRUE))[[1L]]
  unique(sub(.figure_ref_pattern, "\\2", refs, perl = TRUE))
}

render_markdown <- function(items, answers = TRUE, heading_level = 2L) {
  hashes <- strrep("#", as.integer(heading_level))
  blocks <- vapply(items, function(it) {
    paste(c(render_question_md(it, paste0(hashes, " Question ", it$number)),
            if (answers) render_answer_md(it)), collapse = "\n\n")
  }, "")
  paste0(paste(blocks, collapse = "\n\n"), "\n")
}

# One question as Markdown: optional heading, text, exhibit, choices, rounding note.
render_question_md <- function(it, heading = NULL) {
  parts <- c(heading, it$question, it$exhibit)
  if (!is.null(it$choices))
    parts <- c(parts, paste(vapply(it$choices, function(ch)
      paste0("- ", paste(c(ch$label, ch$text), collapse = " ")), ""), collapse = "\n"))
  parts <- c(parts, it$precision_note)
  paste(parts[nzchar(parts)], collapse = "\n\n")
}

# The answer part of one question: correct answer (+ unrounded value) and explanation.
render_answer_md <- function(it) {
  unrounded <- if (is.null(it$unrounded)) "" else paste0(" (unrounded: ", it$unrounded, ")")
  parts <- paste0("**Correct answer:** ", it$answer, unrounded)
  if (nzchar(it$explanation %||% ""))
    parts <- c(parts, paste0("**Explanation:** ", it$explanation))
  paste(parts, collapse = "\n\n")
}
