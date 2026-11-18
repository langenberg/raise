# Stamp sequential integer IDs (1, 2, 3, …) on questions that lack one.
.assign_question_ids <- function(questions) {
  for (i in seq_along(questions)) {
    if (is.null(questions[[i]][["id"]])) questions[[i]][["id"]] <- i
  }
  questions
}

# Return the next unique integer ID for a new question in a set.
# Uses max(existing ids) + 1 so IDs are never reused after deletion.
.next_q_id <- function(existing_questions) {
  if (length(existing_questions) == 0L) return(1L)
  ids <- vapply(existing_questions,
                function(q) as.integer(q[["id"]] %||% 0L), integer(1L))
  max(ids, 0L) + 1L
}

# Return a character vector of validation errors (length 0 = valid).
validate_question <- function(q) {
  errors <- character(0)
  for (key in c("type", "question", "explanation")) {
    if (is.null(q[[key]])) errors <- c(errors, paste0("missing required field: '", key, "'"))
  }
  if (length(errors) > 0L) return(errors)

  qtype <- q[["type"]]
  if (!qtype %in% c("multiple_choice", "numeric")) {
    return(c(errors, paste0("unknown question type: '", qtype, "'")))
  }

  if (qtype == "multiple_choice") {
    choices <- q[["choices"]]
    if (!is.list(choices) || length(choices) < 2L) {
      errors <- c(errors, "'choices' must be a list of at least 2 items")
    } else {
      for (i in seq_along(choices)) {
        if (is.null(choices[[i]][["choice"]]))
          errors <- c(errors, paste0("choices[", i, "] missing 'choice'"))
        if (is.null(choices[[i]][["correct"]]))
          errors <- c(errors, paste0("choices[", i, "] missing 'correct'"))
      }
      if (!any(vapply(choices, function(c) isTRUE(c[["correct"]]), logical(1))))
        errors <- c(errors, "at least one choice must have correct: true")
    }
  } else if (qtype == "numeric") {
    if (is.null(q[["correct_answer"]])) {
      errors <- c(errors, "numeric requires 'correct_answer'")
    } else if (!is.numeric(q[["correct_answer"]])) {
      errors <- c(errors, "'correct_answer' must be a number")
    }
  }
  errors
}

#' Validate and write a question set to question_sets/<id>.json.
#' @param question_data Named list matching the question set JSON schema.
#' @param session_path Path to the session folder.
#' @export
save_question_set <- function(question_data, session_path) {
  if (is.null(question_data[["id"]]))
    stop("save_question_set: 'id' is required")
  if (!is.list(question_data[["questions"]]))
    stop("save_question_set: 'questions' must be a list")

  study_id         <- question_data[["study_id"]]
  source_materials <- question_data[["source_materials"]]
  imported_from    <- question_data[["imported_from"]]
  if ((is.null(study_id) || identical(study_id, "")) &&
      (is.null(source_materials) || length(source_materials) == 0L) &&
      (is.null(imported_from) || identical(imported_from, "")))
    stop("save_question_set: at least one of 'study_id', 'source_materials' or 'imported_from' must be provided")

  all_errors <- character(0)
  for (i in seq_along(question_data[["questions"]])) {
    errs <- validate_question(question_data[["questions"]][[i]])
    if (length(errs) > 0L)
      all_errors <- c(all_errors, paste0("questions[", i, "]: ", errs))
  }
  all_errors <- c(all_errors, check_figure_refs(question_data[["questions"]], session_path))
  if (length(all_errors) > 0L)
    stop(paste0("save_question_set: invalid questions:\n", paste(all_errors, collapse = "\n")))

  question_data$questions <- .assign_question_ids(question_data$questions)

  qset_dir <- file.path(session_path, "question_sets")
  dir.create(qset_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(qset_dir, paste0(question_data[["id"]], ".json"))
  jsonlite::write_json(question_data, dest, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  dest
}

#' Delete a question set JSON file from the session folder.
#' @param qset_id Question set identifier (filename without \code{.json}).
#' @param session_path Path to the session folder.
#' @export
delete_question_set <- function(qset_id, session_path) {
  path <- file.path(session_path, "question_sets", paste0(qset_id, ".json"))
  if (file.exists(path)) file.remove(path)
  invisible(NULL)
}

#' Remove specific questions from a question set by their integer IDs.
#' @param qset_id Question set identifier (filename without \code{.json}).
#' @param question_ids Integer vector of question IDs to remove.
#' @param session_path Path to the session folder.
#' @export
remove_questions <- function(qset_id, question_ids, session_path) {
  qset_file <- file.path(session_path, "question_sets", paste0(qset_id, ".json"))
  if (!file.exists(qset_file))
    return(paste0("Error: question set not found: ", qset_id))

  data <- tryCatch(
    jsonlite::read_json(qset_file),
    error = function(e) return(paste0("Error reading question set: ", conditionMessage(e)))
  )
  if (is.character(data)) return(data)

  ids_to_remove <- as.integer(unlist(question_ids))
  before        <- length(data$questions %||% list())
  data$questions <- Filter(
    function(q) !as.integer(q[["id"]] %||% -1L) %in% ids_to_remove,
    data$questions %||% list()
  )
  removed <- before - length(data$questions)

  jsonlite::write_json(data, qset_file, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  paste0("Removed ", removed, " question(s) from '", qset_id, "'. ",
         length(data$questions), " question(s) remain.")
}

#' Replace specific questions in a question set (matched by integer id).
#' @param qset_id Question set identifier (filename without \code{.json}).
#' @param questions List of complete replacement question objects, each with an integer \code{id}.
#' @param session_path Path to the session folder.
#' @export
update_questions <- function(qset_id, questions, session_path) {
  qset_file <- file.path(session_path, "question_sets", paste0(qset_id, ".json"))
  if (!file.exists(qset_file))
    return(paste0("Error: question set not found: ", qset_id))

  data <- tryCatch(
    jsonlite::read_json(qset_file),
    error = function(e) return(paste0("Error reading question set: ", conditionMessage(e)))
  )
  if (is.character(data)) return(data)

  for (i in seq_along(questions)) {
    if (is.null(questions[[i]][["id"]]))
      return(paste0("Error: replacement questions[", i, "] is missing 'id'. ",
                    "Call load_question_set first to discover question IDs."))
  }

  all_errors <- character(0)
  for (i in seq_along(questions)) {
    errs <- validate_question(questions[[i]])
    if (length(errs) > 0L)
      all_errors <- c(all_errors, paste0("questions[", i, "]: ", errs))
  }
  all_errors <- c(all_errors, check_figure_refs(questions, session_path))
  if (length(all_errors) > 0L)
    return(paste0("Error: invalid replacement questions:\n", paste(all_errors, collapse = "\n")))

  id_map  <- stats::setNames(questions,
               as.character(vapply(questions, function(q) as.integer(q[["id"]]), integer(1L))))
  updated <- 0L
  data$questions <- lapply(data$questions %||% list(), function(q) {
    key <- as.character(as.integer(q[["id"]] %||% -1L))
    if (!is.null(id_map[[key]])) { updated <<- updated + 1L; id_map[[key]] } else q
  })

  supplied_ids <- as.character(vapply(questions, function(q) as.integer(q[["id"]]), integer(1L)))
  found_ids    <- as.character(vapply(data$questions,
                                      function(q) as.integer(q[["id"]] %||% -1L), integer(1L)))
  not_found    <- setdiff(supplied_ids, found_ids)

  jsonlite::write_json(data, qset_file, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  msg <- paste0("Updated ", updated, " question(s) in '", qset_id, "'.")
  if (length(not_found) > 0L)
    msg <- paste0(msg, " Warning: IDs not found in set: ", paste(not_found, collapse = ", "), ".")
  msg
}

#' Append new questions to an existing question set.
#' @param qset_id Question set identifier (filename without \code{.json}).
#' @param questions List of new question objects (without \code{id} fields).
#' @param session_path Path to the session folder.
#' @export
add_questions <- function(qset_id, questions, session_path) {
  qset_file <- file.path(session_path, "question_sets", paste0(qset_id, ".json"))
  if (!file.exists(qset_file))
    return(paste0("Error: question set not found: ", qset_id))

  data <- tryCatch(
    jsonlite::read_json(qset_file),
    error = function(e) return(paste0("Error reading question set: ", conditionMessage(e)))
  )
  if (is.character(data)) return(data)

  all_errors <- character(0)
  for (i in seq_along(questions)) {
    errs <- validate_question(questions[[i]])
    if (length(errs) > 0L)
      all_errors <- c(all_errors, paste0("questions[", i, "]: ", errs))
  }
  all_errors <- c(all_errors, check_figure_refs(questions, session_path))
  if (length(all_errors) > 0L)
    return(paste0("Error: invalid questions:\n", paste(all_errors, collapse = "\n")))

  next_id <- .next_q_id(data$questions %||% list())
  for (i in seq_along(questions)) {
    questions[[i]][["id"]] <- next_id + i - 1L
  }
  data$questions <- c(data$questions %||% list(), questions)

  jsonlite::write_json(data, qset_file, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  paste0("Added ", length(questions), " question(s) to '", qset_id, "'. ",
         "Total: ", length(data$questions), " question(s).")
}

# Read every *.json in question_sets/ and return a list of parsed objects.
load_all_question_sets <- function(session_path) {
  qset_dir <- file.path(session_path, "question_sets")
  if (!dir.exists(qset_dir)) return(list())
  files <- sort(list.files(qset_dir, pattern = "\\.json$", full.names = TRUE))
  result <- list()
  for (f in files) {
    tryCatch(
      result <- c(result, list(jsonlite::read_json(f))),
      error = function(e) NULL
    )
  }
  result
}
