#' Initialise a session folder, creating all required subdirectories.
#' @param session_path Path to the session folder (absolute or relative to the
#'   working directory). Whitespace is trimmed automatically.
#' @return The normalised absolute path (invisibly), or \code{NULL} if
#'   \code{session_path} is \code{NULL} or blank.
#' @export
init_session <- function(session_path) {
  if (is.null(session_path)) return(NULL)
  session_path <- trimws(session_path)
  if (!nzchar(session_path)) return(NULL)
  session_path <- normalizePath(session_path, winslash = "/", mustWork = FALSE)
  for (d in c("materials", "studies", "question_sets", "exports", "figures"))
    dir.create(file.path(session_path, d), recursive = TRUE, showWarnings = FALSE)
  invisible(session_path)
}

#' Return a JSON array summarising all studies in the session.
#' @param session_path Path to the session folder.
#' @export
list_studies <- function(session_path) {
  studies_dir <- file.path(session_path, "studies")
  if (!dir.exists(studies_dir)) return("[]")

  dirs <- sort(list.dirs(studies_dir, full.names = TRUE, recursive = FALSE))
  results <- list()

  for (study_dir in dirs) {
    design_file <- file.path(study_dir, "study_design.json")
    if (!file.exists(design_file)) next
    design <- tryCatch(jsonlite::read_json(design_file), error = function(e) NULL)
    if (is.null(design)) next

    study_id_name <- basename(study_dir)
    m <- regmatches(study_id_name,
                    regexpr("_(\\d{8})_(\\d{6})$", study_id_name))
    created_at <- if (length(m) > 0L && nchar(m) > 0L) {
      parts <- regmatches(m, regexec("_(\\d{8})_(\\d{6})$", m))[[1]]
      d <- parts[2]; t <- parts[3]
      paste0(substr(d,1,4), "-", substr(d,5,6), "-", substr(d,7,8),
             "T", substr(t,1,2), ":", substr(t,3,4), ":", substr(t,5,6), "Z")
    } else NULL

    results <- c(results, list(list(
      study_id   = study_id_name,
      title      = design$title %||% "",
      family     = design$family %||% "",
      status     = design$status %||% "unknown",
      created_at = created_at
    )))
  }
  jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE)
}

#' Return a JSON array of filenames in the session's materials/ folder.
#' @param session_path Path to the session folder.
#' @export
list_materials <- function(session_path) {
  mat_dir <- file.path(session_path, "materials")
  if (!dir.exists(mat_dir)) return("[]")
  files <- sort(list.files(mat_dir, all.files = FALSE))
  files <- files[!startsWith(files, ".")]
  jsonlite::toJSON(files, auto_unbox = FALSE)
}

#' Return the full study design plus regression output as a JSON string.
#' @param study_id Study identifier string.
#' @param session_path Path to the session folder.
#' @export
load_study_context <- function(study_id, session_path) {
  study_dir <- file.path(session_path, "studies", study_id)
  if (!dir.exists(study_dir))
    return(paste0("Study not found: ", study_id))

  design_file <- file.path(study_dir, "study_design.json")
  if (!file.exists(design_file))
    return(paste0("study_design.json not found for study: ", study_id))

  design <- tryCatch(
    jsonlite::read_json(design_file),
    error = function(e) return(paste0("Failed to read study_design.json: ", conditionMessage(e)))
  )
  if (is.character(design)) return(design)

  reg_file <- file.path(study_dir, "regression.txt")
  design$regression_output <- if (file.exists(reg_file))
    trimws(paste(readLines(reg_file, warn = FALSE), collapse = "\n"))
  else
    NULL

  or_file <- file.path(study_dir, "odds_ratios.txt")
  design$odds_ratios_output <- if (file.exists(or_file))
    trimws(paste(readLines(or_file, warn = FALSE), collapse = "\n"))
  else
    NULL

  jsonlite::toJSON(design, pretty = TRUE, auto_unbox = TRUE)
}

#' Delete a material file from the session's materials/ folder.
#' @param material_name Filename inside \code{materials/}.
#' @param session_path Path to the session folder.
#' @export
delete_material <- function(material_name, session_path) {
  mat_path <- file.path(session_path, "materials", material_name)
  if (!file.exists(mat_path))
    return(paste0("Error: material not found: ", material_name))
  file.remove(mat_path)
  paste0("Material '", material_name, "' deleted.")
}

#' Return a JSON array summarising all question sets in the session.
#' @param session_path Path to the session folder.
#' @export
list_question_sets <- function(session_path) {
  qset_dir <- file.path(session_path, "question_sets")
  if (!dir.exists(qset_dir)) return("[]")

  files <- sort(list.files(qset_dir, pattern = "\\.json$", full.names = TRUE))
  results <- list()
  for (f in files) {
    data <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
    if (is.null(data)) next
    results <- c(results, list(list(
      id          = data$id %||% tools::file_path_sans_ext(basename(f)),
      topic       = data$topic %||% "",
      study_id    = data$study_id,
      n_questions = length(data$questions %||% list()),
      created_at  = data$created_at
    )))
  }
  jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE)
}

#' Return a JSON array summarising all exports in the session (newest first).
#' @param session_path Path to the session folder.
#' @export
list_exports <- function(session_path) {
  exports_dir <- file.path(session_path, "exports")
  if (!dir.exists(exports_dir)) return("[]")

  folders <- rev(sort(list.dirs(exports_dir, full.names = FALSE, recursive = FALSE)))
  results <- list()
  for (folder in folders) {
    folder_path <- file.path(exports_dir, folder)
    meta <- tryCatch(
      jsonlite::read_json(file.path(folder_path, "meta.json")),
      error = function(e) list()
    )
    artifact <- NULL
    for (ext in c("zip", "pdf", "md")) {
      matches <- list.files(folder_path, pattern = paste0("\\.", ext, "$"), full.names = TRUE)
      if (length(matches) > 0L) { artifact <- matches[[1L]]; break }
    }
    results <- c(results, list(list(
      export_id    = folder,
      label        = meta$label %||% folder,
      destination  = meta$destination %||% "unknown",
      n_questions  = meta$n_questions %||% 0L,
      created_at   = meta$created_at,
      has_artifact = !is.null(artifact)
    )))
  }
  jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE)
}

#' Return the full contents of a question set as a formatted string.
#' @param qset_id Question set identifier (filename without \code{.json}).
#' @param session_path Path to the session folder.
#' @export
load_question_set <- function(qset_id, session_path) {
  qset_file <- file.path(session_path, "question_sets", paste0(qset_id, ".json"))
  if (!file.exists(qset_file))
    return(paste0("Question set not found: ", qset_id))

  data <- tryCatch(
    jsonlite::read_json(qset_file),
    error = function(e) return(paste0("Failed to read question set ", qset_id, ": ", conditionMessage(e)))
  )
  if (is.character(data)) return(data)

  parts <- c(
    paste0("=== Question Set: ", data$topic %||% qset_id, " ==="),
    paste0("ID: ",       data$id %||% qset_id),
    paste0("Study: ",    data$study_id %||% "(none)"),
    paste0("Source materials: ",
           if (length(data$source_materials) > 0L)
             paste(data$source_materials, collapse = ", ")
           else "(none)"),
    paste0("Created: ",  data$created_at %||% "unknown"),
    paste0("Questions: ", length(data$questions %||% list()))
  )

  for (i in seq_along(data$questions %||% list())) {
    q <- data$questions[[i]]
    parts <- c(parts,
               paste0("\n--- Question ", i, " (", q$type %||% "unknown", ") ---"),
               q$question %||% "")
    # is.character(): sets written before 2026-11-05 can hold "exhibit": {}
    if (is.character(q$exhibit) && length(q$exhibit) == 1L && nzchar(q$exhibit))
      parts <- c(parts, paste0("Exhibit: ", q$exhibit))
    if (!is.null(q$id))
      parts <- c(parts, paste0("Question ID: ", q$id))
    if (identical(q$type, "multiple_choice")) {
      for (ch in q$choices %||% list()) {
        marker <- if (isTRUE(ch$correct)) "[correct]" else "[ ]"
        parts  <- c(parts, paste0("  ", marker, " ", ch$choice %||% ""))
      }
    } else if (identical(q$type, "numeric")) {
      parts <- c(parts, paste0("  Correct answer: ", q$correct_answer))
    }
    parts <- c(parts, paste0("Explanation: ", q$explanation %||% ""))
  }
  paste(parts, collapse = "\n")
}
