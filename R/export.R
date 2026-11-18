# Make LaTeX math in markdown text readable for pandoc: \( \) and \[ \]
# delimiters become $ / $$, and spaces just inside $...$ (which stop pandoc
# from seeing math) are trimmed when the content is clearly math. For PDF,
# underscores inside \text{} are escaped because LaTeX rejects a bare _ there.
normalise_math <- function(p, destination = NULL) {
  p <- gsub("\\\\\\((.+?)\\\\\\)", "$\\1$", p, perl = TRUE)
  p <- gsub("(?s)\\\\\\[(.+?)\\\\\\]", "$$\\1$$", p, perl = TRUE)
  # Pair the dollars left to right, so the text between two formulas is never
  # taken for one.
  m <- gregexpr("(?<![$\\\\])\\$(?!\\$)[^$\n]+?(?<!\\\\)\\$", p, perl = TRUE)
  regmatches(p, m) <- lapply(regmatches(p, m), function(spans) {
    inner <- substr(spans, 2L, nchar(spans) - 1L)
    trim  <- grepl("[\\\\^_={}]", inner)
    spans[trim] <- paste0("$", trimws(inner[trim]), "$")
    spans
  })
  if (identical(destination, "pdf")) {
    repeat {
      q <- gsub("(\\\\text\\{[^}]*?)(?<!\\\\)_", "\\1\\\\_", p, perl = TRUE)
      if (identical(q, p)) break
      p <- q
    }
  }
  p
}

# Unicode that pdflatex cannot typeset, as LaTeX commands for use inside
# math. Keyed by hex code point so matching works in any locale.
.math_symbols <- c(
  "3B1" = "alpha", "3B2" = "beta", "3B3" = "gamma", "3B4" = "delta",
  "3B5" = "epsilon", "3B6" = "zeta", "3B7" = "eta", "3B8" = "theta",
  "3BA" = "kappa", "3BB" = "lambda", "3BC" = "mu", "3BD" = "nu",
  "3BE" = "xi", "3C0" = "pi", "3C1" = "rho", "3C3" = "sigma",
  "3C4" = "tau", "3C5" = "upsilon", "3C6" = "phi", "3C7" = "chi",
  "3C8" = "psi", "3C9" = "omega", "393" = "Gamma", "394" = "Delta",
  "398" = "Theta", "39B" = "Lambda", "39E" = "Xi", "3A0" = "Pi",
  "3A3" = "Sigma", "3A5" = "Upsilon", "3A6" = "Phi", "3A8" = "Psi",
  "3A9" = "Omega", "2248" = "approx", "2264" = "leq", "2265" = "geq",
  "2260" = "neq", "B1" = "pm", "D7" = "times", "F7" = "div",
  "B7" = "cdot", "221E" = "infty"
)

# Inside $...$ / $$...$$, Greek letters and math symbols become LaTeX
# commands; outside math, escape_text() spells them out instead.
unicode_in_math <- function(p) {
  m <- gregexpr("(?<!\\\\)(\\$\\$[^$]+\\$\\$|\\$[^$\n]+\\$)", p, perl = TRUE)
  regmatches(p, m) <- lapply(regmatches(p, m), function(spans) {
    vapply(spans, function(sp) {
      cps <- utf8ToInt(enc2utf8(sp))
      cmd <- .math_symbols[sprintf("%X", cps)]
      out <- vapply(cps, intToUtf8, "")
      out[!is.na(cmd)] <- paste0("\\", cmd[!is.na(cmd)], " ")
      paste(out, collapse = "")
    }, "", USE.NAMES = FALSE)
  })
  p
}

escape_text <- function(x, destination = NULL) {
  if (is.null(x) || length(x) == 0L) return("")
  fence_re <- "(?s)```[^\n]*\n.*?```"
  m        <- gregexpr(fence_re, x, perl = TRUE)
  fences   <- regmatches(x, m)[[1L]]
  parts    <- regmatches(x, m, invert = TRUE)[[1L]]
  escape_plain <- function(p) {
    # Headings would clash with the exams Question/Solution sections (and
    # break LaTeX when they contain math), so they become bold lines. Bold
    # markers are protected with \001 while every other * is escaped.
    hm <- gregexpr("(?m)^#{1,6}[ \t]+.*$", p, perl = TRUE)
    regmatches(p, hm) <- lapply(regmatches(p, hm), function(h) {
      h <- gsub("*", "", sub("^#{1,6}[ \t]+", "", h), fixed = TRUE)
      paste0("\001", trimws(sub("[ \t]#+[ \t]*$", "", h)), "\001")
    })
    p <- gsub("\\*\\*([^*\n]+?)\\*\\*", "\001\\1\001", p, perl = TRUE)
    p <- gsub("*",       "\\*",      p, fixed = TRUE)
    p <- gsub("\001",   "**",       p, fixed = TRUE)
    p <- normalise_math(p, destination)
    p <- unicode_in_math(p)
    p <- gsub("`",       "'",        p)
    # Math operators
    p <- gsub("\u2248", "approx.",  p)
    p <- gsub("\u2264", "<=",       p)
    p <- gsub("\u2265", ">=",       p)
    p <- gsub("\u2260", "!=",       p)
    p <- gsub("\u221E", "Inf",      p)
    p <- gsub("\u221A", "sqrt",     p)
    p <- gsub("\u00B1", "+/-",      p)
    p <- gsub("\u00D7", "x",        p)
    p <- gsub("\u00F7", "/",        p)
    p <- gsub("\u2032", "'",        p)   # prime
    p <- gsub("\u00B2", "^2",       p)
    p <- gsub("\u00B3", "^3",       p)
    p <- gsub("\u00B9", "^1",       p)
    # Arrows
    p <- gsub("\u2192", "->",       p)
    p <- gsub("\u2190", "<-",       p)
    p <- gsub("\u2194", "<->",      p)
    p <- gsub("\u21D2", "=>",       p)
    # Greek (lowercase)
    p <- gsub("\u03B1", "alpha",    p)
    p <- gsub("\u03B2", "beta",     p)
    p <- gsub("\u03B3", "gamma",    p)
    p <- gsub("\u03B4", "delta",    p)
    p <- gsub("\u03B5", "epsilon",  p)
    p <- gsub("\u03B6", "zeta",     p)
    p <- gsub("\u03B7", "eta",      p)
    p <- gsub("\u03B8", "theta",    p)
    p <- gsub("\u03BA", "kappa",    p)
    p <- gsub("\u03BB", "lambda",   p)
    p <- gsub("\u03BC", "mu",       p)
    p <- gsub("\u03BD", "nu",       p)
    p <- gsub("\u03BE", "xi",       p)
    p <- gsub("\u03C0", "pi",       p)
    p <- gsub("\u03C1", "rho",      p)
    p <- gsub("\u03C3", "sigma",    p)
    p <- gsub("\u03C4", "tau",      p)
    p <- gsub("\u03C5", "upsilon",  p)
    p <- gsub("\u03C6", "phi",      p)
    p <- gsub("\u03C7", "chi",      p)
    p <- gsub("\u03C8", "psi",      p)
    p <- gsub("\u03C9", "omega",    p)
    # Greek (uppercase)
    p <- gsub("\u0393", "Gamma",    p)
    p <- gsub("\u0394", "Delta",    p)
    p <- gsub("\u0398", "Theta",    p)
    p <- gsub("\u039B", "Lambda",   p)
    p <- gsub("\u039E", "Xi",       p)
    p <- gsub("\u03A0", "Pi",       p)
    p <- gsub("\u03A3", "Sigma",    p)
    p <- gsub("\u03A5", "Upsilon",  p)
    p <- gsub("\u03A6", "Phi",      p)
    p <- gsub("\u03A8", "Psi",      p)
    p <- gsub("\u03A9", "Omega",    p)
    # Typographic
    p <- gsub("\u2013", "-",        p)   # en dash
    p <- gsub("\u2014", "--",       p)   # em dash
    p <- gsub("\u2018", "'",        p)   # left single quote
    p <- gsub("\u2019", "'",        p)   # right single quote
    p <- gsub("\u201C", "\"",       p)   # left double quote
    p <- gsub("\u201D", "\"",       p)   # right double quote
    p <- gsub("\u2026", "...",      p)   # ellipsis
    p <- gsub("\u00A0", " ",        p)   # non-breaking space
    p
  }
  escaped <- lapply(parts, escape_plain)
  result  <- character(length(escaped) + length(fences))
  result[seq(1L, length(result), 2L)] <- escaped
  if (length(fences) > 0L)
    result[seq(2L, length(result), 2L)] <- fences
  paste(result, collapse = "")
}

r_char_vec <- function(xs) {
  quoted <- vapply(xs, function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
  }, character(1L))
  paste0("c(", paste(quoted, collapse = ", "), ")")
}

fill_template <- function(tpl_path, values) {
  tpl <- paste(readLines(tpl_path, warn = FALSE), collapse = "\n")
  for (nm in names(values)) {
    tpl <- gsub(paste0("{{", nm, "}}"), values[[nm]], tpl, fixed = TRUE)
  }
  tpl
}

# Returns list(decimal_places, tolerance, format_note) from a precision mode.
# mode = "fixed" uses n_digits; mode = "auto" derives dp from correct_answer.
compute_precision <- function(correct_answer, mode, n_digits = 2L) {
  if (mode == "auto") {
    s  <- format(correct_answer, scientific = FALSE)
    dp <- if (grepl("\\.", s)) nchar(sub("^[^.]*\\.", "", s)) else 0L
    if (dp > 4L) {
      warning(paste0("correct_answer = ", correct_answer, " requires ", dp,
                     " decimal places; capping at 4."))
      dp <- 4L
    }
  } else {
    dp <- as.integer(n_digits)
  }
  list(
    decimal_places = dp,
    tolerance      = if (dp == 0L) 0.5 else 0.5 * 10^(-dp),
    format_note    = if (dp == 0L) {
      "*Enter a whole number.*"
    } else {
      unit <- if (dp == 1L) "decimal place" else "decimal places"
      paste0("*Round your answer to ", dp, " ", unit,
             ". Use a period, not a comma, as the decimal separator.*")
    }
  )
}

.find_templates <- function() {
  path <- system.file("templates", package = "RAISE")
  if (!nzchar(path)) path <- file.path("inst", "templates")
  path
}

# Figure references (![alt](figures/f){attrs}) as the exams Rmd needs them.
# PDF: a centred \includegraphics in place (a lone image would otherwise become
# a floating "Figure n" with the alt text as caption); inline = TRUE gives
# inline raw LaTeX for answer choices. HTML (Canvas, TestVision): an inline
# image with no caption. Default width 60%; {width=...} overrides it.
render_figure_refs <- function(x, destination, inline = FALSE) {
  m <- gregexpr(.figure_ref_pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(refs) {
    unname(vapply(refs, function(ref) {
      parts <- regmatches(ref, regexec(.figure_ref_pattern, ref, perl = TRUE))[[1L]]
      alt   <- parts[[2L]]; file <- parts[[3L]]; attrs <- parts[[4L]]
      width <- sub('^.*width\\s*=\\s*"?([^"\\s}]+).*$', "\\1", attrs, perl = TRUE)
      if (!nzchar(attrs) || identical(width, attrs)) width <- "60%"
      if (destination == "pdf") {
        w <- if (endsWith(width, "%"))
          paste0(as.numeric(sub("%$", "", width)) / 100, "\\linewidth") else width
        tex <- paste0("\\includegraphics[width=", w, "]{", file, "}")
        if (inline) paste0("`", tex, "`{=latex}")
        else paste0("\n\n```{=latex}\n\\begin{center}\n", tex, "\n\\end{center}\n```\n\n")
      } else {
        paste0("![", alt, "](", file, "){width=", width, "}\\ ")
      }
    }, ""))
  })
  x
}

# An Rmd chunk that hands the question's figures to exams as supplements.
figure_supplement_chunk <- function(q, figures_dir) {
  files <- question_figures(q)
  if (length(files) == 0L) return("")
  if (is.null(figures_dir))
    stop("question references figures, but no figures folder was given")
  paste0("```{r, include = FALSE}\nexams::include_supplement(",
         r_char_vec(files), ", dir = ", deparse(normalizePath(figures_dir, mustWork = FALSE)),
         ")\n```\n\n")
}

make_rmd <- function(q, index, template_dir, precision_mode = "fixed", n_digits = 2L,
                     include_precision_note = TRUE, destination = "pdf",
                     figures_dir = NULL) {
  type    <- q[["type"]]
  fig     <- function(x, inline = FALSE) render_figure_refs(x, destination, inline)
  stem    <- fig(tce(q[["question"]],    destination))
  explain <- fig(tce(q[["explanation"]], destination))
  exhibit <- q[["exhibit"]]

  exhibit_block <- if (is.character(exhibit) && length(exhibit) == 1L &&
                       nzchar(trimws(exhibit))) {
    paste0("\n", fig(tce(exhibit, destination)))
  } else {
    ""
  }

  if (type == "multiple_choice") {
    choices      <- vapply(q[["choices"]], function(ch)
      fig(escape_text(ch[["choice"]], destination), inline = TRUE), "")
    correct      <- sapply(q[["choices"]], `[[`, "correct")
    n_correct    <- sum(correct)
    extype       <- if (n_correct > 1L) "mchoice" else "schoice"
    solution_str <- paste(as.integer(correct), collapse = "")
    true_false   <- ifelse(correct, "True", "False")

    rmd <- fill_template(
      file.path(template_dir, "multiple_choice_question.Rmd"),
      list(
        stem          = stem,
        exhibit_block = exhibit_block,
        choices_r     = r_char_vec(choices),
        explain       = explain,
        true_false_r  = r_char_vec(true_false),
        extype        = extype,
        solution_str  = solution_str,
        index         = as.character(index)
      )
    )
  } else if (type == "numeric") {
    prec <- compute_precision(q[["correct_answer"]], precision_mode, n_digits)

    rmd <- fill_template(
      file.path(template_dir, "numeric_question.Rmd"),
      list(
        stem           = stem,
        exhibit_block  = exhibit_block,
        format_note    = if (include_precision_note) prec$format_note else "",
        explain        = explain,
        correct_answer = as.character(q[["correct_answer"]]),
        tolerance      = as.character(prec$tolerance),
        index          = as.character(index)
      )
    )
  } else {
    stop(paste0("export.R: unknown question type '", type, "'"))
  }
  paste0(figure_supplement_chunk(q, figures_dir), rmd)
}

read_regression_output <- function(study_id, session_path) {
  study_dir <- file.path(session_path, "studies", study_id)
  reg_path  <- file.path(study_dir, "regression.txt")
  if (!file.exists(reg_path)) return(NULL)
  reg <- paste(trimws(readLines(reg_path, warn = FALSE)), collapse = "\n")
  or_path <- file.path(study_dir, "odds_ratios.txt")
  if (file.exists(or_path)) {
    or <- paste(trimws(readLines(or_path, warn = FALSE)), collapse = "\n")
    paste0(reg, "\n\n", or)
  } else {
    reg
  }
}

inject_regression <- function(q, regression_output) {
  if (is.null(regression_output)) return(q)
  if (startsWith(trimws(q[["question"]]), "```")) return(q)
  q[["question"]] <- paste0("```\n", regression_output, "\n```\n\n", q[["question"]])
  q
}

# Convert every fenced code block in a text field (question / exhibit /
# explanation) to a destination-appropriate pandoc raw block.  Safe to call
# because text fields never contain R code chunks.
transform_code_fences <- function(text, destination) {
  fence_re <- "(?s)```[^\n]*\n.*?```"
  m        <- gregexpr(fence_re, text, perl = TRUE)
  fences   <- regmatches(text, m)[[1L]]
  if (length(fences) == 0L) return(text)
  parts    <- regmatches(text, m, invert = TRUE)[[1L]]

  convert <- function(f) {
    content <- sub("^```[^\n]*\n", "", f)
    content <- sub("\n```$", "", content)
    if (destination == "pdf") {
      paste0("```{=latex}\n\\begin{verbatim}\n", content, "\n\\end{verbatim}\n```")
    } else {
      safe <- gsub("&", "&amp;", content, fixed = TRUE)
      safe <- gsub("<", "&lt;", safe, fixed = TRUE)
      safe <- gsub(">", "&gt;", safe, fixed = TRUE)
      paste0("```{=html}\n<pre>\n", safe, "\n</pre>\n```")
    }
  }

  converted <- lapply(fences, convert)
  result    <- character(length(parts) + length(converted))
  result[seq(1L, length(result), 2L)] <- parts
  result[seq(2L, length(result), 2L)] <- converted
  paste(result, collapse = "")
}

tce <- function(x, destination) transform_code_fences(escape_text(x, destination), destination)

# The exams "plain" PDF template with its solution environment commented out.
pdf_template_without_solutions <- function(dir) {
  tex  <- readLines(system.file("tex", "plain.tex", package = "exams"), warn = FALSE)
  line <- grep("^\\\\newenvironment\\{solution\\}", tex)
  if (length(line) != 1L)
    stop("could not find the solution environment in the exams PDF template")
  tex[line] <- "\\newenvironment{solution}{\\comment}{\\endcomment}"
  path <- file.path(normalizePath(dir), "questions.tex")
  writeLines(tex, path)
  path
}

# Export a question set JSON to the exams package format in-process.
export_to_exams <- function(json_path, output_dir, destination = "pdf",
                            precision_mode = "fixed", n_digits = 2L,
                            include_precision_note = TRUE, answers = TRUE,
                            figures_dir = NULL) {
  if (!requireNamespace("exams", quietly = TRUE))
    stop("Package 'exams' is required for export. Install it with install.packages('exams').")

  valid <- c("testvision", "canvas", "pdf")
  if (!destination %in% valid)
    stop(paste0("export_to_exams: destination must be one of: ",
                paste(valid, collapse = ", ")))

  qset      <- jsonlite::read_json(json_path, simplifyVector = FALSE)
  qset_id   <- qset[["id"]]
  questions <- qset[["questions"]]

  if (length(questions) == 0L)
    stop("export_to_exams: question set contains no questions")

  template_dir <- .find_templates()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  rmd_paths <- character(length(questions))
  for (i in seq_along(questions)) {
    rmd_content <- make_rmd(questions[[i]], i, template_dir,
                            precision_mode, n_digits, include_precision_note,
                            destination, figures_dir)
    rmd_path    <- file.path(output_dir, paste0("q_", i, ".Rmd"))
    writeLines(rmd_content, rmd_path)
    rmd_paths[i] <- rmd_path
  }

  exams_fn     <- get(paste0("exams2", destination), envir = asNamespace("exams"))
  before_files <- normalizePath(list.files(output_dir, full.names = TRUE), mustWork = FALSE)

  args <- list(file = as.list(basename(rmd_paths)), dir = ".", name = qset_id)
  if (destination == "pdf" && !answers)
    args$template <- pdf_template_without_solutions(output_dir)
  withr::with_dir(output_dir, do.call(exams_fn, args))
  if (!is.null(args$template)) file.remove(args$template)

  if (!getOption("keep_rmd", default = FALSE))
    file.remove(rmd_paths)

  after_files  <- normalizePath(list.files(output_dir, full.names = TRUE), mustWork = FALSE)
  new_files    <- setdiff(after_files, before_files)
  output_files <- new_files[!grepl("\\.Rmd$", new_files, ignore.case = TRUE)]

  if (length(output_files) == 0L)
    stop("export_to_exams: no output file produced by exams2", destination)

  output_files[[1L]]
}

to_snake_case <- function(s) {
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  s
}

# Questions of the given sets in order, with the study's regression output
# prepended, plus where each question came from.
load_export_questions <- function(qset_ids, session_path) {
  qset_dir  <- file.path(session_path, "question_sets")
  questions <- list()
  origins   <- list()
  for (qid in qset_ids) {
    data <- tryCatch(
      jsonlite::read_json(file.path(qset_dir, paste0(qid, ".json"))),
      warning = function(w) NULL,
      error   = function(e) NULL
    )
    if (is.null(data)) next
    study_id <- data[["study_id"]]
    reg_out  <- if (!is.null(study_id) && nzchar(study_id %||% ""))
      read_regression_output(study_id, session_path) else NULL
    qs <- lapply(data[["questions"]] %||% list(), inject_regression, reg_out)
    questions <- c(questions, qs)
    origins   <- c(origins, lapply(qs, function(q) list(qset_id = qid, id = q[["id"]] %||% "?")))
  }
  list(questions = questions, origins = origins)
}

# Export each question on its own to find the ones that break an export.
failing_questions <- function(questions, destination, precision_mode,
                              n_digits, include_precision_note, figures_dir = NULL) {
  which(vapply(seq_along(questions), function(i) {
    tmp  <- withr::local_tempdir()
    json <- file.path(tmp, "q.json")
    jsonlite::write_json(list(id = "q", questions = questions[i]), json,
                         pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
    tryCatch({
      suppressMessages(utils::capture.output(export_to_exams(
        json, file.path(tmp, "out"), destination = destination,
        precision_mode = precision_mode, n_digits = n_digits,
        include_precision_note = include_precision_note, figures_dir = figures_dir
      )))
      FALSE
    }, error = function(e) TRUE)
  }, logical(1L)))
}

#' Merge question sets and export to PDF, TestVision, Canvas, or Markdown.
#' @param qset_ids Character vector of question set IDs to export.
#' @param session_path Path to the session folder.
#' @param label Label string for the export (shown in the Exports panel).
#' @param destination Export format: \code{"pdf"}, \code{"testvision"}, \code{"canvas"},
#'   \code{"markdown"}, or \code{"json"} (one Markdown entry per question, see
#'   [questions_to_json()]; written as \code{<export id>.json}, figures embedded).
#' @param precision_mode Numeric precision: \code{"fixed"} or \code{"auto"}.
#' @param n_digits Number of decimal places when \code{precision_mode = "fixed"}.
#' @param include_precision_note Whether to append a rounding note below numeric questions.
#' @param answers Whether to include correct answers and explanations. Only
#'   \code{"pdf"}, \code{"markdown"} and \code{"json"} can leave them out; for \code{"canvas"}
#'   and \code{"testvision"}, \code{FALSE} is ignored with a warning (the LMS
#'   needs the answers to grade).
#' @export
export_question_sets <- function(qset_ids, session_path, label,
                                 destination = "pdf",
                                 precision_mode = "fixed", n_digits = 2L,
                                 include_precision_note = TRUE,
                                 answers = TRUE) {
  valid <- c("pdf", "testvision", "canvas", "markdown", "json")
  if (!destination %in% valid)
    return(paste0("Error: destination must be one of: ", paste(valid, collapse = ", ")))
  note <- NULL
  if (!answers && !destination %in% c("pdf", "markdown", "json")) {
    note <- paste0("answers = FALSE is ignored for ", destination,
                   ": the export includes the answers (only pdf, markdown and json can leave them out).")
    warning(note, call. = FALSE)
    answers <- TRUE
  }

  loaded             <- load_export_questions(qset_ids, session_path)
  combined_questions <- loaded$questions
  origins            <- vapply(loaded$origins, function(o) paste0(o$qset_id, " #", o$id), "")
  if (length(combined_questions) == 0L)
    return("Error: no questions found in the selected question sets.")

  out_dir     <- new_export_dir(session_path, to_snake_case(label),
                                format(Sys.time(), "%Y%m%d_%H%M%S"))
  combined_id <- basename(out_dir)

  combined_json <- list(
    id               = combined_id,
    topic            = "combined",
    study_id         = NULL,
    source_materials = list(),
    questions        = combined_questions,
    model            = "",
    created_at       = ""
  )
  json_path <- file.path(out_dir, "combined.json")
  jsonlite::write_json(combined_json, json_path, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")

  result <- tryCatch(
    if (destination == "markdown") {
      md_path <- file.path(out_dir, paste0(combined_id, ".md"))
      questions_to_markdown(qset_ids, session_path, answers = answers,
                            precision_mode = precision_mode, n_digits = n_digits,
                            include_precision_note = include_precision_note,
                            file = md_path)
      list(path = md_path)
    } else if (destination == "json") {
      json_path <- file.path(out_dir, paste0(combined_id, ".json"))
      questions_to_json(qset_ids, session_path, answers = answers,
                        precision_mode = precision_mode, n_digits = n_digits,
                        include_precision_note = include_precision_note,
                        file = json_path)
      list(path = json_path)
    } else {
      list(path = export_to_exams(json_path, out_dir, destination = destination,
                                  precision_mode = precision_mode, n_digits = n_digits,
                                  include_precision_note = include_precision_note,
                                  answers = answers,
                                  figures_dir = figures_dir(session_path)))
    },
    error = function(e) list(error = conditionMessage(e))
  )

  if (!is.null(result$error)) {
    unlink(out_dir, recursive = TRUE)
    msg <- paste0("Export failed: ", result$error)
    bad <- if (destination %in% c("markdown", "json")) integer(0) else
      failing_questions(combined_questions, destination, precision_mode,
                        n_digits, include_precision_note, figures_dir(session_path))
    if (length(bad) > 0L)
      msg <- paste0(msg, "\nQuestions that fail on their own (question set #id): ",
                    paste(origins[bad], collapse = ", "))
    return(msg)
  }

  meta <- list(
    label       = label,
    destination = destination,
    answers     = answers,
    n_questions = length(combined_questions),
    created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  jsonlite::write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE)

  paste0("Exported ", length(combined_questions), " question(s) to '",
         combined_id, "' (", destination, ").",
         if (!is.null(note)) paste0(" Note: ", note) else "")
}

# Creates exports/<slug>_<ts>; if that exists (two exports with the same label in one
# second), exports/<slug>_<ts>_2, _3, ... so an export never writes into another one.
new_export_dir <- function(session_path, slug, ts) {
  base <- file.path(session_path, "exports", paste0(slug, "_", ts))
  dir  <- base
  k    <- 1L
  while (dir.exists(dir)) {
    k   <- k + 1L
    dir <- paste0(base, "_", k)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Delete an export folder and all its artifacts.
#' @param export_id Export folder name (e.g. \code{"combined_20260506_143012"}).
#' @param session_path Path to the session folder.
#' @export
delete_export <- function(export_id, session_path) {
  export_dir <- file.path(session_path, "exports", export_id)
  if (!dir.exists(export_dir))
    return(paste0("Error: export not found: ", export_id))
  unlink(export_dir, recursive = TRUE)
  paste0("Export '", export_id, "' deleted.")
}
