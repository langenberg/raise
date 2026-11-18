# Figures live in <session>/figures/ as <slug>_<hash8>.<ext>: the hash is the
# first 8 hex digits of the file's MD5, so a file never changes once saved
# (a changed figure is a new file), the same image is stored only once, and
# two different images never share a name. Questions reference figures with
# Markdown image syntax: ![alt](figures/<file>), optionally followed by
# pandoc attributes such as {width=50%}.

FIGURE_EXTS <- c("png", "jpg", "jpeg")

.figure_ref_pattern <- r"{!\[([^\]\n]*)\]\(figures/([^)\s]+)\)(\{[^}\n]*\})?}"

figures_dir <- function(session_path) file.path(session_path, "figures")

.slugify <- function(x, max_chars = 40L) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- substr(x, 1L, max_chars)
  x <- gsub("_+$", "", x)
  if (!nzchar(x)) "figure" else x
}

# Figure file names referenced in a text (NULL / non-character gives none).
figure_files_in <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) return(character(0))
  figure_files_in_all(text)
}

.question_texts <- function(q) {
  choices <- vapply(q[["choices"]] %||% list(), function(ch) {
    x <- ch[["choice"]]; if (is.character(x)) x else ""
  }, "")
  Filter(is.character, c(list(q[["question"]], q[["exhibit"]], q[["explanation"]]),
                         as.list(choices)))
}

# Figure files referenced by a question (all text fields).
question_figures <- function(q) {
  unique(unlist(lapply(.question_texts(q), figure_files_in), use.names = FALSE))
}

# Errors for figure references that do not exist in the session.
check_figure_refs <- function(questions, session_path) {
  errors <- character(0)
  for (i in seq_along(questions)) {
    files   <- question_figures(questions[[i]])
    missing <- files[!file.exists(file.path(figures_dir(session_path), files))]
    if (length(missing) > 0L)
      errors <- c(errors, paste0("questions[", i, "]: figure not found: ",
                                 paste0("figures/", missing, collapse = ", "),
                                 " (use list_figures to see the available files)"))
  }
  errors
}

#' Add an image file to the session's figures folder.
#'
#' The file is stored as `figures/<slug>_<hash>.<ext>`, where the hash comes
#' from the file content. Adding the same image again returns the existing file.
#'
#' @param file Path to a PNG or JPEG file.
#' @param session_path Path to the session folder.
#' @param name Optional readable name for the slug; defaults to the file name.
#' @return A list with `file` (name inside `figures/`), `path`
#'   (`figures/<file>`, as used in questions), `markdown` (an image reference
#'   to paste into a question) and `existed` (`TRUE` if the image was already
#'   in the session).
#' @export
add_figure <- function(file, session_path, name = NULL) {
  if (!file.exists(file)) stop("add_figure: file not found: ", file)
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% FIGURE_EXTS && !is.null(name)) ext <- tolower(tools::file_ext(name))
  if (!ext %in% FIGURE_EXTS)
    stop("add_figure: only PNG and JPEG images are supported")
  if (ext == "jpeg") ext <- "jpg"
  dir  <- figures_dir(session_path)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  hash <- substr(unname(tools::md5sum(file)), 1L, 8L)
  existing <- list.files(dir, pattern = paste0("_", hash, "\\.", ext, "$"))
  existed  <- length(existing) > 0L
  fname <- if (existed) existing[[1L]] else {
    slug <- .slugify(tools::file_path_sans_ext(basename(name %||% file)))
    f <- paste0(slug, "_", hash, ".", ext)
    file.copy(file, file.path(dir, f))
    f
  }
  list(file = fname, path = file.path("figures", fname),
       markdown = paste0("![", gsub("_", " ", sub("_[0-9a-f]{8}$", "",
                                    tools::file_path_sans_ext(fname))),
                         "](figures/", fname, ")"),
       existed = existed)
}

# Named list: figure file -> character vector of "qset_id #id" using it.
figure_usage <- function(session_path) {
  usage <- list()
  for (qs in load_all_question_sets(session_path)) {
    for (q in qs[["questions"]] %||% list()) {
      for (f in question_figures(q))
        usage[[f]] <- c(usage[[f]], paste0(qs[["id"]], " #", q[["id"]] %||% "?"))
    }
  }
  usage
}

#' List the figures of a session.
#' @param session_path Path to the session folder.
#' @return JSON array: one object per figure with `file`, `path`, `size_kb`,
#'   `created_at`, `used_by` (question set and question id of every question
#'   referencing it) and `unused`.
#' @export
list_figures <- function(session_path) {
  dir   <- figures_dir(session_path)
  files <- if (dir.exists(dir))
    list.files(dir, pattern = paste0("\\.(", paste(FIGURE_EXTS, collapse = "|"), ")$"),
               ignore.case = TRUE) else character(0)
  usage <- figure_usage(session_path)
  out <- lapply(files, function(f) {
    info <- file.info(file.path(dir, f))
    used <- usage[[f]] %||% character(0)
    list(file = f, path = file.path("figures", f),
         size_kb = round(info$size / 1024, 1),
         created_at = format(info$mtime, "%Y-%m-%dT%H:%M:%S"),
         used_by = I(used), unused = length(used) == 0L)
  })
  as.character(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE))
}

#' Delete figures from the session.
#'
#' A figure that questions still use is only deleted with `force = TRUE`
#' (the questions then show a broken reference until they are updated).
#' @param files File names inside `figures/` (with or without the `figures/` prefix).
#' @param session_path Path to the session folder.
#' @param force Delete figures that are still in use.
#' @return One message per file.
#' @export
delete_figures <- function(files, session_path, force = FALSE) {
  usage <- figure_usage(session_path)
  msgs <- vapply(sub("^figures/", "", unlist(files)), function(f) {
    path <- file.path(figures_dir(session_path), f)
    if (!file.exists(path)) return(paste0("Figure not found: ", f))
    used <- usage[[f]] %||% character(0)
    if (length(used) > 0L && !force)
      return(paste0("Not deleted: '", f, "' is used by ", paste(used, collapse = ", "),
                    ". Replace it first (replace_figure) or delete with force."))
    file.remove(path)
    paste0("Figure '", f, "' deleted.")
  }, "")
  paste(msgs, collapse = "\n")
}

#' Delete every figure that no question uses.
#' @param session_path Path to the session folder.
#' @export
delete_unused_figures <- function(session_path) {
  figs   <- jsonlite::fromJSON(list_figures(session_path), simplifyVector = FALSE)
  unused <- vapply(Filter(function(f) isTRUE(f$unused), figs), `[[`, "", "file")
  if (length(unused) == 0L) return("No unused figures.")
  delete_figures(unused, session_path)
}

#' Point every reference to one figure at another figure.
#'
#' Rewrites `figures/<old>` to `figures/<new>` in all text fields of all
#' question sets. The old file stays in `figures/` (it is then unused).
#' @param old,new File names inside `figures/`.
#' @param session_path Path to the session folder.
#' @export
replace_figure <- function(old, new, session_path) {
  old <- sub("^figures/", "", old)
  new <- sub("^figures/", "", new)
  if (!file.exists(file.path(figures_dir(session_path), new)))
    return(paste0("Error: figure not found: ", new))
  from <- paste0("](figures/", old, ")")
  to   <- paste0("](figures/", new, ")")
  swap <- function(x) if (is.character(x)) gsub(from, to, x, fixed = TRUE) else x
  n <- 0L
  qset_dir <- file.path(session_path, "question_sets")
  for (f in list.files(qset_dir, pattern = "\\.json$", full.names = TRUE)) {
    data <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
    if (is.null(data)) next
    changed <- FALSE
    data$questions <- lapply(data$questions %||% list(), function(q) {
      if (!old %in% question_figures(q)) return(q)
      changed <<- TRUE
      n <<- n + 1L
      for (k in c("question", "exhibit", "explanation")) q[[k]] <- swap(q[[k]])
      q$choices <- lapply(q$choices, function(ch) { ch$choice <- swap(ch$choice); ch })
      q
    })
    if (changed)
      jsonlite::write_json(data, f, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
  }
  paste0("Replaced figures/", old, " with figures/", new, " in ", n, " question(s).")
}

# A tool result that sends the image to the model and shows text + image in
# the chat. shinychat would otherwise serialise the value (an S7 image object)
# to JSON for the tool card, which fails; extra$display replaces that.
figure_tool_result <- function(text, file, session_path) {
  file <- sub("^figures/", "", file)
  value <- figure_content(file, session_path)
  if (nzchar(text)) value <- list(ellmer::ContentText(text), value)
  ellmer::ContentToolResult(
    value = value,
    extra = list(display = list(
      markdown = paste0(if (nzchar(text)) paste0(text, "\n\n") else "",
                        "![", file, "](figures/", file, ")"),
      open = TRUE
    ))
  )
}

# The image as content for the model, downscaled when magick is available.
figure_content <- function(file, session_path) {
  path <- file.path(figures_dir(session_path), sub("^figures/", "", file))
  if (!file.exists(path)) stop("figure not found: ", file)
  resize <- if (requireNamespace("magick", quietly = TRUE)) "low" else "none"
  ellmer::content_image_file(path, resize = resize)
}

.plot_types <- c("scatter", "boxplot", "histogram", "bar")

#' Draw a figure from a study's simulated data and add it to the session.
#'
#' A fixed set of plot types, drawn with ggplot2 (no free R code).
#' @param study_id Study whose `data.csv` is plotted.
#' @param type One of `"scatter"`, `"boxplot"`, `"histogram"`, `"bar"`.
#' @param x Column on the x axis (the grouping column for boxplots and bars).
#' @param y Column on the y axis: required for scatter and boxplot; for bar,
#'   optional (bars show the mean of `y` per `x`, e.g. a proportion for a 0/1
#'   outcome; without `y` they show counts). Not used for histograms.
#' @param group Optional column for colour (scatter, boxplot, bar) or fill (histogram).
#' @param smooth Scatter only: add a least-squares line (per group).
#' @param title,x_label,y_label Optional title and axis labels.
#' @param name Optional readable name for the file.
#' @param session_path Path to the session folder.
#' @return The result of [add_figure()].
#' @export
create_figure <- function(study_id, type, x, y = NULL, group = NULL, smooth = FALSE,
                          title = NULL, x_label = NULL, y_label = NULL, name = NULL,
                          session_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("create_figure needs the ggplot2 package")
  type <- match.arg(type, .plot_types)
  csv  <- file.path(session_path, "studies", study_id, "data.csv")
  if (!file.exists(csv))
    stop("no simulated data for study '", study_id, "' (simulate the study first)")
  data <- utils::read.csv(csv, check.names = FALSE)
  cols <- c(x = x, y = y, group = group)
  bad  <- cols[!cols %in% names(data)]
  if (length(bad) > 0L)
    stop("unknown column(s): ", paste(bad, collapse = ", "),
         ". Available: ", paste(names(data), collapse = ", "))
  if (type %in% c("scatter", "boxplot") && is.null(y))
    stop(type, " needs y")
  if (!is.null(group)) data[[group]] <- factor(data[[group]])
  if (type %in% c("boxplot", "bar")) data[[x]] <- factor(data[[x]])

  # aes() from column names (do.call passes the symbols, aes() quotes them)
  map <- function(...) {
    m <- Filter(Negate(is.null), list(...))
    do.call(ggplot2::aes, lapply(m, as.name))
  }
  p <- switch(type,
    scatter = {
      p <- ggplot2::ggplot(data, map(x = x, y = y, colour = group)) +
        ggplot2::geom_point(alpha = 0.6)
      if (isTRUE(smooth))
        p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
      p
    },
    boxplot = ggplot2::ggplot(data, map(x = x, y = y, fill = group)) +
      ggplot2::geom_boxplot(),
    histogram = ggplot2::ggplot(data, map(x = x, fill = group)) +
      ggplot2::geom_histogram(bins = 30, colour = "white",
                              position = if (is.null(group)) "stack" else "identity",
                              alpha = if (is.null(group)) 1 else 0.6),
    bar = if (is.null(y)) {
      ggplot2::ggplot(data, map(x = x, fill = group)) +
        ggplot2::geom_bar(position = "dodge")
    } else {
      ggplot2::ggplot(data, map(x = x, y = y, fill = group)) +
        ggplot2::stat_summary(fun = mean, geom = "col", position = "dodge")
    }
  )
  p <- p + ggplot2::theme_bw(base_size = 13) +
    ggplot2::labs(title = title, x = x_label %||% x,
                  y = y_label %||% (if (type == "histogram" || (type == "bar" && is.null(y))) "count"
                                    else if (type == "bar") paste("mean of", y) else y))
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, p, width = 6, height = 4, dpi = 150)
  add_figure(tmp, session_path,
             name = paste0(name %||% paste(type, x, y %||% "", sep = "_"), ".png"))
}
