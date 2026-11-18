#' Read a lecture PDF, Markdown, or plain-text file and return its text.
#' @param file_path Absolute path to the file.
#' @param max_chars Maximum number of characters to return.
#' @export
load_material <- function(file_path, max_chars = 8000L) {
  ext <- tolower(tools::file_ext(file_path))
  if (!file.exists(file_path)) {
    stop(paste0("load_material: file not found: ", file_path))
  }
  text <- switch(ext,
    pdf = {
      pages <- pdftools::pdf_text(file_path)
      paste(pages, collapse = "\n")
    },
    md        = ,
    markdown  = ,
    txt       = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
    stop(paste0("load_material: unsupported file extension '.", ext, "'"))
  )
  if (nchar(text) > max_chars) {
    text <- substr(text, 1L, max_chars)
  }
  text
}
