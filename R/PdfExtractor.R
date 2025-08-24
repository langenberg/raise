#' PdfExtractor class
#'
#' @description Some description
#' @rdname PdfExtractor
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
PdfExtractor <- R6::R6Class(
    "PdfExtractor",
    inherit = PythonInterface,
    private = list(
        py_modules = c(),
        py_sources = c(),
        run_after_startup = function(install = TRUE, ...) {
            if (!private$py_main_imported()) {
                message("Python object PdfExtractor is not instantiated.")
                pdf_extractor_exists <- FALSE
            } else {
                reticulate::py_run_string("pdf_extractor_exists = 'pdf_extractor' in locals() or 'pdf_extractor' in globals()")
                pdf_extractor_exists <-  private$py_main$pdf_extractor_exists
                
                if (!pdf_extractor_exists) {
                    if (install) {
                        private$source_py_files()
                        
                        reticulate::py_run_string("pdf_extractor = PdfExtractor()")
                        message("Python object PdfExtractor has been instantiated.")
                        
                        pdf_extractor_exists <- TRUE
                    } else {
                        message("Python object PdfExtractor is not instantiated.")
                    }
                }
            }
            
            pdf_extractor_exists
        }
    ),
    active = list(
    ),
    public = list(
        #' @keywords internal
        #' @description PdfExtractor class constructor.
        initialize = function() {
            super$initialize()
            private$py_modules <- c("pymupdf4llm","pathlib","unicodedata")
            private$py_sources <- system.file("python", package = "raise")
            invisible(self)
        },
        #' @export
        extract = function(paths) {
            if (!self$is_ready(install = TRUE)) {
                stop("Python instance not ready.")
            }
            if (!is.list(paths)) {
                paths <- as.list(paths)
            }
            private$py_main$pdf_extractor$extract(
                paths = paths
            )
        }
    )
)
