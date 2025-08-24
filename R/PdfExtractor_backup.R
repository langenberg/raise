#' PdfExtractorBackup class
#'
#' @description Some description
#' @rdname PdfExtractorBackup
#' @keywords internal
#' @importFrom R6 R6Class
#' @details Some details.
#'
PdfExtractorBackup <- R6::R6Class(
    "PdfExtractorBackup",
    private = list(
        py_main = NULL,
        #' @importFrom reticulate py_run_string
        source_py_files = function(paths, recursive = T) {
            for (path in paths) {
                if (dir.exists(path) && recursive) {
                    private$source_py_files(file.path(path, dir(path)))
                } else if (file.exists(path) && grepl("\\.py$", path)) {
                    reticulate::py_run_string(paste0(readLines(path), collapse = "\n"))
                }
            }
        }
    ),
    active = list(
    ),
    public = list(
        #' @keywords internal
        #' @description PdfExtractor class constructor.
        initialize = function() {
            
        },
        #' Checks whether python and module openai are installed.
        #' @param ask Logical. Default is FALSE. Indicates whether user is asked whether
        #' openai should be installed if it is not available.
        #' @keywords internal
        #' @importFrom reticulate py_available py_install import_main import py_run_string py_module_available
        is_ready = function(install = T, ...) {
            if (!(py_available <- reticulate::py_available(initialize = T))) {
                message("No python instance could be found. Please install python instance.")
            }
            
            if (!(main_imported <- !is.null(private$py_main))) {
                if (install) {
                    private$py_main <- reticulate::import_main()
                    message("Python main module has been imported.")
                    main_imported <- TRUE
                } else {
                    message("Python main module is not imported.")
                }
            }
            
            if (py_available) {
                installed <- tryCatch({all(
                    reticulate::py_module_available("pymupdf4llm"),
                    reticulate::py_module_available("pathlib"),
                    reticulate::py_module_available("unicodedata")
                )}, error = function(e) {
                    FALSE
                })
            } else {
                installed <- FALSE
            }
            
            if (!installed) {
                if (install) {
                    message("Installing pymupdf4llm\n")
                    py_install("pymupdf4llm", pip = T)
                    message("Installing pymupdf4llm done\n")
                    message("Installing pathlib\n")
                    py_install("pathlib", pip = T)
                    message("Installing pathlib done\n")
                    message("Installing unicodedata\n")
                    py_install("unicodedata", pip = T)
                    message("Installing unicodedata done\n")
                    
                    installed <- TRUE
                    message("Required python modules have been installed.")
                } else {
                    message("Required python modules are not installed.")
                }
            }
            
            if (!main_imported) {
                message("Python object PdfExtractor is not instantiated.")
                pdf_extractor_exists <- FALSE
            } else {
                reticulate::py_run_string("pdf_extractor_exists = 'pdf_extractor' in locals() or 'pdf_extractor' in globals()")
                pdf_extractor_exists <-  private$py_main$pdf_extractor_exists
                
                if (!pdf_extractor_exists) {
                    if (install) {
                        private$source_py_files(system.file("python", package = "raise"))
                        
                        reticulate::py_run_string("pdf_extractor = PdfExtractor()")
                        message("Python object PdfExtractor has been instantiated.")
                        
                        pdf_extractor_exists <- TRUE
                    } else {
                        message("Python object PdfExtractor is not instantiated.")
                    }
                }
            }
            
            all(py_available, main_imported, installed, pdf_extractor_exists)
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

