#' PythonInterface class
#'
#' @description Some description
#' @rdname PythonInterface
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
PythonInterface <- R6::R6Class(
    "PythonInterface",
    private = list(
        py_main = NULL,
        py_sources = c(),
        py_modules = c(),
        py_files_sourced = FALSE,
        #' @importFrom reticulate py_run_string
        source_py_files = function(paths = c(), recursive = T) {
            for (path in paths) {
                if (dir.exists(path) && recursive) {
                    private$source_py_files(file.path(path, dir(path)))
                } else if (file.exists(path) && grepl("\\.py$", path)) {
                    reticulate::py_run_string(paste0(readLines(path), collapse = "\n"))
                }
            }
        },
        run_after_startup = function() {
            TRUE
        },
        py_main_imported = function() {
            !is.null(private$py_main)
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        #' @description PythonInterface class constructor.
        initialize = function(py_sources = c(), py_modules = c()) {
            private$py_sources <- py_sources
            private$py_modules <- py_modules
        },
        #' Checks whether python and module openai are installed.
        #' @param ask Logical. Default is FALSE. Indicates whether user is asked whether
        #' openai should be installed if it is not available.
        #' @export
        #' @importFrom reticulate py_available py_install import_main import py_run_string py_module_available
        #' @importFrom stringr str_glue
        is_ready = function(install = T, source_py_files = F, ...) {
            if (!(py_available <- reticulate::py_available(initialize = T))) {
                message("No python instance could be found. Please install python instance.")
            }
            
            if (!(py_main_imported <- private$py_main_imported())) {
                if (install) {
                    private$py_main <- reticulate::import_main()
                    message("Python main module has been imported.")
                    py_main_imported <- TRUE
                } else {
                    message("Python main module is not imported.")
                }
            }
            
            if (py_available) {
                installed <- tryCatch({
                    all(sapply(private$py_modules, reticulate::py_module_available))
                }, error = function(e) {
                    FALSE
                })
            } else {
                installed <- FALSE
            }
            
            if (!installed) {
                if (install) {
                    for (py_module in private$py_modules) {
                        message(stringr::str_glue(
                            "Installing {module}\n",
                            module = py_module
                        ))
                        py_install(py_module, pip = T)
                        message(stringr::str_glue(
                            "Installing {module} done\n",
                            module = py_module
                        ))
                    }
                    installed <- TRUE
                    message("Required python modules have been installed.")
                } else {
                    message("Required python modules are not installed.")
                }
            }
            
            if (!private$py_files_sourced || source_py_files) {
                private$source_py_files(private$py_sources)
            }
            
            run_after_startup_ok <- private$run_after_startup(install = install, ...)
            
            all(py_available, py_main_imported, installed, run_after_startup_ok)
        }
    )
)
