#' PyNotebookExtractor class
#'
#' @description Some description
#' @rdname PyNotebookExtractor
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
PyNotebookExtractor <- R6::R6Class(
    "PyNotebookExtractor",
    inherit = PythonInterface,
    private = list(
        py_modules = c(),
        py_sources = c(),
        run_after_startup = function(install = TRUE, ...) {
            if (!private$py_main_imported()) {
                message("Python object PyNotebookExtractor is not instantiated.")
                py_notebook_extractor_exists <- FALSE
            } else {
                reticulate::py_run_string("py_notebook_extractor_exists = 'py_notebook_extractor' in locals() or 'py_notebook_extractor' in globals()")
                py_notebook_extractor_exists <-  private$py_main$py_notebook_extractor_exists
                
                if (!py_notebook_extractor_exists) {
                    if (install) {
                        private$source_py_files()
                        
                        reticulate::py_run_string(
"
import json

def myfun(item):
    if isinstance(item, list):
        return [myfun(element) for element in item]
    elif not isinstance(item, dict):
        return item
    elif any([key == 'outputs' for key in list(item.keys())]):
        has_more_keys = any([not output['output_type'] == 'display_data' for output in item['outputs']])
        new_outputs = [output for output in item['outputs'] if not output['output_type'] == 'display_data']
        if has_more_keys:
            item['outputs'] = new_outputs
        else:
            item.pop('outputs', None)
        return item
    else:
        for key in list(item.keys()):
            item[key] = myfun(item[key])
        return item

def py_notebook_extractor(path):
    with open(path, 'r') as file:
        data = json.load(file)
    res = myfun(data)
    return json.dumps(res, indent = 4)

")
                        message("Python object PdfExtractor has been instantiated.")
                        
                        py_notebook_extractor_exists <- TRUE
                    } else {
                        message("Python object PdfExtractor is not instantiated.")
                    }
                }
            }
            
            py_notebook_extractor_exists
        }
    ),
    active = list(
    ),
    public = list(
        #' @keywords internal
        #' @description PdfExtractor class constructor.
        initialize = function() {
            super$initialize()
            private$py_modules <- c("json")
            private$py_sources <- c()
            invisible(self)
        },
        #' @export
        extract = function(paths) {
            if (!self$is_ready(install = TRUE)) {
                stop("Python instance not ready.")
            }
            lapply(paths, function(path) {
                list(
                    path = path,
                    content = private$py_main$py_notebook_extractor(path = path)
                )
            })
            
        }
    )
)

