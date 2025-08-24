#' @keywords internal
#' @importFrom reticulate py_run_string
clean_up_python <- function() {
    cleaning <- '
for element in dir():
    if element[0:2] != "__":
        del globals()[element]
del  element

import gc
gc.collect()'
    reticulate::py_run_string(cleaning)
}
