#' CmdTools class
#'
#' @description Some description
#' @rdname CmdTools
#' @keywords internal
#' @importFrom R6 R6Class
#' @details Some details.
#'
CmdTools <- R6::R6Class(
    "CmdTools",
    private = list(
        py_main = NULL
    ),
    active = list(
    ),
    public = list(
        #' @keywords internal
        #' @description CmdTools class constructor.
        initialize = function() {
            
        },
        #' @keywords internal
        #' @importFrom readr read_file
        #' @importFrom stringr str_glue str_replace_all
        ipynb_to_py = function(
            paths, 
            suppress_messages = T,
            ignore.stdout = T, 
            ignore.stderr = T,
            remove_comments = F,
            remove_non_code = F,
            remove_output = F
        ) {
            ipynb_to_py_inner <- function(path) {
                message(stringr::str_glue(
                    "Parsing file: {path}",
                    path = path
                ))
                tmp_file <- tempfile(tmpdir = tmp_dir)
                file.copy(path, tmp_file)
                system(
                    paste0(
                        'jupyter nbconvert ',
                        if (remove_output) "--clear-output " else "",
                        '--to script "', tmp_file, '"'
                    ),
                    ignore.stdout = ignore.stdout,
                    ignore.stderr = ignore.stderr
                )
                new_file <- dir(tmp_dir)[dir(tmp_dir) != basename(tmp_file)]
                new_file <- file.path(tmp_dir, new_file)
                content <- readr::read_file(new_file)
                file.remove(file.path(tmp_dir, dir(tmp_dir)))
                content
            }
            
            tmp_dir <- tempfile()
            dir.create(tmp_dir)
            
            if (suppress_messages) {
                ipynb_to_py_inner_wrap <- function(path) suppressMessages(ipynb_to_py_inner(path))
            } else {
                ipynb_to_py_inner_wrap <- ipynb_to_py_inner
            }
                
            result <- lapply(paths, function(path) {
                content <- tryCatch(
                    ipynb_to_py_inner_wrap(path), 
                    warning = function(w) ipynb_to_py_inner_wrap(path),
                    error = function(e) NA
                )
                list(
                    path = path,
                    content = content
                )
            })
            file.remove(tmp_dir)
            
            if (remove_non_code) {
                result <- lapply(result, function(element) {
                    element$content <- stringr::str_replace_all(
                        element$content,
                        "(^|\\n)#[ ]*!\\[[^\\]]*\\]\\([^\\)]*\\)[^\\n]*",
                        ""
                    )
                    element
                })
            }
            
            if (remove_comments) {
                result <- lapply(result, function(element) {
                    element$content <- stringr::str_replace_all(
                        element$content,
                        "(^|\\n)#[^\\n]*",
                        ""
                    )
                    element
                })
            }
            
            result <- lapply(result, function(element) {
                element$content <- stringr::str_replace_all(
                    element$content,
                    "(\\n[ ]*(?=\\n))+",
                    "\n"
                )
                element
            })
            
        },
        #' @keywords internal
        #' @importFrom readr read_file
        #' @importFrom stringr str_glue str_replace_all
        ipynb_strip_output = function(
            paths, 
            suppress_messages = T,
            ignore.stdout = T, 
            ignore.stderr = T
        ) {
            ipynb_strip_output_inner <- function(path) {
                message(stringr::str_glue(
                    "Parsing file: {path}",
                    path = path
                ))
                tmp_file <- tempfile(tmpdir = tmp_dir)
                file.copy(path, tmp_file)
                system(
                    paste0(
                        'jupyter nbconvert --clear-output --to notebook "', tmp_file, '"'
                    ),
                    ignore.stdout = ignore.stdout,
                    ignore.stderr = ignore.stderr
                )
                new_file <- dir(tmp_dir)[dir(tmp_dir) != basename(tmp_file)]
                new_file <- file.path(tmp_dir, new_file)
                content <- readr::read_file(new_file)
                file.remove(file.path(tmp_dir, dir(tmp_dir)))
                content
            }
            
            tmp_dir <- tempfile()
            dir.create(tmp_dir)
            
            if (suppress_messages) {
                ipynb_strip_output_inner_wrap <- function(path) suppressMessages(ipynb_strip_output_inner(path))
            } else {
                ipynb_strip_output_inner_wrap <- ipynb_strip_output_inner
            }
            
            result <- lapply(paths, function(path) {
                content <- tryCatch(
                    ipynb_strip_output_inner_wrap(path), 
                    warning = function(w) ipynb_strip_output_inner_wrap(path),
                    error = function(e) NA
                )
                list(
                    path = path,
                    content = content
                )
            })
            file.remove(tmp_dir)
            
            result
        }
    )
)

