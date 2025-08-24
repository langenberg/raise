#' Gpt class
#'
#' @description Some description
#' @rdname Gpt
#' @keywords internal
#' @importFrom R6 R6Class
#' @details Some details.
#'
Gpt <- R6::R6Class(
    "Gpt",
    private = list(
        py_main = NULL
    ),
    active = list(
    ),
    public = list(
        #' @keywords internal
        #' @description Gpt class constructor.
        initialize = function() {
            
        },
        #' Checks whether python and module openai are installed.
        #' @param ask Logical. Default is FALSE. Indicates whether user is asked whether
        #' openai should be installed if it is not available.
        #' @keywords internal
        #' @importFrom reticulate py_available py_install import_main import py_run_string py_module_available
        is_ready = function(ask = T) {
            if (!py_available(initialize = T)) {
                return("No python version could be found. Please install python version.")
            }
            
            installed <- tryCatch({
                # py_main <- import_main()
                # py_main$pkg_resources <- import("pkg_resources")
                # py_run_string("from importlib import reload")
                # py_run_string("reload(pkg_resources)")
                # py_main$pkg_resources$get_distribution("openai")$version == "1.25.0"
                py_module_available("openai")
                py_module_available("statsmodels")
                py_module_available("numpy")
                py_module_available("pandas")
            }, error = function(e) {
                FALSE
            })
            
            if (!installed && ask) {
                if (readline(prompt="Necessary python modules not installed. Do you want to install now? (y/n) ") == tolower("y")) {
                    message("Installing openai\n")
                    py_install("openai", pip = T)
                    message("Installing openai done\n")
                    message("Installing statsmodels\n")
                    py_install("statsmodels", pip = T)
                    message("Installing statsmodels done\n")
                    message("Installing numpy\n")
                    py_install("numpy", pip = T)
                    message("Installing numpy done\n")
                    message("Installing pandas\n")
                    py_install("pandas", pip = T)
                    message("Installing pandas done\n")
                    return(TRUE)
                } else {
                    message("Python modules not installed")
                    return(FALSE)
                }
            } else if (!installed) {
                message("Python modules not installed")
                return(FALSE)
            }
            
            TRUE
        },
        #' @keywords internal
        set_api_key = function(api_key) {
            private$py_main$gpt$set_api_key(api_key = api_key)
        },
        #' @keywords internal
        reconnect = function() {
            private$py_main$gpt$reconnect()
        },
        #' @keywords internal
        check_assistant_ready = function() {
            private$py_main$gpt$check_assistant_ready()
        },
        #' @keywords internal
        check_client_ready = function() {
            private$py_main$gpt$check_client_ready()
        },
        #' @keywords internal
        power_up = function(api_key = NULL, reuse = TRUE, import_main = TRUE, ...) {
            if (!self$is_ready()) {
                stop("Python module 'openai' not available.")
            }
            
            if (import_main || is.null(private$py_main)) {
                private$py_main <- reticulate::import_main()
            }
            
            source_py_files <- function(paths, recursive = T) {
                for (path in paths) {
                    if (dir.exists(path) && recursive) {
                        source_py_files(file.path(path, dir(path)))
                    } else if (file.exists(path) && grepl("\\.py$", path)) {
                        reticulate::py_run_string(paste0(readLines(path), collapse = "\n"))
                    }
                }
            }
            source_py_files(system.file("python", package = "raise"))
            
            reticulate::py_run_string("gpt = Gpt()")
            
            private$py_main$gpt$power_up(
                api_key = api_key,
                reuse = reuse
            )
            
            invisible(self)
        },
        #' @keywords internal
        shut_down = function() {
            private$py_main$gpt$shut_down()
            
            invisible(self)
        },
        #' @keywords internal
        get_assistant_id = function(assistant_name) {
            private$py_main$gpt$get_assistant_id(assistant_name= assistant_name)
        },
        #' @keywords internal
        get_assistant_by_name = function(assistant_name) {
            private$py_main$gpt$get_assistant_by_name(assistant_name= assistant_name)
        },
        #' @keywords internal
        create_assistants = function(reuse = TRUE) {
            private$py_main$gpt$create_assistants(reuse)
        },
        #' @keywords internal
        delete_assistant = function(assistant_id) {
            private$py_main$gpt$delete_assistant(assistant_id)
        },
        #' @keywords internal
        create_thread = function() {
            private$py_main$gpt$create_thread()
        },
        #' @keywords internal
        delete_thread = function(thread_id, delete_messages = TRUE) {
            private$py_main$gpt$delete_thread(
                thread_id = thread_id,
                delete_messages = delete_messages
            )
        },
        #' @keywords internal
        list_messages = function(thread_id) {
            private$py_main$gpt$list_messages(
                thread_id = thread_id
            )
        },
        #' @keywords internal
        flush_assistants = function() {
            private$py_main$gpt$flush_assistants()
        },
        #' @keywords internal
        create_vector_store = function(thread_id = NULL) {
            private$py_main$gpt$create_vector_store(thread_id = thread_id)
        },
        #' @keywords internal
        add_vector_stores_to_thread = function(vector_store_ids, thread_id) {
            private$py_main$gpt$add_vector_stores_to_thread(
                vector_store_ids = vector_store_ids,
                thread_id = thread_id
            )
        },
        #' @keywords internal
        delete_vector_store = function(vector_store_id, delete_files = TRUE) {
            private$py_main$gpt$delete_vector_store(vector_store_id, delete_files)
        },
        #' @keywords internal
        list_vector_stores = function(names_only = FALSE) {
            private$py_main$gpt$list_vector_stores(names_only)
        },
        #' @keywords internal
        create_message = function(thread_id, role, content) {
            private$py_main$gpt$create_message(
                thread_id = thread_id,
                role = role,
                content = content
            )
        },
        #' @keywords internal
        delete_message = function(thread_id, message_id) {
            private$py_main$gpt$delete_message(
                thread_id = thread_id,
                message_id = message_id
            )
        },
        #' @keywords internal
        flush_vector_stores = function(delete_files = TRUE) {
            private$py_main$gpt$flush_vector_stores(delete_files)
        },
        #' @keywords internal
        list_files = function() {
            private$py_main$gpt$list_files()
        },
        #' @keywords internal
        flush_files = function() {
            private$py_main$gpt$flush_files()
        },
        #' @keywords internal
        delete_file = function(file_id) {
            private$py_main$gpt$delete_file(file_id)
        },
        #' @keywords internal
        delete_files = function(file_ids) {
            private$py_main$gpt$delete_files(file_ids)
        },
        #' @keywords internal
        upload_files = function(file_paths) {
            private$py_main$gpt$upload_files(file_paths)
        },
        #' @keywords internal
        upload_files_to_vector_store = function(file_paths, vector_store_id = NULL) {
            private$py_main$gpt$upload_files_to_vector_store(file_paths, vector_store_id)
        },
        #' @keywords internal
        delete_files_from_vector_store = function(file_ids, vector_store_id) {
            if (!is.list(file_ids)) {
                file_ids <- as.list(file_ids)
            }
            private$py_main$gpt$delete_files_from_vector_store(
                file_ids = file_ids,
                vector_store_id = vector_store_id
            )
        },
        #' @keywords internal
        completion_request = function(messages) {
            private$py_main$gpt$completion_request(messages)
        },
        #' @keywords internal
        verify_json = function(json) {
            private$py_main$gpt$verify_json(json)
        },
        #' @keywords internal
        assistant_request = function(
            thread_id, 
            assistant_name = "assistant_json", 
            verify_json = TRUE,
            drop_message = FALSE,
            max_completion_tokens = NULL,
            stream = NULL
        ) {
            private$py_main$gpt$assistant_request(
                thread_id = thread_id,
                assistant_name = assistant_name,
                verify_json = verify_json,
                drop_message = drop_message
            )
        }
    )
)

#' Set the GPT version.
#' @export
#' @rdname Gpt
#' @param version GPT version. Default is "gpt-4o".
set_gpt_model <- function(model) {
    stop("Not yet implemented.")
}

#' Get the GPT version.    
#' @rdname Gpt
#' @export
get_gpt_model <- function() {
    stop("Not yet implemented.")
}

#' Set the GPT version.
#' @export
#' @rdname Gpt
#' @param api_key GPT API key. Must be set before first use.
set_api_key <- function(api_key) {
    stop("Not yet implemented.")
}

#' Power up Gpt object.
#' @export
#' @rdname Gpt
#' @param api_key GPT API key. Must be set before first use.
power_up <- function(api_key = NULL) {
    gpt()$power_up(api_key)
}

#' Get the GPT version.    
#' @rdname Gpt
#' @export
get_api_key <- function() {
    stop("Not yet implemented.")
}
