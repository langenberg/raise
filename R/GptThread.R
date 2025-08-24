#' GptThread class
#'
#' @description Some description
#' @rdname GptThread
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
GptThread <- R6::R6Class(
    "GptThread",
    private = list(
        thread_id = NULL,
        vector_store_id = NULL,
        file_ids = list(),
        cache = list(),
        create_message = function(role, content) {
            private$cache$messages <- c(
                list(list(id = NA_character_, role = role, content = content)),
                private$cache$messages
            )
            
            gpt()$create_message(
                thread_id = private$thread_id, 
                role = role, 
                content = content
            )
            
            invisible(self)
        },
        #' @importFrom reticulate py_run_string
        run = function(seed = NULL, ...) {
            self$power_up()
            
            content <- gpt()$assistant_request(thread_id = private$thread_id, ...)
            
            private$cache$messages <- c(
                list(list(id = NA_character_, role = "assistant", content = content)),
                private$cache$messages
            )
            
            content
        },
        is_ready = function() {
            !is.null(private$vector_store_id) && !is.null(private$thread_id)
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        copy_from = function(from, cache = T) {
            if (!inherits(from, "GptThread")) {
                stop("Cannot copy from different classes.")
            }
            private$thread_id <- from$.__enclos_env__$private$thread_id
            private$vector_store_id <- from$.__enclos_env__$private$vector_store_id
            private$file_ids <- from$.__enclos_env__$private$file_ids
            if (cache) {
                private$cache = from$.__enclos_env__$private$cache
            } else {
                private$cache = list()
            }
            invisible(self)
        },
        #' @export
        #' @note For debugging only. Don't you dare use this in a package!
        expose = function() {
            invisible(private)
        },
        #' @export
        #' @description GptThread class constructor.
        initialize = function(gpt_thread = NULL) {
            
            if (!is.null(gpt_thread)) {
                self$restore(gpt_thread)
            } else {
                self$power_up()
            }
            
            invisible(self)
        },
        #' @export
        create_vector_store = function() {
            private$vector_store_id <- gpt()$create_vector_store(private$thread_id)
        },
        #' @export
        power_up = function(force = F) {
            if (!private$is_ready() || force) {
                private$thread_id <- gpt()$create_thread()
                self$create_vector_store()
            }
            invisible(self)
        },
        #' @export
        cover_tracks = function() {
            if (private$is_ready()) {
                gpt()$delete_vector_store(private$vector_store_id)
                gpt()$delete_thread(private$thread_id)
            }
            private$vector_store_id <- NULL
            private$thread_id <- NULL
            invisible(self)
        },
        #' @export
        upload_files = function(file_paths) {
            self$power_up()
                
            if (!is.list(file_paths)) {
                file_paths <- as.list(file_paths)
            }
            file_ids <- gpt()$upload_files_to_vector_store(
                file_paths = file_paths, 
                vector_store_id = private$vector_store_id
            )
            private$file_ids <- append(
                private$file_ids, 
                as.list(file_ids)
            )
            invisible(self)
        },
        #' @export
        #' @importFrom stringr str_glue
        parse_files = function(paths) {
            parsed_files <- pdf_extractor$extract(paths)
            for (parsed_file in parsed_files) {
                content <- stringr::str_glue(
                    "Below is a file that I would like you to consider for the next prompts. The text was extracted from a pdf file. During the extraction process, mistakes can happen and the structure of the pages could be corrupted. Please make the best out of it. The pages are separated by the sequence -----.\n\nFile name: {file_name}\n\n Here begins the file:\n\n{content}",
                    file_name = basename(parsed_file$path),
                    content = parsed_file$content_normalized
                )
                private$create_message(role = "user", content = content)
            }
        },
        #' @export
        chat = function(message, respond = T, print = T, verify_json = F, ...) {
            private$create_message(role = "user", content = message)
            if (respond) {
                response <- private$run(
                    assistant_name = "assistant_chat",
                    verify_json = verify_json, 
                    ...
                )
                if (print) {
                    cat(response)
                    invisible(response)
                } else {
                    response
                }
            } else {
                invisible(self)
            }
        },
        #' @export
        get_messages = function(force = F) {
            if (force) {
                private$cache$messages <- gpt()$list_messages(thread_id = private$thread_id)
            }
            private$cache$messages
        },
        #' @export
        get_last_message = function(...) {
            self$get_messages(...)[[1]]
        },
        #' @export
        restore = function(gpt_thread = NULL, ...) {
            
            if (is.null(gpt_thread)) {
                messages <-rev(self$get_messages()) 
            } else {
                messages <-rev(gpt_thread$get_messages()) 
            }
            
            self$power_up(...)
            
            for (message in messages) {
                private$create_message(
                    role = message$role,
                    content = message$content
                )
            }
            
            invisible(self)
        }
    )
)

