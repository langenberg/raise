#' Quiz class
#'
#' @description Some description
#' @rdname Quiz
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
Quiz <- R6::R6Class(
    "Quiz",
    inherit = GptThread,
    private = list(
        title = "",
        notes = "",
        description = "",
        questions = list(),
        questions_history = list(),
        prompts = list(),
        read_prompts = function() {
            stop("Not implemented. This is an abstract class.")
        },
        #' @importFrom jsonlite fromJSON
        append_question = function(questions = NULL, json = NULL) {
            if (is.null(questions)) {
                questions <- list()
            }
            if (!is.null(json)) {
                json <- jsonlite::fromJSON(json, simplifyVector = FALSE)
                json <- json$questions
            } else {
                json <- list()
            }
            
            questions <- append(
                questions,
                json
            )
            
            private$questions <- append(
                private$questions,
                questions
            )
            
            private$questions_history <- append(
                private$questions_history,
                questions
            )
        },
        get_startup_message_content = function() {
            stop("Not implemented. This is an abstract class.")
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        copy_from = function(from, ...) {
            if (!inherits(from, "Quiz")) {
                stop("Cannot copy from different classes.")
            }
            super$copy_from(from, ...)
            
            private$title = from$.__enclos_env__$private$title
            private$notes = from$.__enclos_env__$private$notes
            private$description = from$.__enclos_env__$private$description
            private$questions = from$.__enclos_env__$private$questions
            private$questions_history = from$.__enclos_env__$private$questions_history
            private$prompts = from$.__enclos_env__$private$prompts
            
            invisible(self)
        },
        #' @export
        #' @note For debugging only. Don't you dare use this in a package!
        expose = function() {
            invisible(private)
        },
        
        power_up = function(force = F) {
            if (!private$is_ready() || force) {
                super$power_up(force)
                
                private$read_prompts()
                
                private$create_message(
                    role = "user",
                    content = private$get_startup_message_content()
                )
            }
            invisible(self)
        },
        
        #' @description Quiz class constructor.
        initialize = function(title = "", notes = "", ...) {
            private$title <- title
            private$notes <- notes
            super$initialize()
            invisible(self)
        },
        add_questions_legacy = function(..., question_list = list()) {
            
            dots <- list(...)
            dots <- lapply(dots, function(x) if (inherits(x, "question")) x else NULL)
            dots <- dots[sapply(dots, function(x) !is.null(x))]
            
            question_list <- append(
                dots,
                question_list
            )
            
            questions <- lapply(question_list, function(question) {
                replicate(question$n, {
                    if (question$type == "mc") {
                        paste0(
                            "- 1 multiple choice question with ",
                            question$n_choices,
                            " possible answers, of which exactly",
                            if (question$n_correct == 1L) " 1 is correct." else paste0(question$n_correct, " are correct.")
                        )
                    } else {
                        "- a numeric question"
                    }
                }, simplify = F)
            })
            questions <- unlist(questions)
            questions <- paste0(questions, collapse = "\n")
            questions <- paste0(
                "Please generate questions of the following type:\n",
                questions,
                "\n"
            )
            private$create_message(role = "user", content = questions)
            questions_new <- private$run()
            private$append_question(json = questions_new)
            
            invisible(self)
        },
        #' @export
        add_questions = function(..., question_list = list(), details = NULL, print = F) {
            
            dots <- list(...)
            dots <- lapply(dots, function(x) if (inherits(x, "question")) x else NULL)
            if (length(dots) == 0L) {
                dots <- list()
            } else {
                dots <- dots[sapply(dots, function(x) !is.null(x))]
            }
            
            question_list <- append(
                dots,
                question_list
            )
            
            question_list <- lapply(question_list, function(question) {
                replicate(question$n_questions, question, simplify = F)
            })
            
            question_list <- unlist(question_list, recursive = F)
            
            if (length(question_list) == 0L) {
                questions <- if (!is.null(details)) paste0(details, "\n\n") else ""
            } else {
                questions <- lapply(1:length(question_list), function(question_index) {
                    question <- question_list[[question_index]]
                    if (question$type == "mc") {
                        paste0(
                            "# Question ", question_index, "\n\n",
                            if (!is.null(question$details)) paste0(question$details, "\n\n") else "",
                            "- type: multiple choice\n",
                            "- number of possible answers: ", question$n_choices, "\n",
                            "- number of correct answers: exactly ", question$n_correct
                        )
                    } else {
                        paste0(
                            "# Question ", question_index, "\n\n",
                            if (!is.null(question$details)) paste0(question$details, "\n\n") else "",
                            "- type: numeric"
                        )
                    }
                })
                questions <- paste0(questions, collapse = "\n\n")
                questions <- paste0(
                    "Please generate ", length(question_list), " questions.\n\n",
                    if (!is.null(details)) paste0(details, "\n\n") else "",
                    "The question should have the following type:\n\n",
                    questions,
                    "\n"
                )
            }
            
            private$create_message(role = "user", content = questions)
            questions_new <- private$run()
            private$append_question(json = questions_new)
            
            if (print) {
                print(self$get_questions_md())
            }
            
            invisible(self)
        },
        #' @export
        get_questions = function(shuffle_questions = F, shuffle_choices = F, ...) {
            if (shuffle_questions) {
                questions <- private$questions[sample(1:length(private$questions))]
            } else {
                questions <- private$questions
            }
            
            if (!shuffle_choices) {
                return(questions)
            }
            
            lapply(questions, function(question) {
                if (question$type == "multiple_choice") {
                    question$choices <- question$choices[sample(1:length(question$choices))]
                    question
                } else {
                    question
                }
            })
        },
        #' @export
        shuffle_choices = function() {
            private$questions <- lapply(private$questions, function(question) {
                if (question$type == "multiple_choice") {
                    question$choices <- question$choices[sample(1:length(question$choices))]
                    question
                } else {
                    question
                }
            })
            invisible(self)
        },
        #' @export
        shuffle_questions = function() {
            private$questions <- private$questions[sample(1:length(private$questions))]
            invisible(self)
        },
        #' @export
        get_questions_md = function(heading_level = 1, ...) {
            questions <- self$get_questions(...)
            result <- ""
            for (question_index in 1:length(questions)) {
                question <- questions[[question_index]]
                result <- 
                    result %+%
                    "\n\n" %+%
                    strrep("#", heading_level) %+% " Question " %+% question_index %+%
                    "\n\n" %+%
                    question$question %+%
                    "\n\n* Type: " %+% question$type
                    
                if (question$type == "multiple_choice") {
                    result <- 
                        result %+%
                        "\n* Choices:\n" %+%
                        paste0(paste0("  - ", sapply(question$choices, `[[`, "choice")), collapse = "\n") %+%
                        "\n* Correct answer: "
                    
                    correct_answers <- sapply(question$choices, `[[`, "correct")
                    correct_answers <- sapply(question$choices, `[[`, "choice")[correct_answers]
                    if (length(correct_answers) > 1L) {
                        result <- 
                            result %+%
                            "\n" %+%
                            paste0(paste0("  - ", correct_answers), collapse = "\n")
                    } else {
                        result <- 
                            result %+%
                            correct_answers
                    }
                } else {
                    result <- 
                        result %+%
                        "\n* Correct answer: " %+%
                        question$correct_answer[1]
                }
                result <- 
                    result %+%
                    "\n* Solution: " %+%
                    question$explanation
            }
            make_printable(result)
        },
        #' @export
        delete_question = function(index) {
            private$questions <- private$questions[-index]
            invisible(self)
        },
        #' @export
        flush_question = function(index) {
            private$questions <- list()
        },
        #' @export
        export = function(...) {
            quiz_exporter <- ExporterQuiz$new(quiz = self)
            quiz_exporter$export(...)
            
            invisible(self)
        },
        #' @export
        get_description = function(...) {
            private$description
        },
        #' @export
        set_description = function(description) {
            private$description <- description
        },
        #' @export
        get_title = function() {
            private$title
        },
        #' @export
        set_title = function(title) {
            private$title <- title
        },
        #' @export
        restore = function(gpt_thread = NULL) {
            super$restore(gpt_thread)
            
            if (!is.null(gpt_thread) && inherits(gpt_thread, "Quiz")) {
                private$questions <- gpt_thread$get_questions()
            }
            
            invisible(self)
        }
    )
)

