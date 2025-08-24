#' QuizSlides class
#'
#' @description Some description
#' @rdname QuizSlides
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
QuizSlides <- R6::R6Class(
    "QuizSlides",
    inherit = Quiz,
    private = list(
        read_prompts = function() {
            private$prompts$startup_message_content <- paste0(readLines(system.file(
                "prompts", 
                "prompt_quiz_slides_startup.txt", 
                package = "raise"
            )), collapse = "\n")
        },
        get_startup_message_content = function() {
            private$prompts$startup_message_content %>% 
                str_replace(
                    "##title##",
                    private$title
                ) %>% 
                str_replace(
                    "##notes##",
                    private$notes
                )
        }
    ),
    active = list(
    ),
    public = list(
        #' @description QuizSlides class constructor.
        initialize = function(...) {
            super$initialize(...)
            
            invisible(self)
        },
        get_description = function(...) {
            ""
        }
    )
)

