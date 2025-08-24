#' QuizStudy class
#'
#' @description Some description
#' @rdname QuizStudy
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
QuizStudy <- R6::R6Class(
    "QuizStudy",
    inherit = Quiz,
    private = list(
        study_design = NULL,
        read_prompts = function() {
            private$prompts$startup_message_content <- paste0(readLines(system.file(
                "prompts", 
                "prompt_quiz_study_startup.txt", 
                package = "raise"
            )), collapse = "\n")
        },
        #' @importFrom stringr str_replace
        #' @importFrom dplyr `%>%`
        get_startup_message_content = function() {
            private$prompts$startup_message_content %>% 
                str_replace(
                    "##title##",
                    private$title
                ) %>% 
                str_replace(
                    "##notes##",
                    private$notes
                ) %>% 
                str_replace(
                    "##study_design##",
                    private$study_design$get_summary_md()
                )
        }
    ),
    active = list(
    ),
    public = list(
        #' @description QuizStudy class constructor.
        initialize = function(study_design, title, ...) {
            private$study_design <- study_design
            
            if (missing(title)) {
                title <- study_design$get_title()
            }
            
            super$initialize(title = title, ...)
            
            invisible(self)
        },
        #' @importFrom english as.english
        get_questions = function(include_description = F, heading_level = 1, ...) {
            
            questions <- private$questions
            
            if (include_description) {
                questions[[1]]$question <-
                    "# Important Note" %+%
                    "\n\n" %+%
                    "The following questions refer to the study below." %+%
                    "\n\n" %+%
                    private$study_design$get_summary_md(heading_level = heading_level) %+%
                    "\n\n" %+%
                    strrep("#", heading_level) %+% " Question" %+%
                    "\n\n" %+%
                    questions[[1]]$question
            }
            
            questions
        },
        get_questions_md = function(include_description = F, heading_level = 1, ...) {
            
            questions_backup <- private$questions
            
            if (include_description) {
                private$questions[[1]]$question <-
                    "# Important Note" %+%
                    "\n\n" %+%
                    "The following questions refer to the study below." %+%
                    "\n\n" %+%
                    private$study_design$get_summary_md(heading_level = heading_level + 1) %+%
                    "\n\n" %+%
                    strrep("#", heading_level + 1) %+% " Question" %+%
                    "\n\n" %+%
                    private$questions[[1]]$question
            }
            
            result <- super$get_questions_md(heading_level = heading_level)
            
            private$questions <- questions_backup
            
            result
        },
        get_description_md = function(...) {
            private$study_design$get_description_md(...)
        },
        get_summary_md = function(...) {
            private$study_design$get_summary_md(...)
        },
        get_description = function(...) {
            private$study_design$get_description(...)
        }
    )
)

