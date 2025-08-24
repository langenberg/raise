#' ExporterQuiz class
#'
#' @description Some description
#' @rdname ExporterQuiz
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
ExporterQuiz <- R6::R6Class(
    "ExporterQuiz",
    inherit = Exporter,
    private = list(
        quiz = NULL,
        export_file = NULL,
        set_markdown_file = function() {
            private$markdown_file <- "markdown_quiz.Rmd"
            private$export_file <- "markdown_export.Rmd"
        },
        get_export_file = function() {
            if (is.null(private$export_file)) {
                stop("No export template provided.")
            } else {
                private$export_file
            }
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        #' @note For debugging only. Don't you dare use this in a package!
        expose = function() {
            invisible(private)
        },
        #' @description ExporterQuiz class constructor.
        initialize = function(quiz) {
            super$initialize()
            
            private$quiz <- quiz
            
            invisible(self)
        },
        export = function(engine = "exams", ...) {
            if (!(engine %in% c("exams", "raise"))) {
                stop('Engine must be "exams" or "raise')
            }
            
            if (engine == "raise") {
                super$export(export = list(quiz = private$quiz), ...)
            } else if (engine == "exams") {
                self$export_exams(...)
            }
        },
        #' @export
        #' @importFrom exams exams2arsnova exams2blackboard exams2canvas 
        #' @importFrom exams exams2grasple exams2html exams2ilias exams2kahoot 
        #' @importFrom exams exams2lops exams2moodle exams2nops exams2openolat 
        #' @importFrom exams exams2pandoc exams2particify exams2pdf exams2qti12
        #' @importFrom exams exams2qti21 exams2tcexam exams2testvision
        #' @importFrom readr read_file write_file
        #' @importFrom stringr str_replace
        #' @importFrom dplyr `%>%` 
        export_exams = function(
            output_file = "summary.html", 
            output_dir = NULL, 
            destination = "pdf",
            open = T, 
            keep_files = F,
            ...
        ) {
            destinations <- c(
                "arsnova", "blackboard", "canvas", "grasple", "html", "ilias", 
                "kahoot", "lops", "moodle", "nops", "openolat", "pandoc", 
                "particify", "pdf", "qti12", "qti21", "tcexam", "testvision"
            )
            destination <- match.arg(destination, destinations)
            
            if (is.null(output_dir)) {
                output_dir <- dirname(output_file)
            } else {
                output_dir <- gsub("[/]+$", "", output_dir)
                output_file <- basename(output_file)
            }
            
            rmd_template_path <- system.file("rmd", private$export_file, package = "raise")
            
            export <- list(
                quiz = private$quiz
            )
            
            rmd_input_path <- c()
            for (question_index in 1:length(export$quiz$get_questions())) {
                new_rmd_input_path <- paste0(
                    output_dir,
                    "/",
                    gsub("\\.[^\\.]+$", "", output_file),
                    "_",
                    as.character(question_index),
                    ".Rmd"
                )
                
                readr::read_file(rmd_template_path) %>% 
                    str_replace("##index##", as.character(question_index)) %>% 
                    write_file(new_rmd_input_path)
                
                rmd_input_path <- c(
                    rmd_input_path,
                    new_rmd_input_path
                )
            }
            
            
            export_names <- names(export)
            export <- as.environment(export)
            parent.env(export) <- parent.frame()
            
            params <- append(
                list(
                    file = rmd_input_path,
                    dir = output_dir,
                    name = output_file,
                    envir = export
                ),
                list(...)
            )
            
            exams_call <- str2expression(paste0(
                "do.call(exams::exams2", 
                destination,
                ", params)")
            )
            
            eval(exams_call)
            
            if (!keep_files) {
                file.remove(rmd_input_path)
            }
            
            invisible(paste0(output_dir, "/", output_file))
        }
    )
)

