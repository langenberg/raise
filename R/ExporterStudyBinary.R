#' ExporterStudyBinary class
#'
#' @description Some description
#' @rdname ExporterStudy
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
ExporterStudyBinary <- R6::R6Class(
    "ExporterStudyBinary",
    inherit = ExporterStudy,
    private = list(
        set_markdown_file = function() {
            private$markdown_file <- "markdown_study_design.Rmd"
        }
    ),
    active = list(
    ),
    public = list(
        #' @description ExporterStudyBinary class constructor.
        initialize = function(...) {
            super$initialize(...)
            
            invisible(self)
        }
    )
)

