#' ExporterStudyGaussian class
#'
#' @description Some description
#' @rdname ExporterStudy
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
ExporterStudyGaussian <- R6::R6Class(
    "ExporterStudyGaussian",
    inherit = ExporterStudy,
    private = list(
        set_markdown_file = function() {
            private$markdown_file <- "markdown_study_design.Rmd"
        }
    ),
    active = list(
    ),
    public = list(
        #' @description ExporterStudyGaussian class constructor.
        initialize = function(...) {
            super$initialize(...)
            
            invisible(self)
        }
    )
)

