#' ExporterStudy class
#'
#' @description Some description
#' @rdname ExporterStudy
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
ExporterStudy <- R6::R6Class(
    "ExporterStudy",
    inherit = Exporter,
    private = list(
        study_design = NULL,
        markdown_file = NULL
    ),
    active = list(
    ),
    public = list(
        #' @export
        #' @note For debugging only. Don't you dare use this in a package!
        expose = function() {
            invisible(private)
        },
        #' @description ExporterStudy class constructor.
        initialize = function(study_design) {
            super$initialize()
            private$study_design <- study_design
            invisible(self)
        },
        #' @export
        #' @importFrom rmarkdown render
        #' @importFrom dplyr `%>%`
        #' @importFrom stringr str_replace_all
        #' @importFrom readr read_file
        #' @importFrom jsonlite fromJSON
        export = function(...) {
            export <- list(
                study_design = private$study_design
            )
            
            super$export(..., export = export)
        }
    )
)

