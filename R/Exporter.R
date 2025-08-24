#' Exporter class
#'
#' @description Some description
#' @rdname Exporter
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
Exporter <- R6::R6Class(
    "Exporter",
    private = list(
        markdown_file = NULL,
        set_markdown_file = function() {
            stop("Not implemented. This is an abstract class.")
        },
        get_markdown_file = function() {
            if (is.null(private$markdown_file)) {
                stop("No markdown template provided.")
            } else {
                private$markdown_file
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
        
        #' @description Exporter class constructor.
        initialize = function() {
            private$set_markdown_file()
            invisible(private)
        },
        #' @export
        #' @importFrom rmarkdown render
        export = function(
            output_file = "summary.html", 
            output_dir = NULL, 
            open = T, 
            keep_files = F, 
            export = NULL
        ) {
            if (is.null(output_dir)) {
                output_dir <- dirname(output_file)
            } else {
                output_dir <- gsub("[/]+$", "", output_dir)
                output_file <- basename(output_file)
            }
            if (keep_files) {
                tmp_dir <- output_dir
            } else {
                tmp_dir <- tempfile()
                dir.create(tmp_dir)
            }
            
            rmd_template_path <- system.file("rmd", private$get_markdown_file(), package = "raise")
            rmd_input_path <- paste0(tmp_dir, "/", gsub("\\.[^\\.]+$", "", output_file), ".Rmd")
            
            export_names <- names(export)
            export <- as.environment(export)
            parent.env(export) <- parent.frame()
            
            file.copy(rmd_template_path, rmd_input_path, overwrite = T)
            
            rmarkdown::render(
                input = rmd_input_path, 
                output_file = output_file,
                output_dir = output_dir,
                envir = export
            )
            
            if (!keep_files) {
                file.remove(rmd_input_path)
            }
            
            if (open) {
                browseURL(paste0(output_dir, "/", output_file))
            }
            
            invisible(paste0(output_dir, "/", output_file))
        }
    )
)

