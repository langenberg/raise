#' StudyGaussian class
#'
#' @description Some description
#' @rdname Study
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
StudyGaussian <- R6::R6Class(
    "StudyGaussian",
    inherit = Study,
    private = list(
        read_prompts = function() {
            private$prompts$prompt1_description <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_description_gaussian.txt", 
                package = "raise"
            )), collapse = "\n")
            private$prompts$prompt2_correlations <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_correlations.txt", 
                package = "raise"
            )), collapse = "\n") 
            private$prompts$prompt3_coefficients <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_coefficients_gaussian.txt", 
                package = "raise"
            )), collapse = "\n") 
        },
        get_error_sd = function() {
            jsonlite::fromJSON(private$coefficients)$epsilon$residual_error
        }
    ),
    active = list(
    ),
    public = list(
        power_up = function() {
            super$power_up()
            
            private$link <- identity
            private$dist <- function(x) sapply(x, function(y) y + rnorm(1, 0, private$get_error_sd()))
        },
        initialize = function(...) {
            super$initialize(...)
            
            private$title <- "Multiple Linear Regression"
            
            invisible(self)
        },
        
        #' @export
        get_outcome_md = function() {
            outcome <- self$get_description()$variables$outcome_variable
            paste0(
                "* ", outcome$name,
                "\n  - Type: ", outcome$type,
                "\n  - ", outcome$description
            )
        },
        
        #' @export
        generate_description = function(outcome = "continuous", ...) {
            super$generate_description(outcome = outcome, ...)
        },
        
        #' @export
        export = function(...) {
            study_exporter <- ExporterStudyGaussian$new(study_design = self)
            study_exporter$export(...)
        },
        
        #' @export
        get_model = function(...) {
            myformula <- self$get_formula()
            
            mod <- lm(
                myformula,
                data = self$get_data(...)
            )
            
            mod$call[[2]] <- myformula
            
            mod
        },
        
        #' @export
        get_model_summary_md_force = function(...) {
            mod <- self$get_model(...)
            paste0(capture.output(print(summary(mod))), collapse = "\n")
        },
        
        #' @export
        #' @importFrom stringr str_replace_all
        get_equation_numbers_md = function(heading_level = 1) {
            equation <- self$get_coefficients()$equation$numbers %>% 
                stringr::str_replace_all("\\\\text\\{([^\\}]+)\\}", "\\1") %>%
                stringr::str_replace_all("([^ ^=])[ ]*([+-])", "\\1\\\\\\\\\n\\2") %>%
                stringr::str_replace_all("([+-])", "&\\1") %>%
                stringr::str_replace_all("[=]([ ]*[0-9])", "= \\\\, &\\1") %>%
                stringr::str_replace_all("_", "\\\\_") %>%
                stringr::str_replace_all("&\\+ &\\-", "&- ") %>% 
                stringr::str_replace_all("[\\\\]{2,}(?![ ]*\\n)", "\\\\")
            residual <- self$get_coefficients()$equation$residual_error %>% 
                stringr::str_replace_all("\\\\text\\{([^\\}]+)\\}", "\\1") %>%
                stringr::str_replace_all("[=]([ ]*[0-9])", "= \\\\, &\\1") %>%
                stringr::str_replace_all("_", "\\\\_") %>% 
                stringr::str_replace_all("[\\\\]{2,}(?![ ]*\\n)", "\\\\")
            
            
            "\\begin{align}\n" %+%
                equation %+%
                "\\\\\n" %+%
                residual %+%
                "\n\\end{align}"
        },
        get_equation_labels_md = function(heading_level = 1) {
            equation <- self$get_coefficients()$equation$labels %>% 
                stringr::str_replace_all("\\\\text\\{([^\\}]+)\\}", "\\1") %>%
                stringr::str_replace_all("([^ ^=])[ ]*([+-])", "\\1\\\\\\\\\n\\2") %>%
                stringr::str_replace_all("([+-])", "&\\1") %>%
                stringr::str_replace_all("[=]([ ]*\\\\beta_0)", "= \\\\, &\\1") %>%
                stringr::str_replace_all("(?<!beta)_", "\\\\_") %>% 
                stringr::str_replace_all("[\\\\]{2,}(?![ ]*\\n)", "\\\\")
            
            "\\begin{align}\n" %+%
                equation %+%
                "\n\\end{align}"
        }
    )
)

