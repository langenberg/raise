#' StudyBinary class
#'
#' @description Some description
#' @rdname Study
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
StudyBinary <- R6::R6Class(
    "StudyBinary",
    inherit = Study,
    private = list(
        read_prompts = function() {
            private$prompts$prompt1_description <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_description_binary.txt", 
                package = "raise"
            )), collapse = "\n")
            private$prompts$prompt2_correlations <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_correlations.txt", 
                package = "raise"
            )), collapse = "\n") 
            private$prompts$prompt3_coefficients <- paste0(readLines(system.file(
                "prompts", 
                "prompt_study_coefficients_binary.txt", 
                package = "raise"
            )), collapse = "\n") 
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        power_up = function() {
            super$power_up()
            
            private$link <- function(x) exp(x) / (1 + exp(x))
            private$dist <- function(y) sapply(y, function(x) rbinom(1, 1, x))
        },
        
        #' @export
        initialize = function(...) {
            super$initialize(...)
            
            private$title <- "Multiple Logistic Regression"
            
            invisible(self)
        },
        
        #' @export
        generate_description = function(outcome = "binary", ...) {
            super$generate_description(outcome = outcome, ...)
        },
        
        #' @export
        export = function(...) {
            study_exporter <- ExporterStudyBinary$new(study_design = self)
            study_exporter$export(...)
        },
        
        #' @export
        get_outcome_md = function() {
            outcome <- self$get_description()$variables$outcome_variable
            paste0(
                "* ", outcome$name,
                "\n  - Type: ", outcome$type,
                "\n  - Categories:\n",
                paste0(paste0(
                    "    + ", 
                    names(outcome$categories), 
                    ": ", 
                    unlist(outcome$categories)
                ), collapse = "\n"),
                "\n  - ",
                outcome$description
            )
        },
        
        #' @export
        get_model = function(...) {
            myformula <- self$get_formula()
            
            mod <- glm(
                myformula,
                data = self$get_data(...),
                family = "binomial"
            )
            
            mod$call[[2]] <- myformula
            
            mod
        },
        
        #' @export
        get_model_summary_md_force = function(...) {
            mod <- self$get_model(...)
            mod_summary <- summary(mod)
            coefs <- mod_summary$coefficients
            
            coefs <- cbind(
                coefs[,1:3],
                OR = exp(coefs[,"Estimate"]),
                `CI 2.5 (OR)` = exp(coefs[,"Estimate"] + qnorm(0.025) * coefs[,"Std. Error"]),
                `CI 97.5 (OR)` = exp(coefs[,"Estimate"] + qnorm(0.975) * coefs[,"Std. Error"]),
                coefs[,4, drop = F]
            )
            
            mod_summary$coefficients <- coefs
            
            paste0(capture.output(print(mod_summary)), collapse = "\n")
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
            
            "\\begin{align}\n" %+%
                equation %+%
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

