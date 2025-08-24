#' Study class
#'
#' @description Some description
#' @rdname Study
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
Study <- R6::R6Class(
    "Study",
    inherit = GptThread,
    private = list(
        title = "",
        description = NULL,
        correlations = NULL,
        coefficients = NULL,
        link = NULL,
        dist = NULL,
        state = 0,
        last_data = NULL,
        cache = list(),
        prompts = list(),
        read_prompts = function() {
            stop("Not implemented. This is an abstract class.")
        }
    ),
    active = list(
    ),
    public = list(
        #' @export
        copy_from = function(from, cache = T) {
            if (!inherits(from, "Study")) {
                stop("Cannot copy from different classes.")
            }
            super$copy_from(from, cache = cache)
            private$title = from$.__enclos_env__$private$title
            private$description = from$.__enclos_env__$private$description
            private$correlations = from$.__enclos_env__$private$correlations
            private$coefficients = from$.__enclos_env__$private$coefficients
            private$link = from$.__enclos_env__$private$link
            private$dist = from$.__enclos_env__$private$dist
            private$state = from$.__enclos_env__$private$state
            private$last_data = from$.__enclos_env__$private$last_data
            if (cache) {
                private$cache = from$.__enclos_env__$private$cache
            } else {
                private$cache = list()
            }
            private$prompts = from$.__enclos_env__$private$prompts
            invisible(self)
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
        get_link = function(x) {
            private$link(x)
        },
        #' @export
        get_dist = function(x) {
            private$dist(x)
        },
        #' @export
        #' @note For debugging only. Don't you dare use this in a package!
        expose = function() {
            invisible(private)
        },
        
        #' @export
        power_up = function() {
            super$power_up()
            
            private$link <- identity
            private$dist <- rnorm
            
            private$read_prompts()
        },
        
        #' @export
        #' @description DataGenerator class constructor.
        initialize = function(...) {
            super$initialize()
            
            invisible(self)
        },
        
        #' @export
        #' @importFrom stringr str_replace str_glue
        #' @importFrom dplyr `%>%`
        generate_description = function(
            title,
            description, 
            outcome,
            n_continuous = 0, 
            n_categorical = c(), 
            n_binary = 0,
            details = "",
            ...
        ) {
            
            if (missing(title)) {
                title <- private$title
            } else {
                private$title <- title
            }
            
            outcome <- "- The type of the outcome should be: " %+% outcome
            
            categorical_levels <- lapply(
                unique(n_categorical), 
                function(x) list(number = x, count = sum(n_categorical == x))
            )
            n_categorical <- length(n_categorical)
            
            n_independent_variables <- paste0(c(
                if (n_continuous > 0L) n_continuous %+% " continuous" else NULL,
                if (n_binary > 0L) n_binary %+% " binary" else NULL,
                if (n_categorical > 0L) n_categorical %+% " categorical" else NULL
            ), collapse = ", ")
            
            n_independent_variables <- 
                "Please include " %+%
                (n_continuous+n_binary+n_categorical) %+%
                " independent variables: " %+%
                n_independent_variables
            
            continuous_list <- if (n_continuous > 0L) {
                "- " %+%
                    n_continuous %+%
                    " continuous variables"
            } else NULL
            
            binary_list <- if (n_binary > 0L) {
                "- " %+%
                    n_binary %+%
                    " binary variables"
            } else NULL
            
            categorical_list <- if (n_categorical > 0L) {
                sapply(
                    categorical_levels,
                    function(x) 
                        "- " %+% 
                        x$count %+% 
                        " categorical variables with " %+% 
                        x$number %+% 
                        " levels each"
                )
            } else NULL
            
            independent_variables_list <- paste0(c(
                continuous_list,
                binary_list,
                categorical_list
            ), collapse = "\n")
                
            prompt <- private$prompts$prompt1_description %>% 
                str_glue(
                    title = title, 
                    description = description,
                    outcome = outcome,
                    n_independent_variables = n_independent_variables,
                    independent_variables_list = independent_variables_list,
                    details = details
                )
            
            private$create_message(role = "user", content = prompt)
            private$description <- private$run()
            private$state <- 1
            invisible(self)
        },
        
        #' @export
        generate_correlations = function(...) {
            if (private$state < 1) {
                stop("Please generate a correlation matrix first.")
            }
            private$create_message(role = "user", content = private$prompts$prompt2_correlations)
            private$correlations <- private$run()
            private$state <- 2
            invisible(self)
        },
        
        #' @export
        #' @importFrom stringr str_replace str_glue
        #' @importFrom dplyr `%>%`
        #' @importFrom english words
        generate_coefficients = function(
            effects = list(
                effect(custom = "all main effects"),
                effect("categorical", "binary")
            ),
            ...
        ) {
            if (private$state < 2) {
                stop("Please generate a correlation matrix first.")
            }
            
            effects <- sapply(effects, function(effect) {
                n_order <- length(effect$types)
                if (!is.null(effect$custom)) {
                    paste0("- ", effect$custom)
                } else if (n_order == 1) {
                    str_glue(
                        "- a main effect for a {type} variable",
                        type = effect$types
                    )
                } else if (n_order == 2) {
                    str_glue(
                        "- a two-way interaction between a {type1} and a {type2} variable",
                        type1 = effect$types[1],
                        type2 = effect$types[2]
                    )
                } else if (n_order == 3) {
                    str_glue(
                        "- a three-way interaction between a {type1}, a {type2}, and a {type3} variable",
                        type1 = effect$types[1],
                        type2 = effect$types[2],
                        type3 = effect$types[3]
                    )
                } else {
                    type <- sapply(
                        1:length(effect$types), 
                        function(index) str_glue(
                            "    + variable {index}: {type}",
                            index = index,
                            type = effect$types[index]
                        )
                    )
                    str_glue(
                        "- an interaction between {order} variables of the following type:\n{type}",
                        order = english::words(n_order),
                        type = type
                    )
                }
            })
            
            effects <- paste0(effects, collapse = "\n")
            
            prompt <- private$prompts$prompt3_coefficients %>% 
                str_glue(effects = effects)
            
            private$create_message(role = "user", content = prompt)
            private$coefficients <- private$run()
            invisible(self)
        },
        
        #' @export
        generate = function(...) {
            self$generate_description(...)
            self$generate_correlations()
            self$generate_coefficients()
            invisible(self)
        },
        
        #' @export
        get_formula = function() {
            myformula <- paste0(
                self$get_description()$variables$outcome_variable$name,
                " ~ ",
                paste0(
                    sapply(
                        self$get_coefficients()$coefficients,
                        function(effect) {
                            paste0(
                                sapply(effect$variables, function(x) ifelse(x$name == "intercept", 1, x$name)), 
                                collapse = "*"
                            )
                        }
                    ),
                    collapse = " + "
                )
            )
            as.formula(myformula)
        },
        
        #' @export
        #' @importFrom jsonlite fromJSON
        get_description = function() {
            jsonlite::fromJSON(private$description, simplifyVector = F)
        },
        
        #' @export
        get_description_json = function() {
            private$description
        },
        
        #' @export
        get_description_md = function(heading_level = 1) {
            result <-
                strrep("#", heading_level) %+% " Study Description" %+% 
                "\n\n" %+%
                self$get_description()$study_description %+% 
                "\n\n" %+%
                strrep("#", heading_level) %+% " Outcome" %+% 
                "\n\n" %+%
                self$get_outcome_md() %+% 
                "\n\n" %+%
                strrep("#", heading_level) %+% " Independent Variables" %+% 
                "\n\n" %+%
                self$get_independent_variables_md()
            result
            
        },
        
        #' @export
        get_outcome = function(markdown = F) {
            self$get_description()$variables$outcome_variable
        },
        
        #' @export
        get_outcome_md = function(markdown = F) {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        get_independent_variables = function(markdown = F) {
            self$get_description()$variables$independent_variables
        },
        
        #' @export
        get_independent_variables_md = function() {
            variables <- self$get_description()$variables$independent_variables
            variables <- do.call(c, variables)
            md <- sapply(variables, function(variable) {
                paste0(
                    "* ", variable$name,
                    "\n  - Type: ", variable$type,
                    if (variable$type %in% c("binary", "categorical")) {
                        paste0(
                            "\n  - Categories:\n",
                            paste0(paste0(
                                "    + ", 
                                names(variable$categories), 
                                ": ", 
                                unlist(variable$categories)
                            ), collapse = "\n"),
                            "\n  - Reference category: ", variable$reference
                        )
                    },
                    "\n  - ", variable$description
                )
            })
            paste0(md, collapse = "\n\n")
        },
        
        #' @export
        #' @importFrom jsonlite fromJSON
        get_correlations = function() {
            correlations <- jsonlite::fromJSON(private$correlations, simplifyVector = F)
            do.call(cbind, lapply(correlations$correlation_matrix, as.numeric))
        },
        
        #' @export
        get_correlations_json = function() {
            private$correlations
        },
        
        #' @export
        #' @importFrom jsonlite fromJSON
        get_coefficients = function(json = F) {
            jsonlite::fromJSON(private$coefficients, simplifyVector = F)
        },
        
        #' @export
        get_coefficients_json = function() {
            private$coefficients
        },
        
        #' @export
        get_equation_numbers_md = function() {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        get_equation_labels_md = function() {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        sample = function(...) {
            generator <- DataGenerator$new(self)
            dat <- generator$sample(...)
            private$cache$last_data <- dat
            invisible(self$get_model_summary_md(force = T))
            dat
        },
        
        #' @export
        #' @import dplyr
        get_data = function(centering = F, ...) {
            if (is.null(private$cache$last_data)) {
                stop("Please use sample() to create some data first.")
            } else {
                if (centering) {
                    private$cache$last_data %>% 
                        mutate_if(is.numeric, function(x) mean(x, na.rm = T))
                } else {
                    private$cache$last_data
                }
            }
        },
        
        #' @export
        export = function() {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        get_model = function(centering = F, ...) {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        get_model_summary_md_force = function() {
            stop("Not implemented. This is an abstract class.")
        },
        
        #' @export
        get_model_summary_md = function(force = F, ...) {
            if (is.null(private$cache$model_summary_md) || force) {
                private$cache$model_summary_md <- self$get_model_summary_md_force(...)
            }
            
            private$cache$model_summary_md
        },
        
        #' @export
        get_summary_md = function(heading_level = 1) {
            self$get_description_md(heading_level = heading_level) %+%
                "\n\n" %+%
                strrep("#", heading_level) %+% " Model Equation" %+%
                "\n\n" %+%
                self$get_equation_labels_md(heading_level = heading_level) %+%
                "\n\n" %+%
                strrep("#", heading_level) %+% " Software Output\n\n" %+%
                "\n\n" %+%
                "```\n" %+%
                self$get_model_summary_md() %+%
                "\n```"
        }
        
    )
)

