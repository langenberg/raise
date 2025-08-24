#' StudyBinaryPython class
#'
#' @description Some description
#' @rdname Study
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
StudyBinaryPython <- R6::R6Class(
    "StudyBinaryPython",
    inherit = StudyBinary,
    private = list(
    ),
    active = list(
    ),
    public = list(
        #' @export
        get_model = function() {
            NULL
        },
        
        #' @export
        #' @importFrom reticulate py_capture_output import_main py_run_string
        #' @importFrom stringr str_replace str_replace_all
        #' @importFrom dplyr `%>%`
        get_model_summary_md_force = function(...) {
            to_be_run <- "
import statsmodels.formula.api as smf
import pandas as pd
import numpy as np

for variable in variables:
    name = variable['name']
    categories = variable['categories']
    df[name] = pd.Categorical(df[name],categories=categories,ordered=False)

model1 = smf.logit(\"##formula##\", data=df).fit()
print(model1.summary())
model_odds1 = pd.DataFrame(np.exp(model1.params), columns= ['OR'])
model_odds1['p-value']= model1.pvalues
model_odds1[['2.5%', '97.5%']] = np.exp(model1.conf_int())

pd.set_option('display.max_columns', 8)
print(model_odds1)
pd.set_option('display.max_columns', 0)
"
            
            variables <- c(
                self$get_description()$variables$independent_variables$categorical_variables,
                self$get_description()$variables$independent_variables$binary_variables
            )
            variables <- lapply(variables, function(variable) {
                levels <- as.numeric(names(variable$categories))
                labels <- unlist(variable$categories)
                ref_level <- labels[which(variable$reference == levels)]
                categories <- unlist(variable$categories)
                categories <- c(
                    ref_level,
                    categories[categories != ref_level]
                )
                list(
                    name = variable$name,
                    ref = ref_level,
                    categories = categories
                )
            }) 
            
            myformula <- self$get_formula()
            to_be_run <- to_be_run %>% 
                stringr::str_replace("##formula##", myformula)
            
            py_main <- reticulate::import_main()
            
            py_main$df <- self$get_data(...)
            py_main$variables <- variables
            
            model_summary <- reticulate::py_capture_output(reticulate::py_run_string(to_be_run))
            
            model_summary %>% 
                str_replace_all("^[\\n]*", "") %>%
                str_replace_all("[\\n]*$", "") %>%
                str_replace_all("\\n", "\n## ") %>% 
                str_replace_all("^", "## ")
        },
        #' @export
        get_formula = function() {
            contrastify <- function(name) {
                dat <- self$get_data() 
                ivs <- self$get_description()$variables$independent_variables
                ivs <- c(
                    ivs$binary_variables,
                    ivs$categorical_variables
                )
                if (is.numeric(dat[[name]])) {
                    name
                } else if (is.factor(dat[[name]])) {
                    "C(" %+% 
                        name %+%
                        ")"
                } else if (is.character(dat[[name]])) {
                    "C(" %+% 
                        name %+%
                        ")"
                }
            }
            myformula <- paste0(
                self$get_description()$variables$outcome_variable$name,
                " ~ ",
                paste0(
                    sapply(
                        self$get_coefficients()$coefficients,
                        function(effect) {
                            paste0(
                                sapply(effect$variables, function(x) ifelse(x$name == "intercept", 1, contrastify(x$name))), 
                                collapse = "*"
                            )
                        }
                    ),
                    collapse = " + "
                )
            )
            myformula
        },
        #' @export
        get_formula_coding = function() {
            contrastify <- function(name) {
                dat <- self$get_data() 
                ivs <- self$get_description()$variables$independent_variables
                ivs <- c(
                    ivs$binary_variables,
                    ivs$categorical_variables
                )
                if (is.numeric(dat[[name]])) {
                    name
                } else if (is.factor(dat[[name]])) {
                    ref_level <- levels(dat[[name]])[1]
                    "C(" %+% 
                        name %+%
                        ", Treatment('" %+% 
                        ref_level %+% 
                        "'))"
                } else if (is.character(dat[[name]])) {
                    var_index <- which(sapply(ivs, function(iv) iv$name == name))
                    variable <- ivs[[var_index]]
                    levels <- as.numeric(names(variable$categories))
                    labels <- unlist(variable$categories)
                    ref_level <- labels[which(variable$reference == levels)]
                    "C(" %+% 
                        name %+%
                        ", Treatment('" %+% 
                        ref_level %+% 
                        "'))"
                }
            }
            myformula <- paste0(
                self$get_description()$variables$outcome_variable$name,
                " ~ ",
                paste0(
                    sapply(
                        self$get_coefficients()$coefficients,
                        function(effect) {
                            paste0(
                                sapply(effect$variables, function(x) ifelse(x$name == "intercept", 1, contrastify(x$name))), 
                                collapse = "*"
                            )
                        }
                    ),
                    collapse = " + "
                )
            )
            myformula
        }
    )
)
