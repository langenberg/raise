#' DataGenerator class
#'
#' @description Some description
#' @rdname DataGenerator
#' @export
#' @importFrom R6 R6Class
#' @details Some details.
#'
DataGenerator <- R6::R6Class(
    "DataGenerator",
    private = list(
        last_data = FALSE,
        study_design = NULL,
        description = NULL,
        correlations = NULL,
        coefficients = NULL,
        terms_mapping_by_effect = NULL,
        #' @importFrom dplyr `%>%` 
        which_categorical = function(logical = F) {
            variable_names <- private$get_variable_names()
            is_categorical <- private$get_variables() %>% 
                sapply(function(variable) {
                    if (variable$type %in% c("categorical", "binary")) {
                        TRUE
                    } else {
                        FALSE
                    }
                })
            if (logical) {
                which(is_categorical)
            } else {
                variable_names[which(is_categorical)]
            }
        },
        is_categorical = function(variable) {
            if (is.numeric(variable)) {
                variable <- private$get_variable_names()[variable]
            }
            variable %in% private$which_categorical()
        },
        get_outcome = function() {
            private$description$variables$outcome_variable
        },
        get_variables = function () {
            do.call(c, private$description$variables$independent_variables)
        },
        #' @importFrom dplyr `%>%`
        get_variable_names = function() {
            private$description$variables$independent_variables %>% 
                sapply(function(variables) sapply(variables, function(variable) variable$name)) %>% 
                unlist()
        },
        #' @importFrom dplyr `%>%`
        get_mu = function() {
            private$description$variables$independent_variables %>% 
                sapply(function(variables) sapply(variables, function(variable) {
                    if (variable$type == "continuous") {
                        variable$distribution$mean
                    } else {
                        0
                    }
                })) %>% 
                unlist()
        },
        #' @importFrom dplyr `%>%`
        get_sds = function() {
            private$description$variables$independent_variables %>% 
                sapply(function(variables) sapply(variables, function(variable) {
                    if (variable$type == "continuous") {
                        variable$distribution$standard_deviation
                    } else {
                        1
                    }
                })) %>% 
                unlist() %>% 
                diag()
        },
        get_Sigma = function() {
            private$get_sds() %*% private$correlations %*% private$get_sds()
        },
        get_labeled_X = function(X) {
            variables <- private$get_variables()[private$which_categorical(logical = T)]
            
            X_label <- X
            for (variable in variables) {
                levels <- as.numeric(names(variable$categories))
                labels <- unlist(variable$categories)
                ref <- labels[which(variable$reference == levels)]
                X_new <- factor(
                    X_label[[variable$name]],
                    levels = levels,
                    labels = labels
                )
                X_new <- relevel(X_new, ref = ref)
                X_label[[variable$name]] <- X_new
            }
            X_label
        },
        #' @importFrom dplyr `%>%` as_tibble
        get_X = function(n, empirical = F) {
            eq <- function(x, y, tol = sqrt(.Machine$double.eps)) {
                abs(x - y) < tol
            }
            leq <- function(x, y, tol = sqrt(.Machine$double.eps)) {
                (x - y) < tol
            }
            geq <- function(x, y, tol = sqrt(.Machine$double.eps)) {
                (y - x) < tol
            }
            neq <- function(x, y, tol = sqrt(.Machine$double.eps)) {
                abs(x - y) > tol
            }
            
            variable_names <- private$get_variable_names()
            variables <- private$get_variables()
            
            mu <- private$get_mu()
            Sigma <- private$get_Sigma()
            
            X <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma, empirical = empirical)
            
            X_new <- as_tibble(X)
            names(X_new) <- variable_names
            
            for (mycol in 1:ncol(X_new)) {
                if (private$is_categorical(mycol)) {
                    myX <- X_new[[mycol]]
                    mylevels <- names(variables[[mycol]]$categories)
                    mylabels <- unlist(variables[[mycol]]$categories)
                    myref <- variables[[mycol]]$reference
                    myprops <- unlist(variables[[mycol]]$proportion)
                    nprops <- length(myprops)
                    mycumprops <- quantile(myX, cumsum(myprops))
                    mycats <- sapply(mycumprops, function(myprop) leq(myX, myprop)) %>% 
                        rowSums() %>% 
                        (function(x) nprops - x + 1)
                    mycats <- mylevels[mycats]
                    myX <- factor(mycats, levels = mylevels)
                    myX <- relevel(myX, ref = myref)
                    X_new[[mycol]] <- myX
                }
            }
            X_new
        },
        get_coefficients = function(X) {
            variable_names <- private$get_variable_names()
            
            full_formula <- as.formula(paste0("~", paste0(variable_names, collapse = "*")))
            imatrix <- model.matrix(full_formula, X)
            
            effects_to_variables <- attr(terms(full_formula),"factors")
            effects_to_terms <- attr(imatrix, "assign")
            variable_names <- as.character(attr(terms(full_formula),"variables"))[-1]
            nvariables <- length(variable_names)
            norder <- attr(terms(full_formula),"order")
            nterms <- ncol(imatrix)
            term_names <- colnames(imatrix)
            
            terms_mapping_by_effect <- private$get_terms_mapping_by_effect(X)
            terms_mapping_by_term <- do.call(c, terms_mapping_by_effect)
            
            mapping <- sapply(private$coefficients$coefficients, private$which_term, terms_mapping_by_term = terms_mapping_by_term)
            coefs <- sapply(private$coefficients$coefficients, `[[`, "value")
            if (any(sapply(mapping, length) == 0L)) {
                warning("Some coefficients could not be matched")
                coefs <- coefs[sapply(mapping, length) > 0L]
                mapping <- unlist(mapping[sapply(mapping, length) > 0L])
            }
            coefficients <- rep_len(0, nterms)
            names(coefficients) <- colnames(imatrix)
            coefficients[mapping] <- coefs
            
            coefficients
        },
        get_model_matrix = function(X) {
            full_formula <- as.formula(paste0("~", paste0(private$get_variable_names(), collapse = "*")))
            # opts <- options("na.action")
            # options(na.action = "na.pass")
            imatrix <- model.matrix(full_formula, X)
            # options(opts)
            imatrix
        },
        #' @importFrom stringr str_split str_replace
        get_terms_mapping_by_effect = function(X) {
            if (!is.null(private$terms_mapping_by_effect)) {
                return(private$terms_mapping_by_effect)
            }
            
            full_formula <- as.formula(paste0("~", paste0(private$get_variable_names(), collapse = "*")))
            imatrix <- model.matrix(full_formula, X)
            
            effects_to_variables <- attr(terms(full_formula),"factors")
            effects_to_terms <- attr(imatrix, "assign")
            variable_names <- as.character(attr(terms(full_formula),"variables"))[-1]
            nvariables <- length(variable_names)
            norder <- attr(terms(full_formula),"order")
            nterms <- ncol(imatrix)
            term_names <- colnames(imatrix)
            
            lapply(unique(effects_to_terms), function(effect) {
                if (effect == 0) {
                    return(list(list(name = "intercept",level = NULL,effect = effect)))
                }
                myvars_id <- effects_to_variables[,effect]
                myvars_id <- which(myvars_id == TRUE)
                myvars_names <- variable_names[myvars_id]
                myterms_id <- which(effects_to_terms == effect)
                myterms_names <- term_names[myterms_id]
                lapply(myterms_names, function(myterms_name) {
                    # myterms_name <- myterms_names[1]
                    term_parts <- unlist(str_split(myterms_name, ":", ))
                    lapply(1:length(term_parts), function(term_part_id) {
                        # term_part_id <- 1
                        term_part <- term_parts[term_part_id]
                        var_id <- myvars_id[term_part_id]
                        var_name <- variable_names[var_id]
                        if (!private$is_categorical(var_id)) {
                            mylevel <- NULL
                        } else {
                            mylevel <- str_replace(term_part, paste0("^", var_name), "")
                        }
                        list(
                            name = var_name,
                            level = mylevel,
                            effect = effect
                        )
                    })
                })
            })
        },
        #' @importFrom dplyr `%>%`
        which_term = function(gpt_term, terms_mapping_by_term) {
            if (tolower(gpt_term$variables[[1]]$name) == "intercept") {
                return(1)
            }
            # gpt_term <- data_eq$coefficients$beta1
            sapply(terms_mapping_by_term, function(model_term) {
                # model_term <- terms_mapping_by_term[[2]]
                if (length(gpt_term$variables) != length(model_term)) {
                    return(FALSE)
                }
                gpt_names <- sapply(gpt_term$variables, `[[`, "name")
                model_names <- sapply(model_term, `[[`, "name")
                if (!all(sapply(gpt_names, function(gpt_name) gpt_name %in% model_names))) {
                    return(FALSE)
                }
                sapply(gpt_term$variables, function(gpt_var) {
                    gpt_name <- gpt_var$name
                    gpt_level <- gpt_var$level
                    if (!private$is_categorical(which(private$get_variable_names() == gpt_name))) {
                        return(TRUE)
                    }
                    model_id <- which(gpt_name == model_names)
                    model_var <- model_term[[model_id]]
                    model_level <- model_var$level
                    model_level == gpt_level
                }) %>% 
                    all()
            }) %>% 
                which()
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
        #' @export
        #' @description RaiseModel class constructor.
        #' @importFrom jsonlite fromJSON
        initialize = function(study_design) {
            private$study_design <- study_design
            private$description <- study_design$get_description()
            private$correlations <- study_design$get_correlations()
            private$coefficients <- study_design$get_coefficients()
            invisible(self)
        },
        #' @export
        #' @importFrom MASS mvrnorm
        #' @importFrom dplyr `%>%` as_tibble tibble bind_cols
        sample = function(labeled = T, centering = F, ...) {
            # X <- private$get_X(n = 1000) # for debugging 
            X <- private$get_X(...)
            
            # stabilizes estimates
            X_means <- lapply(X, function(x) if (is.numeric(x)) mean(x, na.rm = T) else NULL)
            X_means <- X_means[sapply(X_means, function(x) !is.null(x))]
            X <- X %>% 
                mutate_if(is.numeric, function(x) x - mean(x, na.rm = T))
            
            imatrix <- private$get_model_matrix(X)
            coefficients <- private$get_coefficients(X)
            
            y <- as.vector(imatrix %*% coefficients)
            y <- private$study_design$get_link(y)
            y <- private$study_design$get_dist(y)
            y <- tibble(y)
            if (options()$na.action == "na.omit") {
                X_na <- as.logical(rowSums(is.na(X)))
                y_na <- as.logical(is.na(y))
                y <- y[!y_na & !X_na,,drop=F]
                X <- X[!y_na & !X_na,,drop=F]
            }
            
            # only revert centering if not wanted by user
            if (!centering) {
                for (x_name in names(X_means)) {
                    X[[x_name]] <- X[[x_name]] + X_means[[x_name]]
                }
            }
            
            names(y) <- private$description$variables$outcome_variable$name
            X_labeled <- private$get_labeled_X(X)
            
            private$last_data <- list(
                data_labeled = bind_cols(y, X_labeled),
                data_unlabeled = bind_cols(y, X),
                X_unlabeled = X,
                X_labeled = X_labeled,
                y = y
            )
            
            if (labeled) {
                private$last_data$data_labeled
            } else {
                private$last_data$data_unlabeled
            }
        },
        #' @export
        get_last_data = function(labeled = T) {
            if (labeled) {
                private$last_data$data_labeled
            } else {
                private$last_data$data_unlabeled
            }
        }
    )
)

