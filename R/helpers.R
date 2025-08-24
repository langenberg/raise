
#' Helper function for adding a new multiple choice question to the quiz.
#' @export
#' @rdname questions
question_mc <- function(details = NULL, n_questions = 1, n_choices, n_correct = 1) {
    result <- list(
        type = "mc",
        n_questions = n_questions,
        n_correct = n_correct,
        n_choices = n_choices,
        details = details
    )
    class(result) <- "question"
    result
}

#' Helper function for adding a new numeric question to the quiz.
#' @export
#' @rdname questions
question_numeric <- function(details = NULL, n_questions = 1) {
    result <- list(
        type = "numeric",
        n_questions = n_questions,
        details = details
    )
    class(result) <- "question"
    result
}

#' @keywords internal
`%+%` <- function(e1, e2) {
    paste(as.character(e1), as.character(e2), sep = "")
}

#' Helper function for adding a main effects to a study.
#' @export
#' @rdname Study
effect <- function(..., effects = c(), custom = NULL) {
    dots <- list(...)
    if (length(dots) > 0L) {
        dots <- lapply(dots, function(x) if (inherits(x, "character")) x else NULL)
        dots <- dots[sapply(dots, function(x) !is.null(x))]
    }
    dots <- unlist(dots)
    
    effects <- c(
        dots,
        effects
    )
    
    effects <- tolower(effects)
    
    type_ok <- all(sapply(effects, function(effect) effect %in% c("continuous", "categorical", "binary")))
    
    if (!type_ok) {
        stop("I can only model effects between continuous, binary, and categorical variables.")
    }
    
    effects <- list(
        custom = custom,
        types = effects
    )
    
    class(effects) <- "effect"
    effects
}

#' @keywords internal
make_printable <- function(x) {
    class(x) <- "printable"
    x
}

#' @export
print.printable <- function(x) {
    cat(x)
}
