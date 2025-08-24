.onAttach <- function(libname, pkgname) {
    # Welcome message to be friendly
    version <- read.dcf(
        file = system.file("DESCRIPTION", package = pkgname),
        fields = "Version"
    )
    packageStartupMessage("This is ", paste(pkgname, version))
    packageStartupMessage(pkgname, " is BETA software! Please report any bugs.")
    
    pkg_env$cmd_tools <- CmdTools$new()
    pkg_env$gpt <- Gpt$new()
    pkg_env$pdf_extractor <- PdfExtractor$new()
    pkg_env$py_notebook_extractor <- PyNotebookExtractor$new()
}

pkg_env <- new.env()

#' @export
#' @rdname CmdTools
cmd_tools <- function() pkg_env$cmd_tools

#' @export
#' @rdname Gpt
gpt <- function() pkg_env$gpt

#' @export
#' @rdname PdfExtractor
pdf_extractor <- function() pkg_env$pdf_extractor

#' @export
#' @rdname PyNotebookExtractor
py_notebook_extractor <- function() pkg_env$py_notebook_extractor
