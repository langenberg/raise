# Markdown parsers (commonmark in the app's modals, marked.js in the chat) treat
# LaTeX as Markdown: `\_` loses its backslash, `_x_` turns into italics. Inside
# every math span, each ASCII punctuation character gets a backslash; the parser
# removes exactly those backslashes, so MathJax receives the LaTeX unchanged.
# Code (fenced blocks, inline code) is left alone; a literal `\$` becomes `\\\$`,
# so it reaches MathJax as `\$` (a dollar sign, not a math delimiter).
# inst/www/raise-math.js applies the same rule in the chat. Keep both in sync.
.math_md_pattern <- paste0(
  r"{(`{3,}|~{3,})[^\n]*\n[\s\S]*?(?:\n\1[`~]*[ \t]*(?=\n|$)|$)}", "|",
  r"{`+[^`\n]*`+}",                "|",
  r"{\\\$}",                       "|",
  r"{\$\$[\s\S]+?\$\$}",           "|",
  r"{\\\[[\s\S]+?\\\]}",           "|",
  r"{\\\([\s\S]+?\\\)}",           "|",
  r"{\$(?:\\.|[^$\\])+?\$}"
)

protect_math_markdown <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !grepl("[$\\\\]", x))
    return(x)
  m <- gregexpr(.math_md_pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(spans) {
    unname(vapply(spans, function(sp) {
      if (grepl("^[`~]", sp)) return(sp)
      if (identical(sp, "\\$")) return("\\\\\\$")
      gsub(r"{([!-/:-@\[-`{-~])}", r"{\\\1}", sp, perl = TRUE)
    }, character(1L)))
  })
  x
}

# Markdown to HTML for the app's modals, with math kept intact for MathJax.
# commonmark has no image attributes, so a figure with {width=...} becomes an
# <img> tag with that width.
md_html <- function(x) {
  x <- x %||% ""
  if (is.character(x) && grepl("](figures/", x, fixed = TRUE)) {
    m <- gregexpr(.figure_ref_pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(refs) {
      unname(vapply(refs, function(ref) {
        p <- regmatches(ref, regexec(.figure_ref_pattern, ref, perl = TRUE))[[1L]]
        if (!nzchar(p[[4L]])) return(ref)
        width <- sub('^.*width\\s*=\\s*"?([^"\\s}]+).*$', "\\1", p[[4L]], perl = TRUE)
        style <- if (identical(width, p[[4L]])) "" else paste0(' style="width:', width, '"')
        paste0('<img src="figures/', p[[3L]], '" alt="',
               htmltools::htmlEscape(p[[2L]], attribute = TRUE), '"', style, '>')
      }, ""))
    })
  }
  shiny::markdown(protect_math_markdown(x))
}
