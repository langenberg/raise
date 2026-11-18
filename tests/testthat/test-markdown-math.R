html_text <- function(x) {
  h <- gsub("<[^>]+>", "", as.character(md_html(x)))
  for (e in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&amp;", "&")))
    h <- gsub(e[1], e[2], h, fixed = TRUE)
  h
}

test_that("math spans pass through Markdown unchanged", {
  spans <- c(
    "$\\widehat{happiness\\_score} = b_0 + b_1 \\cdot x_{i}$",
    "$a_i * b_j * c$",
    "$$\\log(\\widehat{Odds}) = \\beta_0 + \\beta_1 \\cdot gdp\\_percap$$",
    "\\(x_1 * y_2\\)",
    "\\[\\begin{aligned} y &= a \\\\ z &= b \\end{aligned}\\]",
    "$p < .05 | H_0$",
    "$\\{1, 2\\}$"
  )
  for (sp in spans) {
    expect_true(grepl(sp, html_text(paste("Text", sp, "more.")), fixed = TRUE), info = sp)
  }
})

test_that("unprotected Markdown would have broken these spans", {
  x <- "$\\widehat{happiness\\_score}$"
  expect_false(grepl(x, as.character(shiny::markdown(x)), fixed = TRUE))
})

test_that("text outside math keeps its Markdown", {
  h <- as.character(md_html("Some *italic* and **bold**, then $x_1$."))
  expect_match(h, "<em>italic</em>")
  expect_match(h, "<strong>bold</strong>")
})

test_that("code and literal dollars are left alone", {
  x <- "Inline `fit$coef_x`.\n\n```\nm$a_b <- 1 $ 2\n```\n\nPrice \\$5."
  p <- protect_math_markdown(x)
  expect_match(p, "`fit$coef_x`", fixed = TRUE)
  expect_match(p, "m$a_b <- 1 $ 2", fixed = TRUE)
  expect_match(p, "\\\\\\$5", fixed = TRUE)
  expect_match(html_text(x), "Price \\$5.", fixed = TRUE)
})

test_that("non-strings and text without math are returned as is", {
  expect_null(protect_math_markdown(NULL))
  expect_identical(protect_math_markdown("plain *text*"), "plain *text*")
  expect_identical(protect_math_markdown(NA_character_), NA_character_)
})
