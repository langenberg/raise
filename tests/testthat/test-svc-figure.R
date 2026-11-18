make_png <- function(path, seed = 1) {
  grDevices::png(path, width = 200, height = 150)
  set.seed(seed)
  graphics::plot(stats::rnorm(10))
  grDevices::dev.off()
  path
}

fig_session <- function() {
  init_session("s")
  dir.create("s/studies/st1", recursive = TRUE)
  utils::write.csv(data.frame(hours = c(1, 2, 3, 4, 5, 6), score = c(50, 55, 61, 64, 70, 72),
                              place = c("Home", "Library", "Home", "Library", "Home", "Library"),
                              pass = c(0, 0, 1, 1, 1, 1)),
                   "s/studies/st1/data.csv", row.names = FALSE)
  "s"
}

fig_question <- function(file, where = "question") {
  q <- list(type = "multiple_choice", question = "Which is right?",
            choices = list(list(choice = "A", correct = TRUE), list(choice = "B", correct = FALSE)),
            explanation = "A.")
  ref <- paste0("![Plot](figures/", file, ")")
  if (where == "choice") q$choices[[2]]$choice <- ref
  else q[[where]] <- paste(q[[where]], "\n\n", ref)
  q
}

test_that("add_figure stores by content hash and deduplicates", {
  withr::with_tempdir({
    s <- fig_session()
    a <- add_figure(make_png("my plot.png"), s)
    expect_match(a$file, "^my_plot_[0-9a-f]{8}\\.png$")
    expect_equal(a$path, file.path("figures", a$file))
    expect_match(a$markdown, paste0("](figures/", a$file, ")"), fixed = TRUE)
    expect_false(a$existed)
    b <- add_figure("my plot.png", s, name = "other name.png")
    expect_true(b$existed)
    expect_equal(b$file, a$file)
    c <- add_figure(make_png("my plot.png", seed = 2), s)
    expect_false(identical(c$file, a$file))
    expect_length(list.files("s/figures"), 2L)
  })
})

test_that("add_figure takes the type from name for extensionless uploads and rejects others", {
  withr::with_tempdir({
    s <- fig_session()
    file.copy(make_png("p.png"), "upload0")
    expect_match(add_figure("upload0", s, name = "Scatter.PNG")$file, "^scatter_.*\\.png$")
    writeLines("x", "notes.txt")
    expect_error(add_figure("notes.txt", s), "PNG and JPEG")
  })
})

test_that("figure references are found in all text fields", {
  q <- fig_question("a_12345678.png")
  q$choices[[2]]$choice <- "![x](figures/b_12345678.jpg){width=30%}"
  q$explanation <- "See ![y](figures/a_12345678.png) and ![z](elsewhere/c.png)."
  expect_setequal(question_figures(q), c("a_12345678.png", "b_12345678.jpg"))
  expect_length(figure_files_in(NULL), 0L)
})

test_that("saving, updating and adding questions checks figure references", {
  withr::with_tempdir({
    s <- fig_session()
    f <- add_figure(make_png("p.png"), s)$file
    qs <- list(id = "set1", topic = "t", study_id = "st1",
               questions = list(fig_question("missing_00000000.png")))
    expect_error(save_question_set(qs, s), "figure not found: figures/missing_00000000.png")
    qs$questions <- list(fig_question(f))
    expect_true(file.exists(save_question_set(qs, s)))
    r <- add_questions("set1", list(fig_question("nope.png", "explanation")), s)
    expect_match(r, "^Error.*figure not found")
    bad <- fig_question("nope.png", "choice"); bad$id <- 1L
    r <- update_questions("set1", list(bad), s)
    expect_match(r, "^Error.*figure not found")
  })
})

test_that("list_figures reports usage; delete_figures protects used figures", {
  withr::with_tempdir({
    s <- fig_session()
    used   <- add_figure(make_png("used.png"), s)$file
    unused <- add_figure(make_png("unused.png", seed = 3), s)$file
    save_question_set(list(id = "set1", topic = "t", study_id = "st1",
                           questions = list(fig_question(used))), s)
    figs <- jsonlite::fromJSON(list_figures(s), simplifyVector = FALSE)
    by_file <- stats::setNames(figs, vapply(figs, `[[`, "", "file"))
    expect_equal(unlist(by_file[[used]]$used_by), "set1 #1")
    expect_true(by_file[[unused]]$unused)
    r <- delete_figures(used, s)
    expect_match(r, "^Not deleted")
    expect_true(file.exists(file.path("s/figures", used)))
    r <- delete_unused_figures(s)
    expect_match(r, "deleted")
    expect_false(file.exists(file.path("s/figures", unused)))
    expect_equal(delete_unused_figures(s), "No unused figures.")
    r <- delete_figures(paste0("figures/", used), s, force = TRUE)
    expect_match(r, "deleted")
    expect_false(file.exists(file.path("s/figures", used)))
    r <- delete_figures("gone.png", s)
    expect_match(r, "not found")
  })
})

test_that("replace_figure rewrites references in all sets and fields", {
  withr::with_tempdir({
    s <- fig_session()
    old <- add_figure(make_png("old.png"), s)$file
    new <- add_figure(make_png("new.png", seed = 4), s)$file
    save_question_set(list(id = "set1", topic = "t", study_id = "st1",
                           questions = list(fig_question(old), fig_question(old, "choice"))), s)
    save_question_set(list(id = "set2", topic = "t", study_id = "st1",
                           questions = list(fig_question(old, "explanation"))), s)
    r <- replace_figure(old, new, s)
    expect_match(r, "in 3 question")
    usage <- figure_usage(s)
    expect_null(usage[[old]])
    expect_setequal(usage[[new]], c("set1 #1", "set1 #2", "set2 #1"))
    r <- replace_figure(old, "missing.png", s)
    expect_match(r, "^Error")
  })
})

test_that("create_figure draws each plot type from study data", {
  skip_if_not_installed("ggplot2")
  withr::with_tempdir({
    s <- fig_session()
    r <- create_figure("st1", "scatter", "hours", "score", group = "place", smooth = TRUE,
                       name = "score by hours", session_path = s)
    expect_match(r$file, "^score_by_hours_[0-9a-f]{8}\\.png$")
    expect_true(file.exists(file.path(s, r$path)))
    expect_true(create_figure("st1", "scatter", "hours", "score", group = "place", smooth = TRUE,
                              name = "score by hours", session_path = s)$existed)
    for (args in list(list("boxplot", "place", "score"), list("histogram", "score"),
                      list("bar", "place", "pass"), list("bar", "place")))
      expect_true(file.exists(file.path(s, do.call(create_figure,
        c(list("st1"), args, list(session_path = s)))$path)))
    expect_error(create_figure("st1", "scatter", "hours", "nope", session_path = s), "unknown column")
    expect_error(create_figure("st1", "boxplot", "place", session_path = s), "needs y")
    expect_error(create_figure("st9", "histogram", "score", session_path = s), "simulate the study")
  })
})

test_that("figure_content returns an image for the model", {
  withr::with_tempdir({
    s <- fig_session()
    f <- add_figure(make_png("p.png"), s)$file
    expect_true(inherits(figure_content(f, s), "ellmer::ContentImageInline"))
    expect_error(figure_content("nope.png", s), "not found")
  })
})

test_that("render_figure_refs: PDF block / inline LaTeX, HTML inline image", {
  x <- "Look:\n\n![A plot](figures/a_12345678.png)\n\nand ![b](figures/b_1.png){width=25%}"
  pdf <- render_figure_refs(x, "pdf")
  expect_match(pdf, "\\includegraphics[width=0.6\\linewidth]{a_12345678.png}", fixed = TRUE)
  expect_match(pdf, "\\includegraphics[width=0.25\\linewidth]{b_1.png}", fixed = TRUE)
  expect_match(pdf, "```{=latex}\n\\begin{center}", fixed = TRUE)
  expect_match(render_figure_refs("![b](figures/b_1.png)", "pdf", inline = TRUE),
               "^`\\\\includegraphics\\[width=0.6\\\\linewidth\\]\\{b_1.png\\}`\\{=latex\\}$")
  html <- render_figure_refs(x, "canvas")
  expect_match(html, "![A plot](a_12345678.png){width=60%}\\ ", fixed = TRUE)
  expect_match(html, "![b](b_1.png){width=25%}\\ ", fixed = TRUE)
  expect_false(grepl("figures/", html, fixed = TRUE))
})

test_that("exports include figures (PDF, Canvas, Markdown copy and embed)", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    s <- fig_session()
    f <- add_figure(make_png("p.png"), s)$file
    save_question_set(list(id = "set1", topic = "t", study_id = "st1",
                           questions = list(fig_question(f), fig_question(f, "choice"))), s)
    # assign first: expect_match() evaluates its argument twice
    r <- export_question_sets("set1", s, "pdf", destination = "pdf")
    expect_match(r, "^Exported 2")
    r <- export_question_sets("set1", s, "cv", destination = "canvas")
    expect_match(r, "^Exported 2")
    zip <- list.files(file.path(s, "exports"), "\\.zip$", recursive = TRUE, full.names = TRUE)
    expect_true(any(grepl(f, utils::unzip(zip, list = TRUE)$Name, fixed = TRUE)))
    r <- export_question_sets("set1", s, "md", destination = "markdown")
    expect_match(r, "^Exported 2")
    md_dir <- dirname(list.files(file.path(s, "exports"), "\\.md$", recursive = TRUE, full.names = TRUE))
    expect_true(file.exists(file.path(md_dir, "figures", f)))
    md <- questions_to_markdown("set1", s, figures = "embed")
    expect_match(md, "](data:image/png;base64,", fixed = TRUE)
    expect_false(grepl("figures/", md, fixed = TRUE))
    js <- questions_to_json("set1", s)
    expect_match(js, "](data:image/png;base64,", fixed = TRUE)
    expect_false(grepl("figures/", js, fixed = TRUE))
    questions_to_json("set1", s, file = "q.json", figures = "copy")
    expect_true(file.exists(file.path("figures", f)))
  })
})

test_that("figure_tool_result carries the image and a display for shinychat", {
  withr::with_tempdir({
    s <- fig_session()
    f <- add_figure(make_png("p.png"), s)$file
    r <- figure_tool_result("Figure saved.", f, s)
    expect_true(inherits(r, "ellmer::ContentToolResult"))
    expect_length(r@value, 2L)
    expect_true(inherits(r@value[[2]], "ellmer::ContentImageInline"))
    expect_match(r@extra$display$markdown, paste0("](figures/", f, ")"), fixed = TRUE)
    r1 <- figure_tool_result("", paste0("figures/", f), s)
    expect_true(inherits(r1@value, "ellmer::ContentImageInline"))
  })
})
