library(RAISE)

# Two sets: one from materials (LaTeX in \( \) delimiters, MC + numeric) and
# one linked to a study whose regression output is prepended.
write_md_session <- function() {
  dir.create("question_sets", showWarnings = FALSE)
  dir.create(file.path("studies", "s1"), recursive = TRUE, showWarnings = FALSE)
  writeLines("coef  0.5", file.path("studies", "s1", "regression.txt"))
  jsonlite::write_json(list(
    id = "set_a", topic = "A", source_materials = list("l1.pdf"),
    questions = list(
      list(id = 1L, type = "multiple_choice",
           question = "Which equation is the model, \\( Y_i = \\beta_0 + \\epsilon_i \\)?",
           explanation = "Only $\\beta_0$ is estimated.",
           choices = list(list(choice = "$\\beta_1 = 0$", correct = FALSE),
                          list(choice = "$Y_i = \\beta_0$", correct = TRUE))),
      list(id = 2L, type = "numeric", question = "What is the slope?",
           explanation = "Read it off.", correct_answer = 0.6061)
    )
  ), file.path("question_sets", "set_a.json"), auto_unbox = TRUE, digits = NA)
  jsonlite::write_json(list(
    id = "set_b", topic = "B", study_id = "s1",
    questions = list(list(id = 7L, type = "numeric", question = "Coefficient?",
                          explanation = "", correct_answer = 0.5))
  ), file.path("question_sets", "set_b.json"), auto_unbox = TRUE, digits = NA)
}

test_that("format_questions numbers questions across sets and rounds answers", {
  withr::with_tempdir({
    write_md_session()
    items <- format_questions(c("set_a", "set_b"), ".")
    expect_length(items, 3L)
    expect_equal(vapply(items, `[[`, 1L, "number"), 1:3)
    expect_equal(items[[3]]$qset_id, "set_b")
    expect_equal(items[[3]]$id, 7L)
    expect_equal(items[[1]]$answer, "$Y_i = \\beta_0$")
    expect_equal(items[[2]]$answer, "0.61")
    expect_match(items[[2]]$precision_note, "2 decimal places")
    expect_match(items[[1]]$question, "$Y_i = \\beta_0 + \\epsilon_i$", fixed = TRUE)
    expect_match(items[[3]]$question, "^```\ncoef  0.5\n```")
  })
})

test_that("format_questions supports letter labels, auto precision and no note", {
  withr::with_tempdir({
    write_md_session()
    items <- format_questions("set_a", ".", precision_mode = "auto",
                              include_precision_note = FALSE, choice_style = "letter")
    expect_equal(items[[1]]$choices[[2]]$label, "b)")
    expect_equal(items[[1]]$answer, "b) $Y_i = \\beta_0$")
    expect_equal(items[[2]]$answer, "0.6061")
    expect_null(items[[2]]$precision_note)
  })
})

test_that("questions_to_markdown with answers", {
  withr::with_tempdir({
    write_md_session()
    md <- questions_to_markdown("set_a", ".", answers = TRUE)
    expect_match(md, "^## Question 1\n\nWhich equation")
    expect_match(md, "- $\\beta_1 = 0$\n- $Y_i = \\beta_0$", fixed = TRUE)
    expect_match(md, "**Correct answer:** $Y_i = \\beta_0$\n\n**Explanation:** Only", fixed = TRUE)
    expect_match(md, "## Question 2\n\nWhat is the slope?\n\n*Round your answer", fixed = TRUE)
    expect_match(md, "**Correct answer:** 0.61 (unrounded: 0.6061)", fixed = TRUE)
  })
})

test_that("questions_to_markdown without answers, other heading level, letters", {
  withr::with_tempdir({
    write_md_session()
    md <- questions_to_markdown("set_a", ".", answers = FALSE, heading_level = 3L,
                                choice_style = "letter")
    expect_match(md, "^### Question 1")
    expect_match(md, "- a) $\\beta_1 = 0$", fixed = TRUE)
    expect_false(grepl("Correct answer|Explanation", md))
  })
})

test_that("questions_to_markdown skips an empty explanation", {
  withr::with_tempdir({
    write_md_session()
    md <- questions_to_markdown("set_b", ".")
    expect_match(md, "**Correct answer:** 0.50\n", fixed = TRUE)
    expect_false(grepl("Explanation", md))
  })
})

test_that("questions_to_markdown writes the file and returns the text invisibly", {
  withr::with_tempdir({
    write_md_session()
    expect_invisible(md <- questions_to_markdown("set_a", ".", file = "quiz.md"))
    expect_equal(paste0(paste(readLines("quiz.md"), collapse = "\n"), "\n"), md)
  })
})

test_that("export_question_sets writes a markdown export with meta.json", {
  withr::with_tempdir({
    write_md_session()
    msg <- export_question_sets("set_a", ".", label = "Quiz", destination = "markdown",
                                answers = FALSE)
    expect_match(msg, "Exported 2 question")
    dir  <- list.dirs("exports", recursive = FALSE)
    md   <- list.files(dir, "\\.md$", full.names = TRUE)
    expect_length(md, 1L)
    expect_false(any(grepl("Correct answer", readLines(md))))
    meta <- jsonlite::read_json(file.path(dir, "meta.json"))
    expect_equal(meta$destination, "markdown")
    expect_false(meta$answers)
  })
})

test_that("questions_to_json has one entry per question, same Markdown as questions_to_markdown", {
  withr::with_tempdir({
    write_md_session()
    q <- jsonlite::fromJSON(questions_to_json(c("set_a", "set_b"), "."), simplifyVector = FALSE)$questions
    expect_length(q, 3L)
    expect_equal(vapply(q, `[[`, 1L, "number"), 1:3)
    expect_equal(q[[3]]$qset_id, "set_b")
    expect_equal(q[[3]]$id, 7L)
    expect_equal(q[[2]]$type, "numeric")
    md <- questions_to_markdown(c("set_a", "set_b"), ".")
    rebuilt <- paste0(paste(vapply(q, function(x) paste0("## Question ", x$number, "\n\n",
                                                         x$question, "\n\n", x$answer), ""),
                            collapse = "\n\n"), "\n")
    expect_identical(rebuilt, md)
    expect_match(q[[2]]$question, "2 decimal places", fixed = TRUE)
    expect_equal(q[[2]]$answer, "**Correct answer:** 0.61 (unrounded: 0.6061)\n\n**Explanation:** Read it off.")
    expect_equal(q[[3]]$answer, "**Correct answer:** 0.50")
  })
})

test_that("questions_to_json without answers, and to a file", {
  withr::with_tempdir({
    write_md_session()
    q <- jsonlite::fromJSON(questions_to_json("set_a", ".", answers = FALSE),
                            simplifyVector = FALSE)$questions
    expect_null(q[[1]]$answer)
    expect_false(any(grepl("Correct answer", vapply(q, `[[`, "", "question"))))
    res <- withVisible(questions_to_json("set_a", ".", file = "q.json"))
    expect_false(res$visible)
    expect_identical(paste(readLines("q.json", encoding = "UTF-8"), collapse = "\n"), res$value)
  })
})

test_that("export_question_sets writes a json export that list_exports finds", {
  withr::with_tempdir({
    write_md_session()
    msg <- export_question_sets("set_a", ".", label = "Quiz", destination = "json",
                                answers = FALSE)
    expect_match(msg, "Exported 2 question")
    dir  <- list.dirs("exports", recursive = FALSE)
    art  <- file.path(dir, paste0(basename(dir), ".json"))
    expect_true(file.exists(art))
    q <- jsonlite::read_json(art)$questions
    expect_length(q, 2L)
    expect_null(q[[1]]$answer)
    expect_equal(export_artifact(dir), art)
    meta <- jsonlite::read_json(file.path(dir, "meta.json"))
    expect_equal(meta$destination, "json")
    expect_false(meta$answers)
    ex <- jsonlite::fromJSON(list_exports("."))
    expect_true(ex$has_artifact)
  })
})

test_that("new_export_dir never reuses a folder (same label, same second)", {
  withr::with_tempdir({
    a <- new_export_dir(".", "quiz", "20260101_000000")
    b <- new_export_dir(".", "quiz", "20260101_000000")
    c <- new_export_dir(".", "quiz", "20260101_000000")
    expect_equal(basename(c(a, b, c)),
                 c("quiz_20260101_000000", "quiz_20260101_000000_2", "quiz_20260101_000000_3"))
    expect_true(all(dir.exists(c(a, b, c))))
  })
})

test_that("export_artifact ignores combined.json and meta.json", {
  withr::with_tempdir({
    dir.create("e_20260101_000000")
    for (f in c("combined.json", "meta.json")) writeLines("{}", file.path("e_20260101_000000", f))
    expect_null(export_artifact("e_20260101_000000"))
  })
})

test_that("export_question_sets rejects unknown destinations", {
  withr::with_tempdir({
    write_md_session()
    expect_match(export_question_sets("set_a", ".", "x", destination = "docx"),
                 "^Error: destination must be one of")
    expect_false(dir.exists("exports"))
  })
})

test_that("answers = FALSE is ignored with a warning for LMS exports", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    write_md_session()
    expect_warning(
      res <- export_question_sets("set_a", ".", "x", destination = "canvas",
                                  answers = FALSE),
      "answers = FALSE is ignored for canvas"
    )
    expect_match(res, "^Exported .*Note: answers = FALSE is ignored")
    meta <- jsonlite::read_json(list.files("exports", "meta.json", recursive = TRUE,
                                           full.names = TRUE))
    expect_true(meta$answers)
    expect_length(list.files("exports", "\\.zip$", recursive = TRUE), 1L)
  })
})

test_that("PDF export without answers leaves out the solutions", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    write_md_session()
    for (a in c(TRUE, FALSE))
      export_question_sets("set_a", ".", label = paste0("ans_", a),
                           destination = "pdf", answers = a)
    pdf_text <- function(label) {
      f <- list.files("exports", paste0("^", label, ".*\\.pdf$"), recursive = TRUE,
                      full.names = TRUE)
      paste(pdftools::pdf_text(f), collapse = "\n")
    }
    expect_match(pdf_text("ans_true"), "Solution")
    expect_false(grepl("Solution", pdf_text("ans_false")))
    expect_match(pdf_text("ans_false"), "What is the slope")
    expect_length(list.files("exports", "questions\\.tex$", recursive = TRUE), 0L)
  })
})
