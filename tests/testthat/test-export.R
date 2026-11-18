library(RAISE)

make_mc_qset <- function(id = "export_test") {
  list(
    id               = id,
    topic            = "Regression basics",
    study_id         = "study_abc",
    source_materials = list(),
    questions        = list(
      list(
        type        = "multiple_choice",
        question    = "What does a significant p-value indicate?",
        explanation = "A significant p-value means we reject the null hypothesis.",
        exhibit     = NULL,
        choices = list(
          list(choice = "The null hypothesis is true",        correct = FALSE),
          list(choice = "We reject the null hypothesis",      correct = TRUE),
          list(choice = "The effect size is large",           correct = FALSE),
          list(choice = "The confidence interval contains 0", correct = FALSE)
        )
      )
    ),
    model      = "claude-test",
    created_at = "2026-01-01T00:00:00Z"
  )
}

make_numeric_qset <- function(id = "num_test") {
  list(
    id               = id,
    topic            = "Numeric",
    study_id         = "study_abc",
    source_materials = list(),
    questions        = list(
      list(
        type           = "numeric",
        question       = "What is the estimated coefficient for study_hrs (rounded to 2 d.p.)?",
        explanation    = "Read the coefficient from the Coefficients table.",
        exhibit        = NULL,
        correct_answer = 2.34
      )
    ),
    model      = "claude-test",
    created_at = "2026-01-01T00:00:00Z"
  )
}

write_qset <- function(qset, dir) {
  path <- file.path(dir, paste0(qset$id, ".json"))
  jsonlite::write_json(qset, path, pretty = TRUE, auto_unbox = TRUE)
  path
}

# -- compute_precision ---------------------------------------------------------

test_that("compute_precision fixed 2dp returns correct values", {
  prec <- compute_precision(1.84, mode = "fixed", n_digits = 2L)
  expect_equal(prec$decimal_places, 2L)
  expect_equal(prec$tolerance, 0.005)
  expect_match(prec$format_note, "2 decimal places")
  expect_match(prec$format_note, "period")
})

test_that("compute_precision fixed 1dp uses singular 'decimal place'", {
  prec <- compute_precision(3.5, mode = "fixed", n_digits = 1L)
  expect_match(prec$format_note, "1 decimal place[^s]")
})

test_that("compute_precision fixed 0dp returns integer note", {
  prec <- compute_precision(42, mode = "fixed", n_digits = 0L)
  expect_equal(prec$tolerance, 0.5)
  expect_match(prec$format_note, "whole number")
})

test_that("compute_precision auto derives dp from answer", {
  prec <- compute_precision(2.34, mode = "auto")
  expect_equal(prec$decimal_places, 2L)
  expect_equal(prec$tolerance, 0.005)
})

test_that("compute_precision auto treats integer answer as 0dp", {
  prec <- compute_precision(42, mode = "auto")
  expect_equal(prec$decimal_places, 0L)
  expect_match(prec$format_note, "whole number")
})

test_that("compute_precision auto caps at 4dp with warning", {
  expect_warning(
    prec <- compute_precision(1e-5, mode = "auto"),
    "capping at 4"
  )
  expect_equal(prec$decimal_places, 4L)
})

# -- fill_template / make_rmd --------------------------------------------------

test_that("fill_template replaces all placeholders", {
  withr::with_tempdir({
    writeLines("Hello {{name}}, value is {{val}}.", "tpl.txt")
    result <- fill_template("tpl.txt", list(name = "World", val = "42"))
    expect_equal(result, "Hello World, value is 42.")
  })
})

test_that("make_rmd MC produces schoice Rmd with correct structure", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  q <- list(
    type        = "multiple_choice",
    question    = "What is 2+2?",
    explanation = "Basic arithmetic.",
    exhibit     = NULL,
    choices     = list(
      list(choice = "3", correct = FALSE),
      list(choice = "4", correct = TRUE)
    )
  )
  rmd <- make_rmd(q, 1L, tpl_dir)
  expect_match(rmd, "extype: schoice")
  expect_match(rmd, "exsolution: 01")
  expect_match(rmd, "raise_q_1")
})

test_that("make_rmd MC keeps LaTeX backslashes and quotes in choices parseable", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  choices <- c("$\\log(\\widehat{\\text{Odds}}) = b_0$", 'The "odds" ratio')
  q <- list(
    type = "multiple_choice", question = "Which equation?", explanation = "E.",
    choices = list(list(choice = choices[1], correct = TRUE),
                   list(choice = choices[2], correct = FALSE))
  )
  rmd  <- make_rmd(q, 1L, tpl_dir)
  call <- regmatches(rmd, regexpr("exams::answerlist\\(c\\(.*?\\), markup", rmd, perl = TRUE))
  vec  <- sub("^exams::answerlist\\((.*), markup$", "\\1", call)
  expect_identical(eval(parse(text = vec)), choices)
})

test_that("make_rmd numeric includes format note when include_precision_note=TRUE", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  q <- list(
    type           = "numeric",
    question       = "What is the coefficient?",
    explanation    = "Read from output.",
    exhibit        = NULL,
    correct_answer = 2.34
  )
  rmd <- make_rmd(q, 1L, tpl_dir, precision_mode = "fixed", n_digits = 2L,
                  include_precision_note = TRUE)
  expect_match(rmd, "2 decimal places")
  expect_match(rmd, "extol: 0.005")
})

test_that("make_rmd numeric omits format note when include_precision_note=FALSE", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  q <- list(
    type           = "numeric",
    question       = "What is the coefficient?",
    explanation    = "Read from output.",
    exhibit        = NULL,
    correct_answer = 2.34
  )
  rmd <- make_rmd(q, 1L, tpl_dir, precision_mode = "fixed", n_digits = 2L,
                  include_precision_note = FALSE)
  expect_false(grepl("decimal places", rmd))
  expect_match(rmd, "extol: 0.005")
})

test_that("make_rmd numeric uses exhibit when present", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  q <- list(
    type           = "numeric",
    question       = "What is the OR?",
    explanation    = "exp(0.61) = 1.84.",
    exhibit        = "study_hrs: OR = 1.84",
    correct_answer = 1.84
  )
  rmd <- make_rmd(q, 2L, tpl_dir)
  expect_match(rmd, "study_hrs: OR = 1.84")
})

# -- export_to_exams -----------------------------------------------------------

test_that("export_to_exams rejects invalid destination", {
  withr::with_tempdir({
    dir.create("question_sets")
    path <- write_qset(make_mc_qset(), ".")
    expect_error(export_to_exams(path, "out", destination = "word"), "destination")
  })
})

test_that("export_to_exams produces a PDF for a multiple-choice question set", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    path   <- write_qset(make_mc_qset(), ".")
    result <- export_to_exams(path, "out", destination = "pdf")
    expect_true(file.exists(result))
    expect_true(grepl("\\.pdf$", result, ignore.case = TRUE))
  })
})

test_that("export_to_exams produces a PDF for a numeric question set", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    path   <- write_qset(make_numeric_qset(), ".")
    result <- export_to_exams(path, "out", destination = "pdf",
                              precision_mode = "fixed", n_digits = 2L)
    expect_true(file.exists(result))
  })
})

test_that("export_to_exams respects include_precision_note=FALSE", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    path   <- write_qset(make_numeric_qset(), ".")
    result <- export_to_exams(path, "out", destination = "pdf",
                              include_precision_note = FALSE)
    expect_true(file.exists(result))
  })
})

# -- batch export helpers ------------------------------------------------------

make_combined_json <- function(questions, id = "combined_test") {
  list(
    id               = id,
    topic            = "combined",
    study_id         = NULL,
    source_materials = list(),
    questions        = questions,
    model            = "",
    created_at       = ""
  )
}

test_that("batch export: combined JSON merges questions from multiple sets", {
  withr::with_tempdir({
    q1 <- make_mc_qset("q1")$questions
    q2 <- make_numeric_qset("q2")$questions
    combined <- make_combined_json(c(q1, q2))
    expect_length(combined$questions, 2L)
    expect_equal(combined$id, "combined_test")
    expect_null(combined$study_id)
  })
})

test_that("batch export: export_to_exams accepts combined JSON and produces PDF", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    q1 <- make_mc_qset("q1")$questions
    q2 <- make_numeric_qset("q2")$questions
    combined <- make_combined_json(c(q1, q2), id = "combined_20260507_120000")
    json_path <- file.path(".", "combined.json")
    jsonlite::write_json(combined, json_path, pretty = TRUE, auto_unbox = TRUE)
    result <- export_to_exams(json_path, "out", destination = "pdf")
    expect_true(file.exists(result))
    expect_true(grepl("\\.pdf$", result, ignore.case = TRUE))
  })
})

# -- meta.json -----------------------------------------------------------------

test_that("meta.json fields are correct", {
  withr::with_tempdir({
    meta <- list(
      label       = "Test Export",
      destination = "pdf",
      n_questions = 2L,
      created_at  = "2026-05-07T12:00:00"
    )
    jsonlite::write_json(meta, "meta.json", auto_unbox = TRUE)
    written <- jsonlite::read_json("meta.json")
    expect_equal(written$label, "Test Export")
    expect_equal(written$destination, "pdf")
    expect_equal(written$n_questions, 2L)
    expect_true(nzchar(written$created_at))
  })
})

# -- export_question_sets ------------------------------------------------------

test_that("export_question_sets returns error when no questions found", {
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    msg <- export_question_sets(list("nonexistent_set"), ".", label = "Test Export",
                                destination = "pdf")
    expect_match(msg, "Error")
  })
})

test_that("export_question_sets merges questions from two sets", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    write_qset(make_mc_qset("qs1"),      "question_sets")
    write_qset(make_numeric_qset("qs2"), "question_sets")
    msg <- export_question_sets(list("qs1", "qs2"), ".", label = "Test Export",
                                destination = "pdf")
    expect_false(grepl("^Error", msg))
    expect_match(msg, "2 question")
  })
})

test_that("export_question_sets writes meta.json with correct fields", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    write_qset(make_mc_qset("qs1"), "question_sets")
    export_question_sets(list("qs1"), ".", label = "Test Export", destination = "pdf")
    export_dirs <- list.dirs("exports", recursive = FALSE, full.names = TRUE)
    expect_length(export_dirs, 1L)
    meta <- jsonlite::read_json(file.path(export_dirs[[1]], "meta.json"))
    expect_equal(meta$label, "Test Export")
    expect_equal(meta$destination, "pdf")
    expect_equal(meta$n_questions, 1L)
    expect_true(nzchar(meta$created_at))
  })
})

test_that("export_question_sets passes precision_mode to export", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    write_qset(make_numeric_qset("qs1"), "question_sets")
    msg <- export_question_sets(list("qs1"), ".", label = "Test Export",
                                destination = "pdf", precision_mode = "fixed", n_digits = 3L)
    expect_false(grepl("^Error", msg))
  })
})

# -- export_question_sets label -----------------------------------------------

test_that("export_question_sets uses label as folder slug", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    write_qset(make_mc_qset("qs1"), "question_sets")
    msg <- export_question_sets(list("qs1"), ".", label = "Midterm Exam",
                                destination = "pdf")
    export_dirs <- list.dirs("exports", recursive = FALSE, full.names = FALSE)
    expect_length(export_dirs, 1L)
    expect_true(startsWith(export_dirs[[1L]], "midterm_exam_"))
    expect_false(grepl("^Error", msg))
  })
})

# -- delete_export -------------------------------------------------------------

test_that("delete_export removes the export folder", {
  withr::with_tempdir({
    exp_dir <- file.path(".", "exports", "combined_20260101_120000")
    dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("", file.path(exp_dir, "output.pdf"))
    msg <- delete_export("combined_20260101_120000", ".")
    expect_match(msg, "deleted")
    expect_false(dir.exists(exp_dir))
  })
})

test_that("delete_export returns error for missing export", {
  withr::with_tempdir({
    dir.create(file.path(".", "exports"), showWarnings = FALSE)
    msg <- delete_export("no_such_export", ".")
    expect_match(msg, "Error")
  })
})

# -- escape_text ---------------------------------------------------------------

test_that("escape_text preserves fenced code block delimiters and content", {
  input  <- "Before\n```\n%% some output\n```\nAfter"
  result <- escape_text(input)
  expect_true(grepl("```", result, fixed = TRUE))
  expect_true(grepl("%% some output", result, fixed = TRUE))
})

test_that("escape_text escapes outside fences but not inside", {
  sym    <- intToUtf8(0x2248)
  input  <- paste0("approx ", sym, " x\n```\n", sym, " inside\n```\nafter ", sym)
  result <- escape_text(input)
  expect_true(grepl("approx approx. x", result, fixed = TRUE))
  expect_true(grepl("after approx.", result, fixed = TRUE))
  expect_true(grepl(paste0(sym, " inside"), result, fixed = TRUE))
})

test_that("escape_text preserves %% lines inside fenced block", {
  reg <- "%% Call:\n%% stats::lm(formula = y ~ x)\n%% Coefficients:\n%%   x  0.5"
  input  <- paste0("```\n", reg, "\n```\n\nWhat is the slope?")
  result <- escape_text(input)
  expect_true(grepl("%% Call:", result, fixed = TRUE))
  expect_true(grepl("%% stats::lm", result, fixed = TRUE))
})

test_that("escape_text still replaces lone backtick outside a fence", {
  result <- escape_text("use `lm()` to fit")
  expect_false(grepl("`", result, fixed = TRUE))
  expect_true(grepl("'lm()'", result, fixed = TRUE))
})

# -- read_regression_output ----------------------------------------------------

test_that("read_regression_output returns content when regression.txt exists", {
  withr::with_tempdir({
    study_dir <- file.path(".", "studies", "study_abc")
    dir.create(study_dir, recursive = TRUE)
    writeLines(c("Call:", "lm(y ~ x)", "Coefficients:", "x  0.5"), file.path(study_dir, "regression.txt"))
    result <- read_regression_output("study_abc", ".")
    expect_true(grepl("lm(y ~ x)", result, fixed = TRUE))
    expect_true(grepl("Coefficients", result, fixed = TRUE))
  })
})

test_that("read_regression_output returns NULL when study directory is missing", {
  withr::with_tempdir({
    result <- read_regression_output("nonexistent_study", ".")
    expect_null(result)
  })
})

# -- inject_regression ---------------------------------------------------------

test_that("inject_regression prepends plain fenced block when question has no code", {
  q      <- list(question = "What is the slope?", type = "numeric", correct_answer = 0.5)
  result <- inject_regression(q, "Call:\nlm(y ~ x)")
  expect_true(startsWith(result[["question"]], "```\n"))
  expect_true(grepl("What is the slope?", result[["question"]], fixed = TRUE))
})

test_that("inject_regression skips injection when question already starts with a fence", {
  original <- "```\noutput\n```\n\nWhat is the slope?"
  q        <- list(question = original, type = "numeric", correct_answer = 0.5)
  result   <- inject_regression(q, "Call:\nlm(y ~ x)")
  expect_equal(result[["question"]], original)
})

# -- transform_code_fences -----------------------------------------------------

test_that("transform_code_fences converts plain fence to latex verbatim for pdf", {
  rmd    <- "Before\n```\nsome ***output*** <.001\n```\nAfter"
  result <- transform_code_fences(rmd, "pdf")
  expect_false(grepl("^```$", result, perl = TRUE))
  expect_true(grepl("```{=latex}", result, fixed = TRUE))
  expect_true(grepl("\\begin{verbatim}", result, fixed = TRUE))
  expect_true(grepl("***", result, fixed = TRUE))
  expect_true(grepl("<.001", result, fixed = TRUE))
})

test_that("transform_code_fences converts plain fence to html pre block for canvas", {
  rmd    <- "Before\n```\n<.001 >.999 & ***\n```\nAfter"
  result <- transform_code_fences(rmd, "canvas")
  expect_true(grepl("```{=html}", result, fixed = TRUE))
  expect_true(grepl("<pre>", result, fixed = TRUE))
  expect_true(grepl("&lt;.001", result, fixed = TRUE))
  expect_true(grepl("&gt;.999", result, fixed = TRUE))
  expect_true(grepl("&amp;", result, fixed = TRUE))
})

test_that("transform_code_fences converts named fence (e.g. text) to raw block", {
  rmd    <- "```text\nhello world\n```"
  result <- transform_code_fences(rmd, "pdf")
  expect_true(grepl("```{=latex}", result, fixed = TRUE))
})

test_that("transform_code_fences returns unchanged content when no plain fences present", {
  rmd    <- "Just text\n\nNo fences here."
  result <- transform_code_fences(rmd, "pdf")
  expect_equal(result, rmd)
})

test_that("normalise_math converts \\( \\) and \\[ \\] delimiters", {
  expect_equal(normalise_math("so \\( \\hat{Y} = 2 \\) here"), "so $\\hat{Y} = 2$ here")
  expect_equal(normalise_math("\\[ y = x \\]"), "$$ y = x $$")
})

test_that("normalise_math trims spaces inside $...$ only for math content", {
  expect_equal(normalise_math("odds $ e^{-0.994} \\approx 0.370 $."),
               "odds $e^{-0.994} \\approx 0.370$.")
  expect_equal(normalise_math("costs $ 5 and $ 10"), "costs $ 5 and $ 10")
  expect_equal(normalise_math("$a_1$ and $ b_2 $"), "$a_1$ and $b_2$")
  expect_equal(normalise_math("$$ y = x $$"), "$$ y = x $$")
  x <- "Coefficient $\\times$ Centered Educ = $-1.3210 + 0.0192 \\times 0.212$"
  expect_equal(normalise_math(x), x)
  expect_equal(normalise_math("$\\times$ b = $ c^2 $"), "$\\times$ b = $c^2$")
})

test_that("normalise_math escapes underscores in \\text{} for pdf only", {
  x <- "$\\text{study_hours_x} + b_1$"
  expect_equal(normalise_math(x, "pdf"), "$\\text{study\\_hours\\_x} + b_1$")
  expect_equal(normalise_math(x, "canvas"), x)
  expect_equal(normalise_math("$\\text{a\\_b}$", "pdf"), "$\\text{a\\_b}$")
})

test_that("escape_text turns headings into bold lines and keeps bold pairs", {
  x <- "## Part 2: *Linear* Regression\n\n# **Question:** What is $R^2$?\n\nUse **score** and b * x."
  out <- escape_text(x)
  expect_match(out, "^\\*\\*Part 2: Linear Regression\\*\\*\n")
  expect_match(out, "\n\\*\\*Question: What is \\$R\\^2\\$\\?\\*\\*\n", perl = TRUE)
  expect_match(out, "Use **score** and b \\* x.", fixed = TRUE)
  expect_false(grepl("(?m)^#", out, perl = TRUE))
})

test_that("escape_text leaves headings inside fences alone", {
  x <- "Text\n```\n# a comment\n```\n"
  expect_match(escape_text(x), "# a comment", fixed = TRUE)
})

test_that("unicode_in_math turns Greek letters and symbols into LaTeX commands", {
  expect_equal(unicode_in_math("$β1 = 0$"), "$\\beta 1 = 0$")
  expect_equal(unicode_in_math("$$a ≈ b$$"), "$$a \\approx  b$$")
  expect_equal(unicode_in_math("β outside"), "β outside")
})

test_that("escape_text spells out Greek outside math and uses commands inside", {
  expect_equal(escape_text("Yi = β0 + εi, $β_1$"),
               "Yi = beta0 + epsiloni, $\\beta _1$")
})

test_that("make_rmd MC escapes Unicode in choices", {
  tpl_dir <- system.file("templates", package = "RAISE")
  if (!nzchar(tpl_dir)) tpl_dir <- file.path("inst", "templates")
  q <- list(
    type = "multiple_choice", question = "Which model?", explanation = "E.",
    choices = list(list(choice = "Yi = β0 + εi", correct = TRUE),
                   list(choice = "$β1 = 0$", correct = FALSE))
  )
  rmd <- make_rmd(q, 1L, tpl_dir)
  expect_false(grepl("β|ε", rmd))
  expect_match(rmd, "Yi = beta0 + epsiloni", fixed = TRUE)
  expect_match(rmd, "$\\\\beta 1 = 0$", fixed = TRUE)
})

test_that("failed export removes its folder and names the failing questions", {
  skip_if_not_installed("exams")
  withr::with_tempdir({
    dir.create("question_sets", showWarnings = FALSE)
    qset <- make_numeric_qset("broken")
    bad  <- qset$questions[[1]]
    bad$id <- 7L
    bad$explanation <- "Odds = 0.0192 \\times 0.212"
    good <- qset$questions[[1]]
    good$id <- 3L
    qset$questions <- list(good, bad)
    write_qset(qset, "question_sets")
    msg <- export_question_sets("broken", ".", label = "broken", destination = "pdf")
    expect_match(msg, "^Export failed")
    expect_match(msg, "broken #7", fixed = TRUE)
    expect_false(grepl("broken #3", msg, fixed = TRUE))
    expect_length(list.files("exports"), 0L)
  })
})
