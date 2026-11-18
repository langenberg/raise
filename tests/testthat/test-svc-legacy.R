library(RAISE)

# Mimics the legacy `raise` QuizSlides object: questions plus a cache that
# held the OpenAI key, which must never reach the question set.
write_legacy_quiz <- function(path, questions, obj_name = "quiz") {
  Quiz <- R6::R6Class("Quiz", private = list(
    questions = NULL, title = "", cache = NULL
  ), public = list(initialize = function(questions) {
    private$questions <- questions
    private$cache <- list(api_key = "sk-legacy-secret")
  }))
  QuizSlides <- R6::R6Class("QuizSlides", inherit = Quiz)
  e <- new.env()
  assign(obj_name, QuizSlides$new(questions), envir = e)
  save(list = obj_name, envir = e, file = path)
  path
}

legacy_questions <- function() list(
  list(
    question = "What does a statistical model aim to do?",
    type = "multiple_choice",
    choices = list(
      list(choice = "Eliminate variability", correct = FALSE),
      list(choice = "Simplify reality", correct = TRUE)
    ),
    explanation = "A model simplifies reality.",
    legacy_extra = "dropped"
  ),
  list(
    question = "What is the slope?",
    type = "numeric",
    correct_answer = "1.5",
    explanation = "From the output."
  )
)

test_that("import_legacy_quiz writes a valid question set", {
  withr::with_tempdir({
    rdata <- write_legacy_quiz("quiz1.RData", legacy_questions())
    dest <- import_legacy_quiz(rdata, "sess", "lecture1", "Lecture 1 quiz")
    expect_equal(dest, file.path("sess", "question_sets", "lecture1.json"))
    expect_true(dir.exists(file.path("sess", "materials")))

    qset <- jsonlite::read_json(dest)
    expect_equal(qset$id, "lecture1")
    expect_equal(qset$topic, "Lecture 1 quiz")
    expect_equal(qset$imported_from, "quiz1.RData")
    expect_false("study_id" %in% names(qset))
    expect_length(qset$questions, 2L)
    expect_equal(vapply(qset$questions, function(q) q$id, integer(1)), 1:2)
    expect_true(qset$questions[[1]]$choices[[2]]$correct)
    expect_null(qset$questions[[1]]$legacy_extra)
    expect_identical(qset$questions[[2]]$correct_answer, 1.5)
  })
})

test_that("import_legacy_quiz never copies the legacy cache", {
  withr::with_tempdir({
    rdata <- write_legacy_quiz("quiz1.RData", legacy_questions())
    dest <- import_legacy_quiz(rdata, "sess", "lecture1", "Lecture 1 quiz")
    expect_false(any(grepl("sk-legacy-secret", readLines(dest))))
  })
})

test_that("import_legacy_quiz records source_materials", {
  withr::with_tempdir({
    rdata <- write_legacy_quiz("quiz1.RData", legacy_questions())
    dest <- import_legacy_quiz(rdata, "sess", "l1", "L1",
                               source_materials = "lecture1.pdf")
    expect_equal(jsonlite::read_json(dest)$source_materials, list("lecture1.pdf"))
  })
})

test_that("import_legacy_quiz rejects files without a Quiz object", {
  withr::with_tempdir({
    thread <- data.frame(x = 1)
    save(thread, file = "thread.RData")
    expect_error(import_legacy_quiz("thread.RData", "sess", "t", "T"),
                 "no legacy Quiz object")
    expect_error(import_legacy_quiz("missing.RData", "sess", "t", "T"),
                 "file not found")
  })
})

test_that("import_legacy_quiz reports invalid legacy questions", {
  withr::with_tempdir({
    bad <- list(list(question = "Q?", type = "numeric", explanation = "E"))
    rdata <- write_legacy_quiz("bad.RData", bad)
    expect_error(import_legacy_quiz(rdata, "sess", "b", "B"), "correct_answer")
  })
})
