library(RAISE)

# validate_question -------------------------------------------------------------

test_that("validate_question returns no errors for valid MC question", {
  q <- list(
    type        = "multiple_choice",
    question    = "What is 2+2?",
    explanation = "Basic arithmetic.",
    choices = list(
      list(choice = "3", correct = FALSE),
      list(choice = "4", correct = TRUE),
      list(choice = "5", correct = FALSE)
    )
  )
  expect_length(validate_question(q), 0L)
})

test_that("validate_question returns no errors for valid numeric question", {
  q <- list(
    type           = "numeric",
    question       = "What is the beta?",
    explanation    = "Read from table.",
    correct_answer = 2.34
  )
  expect_length(validate_question(q), 0L)
})

test_that("validate_question accepts numeric question with legacy tolerance field", {
  q <- list(
    type           = "numeric",
    question       = "What is the beta?",
    explanation    = "Read from table.",
    correct_answer = 2.34,
    tolerance      = 0.12
  )
  expect_length(validate_question(q), 0L)
})

test_that("validate_question catches missing required fields", {
  q <- list(type = "numeric", question = "Q?")
  errs <- validate_question(q)
  expect_true(any(grepl("explanation", errs)))
})

test_that("validate_question catches unknown type", {
  q <- list(type = "essay", question = "Q?", explanation = "E.")
  errs <- validate_question(q)
  expect_true(any(grepl("unknown question type", errs)))
})

test_that("validate_question catches MC with no correct choice", {
  q <- list(
    type        = "multiple_choice",
    question    = "Q?",
    explanation = "E.",
    choices = list(
      list(choice = "A", correct = FALSE),
      list(choice = "B", correct = FALSE)
    )
  )
  errs <- validate_question(q)
  expect_true(any(grepl("at least one choice", errs)))
})

test_that("validate_question catches MC with fewer than 2 choices", {
  q <- list(
    type        = "multiple_choice",
    question    = "Q?",
    explanation = "E.",
    choices = list(list(choice = "A", correct = TRUE))
  )
  errs <- validate_question(q)
  expect_true(any(grepl("at least 2", errs)))
})

test_that("validate_question catches missing correct_answer", {
  q <- list(
    type        = "numeric",
    question    = "Q?",
    explanation = "E."
  )
  errs <- validate_question(q)
  expect_true(any(grepl("correct_answer", errs)))
})

# save_question_set / delete_question_set / load_all_question_sets --------------

make_qset <- function(id = "test_qset") {
  list(
    id       = id,
    topic    = "Test topic",
    study_id = "study_abc",
    source_materials = list(),
    questions = list(list(
      type           = "numeric",
      question       = "Q?",
      explanation    = "E.",
      correct_answer = 1.0
    )),
    model      = "claude-test",
    created_at = "2026-01-01T00:00:00Z"
  )
}

test_that("save_question_set writes JSON and returns path", {
  withr::with_tempdir({
    dir.create("question_sets")
    dest <- save_question_set(make_qset(), ".")
    expect_true(file.exists(dest))
    data <- jsonlite::read_json(dest)
    expect_equal(data$id, "test_qset")
  })
})

test_that("save_question_set errors when id missing", {
  withr::with_tempdir({
    qset <- make_qset(); qset$id <- NULL
    expect_error(save_question_set(qset, "."), "id")
  })
})

test_that("save_question_set errors when no study_id or source_materials", {
  withr::with_tempdir({
    qset <- make_qset()
    qset$study_id        <- NULL
    qset$source_materials <- list()
    expect_error(save_question_set(qset, "."), "study_id.*source_materials")
  })
})

test_that("save_question_set accepts imported_from as the only provenance", {
  withr::with_tempdir({
    qset <- make_qset()
    qset$study_id         <- NULL
    qset$source_materials <- list()
    qset$imported_from    <- "old/quiz1.RData"
    dest <- save_question_set(qset, ".")
    expect_equal(jsonlite::read_json(dest)$imported_from, "old/quiz1.RData")
  })
})

test_that("save_question_set keeps full numeric precision", {
  withr::with_tempdir({
    qset <- make_qset()
    qset$questions <- list(list(type = "numeric", question = "Q?",
                                explanation = "E", correct_answer = 0.606045629222928))
    dest <- save_question_set(qset, ".")
    expect_identical(jsonlite::read_json(dest)$questions[[1]]$correct_answer,
                     0.606045629222928)
  })
})

test_that("delete_question_set removes the file", {
  withr::with_tempdir({
    dir.create("question_sets")
    dest <- save_question_set(make_qset("del_me"), ".")
    expect_true(file.exists(dest))
    delete_question_set("del_me", ".")
    expect_false(file.exists(dest))
  })
})

test_that("load_all_question_sets returns list of parsed objects", {
  withr::with_tempdir({
    dir.create("question_sets")
    save_question_set(make_qset("q1"), ".")
    save_question_set(make_qset("q2"), ".")
    all <- load_all_question_sets(".")
    expect_length(all, 2L)
  })
})

test_that("load_all_question_sets returns empty list when folder absent", {
  withr::with_tempdir({
    expect_length(load_all_question_sets("."), 0L)
  })
})

# save_question_set auto-assigns integer IDs -----------------------------------

test_that("save_question_set assigns sequential integer IDs when questions lack them", {
  withr::with_tempdir({
    dir.create("question_sets")
    dest <- save_question_set(make_qset(), ".")
    data <- jsonlite::read_json(dest, simplifyVector = FALSE)
    expect_equal(data$questions[[1]]$id, 1L)
  })
})

test_that("save_question_set preserves existing integer IDs", {
  withr::with_tempdir({
    dir.create("question_sets")
    qset <- make_qset()
    qset$questions[[1]]$id <- 5L
    dest <- save_question_set(qset, ".")
    data <- jsonlite::read_json(dest, simplifyVector = FALSE)
    expect_equal(data$questions[[1]]$id, 5L)
  })
})

# .assign_question_ids ---------------------------------------------------------

test_that(".assign_question_ids stamps missing IDs starting at 1", {
  qs <- list(list(type = "numeric"), list(type = "multiple_choice"))
  result <- .assign_question_ids(qs)
  expect_equal(result[[1]]$id, 1L)
  expect_equal(result[[2]]$id, 2L)
})

# .next_q_id -------------------------------------------------------------------

test_that(".next_q_id returns 1 for empty list", {
  expect_equal(.next_q_id(list()), 1L)
})

test_that(".next_q_id returns max + 1 even after deletions", {
  qs <- list(list(id = 2L), list(id = 3L))
  expect_equal(.next_q_id(qs), 4L)
})

# remove_questions -------------------------------------------------------------

make_saved_qset <- function(session_path, n = 3L) {
  qset <- list(
    id               = "edit_test",
    topic            = "Edit test",
    study_id         = "study_abc",
    source_materials = list(),
    questions        = lapply(seq_len(n), function(i) list(
      id             = i,
      type           = "numeric",
      question       = paste0("Q", i, "?"),
      explanation    = "E.",
      correct_answer = as.numeric(i)
    )),
    model      = "test",
    created_at = "2026-01-01T00:00:00Z"
  )
  qset_dir <- file.path(session_path, "question_sets")
  dir.create(qset_dir, showWarnings = FALSE)
  jsonlite::write_json(qset, file.path(qset_dir, "edit_test.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  "edit_test"
}

test_that("remove_questions removes the correct questions", {
  withr::with_tempdir({
    make_saved_qset(".")
    msg <- remove_questions("edit_test", list(1L, 3L), ".")
    expect_match(msg, "Removed 2")
    data <- jsonlite::read_json(file.path("question_sets", "edit_test.json"),
                                simplifyVector = FALSE)
    expect_length(data$questions, 1L)
    expect_equal(data$questions[[1]]$id, 2L)
  })
})

test_that("remove_questions with unknown ID is a no-op", {
  withr::with_tempdir({
    make_saved_qset(".")
    msg <- remove_questions("edit_test", list(99L), ".")
    expect_match(msg, "Removed 0")
    data <- jsonlite::read_json(file.path("question_sets", "edit_test.json"),
                                simplifyVector = FALSE)
    expect_length(data$questions, 3L)
  })
})

test_that("remove_questions returns error for missing set", {
  withr::with_tempdir({
    dir.create("question_sets")
    msg <- remove_questions("no_such", list(1L), ".")
    expect_match(msg, "not found")
  })
})

# update_questions -------------------------------------------------------------

test_that("update_questions replaces matching questions", {
  withr::with_tempdir({
    make_saved_qset(".")
    replacement <- list(list(
      id             = 2L,
      type           = "numeric",
      question       = "Updated Q?",
      explanation    = "Updated E.",
      exhibit        = NULL,
      correct_answer = 9.9
    ))
    msg <- update_questions("edit_test", replacement, ".")
    expect_match(msg, "Updated 1")
    data <- jsonlite::read_json(file.path("question_sets", "edit_test.json"),
                                simplifyVector = FALSE)
    updated_q <- Filter(function(q) q$id == 2L, data$questions)[[1]]
    expect_equal(updated_q$question, "Updated Q?")
  })
})

test_that("update_questions returns error when replacement lacks id", {
  withr::with_tempdir({
    make_saved_qset(".")
    bad <- list(list(type = "numeric", question = "Q?", explanation = "E.",
                     correct_answer = 1.0))
    msg <- update_questions("edit_test", bad, ".")
    expect_match(msg, "missing 'id'")
  })
})

test_that("update_questions warns for unknown ID", {
  withr::with_tempdir({
    make_saved_qset(".")
    replacement <- list(list(
      id             = 99L,
      type           = "numeric",
      question       = "Ghost Q?",
      explanation    = "E.",
      exhibit        = NULL,
      correct_answer = 1.0
    ))
    msg <- update_questions("edit_test", replacement, ".")
    expect_match(msg, "Warning")
  })
})

# add_questions ----------------------------------------------------------------

test_that("add_questions appends with non-reusing IDs", {
  withr::with_tempdir({
    make_saved_qset(".", n = 3L)
    remove_questions("edit_test", list(1L), ".")
    new_q <- list(list(
      type           = "numeric",
      question       = "New Q?",
      explanation    = "New E.",
      exhibit        = NULL,
      correct_answer = 5.0
    ))
    msg <- add_questions("edit_test", new_q, ".")
    expect_match(msg, "Added 1")
    data <- jsonlite::read_json(file.path("question_sets", "edit_test.json"),
                                simplifyVector = FALSE)
    new_id <- data$questions[[length(data$questions)]]$id
    expect_true(new_id >= 4L)
  })
})

test_that("add_questions validates incoming questions", {
  withr::with_tempdir({
    make_saved_qset(".")
    bad <- list(list(type = "numeric", question = "Q?", explanation = "E."))
    msg <- add_questions("edit_test", bad, ".")
    expect_match(msg, "Error")
  })
})

test_that("add_questions returns error for missing set", {
  withr::with_tempdir({
    dir.create("question_sets")
    new_q <- list(list(type = "numeric", question = "Q?", explanation = "E.",
                       correct_answer = 1.0))
    msg <- add_questions("no_such", new_q, ".")
    expect_match(msg, "not found")
  })
})

test_that("null fields survive editing; old {} exhibits still load", {
  withr::with_tempdir({
    init_session("s")
    q <- list(type = "numeric", question = "Q?", exhibit = NULL, correct_answer = 1,
              explanation = "E.")
    save_question_set(list(id = "set1", topic = "t", study_id = "st", questions = list(q)), "s")
    r <- add_questions("set1", list(q), "s")
    r <- remove_questions("set1", 99L, "s")
    txt <- readLines("s/question_sets/set1.json")
    expect_false(any(grepl("{}", txt, fixed = TRUE)))
    expect_equal(sum(grepl('"exhibit": null', txt, fixed = TRUE)), 2L)
    # a set written by an older version
    writeLines(gsub('"exhibit": null', '"exhibit": {}', txt, fixed = TRUE), "s/question_sets/set1.json")
    out <- load_question_set("set1", "s")
    expect_match(out, "Question ID: 2")
    expect_false(grepl("Exhibit:", out, fixed = TRUE))
    expect_match(questions_to_markdown("set1", "s"), "## Question 2")
  })
})
