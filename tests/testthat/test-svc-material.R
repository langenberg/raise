library(RAISE)

test_that("load_material reads a plain text file", {
  withr::with_tempdir({
    writeLines(c("Hello", "world"), "test.txt")
    result <- load_material("test.txt")
    expect_true(grepl("Hello", result))
    expect_true(grepl("world", result))
  })
})

test_that("load_material reads a markdown file", {
  withr::with_tempdir({
    writeLines(c("# Title", "Some content"), "notes.md")
    result <- load_material("notes.md")
    expect_true(grepl("Title", result))
  })
})

test_that("load_material reads a .markdown file", {
  withr::with_tempdir({
    writeLines("content here", "notes.markdown")
    result <- load_material("notes.markdown")
    expect_equal(result, "content here")
  })
})

test_that("load_material truncates to max_chars", {
  withr::with_tempdir({
    writeLines(paste(rep("x", 200), collapse = ""), "big.txt")
    result <- load_material("big.txt", max_chars = 50L)
    expect_lte(nchar(result), 50L)
  })
})

test_that("load_material errors on unsupported extension", {
  withr::with_tempdir({
    writeLines("data", "data.csv")
    expect_error(load_material("data.csv"), "unsupported file extension")
  })
})

test_that("load_material errors on missing file", {
  expect_error(load_material("/nonexistent/path/file.txt"), "file not found")
})
