library(RAISE)

make_rag_session <- function(path) {
  for (d in c("materials", "rag"))
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
}

write_text_material <- function(mat_dir, filename, content = "Hello world content for testing.") {
  writeLines(content, file.path(mat_dir, filename))
  file.path(mat_dir, filename)
}

# ── make_file_manifest ─────────────────────────────────────────────────────────

test_that("make_file_manifest records filename, size, mtime", {
  withr::with_tempdir({
    writeLines("some content", "test.txt")
    result <- make_file_manifest("test.txt")
    expect_length(result, 1L)
    entry <- result[[1L]]
    expect_equal(entry$filename, "test.txt")
    expect_true(is.integer(entry$size_bytes))
    expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}T", entry$mtime))
  })
})

# ── read_rag_meta / write_rag_meta ─────────────────────────────────────────────

test_that("read_rag_meta and write_rag_meta round-trip correctly", {
  withr::with_tempdir({
    dir.create("rag_dir")
    meta <- list(
      label          = "Test RAG",
      rag_id         = "bm25_20260509_143012",
      embed_provider = "bm25",
      embed_model    = NULL,
      chunk_size     = 500L,
      created_at     = "2026-05-09T14:30:12",
      updated_at     = "2026-05-09T14:30:12",
      file_manifest  = list(list(filename = "lecture.txt", size_bytes = 100L,
                                 mtime = "2026-05-09T10:00:00"))
    )
    write_rag_meta("rag_dir", meta)
    result <- read_rag_meta("rag_dir")
    expect_equal(result$label, "Test RAG")
    expect_equal(result$rag_id, "bm25_20260509_143012")
    expect_equal(result$embed_provider, "bm25")
    expect_equal(result$chunk_size, 500L)
    expect_length(result$file_manifest, 1L)
    expect_equal(result$file_manifest[[1L]]$filename, "lecture.txt")
  })
})

test_that("read_rag_meta returns NULL when meta.json is absent", {
  withr::with_tempdir({
    dir.create("rag_dir")
    expect_null(read_rag_meta("rag_dir"))
  })
})

# ── rag_sync_status ────────────────────────────────────────────────────────────

test_that("rag_sync_status returns in_sync when manifest matches current materials", {
  withr::with_tempdir({
    make_rag_session(".")
    mat_dir <- file.path(".", "materials")
    fpath   <- write_text_material(mat_dir, "lecture.txt")
    manifest <- make_file_manifest(fpath)
    dir.create("rag/bm25_test", recursive = TRUE)
    write_rag_meta("rag/bm25_test",
                   list(rag_id = "bm25_test", file_manifest = manifest))
    expect_equal(rag_sync_status("rag/bm25_test", mat_dir), "in_sync")
  })
})

test_that("rag_sync_status returns out_of_sync when a new file is added", {
  withr::with_tempdir({
    make_rag_session(".")
    mat_dir <- file.path(".", "materials")
    fpath   <- write_text_material(mat_dir, "lecture.txt")
    manifest <- make_file_manifest(fpath)
    dir.create("rag/bm25_test", recursive = TRUE)
    write_rag_meta("rag/bm25_test",
                   list(rag_id = "bm25_test", file_manifest = manifest))
    write_text_material(mat_dir, "notes.txt")
    expect_equal(rag_sync_status("rag/bm25_test", mat_dir), "out_of_sync")
  })
})

test_that("rag_sync_status returns out_of_sync when a file is deleted", {
  withr::with_tempdir({
    make_rag_session(".")
    mat_dir <- file.path(".", "materials")
    fpath1  <- write_text_material(mat_dir, "lecture.txt")
    fpath2  <- write_text_material(mat_dir, "notes.txt")
    manifest <- make_file_manifest(c(fpath1, fpath2))
    dir.create("rag/bm25_test", recursive = TRUE)
    write_rag_meta("rag/bm25_test",
                   list(rag_id = "bm25_test", file_manifest = manifest))
    file.remove(fpath2)
    expect_equal(rag_sync_status("rag/bm25_test", mat_dir), "out_of_sync")
  })
})

# ── list_rags / delete_rags ────────────────────────────────────────────────────

test_that("list_rags returns list() when rag/ folder is absent", {
  withr::with_tempdir({
    result <- list_rags(".")
    expect_length(result, 0L)
  })
})

test_that("list_rags returns all RAG metadata including label", {
  withr::with_tempdir({
    for (item in list(
      list(id = "bm25_20260509_100000", label = "RAG A"),
      list(id = "bm25_20260509_110000", label = "RAG B")
    )) {
      dir.create(file.path("rag", item$id), recursive = TRUE)
      write_rag_meta(file.path("rag", item$id),
                     list(label = item$label, rag_id = item$id, embed_provider = "bm25"))
    }
    result <- list_rags(".")
    expect_length(result, 2L)
    ids    <- vapply(result, `[[`, character(1L), "rag_id")
    labels <- vapply(result, `[[`, character(1L), "label")
    expect_true("bm25_20260509_100000" %in% ids)
    expect_true("bm25_20260509_110000" %in% ids)
    expect_true("RAG A" %in% labels)
    expect_true("RAG B" %in% labels)
  })
})

test_that("delete_rags removes the folder", {
  withr::with_tempdir({
    dir.create("rag/bm25_test", recursive = TRUE)
    write_rag_meta("rag/bm25_test", list(rag_id = "bm25_test"))
    msg <- delete_rags(".", "bm25_test")
    expect_false(dir.exists("rag/bm25_test"))
    expect_match(msg, "deleted")
  })
})

test_that("delete_rags returns error message for unknown id", {
  withr::with_tempdir({
    dir.create("rag", showWarnings = FALSE)
    msg <- delete_rags(".", "ghost_id")
    expect_match(msg, "not found")
  })
})

test_that("delete_rags deletes multiple stores and reports each", {
  withr::with_tempdir({
    for (id in c("bm25_a", "bm25_b")) {
      dir.create(file.path("rag", id), recursive = TRUE)
      write_rag_meta(file.path("rag", id), list(rag_id = id))
    }
    msg <- delete_rags(".", c("bm25_a", "bm25_b"))
    expect_false(dir.exists("rag/bm25_a"))
    expect_false(dir.exists("rag/bm25_b"))
    expect_match(msg, "bm25_a")
    expect_match(msg, "bm25_b")
  })
})

# ── build_rag_store ────────────────────────────────────────────────────────────

test_that("build_rag_store returns NULL for empty file list", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    settings <- list(chunk_size = 200L, model = NULL)
    result <- build_rag_store("rag_dir", character(0L), "bm25", settings)
    expect_null(result)
  })
})

test_that("build_rag_store skips non-material files", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines("content", "spreadsheet.xlsx")
    settings <- list(chunk_size = 200L, model = NULL)
    result <- build_rag_store("rag_dir", "spreadsheet.xlsx", "bm25", settings)
    expect_null(result)
  })
})

test_that("build_rag_store creates store.duckdb from a text file", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    result <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    expect_false(is.null(result))
    expect_true(file.exists(file.path("rag_dir", "store.duckdb")))
  })
})

test_that("build_rag_store deletes old store before rebuild", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    local({
      build_rag_store("rag_dir", "material.txt", "bm25", settings)
    })
    gc()  # close the DuckDB connection from the first build
    mtime1 <- file.info(file.path("rag_dir", "store.duckdb"))$mtime
    Sys.sleep(1)
    result2 <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    mtime2 <- file.info(file.path("rag_dir", "store.duckdb"))$mtime
    expect_false(is.null(result2))
    expect_gt(as.numeric(mtime2), as.numeric(mtime1))
  })
})

# ── connect_rag_store ──────────────────────────────────────────────────────────

test_that("connect_rag_store returns NULL when store.duckdb is absent", {
  withr::with_tempdir({
    dir.create("rag_dir")
    expect_null(connect_rag_store("rag_dir"))
  })
})

# ── retrieve_from_store / retrieve_from_rags ───────────────────────────────────

test_that("retrieve_from_store returns no-results message for unmatched query", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    store <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    expect_false(is.null(store))
    result <- retrieve_from_store(store, "xyznonexistentterm98765", n = 5L, provider = "bm25")
    expect_equal(result, "No relevant content found.")
  })
})

test_that("retrieve_from_store returns chunks for a matching BM25 query", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    store  <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    result <- retrieve_from_store(store, "regression", n = 3L, provider = "bm25")
    expect_false(identical(result, "No relevant content found."))
    expect_true(nchar(result) > 0L)
  })
})

test_that("retrieve_from_store labels chunks with [Source: filename]", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    store  <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    result <- retrieve_from_store(store, "regression", n = 3L, provider = "bm25")
    expect_true(grepl("[Source: material.txt]", result, fixed = TRUE))
  })
})

test_that("retrieve_from_rags merges results from two stores with === separator", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    make_rag_session(".")
    dir.create("rag/r1", recursive = TRUE)
    dir.create("rag/r2", recursive = TRUE)
    settings <- list(chunk_size = 200L, model = NULL)
    writeLines(rep("Regression analysis content alpha.", 20L), "materials/alpha.txt")
    writeLines(rep("Regression analysis content beta.",  20L), "materials/beta.txt")
    build_rag_store("rag/r1", "materials/alpha.txt", "bm25", settings)
    build_rag_store("rag/r2", "materials/beta.txt",  "bm25", settings)
    write_rag_meta("rag/r1", list(rag_id = "r1", embed_provider = "bm25", active = TRUE))
    write_rag_meta("rag/r2", list(rag_id = "r2", embed_provider = "bm25", active = TRUE))
    result <- retrieve_from_rags(".", c("r1", "r2"), "regression", n = 2L)
    expect_true(grepl("===", result, fixed = TRUE))
  })
})

test_that("retrieve_from_rags returns no-active-stores message for empty rag_ids", {
  withr::with_tempdir({
    result <- retrieve_from_rags(".", character(0L), "any query")
    expect_equal(result, "No RAG stores active.")
  })
})

test_that("retrieve_from_rags returns not-found message for missing rag_id", {
  withr::with_tempdir({
    make_rag_session(".")
    result <- retrieve_from_rags(".", "ghost_rag", "any query")
    expect_match(result, "not found")
  })
})

test_that("retrieve_from_rags prepends warning prefix for out-of-sync store", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    make_rag_session(".")
    dir.create("rag/r1", recursive = TRUE)
    settings <- list(chunk_size = 200L, model = NULL)
    writeLines(rep("Regression analysis content alpha.", 20L), "materials/alpha.txt")
    build_rag_store("rag/r1", "materials/alpha.txt", "bm25", settings)
    write_rag_meta("rag/r1", list(rag_id = "r1", embed_provider = "bm25",
                                  active = TRUE, file_manifest = list()))
    result <- retrieve_from_rags(".", "r1", "regression", n = 2L)
    expect_true(grepl("out of sync", result, fixed = TRUE))
    expect_true(grepl("Warning:", result, fixed = TRUE))
  })
})

# ── .get_or_connect cache ──────────────────────────────────────────────────────

test_that(".get_or_connect auto-connects on first call and caches result", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    build_rag_store("rag_dir", "material.txt", "bm25", settings)
    key <- file.path(getwd(), "rag_dir", "store.duckdb")
    rm(list = intersect(key, ls(RAISE:::.rag_cache)), envir = RAISE:::.rag_cache)
    store1 <- .get_or_connect("rag_dir", "test_rag")
    expect_false(is.null(store1))
    store2 <- .get_or_connect("rag_dir", "test_rag")
    expect_identical(store1, store2)
  })
})

test_that("build_rag_store replaces the cached connection", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag_dir")
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    settings <- list(chunk_size = 200L, model = NULL)
    store1 <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    key <- RAISE:::.rag_key("rag_dir")
    expect_identical(get(key, envir = RAISE:::.rag_cache), store1)
    store2 <- build_rag_store("rag_dir", "material.txt", "bm25", settings)
    expect_false(is.null(store2))
    expect_identical(get(key, envir = RAISE:::.rag_cache), store2)
    expect_s3_class(ragnar::ragnar_retrieve_bm25(store2, "regression", top_k = 1L),
                    "data.frame")
  })
})

test_that("delete_rags closes the cached connection", {
  skip_if_not_installed("ragnar")
  withr::with_tempdir({
    dir.create("rag/r1", recursive = TRUE)
    writeLines(rep("Hello world content for regression testing.", 20L), "material.txt")
    build_rag_store("rag/r1", "material.txt", "bm25", list(chunk_size = 200L, model = NULL))
    key <- RAISE:::.rag_key("rag/r1")
    expect_true(exists(key, envir = RAISE:::.rag_cache, inherits = FALSE))
    delete_rags(".", "r1")
    expect_false(exists(key, envir = RAISE:::.rag_cache, inherits = FALSE))
    expect_false(dir.exists("rag/r1"))
  })
})

# ── get_active_rag_ids / set_rag_active ───────────────────────────────────────

test_that("get_active_rag_ids returns only active stores", {
  withr::with_tempdir({
    for (item in list(
      list(id = "r1", active = TRUE),
      list(id = "r2", active = FALSE),
      list(id = "r3", active = TRUE)
    )) {
      dir.create(file.path("rag", item$id), recursive = TRUE)
      write_rag_meta(file.path("rag", item$id),
                     list(rag_id = item$id, embed_provider = "bm25",
                          active = item$active))
    }
    ids <- get_active_rag_ids(".")
    expect_setequal(ids, c("r1", "r3"))
  })
})

test_that("get_active_rag_ids returns empty vector when no stores are active", {
  withr::with_tempdir({
    dir.create("rag/r1", recursive = TRUE)
    write_rag_meta("rag/r1", list(rag_id = "r1", embed_provider = "bm25", active = FALSE))
    ids <- get_active_rag_ids(".")
    expect_length(ids, 0L)
  })
})

test_that("set_rag_active round-trips active flag through meta.json", {
  withr::with_tempdir({
    dir.create("rag/r1", recursive = TRUE)
    write_rag_meta("rag/r1", list(rag_id = "r1", embed_provider = "bm25", active = FALSE))
    set_rag_active(".", "r1", TRUE)
    meta <- read_rag_meta("rag/r1")
    expect_true(isTRUE(meta$active))
    set_rag_active(".", "r1", FALSE)
    meta <- read_rag_meta("rag/r1")
    expect_false(isTRUE(meta$active))
  })
})

test_that("set_rag_active returns NULL invisibly for missing rag_id", {
  withr::with_tempdir({
    dir.create("rag", showWarnings = FALSE)
    result <- set_rag_active(".", "ghost", TRUE)
    expect_null(result)
  })
})
