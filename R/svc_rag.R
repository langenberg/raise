MATERIAL_EXTS <- c("pdf", "md", "markdown", "txt")

.rag_cache <- new.env(parent = emptyenv())

.get_or_connect <- function(rag_dir, rag_id) {
  key <- .rag_key(rag_dir)
  if (exists(key, envir = .rag_cache, inherits = FALSE))
    return(get(key, envir = .rag_cache, inherits = FALSE))
  message("Connecting RAG store '", rag_id, "'...")
  store <- connect_rag_store(rag_dir)
  if (!is.null(store)) assign(key, store, envir = .rag_cache)
  store
}

# Cache key of a store: the resolved directory, which exists before the store
# file does (normalizePath() of a missing file can differ, e.g. /var vs /private/var).
.rag_key <- function(rag_dir) file.path(normalizePath(rag_dir, mustWork = FALSE), "store.duckdb")

# Close and forget a cached store connection (duckdb keeps the file open).
.release_rag_connection <- function(key) {
  if (!exists(key, envir = .rag_cache, inherits = FALSE)) return(invisible())
  store <- get(key, envir = .rag_cache, inherits = FALSE)
  try(DBI::dbDisconnect(store@con, shutdown = TRUE), silent = TRUE)
  rm(list = key, envir = .rag_cache)
  invisible()
}

make_file_manifest <- function(file_paths) {
  lapply(file_paths, function(fp) {
    info <- file.info(fp)
    list(
      filename   = basename(fp),
      size_bytes = as.integer(info$size),
      mtime      = format(info$mtime, "%Y-%m-%dT%H:%M:%S")
    )
  })
}

read_rag_meta <- function(rag_dir) {
  meta_path <- file.path(rag_dir, "meta.json")
  if (!file.exists(meta_path)) return(NULL)
  tryCatch(
    jsonlite::read_json(meta_path),
    error = function(e) NULL
  )
}

write_rag_meta <- function(rag_dir, meta) {
  jsonlite::write_json(meta, file.path(rag_dir, "meta.json"),
                       pretty = TRUE, auto_unbox = TRUE)
}

#' List all RAG stores in the session with their metadata.
#' @param session_path Path to the session folder.
#' @export
list_rags <- function(session_path) {
  rag_dir <- file.path(session_path, "rag")
  if (!dir.exists(rag_dir)) return(list())
  dirs <- sort(list.dirs(rag_dir, full.names = TRUE, recursive = FALSE))
  results <- list()
  for (d in dirs) {
    meta <- read_rag_meta(d)
    if (!is.null(meta)) results <- c(results, list(meta))
  }
  results
}

#' Delete one or more RAG stores from the session.
#' @param session_path Path to the session folder.
#' @param rag_ids Character vector of RAG store IDs to delete.
#' @export
delete_rags <- function(session_path, rag_ids) {
  msgs <- vapply(rag_ids, function(rag_id) {
    rag_path <- file.path(session_path, "rag", rag_id)
    if (!dir.exists(rag_path))
      return(paste0("RAG not found: ", rag_id))
    .release_rag_connection(.rag_key(rag_path))
    unlink(rag_path, recursive = TRUE)
    paste0("RAG '", rag_id, "' deleted.")
  }, character(1L))
  paste(msgs, collapse = "\n")
}

rag_sync_status <- function(rag_dir, mat_dir) {
  meta <- read_rag_meta(rag_dir)
  if (is.null(meta)) return("out_of_sync")
  if (!dir.exists(mat_dir)) return("out_of_sync")

  cur_files <- list.files(mat_dir, full.names = FALSE)
  cur_files <- cur_files[!startsWith(cur_files, ".")]
  cur_files <- cur_files[tolower(tools::file_ext(cur_files)) %in% MATERIAL_EXTS]

  manifest  <- if (is.null(meta$file_manifest)) list() else meta$file_manifest
  man_names <- vapply(manifest, `[[`, character(1L), "filename")

  if (!setequal(cur_files, man_names)) return("out_of_sync")

  for (entry in manifest) {
    cur_info  <- file.info(file.path(mat_dir, entry$filename))
    if (is.na(cur_info$size)) return("out_of_sync")
    cur_mtime <- format(cur_info$mtime, "%Y-%m-%dT%H:%M:%S")
    if (as.integer(cur_info$size) != as.integer(entry$size_bytes)) return("out_of_sync")
    if (cur_mtime != entry$mtime) return("out_of_sync")
  }

  "in_sync"
}

build_rag_store <- function(rag_dir, files, provider = raise_default_embed_provider,
                            settings = list(chunk_size = 500L, model = NULL)) {
  files <- files[tolower(tools::file_ext(files)) %in% MATERIAL_EXTS]
  if (length(files) == 0L) return(NULL)

  store_path <- file.path(rag_dir, "store.duckdb")
  store_key  <- .rag_key(rag_dir)

  .release_rag_connection(store_key)

  embed_fn <- embed_function(provider, settings$model)

  tryCatch({
    store <- ragnar::ragnar_store_create(location = store_path, embed = embed_fn,
                                         overwrite = TRUE)
    for (fpath in files) {
      text   <- load_material(fpath, max_chars = .Machine$integer.max)
      doc    <- ragnar::MarkdownDocument(text = text, origin = basename(fpath))
      chunks <- ragnar::markdown_chunk(doc, target_size = settings$chunk_size)
      ragnar::ragnar_store_insert(store, chunks)
    }
    if (is.null(embed_fn)) {
      ragnar::ragnar_store_build_index(store, type = "fts")
    } else {
      ragnar::ragnar_store_build_index(store)
    }
    # duckdb >= 2.0 refuses a read-only connection while the write instance
    # is alive, so close it and hand out (and cache) a read-only connection.
    DBI::dbDisconnect(store@con, shutdown = TRUE)
    store <- connect_rag_store(rag_dir)
    if (!is.null(store)) assign(store_key, store, envir = .rag_cache)
    store
  }, error = function(e) {
    message("build_rag_store: ", conditionMessage(e))
    NULL
  })
}

connect_rag_store <- function(rag_dir, attach_embed = TRUE) {
  store_path <- file.path(rag_dir, "store.duckdb")
  if (!file.exists(store_path)) return(NULL)
  tryCatch(
    ragnar::ragnar_store_connect(store_path),
    error = function(e) {
      message("connect_rag_store: ", conditionMessage(e))
      NULL
    }
  )
}

test_rag_connection <- function(provider, settings) {
  if (provider == "bm25") return(TRUE)
  tryCatch({
    embed_fn <- embed_function(provider, settings$model)
    embed_fn("test")
    TRUE
  }, error = function(e) FALSE)
}

retrieve_from_store <- function(store, query, n = 5L, provider) {
  results <- if (provider == "bm25") {
    ragnar::ragnar_retrieve_bm25(store, text = query, top_k = n)
  } else {
    ragnar::ragnar_retrieve(store, text = query, top_k = n)
  }
  if (nrow(results) == 0L) return("No relevant content found.")
  chunks <- vapply(seq_len(nrow(results)), function(i) {
    paste0("[Source: ", results$origin[[i]], "]\n", results$text[[i]])
  }, character(1L))
  paste(chunks, collapse = "\n\n---\n\n")
}

#' Retrieve relevant chunks from one or more RAG stores by query.
#' @param session_path Path to the session folder.
#' @param rag_ids Character vector of RAG store IDs to search.
#' @param query Search query string.
#' @param n Number of chunks to retrieve per store.
#' @export
retrieve_from_rags <- function(session_path, rag_ids, query, n = 5L) {
  if (length(rag_ids) == 0L) return("No RAG stores active.")
  mat_dir <- file.path(session_path, "materials")
  parts <- vapply(rag_ids, function(id) {
    rag_dir <- file.path(session_path, "rag", id)
    meta    <- read_rag_meta(rag_dir)
    if (is.null(meta)) return(paste0("RAG '", id, "' not found."))
    sync    <- rag_sync_status(rag_dir, mat_dir)
    prefix  <- if (identical(sync, "out_of_sync"))
      paste0("Warning: RAG '", id, "' is out of sync with materials/ \u2014 results may be stale.\n\n")
    else ""
    store   <- .get_or_connect(rag_dir, id)
    if (is.null(store)) return(paste0("Could not connect to RAG '", id, "'."))
    provider <- meta$embed_provider %||% "bm25"
    paste0(prefix, retrieve_from_store(store, query, n = n, provider = provider))
  }, character(1L))
  paste(parts, collapse = "\n\n===\n\n")
}

get_active_rag_ids <- function(session_path) {
  rags <- list_rags(session_path)
  ids  <- vapply(rags, `[[`, character(1L), "rag_id")
  ids[vapply(rags, function(r) isTRUE(r$active), logical(1L))]
}

#' Activate or deactivate a RAG store.
#' @param session_path Path to the session folder.
#' @param rag_id RAG store ID.
#' @param active Logical; TRUE to activate, FALSE to deactivate.
#' @export
set_rag_active <- function(session_path, rag_id, active) {
  rag_dir <- file.path(session_path, "rag", rag_id)
  meta    <- read_rag_meta(rag_dir)
  if (is.null(meta)) return(invisible(NULL))
  meta$active <- isTRUE(active)
  write_rag_meta(rag_dir, meta)
  invisible(NULL)
}
