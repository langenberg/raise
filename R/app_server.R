# -- RAG create / update modal -------------------------------------------------

.rag_backend_choices <- function(extra = character(0)) {
  ch <- c("BM25 keyword (no setup)" = "bm25",
          "Ollama local embeddings" = "ollama",
          "OpenAI embeddings"       = "openai")
  extra <- setdiff(extra, ch)
  if (length(extra)) ch <- c(ch, stats::setNames(extra, paste(extra, "embeddings")))
  ch
}

.show_rag_create_update_modal <- function(mode, avail_files,
                                           selected_files = avail_files,
                                           deleted_files = character(0),
                                           provider = raise_default_embed_provider,
                                           label = "") {
  base_ui <- if (length(avail_files) == 0L) {
    shiny::p("No materials available. Upload files first.",
             style = "color: grey; font-size: 0.9em;")
  } else {
    shiny::checkboxGroupInput(
      "rag_files", "Select materials",
      choices  = avail_files,
      selected = selected_files
    )
  }
  file_ui <- if (length(deleted_files) == 0L) {
    base_ui
  } else {
    deleted_rows <- lapply(deleted_files, function(f) {
      shiny::tags$div(
        style = "display:flex;align-items:flex-start;gap:6px;margin-bottom:4px;opacity:0.6;",
        shiny::tags$input(type = "checkbox", checked = NA, disabled = NA,
                          style = "margin-top:3px;flex-shrink:0;"),
        shiny::tags$div(
          shiny::tags$span(f, style = "color:#666;"),
          shiny::tags$div(
            style = "font-size:0.82em;color:#888;font-style:italic;",
            "File no longer found -- will not be included on update"
          )
        )
      )
    })
    shiny::tagList(
      base_ui,
      shiny::tags$div(style = "margin-top:6px;", do.call(shiny::tagList, deleted_rows))
    )
  }
  shiny::showModal(shiny::modalDialog(
    title = if (identical(mode, "create")) "Create RAG Store" else "Update RAG Store",
    shiny::textInput("rag_label", "Label", value = label,
                     placeholder = "e.g. Lecture Notes BM25"),
    file_ui,
    shiny::radioButtons(
      "rag_provider", "Backend",
      choices  = .rag_backend_choices(provider),
      selected = provider
    ),
    shiny::uiOutput("rag_settings_ui"),
    footer    = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        "rag_modal_confirm",
        if (identical(mode, "create")) "Create" else "Update",
        class = "btn-primary"
      )
    ),
    size      = "m",
    easyClose = TRUE
  ))
}

# -- Export modal --------------------------------------------------------------

.show_export_modal <- function(qset_files) {
  cards <- if (length(qset_files) == 0L) {
    list(shiny::p("No question sets available.",
                  style = "color: grey; font-size: 0.9em;"))
  } else {
    lapply(qset_files, function(f) {
      data    <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
      qset_id <- tools::file_path_sans_ext(basename(f))
      topic   <- if (!is.null(data)) data$topic %||% qset_id else qset_id
      nq      <- if (!is.null(data)) length(data$questions %||% list()) else 0L
      badges  <- if (!is.null(data) && nq > 0L) {
        shiny::tagList(lapply(data$questions, function(q) {
          type <- q$type %||% ""
          if (identical(type, "multiple_choice")) {
            shiny::tags$span("MC",
              style = paste0("display:inline-block;padding:1px 5px;border-radius:8px;",
                             "font-size:0.75em;margin:1px;background:#cce5ff;color:#004085;"))
          } else {
            shiny::tags$span("#",
              style = paste0("display:inline-block;padding:1px 5px;border-radius:8px;",
                             "font-size:0.75em;margin:1px;background:#e2e3e5;color:#383d41;"))
          }
        }))
      } else NULL

      shiny::tags$div(
        style = paste0("border:1px solid #ddd;border-radius:4px;padding:6px 8px;",
                       "margin-bottom:6px;display:flex;align-items:flex-start;gap:6px;"),
        shiny::tagAppendAttributes(
          shiny::checkboxInput(
            paste0("export_modal_sel_", qset_id), label = NULL,
            value = FALSE, width = "20px"
          ),
          style = "margin-bottom:0;"
        ),
        shiny::tags$div(
          shiny::strong(topic),
          shiny::tags$span(paste0(" (", nq, " Qs)"),
                           style = "font-size:0.85em;color:#666;"),
          shiny::tags$div(style = "margin-top:4px;", badges)
        )
      )
    })
  }

  shiny::showModal(shiny::modalDialog(
    title = "Export Question Sets",
    shiny::tagList(
      cards,
      shiny::tags$hr(),
      shiny::tags$div(
        style = "display:flex;gap:6px;align-items:center;margin-top:4px;",
        shiny::tagAppendAttributes(
          shiny::textInput("export_modal_label", label = NULL,
                           value = "", placeholder = "Export label (e.g. Midterm Exam 2026)", width = "100%"),
          style = "flex:1;margin-bottom:0;"
        ),
        shiny::tagAppendAttributes(
          shiny::selectInput("export_modal_destination", label = NULL,
                             choices  = c("PDF" = "pdf", "TestVision" = "testvision",
                                          "Canvas" = "canvas", "Markdown" = "markdown",
                                          "JSON" = "json"),
                             selected = "pdf", width = "100%"),
          style = "flex:1;margin-bottom:0;"
        )
      ),
      shiny::tags$div(
        style = "margin-top:4px;display:flex;flex-direction:column;gap:2px;",
        shiny::tags$label("Decimal places for numeric Questions",
                          style = "margin-bottom:0;font-weight:normal;"),
        shiny::tagAppendAttributes(
          shiny::selectInput("export_modal_precision", label = NULL,
                             choices  = c("Fixed 1 dp" = "fixed_1", "Fixed 2 dp" = "fixed_2",
                                          "Fixed 3 dp" = "fixed_3", "Auto" = "auto"),
                             selected = "fixed_2", width = "100%"),
          style = "margin-bottom:0;"
        ),
        shiny::tagAppendAttributes(
          shiny::checkboxInput("export_modal_show_note", "Show precision note", value = TRUE,
                               width = "100%"),
          style = "margin-bottom:0;"
        ),
        shiny::tagAppendAttributes(
          shiny::checkboxInput("export_modal_answers",
                               "Include answers and explanations (Canvas and TestVision always include them)",
                               value = TRUE, width = "100%"),
          style = "margin-bottom:0;"
        )
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("export_modal_confirm", "Export", class = "btn-primary")
    ),
    size      = "m",
    easyClose = TRUE
  ))
}

# -- Study modal content builder -----------------------------------------------
# Handles two schema variants:
#   v2 monolithic  : $family, $n_subjects/$n_reps, $variables$between/$within,
#                    correlations as [{vars,value}], coefficients as [{vars,value}]
#   step-by-step   : $regression_type, $n_obs, $variables$independent,
#                    correlations as matrix of rows, coefficients as named list
# Both schemas may carry $random_effects as [{vars,value}] and share $context.

# Note under the correlation table: for binary/categorical variables, r is the
# correlation with the latent normal score that simulate.R cuts into categories
# (in key order), so it is not the correlation with the dummy codes.
.latent_corr_note <- function(design, corr) {
  vars <- c(design$variables$between %||% list(), design$variables$within %||% list())
  used <- unique(unlist(lapply(corr, function(e) unlist(e$vars))))
  cats <- Filter(function(v) isTRUE(v$type %in% c("binary", "categorical")) &&
                   isTRUE(v$name %in% used), vars)
  if (length(cats) == 0L) return(NULL)
  orders <- vapply(cats, function(v) {
    keys <- sort(as.integer(names(v$categories)))
    paste0(v$name, ": ", paste(vapply(as.character(keys), function(k)
      as.character(v$categories[[k]]), ""), collapse = " < "))
  }, "")
  shiny::tags$p(
    style = "font-size:0.8em;color:#666;margin-top:-4px;",
    "For binary and categorical variables, r is the correlation with a hidden normal ",
    "score that the simulation cuts into the categories, from low to high: ",
    paste0(paste(orders, collapse = "; "), "."),
    " A positive r means higher values go with the later categories. ",
    "The correlation in the simulated data is somewhat weaker."
  )
}

.build_study_modal_content <- function(design, study_dir) {
  status <- design$status %||% "unknown"
  is_v2  <- !is.null(design$family)   # v2 uses $family; step-by-step uses $regression_type
  parts  <- list()

  # ---- Description ----

  status_style <- switch(status,
    "simulated"          = "background:#d4edda;color:#155724;",
    "design_saved"       = ,
    "random_effects_set" = ,
    "coefficients_set"   = ,
    "correlations_set"   = "background:#cce5ff;color:#004085;",
    "description_saved"  = "background:#fff3cd;color:#856404;",
    "background:#e2e3e5;color:#383d41;"
  )

  desc_rows <- list(
    shiny::tags$tr(
      shiny::tags$th("Status", style = "width:150px;"),
      shiny::tags$td(shiny::tags$span(
        status,
        style = paste0("padding:2px 8px;border-radius:4px;font-size:0.85em;", status_style)
      ))
    ),
    shiny::tags$tr(
      shiny::tags$th("Family"),
      shiny::tags$td(design$family %||% design$regression_type %||% "")
    )
  )

  if (is_v2) {
    desc_rows <- c(desc_rows, list(
      shiny::tags$tr(shiny::tags$th("N subjects"),
                     shiny::tags$td(as.character(design$n_subjects %||% ""))),
      shiny::tags$tr(shiny::tags$th("N reps"),
                     shiny::tags$td(as.character(design$n_reps %||% ""))),
      shiny::tags$tr(shiny::tags$th("Subject ID"),
                     shiny::tags$td(design$subject_id_name %||% ""))
    ))
  } else {
    desc_rows <- c(desc_rows, list(
      shiny::tags$tr(shiny::tags$th("N obs"),
                     shiny::tags$td(as.character(design$n_obs %||% "")))
    ))
  }

  desc_rows <- c(desc_rows, list(
    shiny::tags$tr(shiny::tags$th("Context"), shiny::tags$td(md_html(design$context %||% "")))
  ))

  parts <- c(parts, list(
    shiny::h6("Description", style = "font-weight:bold;margin-top:0;"),
    shiny::tags$table(
      class = "table table-sm table-condensed", style = "font-size:0.9em;",
      do.call(shiny::tags$tbody, desc_rows)
    )
  ))

  # ---- Variables ----

  vars <- design$variables
  if (!is.null(vars)) {
    dep_v <- list(
      name  = vars$dependent$name  %||% "",
      label = vars$dependent$label %||% "",
      type  = vars$dependent$type  %||% ""
    )

    # Flatten to a list of (spec + .role) for uniform rendering
    all_preds <- list()
    if (is_v2) {
      for (v in vars$between %||% list()) all_preds <- c(all_preds, list(c(v, list(.role = "between"))))
      for (v in vars$within  %||% list()) all_preds <- c(all_preds, list(c(v, list(.role = "within"))))
    } else {
      for (v in vars$independent %||% list()) all_preds <- c(all_preds, list(c(v, list(.role = "predictor"))))
    }

    role_style <- function(role) switch(role,
      "between"   = "background:#cce5ff;color:#004085;",
      "within"    = "background:#e2ccff;color:#4b0082;",
      "predictor" = "background:#cce5ff;color:#004085;",
      "background:#e2e3e5;color:#383d41;"
    )

    .dist_text <- function(v) {
      if (!is.null(v$distribution))
        return(paste0("\u03BC=", v$distribution$mean, ", \u03C3=", v$distribution$sd))
      props <- v$proportion %||% v$proportions
      cats  <- v$categories
      if (!is.null(props)) {
        labels <- if (!is.null(cats))
          vapply(names(props), function(k) cats[[k]] %||% k, character(1))
        else names(props)
        return(paste(labels, "=", round(unlist(props), 2), collapse = ", "))
      }
      asn <- as.character(v$assignment %||% "")
      if (nchar(asn) > 0L) return(paste0("assignment: ", asn))
      ""
    }

    var_rows <- list(shiny::tags$tr(
      shiny::tags$td(shiny::tags$strong(dep_v$name)),
      shiny::tags$td(dep_v$label),
      shiny::tags$td(shiny::tags$span("dependent",
        style = "background:#d4edda;color:#155724;padding:1px 5px;border-radius:4px;font-size:0.85em;")),
      shiny::tags$td(dep_v$type),
      shiny::tags$td("")
    ))

    for (v in all_preds) {
      role <- v$.role %||% "predictor"
      var_rows <- c(var_rows, list(shiny::tags$tr(
        shiny::tags$td(shiny::tags$strong(v$name %||% "")),
        shiny::tags$td(v$label %||% ""),
        shiny::tags$td(shiny::tags$span(role,
          style = paste0(role_style(role), "padding:1px 5px;border-radius:4px;font-size:0.85em;"))),
        shiny::tags$td(v$type %||% ""),
        shiny::tags$td(.dist_text(v), style = "font-size:0.85em;color:#666;")
      )))
    }

    parts <- c(parts, list(
      shiny::tags$hr(style = "margin:8px 0;"),
      shiny::h6("Variables", style = "font-weight:bold;"),
      shiny::tags$table(
        class = "table table-sm table-condensed", style = "font-size:0.9em;",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Name"), shiny::tags$th("Label"), shiny::tags$th("Role"),
          shiny::tags$th("Type"), shiny::tags$th("Distribution")
        )),
        do.call(shiny::tags$tbody, var_rows)
      )
    ))
  }

  # ---- Correlations ----
  # v2: list of {vars: ["A","B"], value: r}
  # step-by-step: matrix as list of numeric rows

  corr <- design$correlations
  if (!is.null(corr) && length(corr) > 0L) {
    if (!is.null(corr[[1L]]$vars)) {
      # v2 {vars, value} pairs
      corr_rows <- lapply(corr, function(entry) {
        val <- as.numeric(entry$value)
        shiny::tags$tr(
          shiny::tags$td(as.character(entry$vars[[1L]])),
          shiny::tags$td(as.character(entry$vars[[2L]])),
          shiny::tags$td(
            style = paste0("text-align:right;",
                           if (abs(val) >= 0.5) "font-weight:bold;color:#155724;" else ""),
            sprintf("%.2f", val)
          )
        )
      })
      parts <- c(parts, list(
        shiny::tags$hr(style = "margin:8px 0;"),
        shiny::h6("Correlations", style = "font-weight:bold;"),
        shiny::tags$table(
          class = "table table-sm table-condensed", style = "font-size:0.9em;width:auto;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Variable A"), shiny::tags$th("Variable B"),
            shiny::tags$th(style = "text-align:right;", "r")
          )),
          do.call(shiny::tags$tbody, corr_rows)
        ),
        .latent_corr_note(design, corr)
      ))
    } else {
      # step-by-step matrix of rows
      n          <- length(corr)
      pred_names <- if (!is.null(vars))
        vapply(vars$independent %||% list(), function(v) v$name %||% "", character(1))
      else character(0L)
      lbl <- function(i) if (i <= length(pred_names)) pred_names[[i]] else as.character(i)
      header_cells <- c(list(shiny::tags$th("")), lapply(seq_len(n), function(i) shiny::tags$th(lbl(i))))
      corr_rows <- lapply(seq_len(n), function(i) {
        cells <- c(list(shiny::tags$th(lbl(i))), lapply(seq_len(n), function(j) {
          val <- corr[[i]][[j]]
          shiny::tags$td(
            style = if (i == j) "color:#aaa;"
                    else if (abs(val) >= 0.5) "color:#155724;font-weight:bold;"
                    else "",
            sprintf("%.2f", val)
          )
        }))
        do.call(shiny::tags$tr, cells)
      })
      parts <- c(parts, list(
        shiny::tags$hr(style = "margin:8px 0;"),
        shiny::h6("Correlations", style = "font-weight:bold;"),
        shiny::tags$div(style = "overflow-x:auto;", shiny::tags$table(
          class = "table table-sm table-condensed", style = "font-size:0.85em;width:auto;",
          shiny::tags$thead(do.call(shiny::tags$tr, header_cells)),
          do.call(shiny::tags$tbody, corr_rows)
        ))
      ))
    }
  }

  # ---- Coefficients ----
  # v2: list of {vars: [{name, level?},...], value}
  # step-by-step: named list of numbers

  coef <- design$coefficients
  if (!is.null(coef) && length(coef) > 0L) {
    .vars_label <- function(vars_list) {
      parts_lbl <- vapply(vars_list, function(ref) {
        nm <- ref$name %||% ""
        lv <- as.character(ref$level %||% "")
        if (nchar(lv) > 0L) paste0(nm, "[", lv, "]") else nm
      }, character(1))
      paste(parts_lbl, collapse = " \u00D7 ")
    }

    if (is.list(coef[[1L]]) && !is.null(coef[[1L]]$vars)) {
      coef_rows <- lapply(coef, function(entry) shiny::tags$tr(
        shiny::tags$td(.vars_label(entry$vars)),
        shiny::tags$td(style = "text-align:right;", as.character(entry$value))
      ))
    } else {
      coef_rows <- lapply(names(coef), function(nm) shiny::tags$tr(
        shiny::tags$td(nm),
        shiny::tags$td(style = "text-align:right;", as.character(coef[[nm]]))
      ))
    }

    parts <- c(parts, list(
      shiny::tags$hr(style = "margin:8px 0;"),
      shiny::h6("Coefficients", style = "font-weight:bold;"),
      shiny::tags$table(
        class = "table table-sm table-condensed", style = "font-size:0.9em;width:auto;",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Parameter"), shiny::tags$th(style = "text-align:right;", "Value")
        )),
        do.call(shiny::tags$tbody, coef_rows)
      )
    ))
  }

  # ---- Random Effects ----
  # Dict format: {type, variance?, covariance?, components?}

  re <- design$random_effects
  if (!is.null(re) && !is.null(re$type)) {
    .re_label <- function(entry) {
      refs <- vapply(entry$vars, function(ref) {
        nm <- ref$name %||% ""
        lv <- as.character(ref$level %||% "")
        if (nchar(lv) > 0L) paste0(nm, "[", lv, "]") else nm
      }, character(1))
      if (length(entry$vars) == 1L) paste0("Var(", refs, ")")
      else                          paste0("Cov(", paste(refs, collapse = ", "), ")")
    }

    meta_rows <- list(
      shiny::tags$tr(shiny::tags$td(shiny::tags$strong("Type")),
                     shiny::tags$td(as.character(re$type)))
    )
    if (!is.null(re$variance))
      meta_rows <- c(meta_rows, list(shiny::tags$tr(
        shiny::tags$td("Variance"),
        shiny::tags$td(style = "text-align:right;", as.character(re$variance))
      )))
    if (!is.null(re$covariance))
      meta_rows <- c(meta_rows, list(shiny::tags$tr(
        shiny::tags$td("Covariance"),
        shiny::tags$td(style = "text-align:right;", as.character(re$covariance))
      )))
    for (entry in re$components %||% list())
      meta_rows <- c(meta_rows, list(shiny::tags$tr(
        shiny::tags$td(.re_label(entry), style = "padding-left:1.5em;color:#555;"),
        shiny::tags$td(style = "text-align:right;", as.character(entry$value))
      )))
    parts <- c(parts, list(
      shiny::tags$hr(style = "margin:8px 0;"),
      shiny::h6("Random Effects", style = "font-weight:bold;"),
      shiny::tags$table(
        class = "table table-sm table-condensed", style = "font-size:0.9em;width:auto;",
        do.call(shiny::tags$tbody, meta_rows)
      )
    ))
  }

  # ---- Regression Output ----

  reg_file <- file.path(study_dir, "regression.txt")
  if (identical(status, "simulated") && file.exists(reg_file)) {
    reg_text <- paste(readLines(reg_file, warn = FALSE), collapse = "\n")
    parts <- c(parts, list(
      shiny::tags$hr(style = "margin:8px 0;"),
      shiny::h6("Regression Output", style = "font-weight:bold;"),
      shiny::tags$pre(
        style = "font-size:0.78em;background:#f8f9fa;padding:8px;border-radius:4px;overflow-x:auto;max-height:260px;",
        reg_text
      )
    ))
    or_file <- file.path(study_dir, "odds_ratios.txt")
    if (file.exists(or_file)) {
      or_text <- paste(readLines(or_file, warn = FALSE), collapse = "\n")
      parts <- c(parts, list(
        shiny::h6("Odds Ratios", style = "font-weight:bold;margin-top:8px;"),
        shiny::tags$pre(
          style = "font-size:0.78em;background:#f8f9fa;padding:8px;border-radius:4px;overflow-x:auto;",
          or_text
        )
      ))
    }
  } else {
    parts <- c(parts, list(
      shiny::tags$hr(style = "margin:8px 0;"),
      shiny::tags$div(
        class = "alert alert-info", style = "font-size:0.9em;margin-bottom:0;",
        shiny::tags$strong("Simulation not run yet."),
        " Regression output will appear here once the study has been simulated."
      )
    ))
  }

  do.call(shiny::tagList, parts)
}

# -- Export modal content builder -----------------------------------------------

.build_export_modal_content <- function(meta, artifact, questions = list()) {
  art_ext <- tools::file_ext(artifact)
  shiny::tagList(
    shiny::tags$table(
      class = "table table-sm table-condensed",
      style = "font-size: 0.9em;",
      shiny::tags$tbody(
        shiny::tags$tr(
          shiny::tags$th("Destination", style = "width: 150px;"),
          shiny::tags$td(meta$destination %||% art_ext)
        ),
        shiny::tags$tr(
          shiny::tags$th("Questions"),
          shiny::tags$td(as.character(meta$n_questions %||% "?"))
        ),
        shiny::tags$tr(
          shiny::tags$th("Answers"),
          shiny::tags$td(if (isFALSE(meta$answers)) "Not included (student version)" else "Included")
        ),
        shiny::tags$tr(
          shiny::tags$th("Created"),
          shiny::tags$td(meta$created_at %||% "")
        ),
        shiny::tags$tr(
          shiny::tags$th("File"),
          shiny::tags$td(basename(artifact))
        )
      )
    ),
    .build_questions_ui(questions)
  )
}

# -- Shared question list renderer ---------------------------------------------

.build_questions_ui <- function(questions) {
  if (length(questions) == 0L)
    return(shiny::p("(no questions)", style = "color: grey;"))

  parts <- list(shiny::tags$hr(style = "margin:8px 0;"))

  for (i in seq_along(questions)) {
    q    <- questions[[i]]
    type <- q$type %||% ""

    if (identical(type, "multiple_choice")) {
      type_badge <- shiny::tags$span(
        "MC",
        style = "background:#cce5ff;color:#004085;padding:1px 6px;border-radius:8px;font-size:0.8em;margin-right:4px;"
      )
    } else {
      type_badge <- shiny::tags$span(
        "#",
        style = "background:#e2e3e5;color:#383d41;padding:1px 6px;border-radius:8px;font-size:0.8em;margin-right:4px;"
      )
    }

    q_parts <- list(
      shiny::tags$div(
        style = "font-size: 0.9em; font-weight: bold; margin-bottom: 4px;",
        type_badge,
        paste0("Q", i, if (!is.null(q$id)) paste0(" (id ", q$id, ")") else "")
      ),
      shiny::div(class = "raise-md", md_html(q$question %||% ""))
    )

    if (is.character(q$exhibit) && nzchar(q$exhibit)) {
      q_parts <- c(q_parts, list(
        shiny::tags$div(
          style = "background:#f8f9fa;border-left:3px solid #adb5bd;padding:4px 8px;font-size:0.85em;margin-bottom:4px;",
          shiny::tags$p(style = "font-style:italic;margin:0 0 4px;", "Exhibit"),
          shiny::div(class = "raise-md", md_html(q$exhibit))
        )
      ))
    }

    if (identical(type, "multiple_choice")) {
      choice_rows <- lapply(q$choices %||% list(), function(ch) {
        correct <- isTRUE(ch$correct)
        shiny::tags$div(
          style = if (correct)
            "display:flex;align-items:baseline;gap:4px;background:#d4edda;border-radius:3px;padding:2px 6px;margin-bottom:2px;font-size:0.88em;"
          else
            "display:flex;align-items:baseline;gap:4px;padding:2px 6px;margin-bottom:2px;font-size:0.88em;",
          shiny::tags$span(if (correct) "\u2714" else "\u25CB"),
          shiny::div(class = "raise-choice-md", md_html(ch$choice %||% ""))
        )
      })
      q_parts <- c(q_parts, choice_rows)
    } else if (identical(type, "numeric")) {
      q_parts <- c(q_parts, list(
        shiny::tags$div(
          style = "font-size: 0.88em; padding: 2px 6px;",
          shiny::tags$strong("Answer: "),
          as.character(q$correct_answer)
        )
      ))
    }

    q_parts <- c(q_parts, list(
      shiny::tags$div(
        style = "font-size: 0.85em; color: #555; margin-top: 4px;",
        shiny::tags$p(style = "font-style:italic;margin:0 0 2px;", "Explanation"),
        shiny::div(class = "raise-md", md_html(q$explanation %||% ""))
      )
    ))

    parts <- c(parts, list(
      shiny::tags$div(
        style = "border:1px solid #dee2e6;border-radius:4px;padding:8px 10px;margin-bottom:8px;",
        do.call(shiny::tagList, q_parts)
      )
    ))
  }

  do.call(shiny::tagList, parts)
}

# -- Question set modal content builder ----------------------------------------

.build_qset_modal_content <- function(data) {
  mats <- if (length(data$source_materials %||% list()) > 0L)
    paste(data$source_materials, collapse = ", ")
  else "(none)"

  shiny::tagList(
    shiny::h6("Details", style = "font-weight: bold; margin-top: 0;"),
    shiny::tags$table(
      class = "table table-sm table-condensed",
      style = "font-size: 0.9em;",
      shiny::tags$tbody(
        shiny::tags$tr(
          shiny::tags$th("Study", style = "width: 150px;"),
          shiny::tags$td(data$study_id %||% "(none)")
        ),
        shiny::tags$tr(
          shiny::tags$th("Source materials"),
          shiny::tags$td(mats)
        ),
        shiny::tags$tr(
          shiny::tags$th("Created"),
          shiny::tags$td(data$created_at %||% "")
        ),
        shiny::tags$tr(
          shiny::tags$th("Model"),
          shiny::tags$td(data$model %||% "")
        )
      )
    ),
    .build_questions_ui(data$questions %||% list())
  )
}

# Token totals from the assistant turns. ellmer 0.5's chat$get_tokens() pairs
# user and assistant turns and fails once a [UI Notification] user turn has
# been injected (two user turns in a row).
chat_token_totals <- function(chat) {
  turns <- Filter(function(t) inherits(t, "ellmer::AssistantTurn"), chat$get_turns())
  tok <- lapply(turns, function(t) as.numeric(t@tokens)[1:3])
  if (length(tok) == 0L) return(NULL)
  m <- do.call(rbind, tok)
  m[is.na(m)] <- 0
  c(input = sum(m[, 1L]), output = sum(m[, 2L]), cached = sum(m[, 3L]))
}

# Modal body for a material: PDFs drawn with PDF.js at full width (served from
# the session's materials/ folder), Markdown rendered, other text verbatim.
.material_preview <- function(session_path, file) {
  path <- file.path(session_path, "materials", file)
  if (!file.exists(path)) return(shiny::p("File not found."))
  ext <- tolower(tools::file_ext(file))
  if (ext == "pdf") {
    src <- paste0("materials/", utils::URLencode(file, reserved = TRUE))
    # drawn page by page at full width by inst/www/raise-pdf.js (PDF.js)
    return(shiny::tagList(
      shiny::tags$div(class = "raise-pdf", `data-src` = src,
                      style = "height:78vh;overflow-y:auto;background:#f1f3f5;padding:8px;",
                      "Loading PDF ..."),
      shiny::tags$p(style = "font-size:0.85em;margin:4px 0 0;",
                    shiny::tags$a(href = src, target = "_blank", "Open in a new tab"))
    ))
  }
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (ext %in% c("md", "markdown")) shiny::div(class = "raise-md", md_html(text))
  else shiny::tags$pre(style = "white-space:pre-wrap;max-height:78vh;overflow:auto;", text)
}

# Input-id-safe key for a figure file name.
.figure_id <- function(file) gsub("[^A-Za-z0-9_-]", "_", file)

# -- State panel renderer ------------------------------------------------------

.status_badge <- function(status) {
  emoji <- switch(status,
    simulated         = "\U0001F7E2",
    coefficients_set  = "\U0001F535",
    correlations_set  = "\U0001F7E0",
    description_saved = "\U0001F7E1",
    "\u26AA"
  )
  cls <- switch(status,
    simulated         = "badge-simulated",
    coefficients_set  = "badge-coefficients",
    correlations_set  = "badge-correlations",
    description_saved = "badge-description",
    "badge-unknown"
  )
  shiny::tags$span(class = paste("status-badge", cls), paste(emoji, status))
}

.list_exports <- function(exports_dir) {
  if (!dir.exists(exports_dir)) return(character(0))
  folders <- list.dirs(exports_dir, full.names = FALSE, recursive = FALSE)
  rev(sort(folders))
}

.parse_export_ts <- function(folder_name) {
  m <- regmatches(folder_name, regexpr("[0-9]{8}_[0-9]{6}", folder_name))
  if (length(m) == 0L) return(folder_name)
  d <- m[[1L]]
  paste0(
    substr(d,  1L,  4L), "-", substr(d, 5L, 6L), "-", substr(d, 7L, 8L), " ",
    substr(d, 10L, 11L), ":", substr(d, 12L, 13L), ":", substr(d, 14L, 15L)
  )
}

# Render the right-column state panel from disk.
render_state_panel <- function(session_path, confirm_state = NULL) {
  if (is.null(session_path) || !dir.exists(session_path)) {
    return(shiny::p("No session open.", style = "color: grey;"))
  }

  .section <- function(heading, items) {
    do.call(shiny::tags$details, c(
      list(
        open = "",
        shiny::tags$summary(
          style = "cursor: pointer; list-style: none; user-select: none; padding: 4px 0;",
          shiny::h5(heading, style = "display: inline; margin: 0;")
        )
      ),
      items
    ))
  }

  # -- Studies ------------------------------------------------------------------
  studies_dir <- file.path(session_path, "studies")
  study_dirs  <- if (dir.exists(studies_dir))
    sort(list.dirs(studies_dir, full.names = TRUE, recursive = FALSE))
  else character(0)

  study_items <- if (length(study_dirs) == 0L) {
    list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
  } else {
    study_cards <- list()
    for (study_dir in study_dirs) {
      design_file <- file.path(study_dir, "study_design.json")
      if (!file.exists(design_file)) next
      design   <- tryCatch(jsonlite::read_json(design_file), error = function(e) NULL)
      if (is.null(design)) next
      study_id <- basename(study_dir)
      title    <- design$title %||% study_id
      status   <- design$status %||% "unknown"
      rt       <- design$family %||% ""

      is_confirming <- !is.null(confirm_state) &&
                       isTRUE(confirm_state[[paste0("study_", study_id)]])

      has_csv <- file.exists(file.path(study_dir, "data.csv"))

      delete_ui <- if (is_confirming) {
        shiny::tagList(
          shiny::tags$span("Delete?",
                           style = "font-size: 0.8em; color: #721c24; margin-right: 4px;"),
          shiny::actionButton(
            paste0("confirm_yes_study_", study_id), "Yes",
            class = "btn-danger btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("confirm_cancel_study_", study_id), "Cancel",
            class = "btn-default btn-xs",
            style = "font-size: 0.75em; padding: 1px 6px;"
          )
        )
      } else {
        shiny::tagList(
          if (has_csv) shiny::downloadButton(
            paste0("study_dl_csv_", study_id), "Download csv",
            class = "btn-default btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("show_study_", study_id), "Show",
            class = "btn-info btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("del_study_", study_id), "Delete",
            class = "btn-danger btn-xs",
            style = "font-size: 0.75em; padding: 1px 6px;"
          )
        )
      }

      status_cls <- switch(status,
        simulated         = "badge-simulated",
        coefficients_set  = "badge-coefficients",
        correlations_set  = "badge-correlations",
        description_saved = "badge-description",
        "badge-unknown"
      )
      status_pill <- shiny::tags$span(
        class = paste("status-badge", status_cls),
        style = "font-weight: normal; margin-left: 0;",
        status
      )
      type_pill <- shiny::tags$span(
        rt,
        style = paste0("display: inline-block; padding: 2px 8px; border-radius: 10px; ",
                       "font-size: 0.8em; background: #e2e3e5; color: #383d41; margin-left: 4px;")
      )

      study_cards <- c(study_cards, list(shiny::tags$div(
        style = "border: 1px solid #ddd; border-radius: 4px; padding: 6px 8px; margin-bottom: 6px;",
        shiny::strong(title),
        shiny::tags$div(
          style = "margin-top: 4px; margin-bottom: 4px;",
          status_pill, type_pill
        ),
        delete_ui
      )))
    }
    study_cards
  }

  # -- Question sets ------------------------------------------------------------
  qset_dir   <- file.path(session_path, "question_sets")
  qset_files <- if (dir.exists(qset_dir))
    sort(list.files(qset_dir, pattern = "\\.json$", full.names = TRUE))
  else character(0)

  qset_items <- if (length(qset_files) == 0L) {
    list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
  } else {
    qset_cards <- list()
    for (f in qset_files) {
      data <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
      if (is.null(data)) next
      qset_id  <- tools::file_path_sans_ext(basename(f))
      topic    <- data$topic %||% qset_id
      nq       <- length(data$questions %||% list())

      sid <- data$study_id
      study_warning <- if (is.character(sid) && nzchar(sid) &&
                           !dir.exists(file.path(studies_dir, sid)))
        shiny::tags$span(" [!] study deleted",
                         style = "color: orange; font-size: 0.8em;")
      else NULL

      question_badges <- if (nq > 0L) {
        shiny::tagList(lapply(data$questions, function(q) {
          type <- q$type %||% ""
          if (identical(type, "multiple_choice")) {
            label  <- "MC"
            bstyle <- "background: #cce5ff; color: #004085;"
          } else {
            label  <- "#"
            bstyle <- "background: #e2e3e5; color: #383d41;"
          }
          shiny::tags$span(
            style = paste0("display: inline-block; padding: 1px 5px; border-radius: 8px; ",
                           "font-size: 0.75em; margin: 1px; ", bstyle),
            title = substr(q$question %||% "", 1L, 90L),
            label
          )
        }))
      } else NULL

      is_confirming <- !is.null(confirm_state) &&
                       isTRUE(confirm_state[[paste0("qset_", qset_id)]])

      buttons_ui <- if (is_confirming) {
        shiny::tagList(
          shiny::tags$span("Delete?",
                           style = "font-size: 0.8em; color: #721c24; margin-right: 4px;"),
          shiny::actionButton(
            paste0("confirm_yes_qset_", qset_id), "Yes",
            class = "btn-danger btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("confirm_cancel_qset_", qset_id), "Cancel",
            class = "btn-default btn-xs",
            style = "font-size: 0.75em; padding: 1px 6px;"
          )
        )
      } else {
        shiny::tagList(
          shiny::actionButton(
            paste0("show_qset_", qset_id), "Show",
            class = "btn-info btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("del_qset_", qset_id), "Delete",
            class = "btn-danger btn-xs",
            style = "font-size: 0.75em; padding: 1px 6px;"
          )
        )
      }

      qset_cards <- c(qset_cards, list(shiny::tags$div(
        style = "border: 1px solid #ddd; border-radius: 4px; padding: 6px 8px; margin-bottom: 6px;",
        shiny::tags$div(
          shiny::strong(topic),
          shiny::tags$span(paste0(" (", nq, " Qs)"),
                           style = "font-size: 0.85em; color: #666;"),
          study_warning
        ),
        shiny::tags$div(
          style = "padding-top: 6px;",
          question_badges,
          shiny::tags$div(style = "margin-top: 4px;", buttons_ui)
        )
      )))
    }

    c(qset_cards, list(
      shiny::actionButton("export_btn", "Export",
                          class = "btn-primary btn-sm",
                          style = "width: 100%; margin-top: 4px;")
    ))
  }

  # -- Exports ------------------------------------------------------------------
  exports_dir    <- file.path(session_path, "exports")
  export_folders <- .list_exports(exports_dir)

  export_items <- if (length(export_folders) == 0L) {
    list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
  } else {
    export_cards <- list()
    for (folder_name in export_folders) {
      folder_path <- file.path(exports_dir, folder_name)

      artifact <- export_artifact(folder_path)
      if (is.null(artifact)) next

      meta <- tryCatch(
        jsonlite::read_json(file.path(folder_path, "meta.json")),
        error = function(e) list()
      )

      dest        <- meta$destination %||% tools::file_ext(artifact)
      n_questions <- meta$n_questions %||% "?"
      ts_label    <- .parse_export_ts(folder_name)

      exp_confirming <- !is.null(confirm_state) &&
                        isTRUE(confirm_state[[paste0("export_", folder_name)]])
      exp_delete_ui <- if (exp_confirming) {
        shiny::tagList(
          shiny::tags$span("Delete?",
                           style = "font-size: 0.8em; color: #721c24; margin-right: 4px;"),
          shiny::actionButton(
            paste0("confirm_yes_export_", folder_name), "Yes",
            class = "btn-danger btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("confirm_cancel_export_", folder_name), "Cancel",
            class = "btn-default btn-xs",
            style = "font-size: 0.75em; padding: 1px 6px;"
          )
        )
      } else {
        shiny::actionButton(
          paste0("del_export_", folder_name), "Delete",
          class = "btn-danger btn-xs",
          style = "font-size: 0.75em; padding: 1px 6px;"
        )
      }

      buttons_ui <- if (exp_confirming) {
        exp_delete_ui
      } else {
        shiny::tagList(
          shiny::downloadButton(
            paste0("export_dl_", folder_name), "Download",
            class = "btn-default btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          shiny::actionButton(
            paste0("show_export_", folder_name), "Show",
            class = "btn-info btn-xs",
            style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
          ),
          exp_delete_ui
        )
      }

      export_cards <- c(export_cards, list(shiny::tags$div(
        style = "border: 1px solid #ddd; border-radius: 4px; padding: 6px 8px; margin-bottom: 6px;",
        shiny::strong(meta$label %||% folder_name),
        shiny::tags$div(
          style = "font-size: 0.85em; color: #666; padding-top: 2px; margin-bottom: 4px;",
          paste0(n_questions, " questions | ", dest, " | ", ts_label, " | ", basename(artifact))
        ),
        shiny::tags$div(style = "padding-top: 2px;", buttons_ui)
      )))
    }
    export_cards
  }

  shiny::tagList(
    .section("Studies", study_items),
    shiny::tags$hr(),
    .section("Question Sets", qset_items),
    shiny::tags$hr(),
    .section("Exports", export_items)
  )
}

# -- Server --------------------------------------------------------------------

raise_server <- function(input, output, session, initial_session_path = NULL,
                         provider = raise_default_provider, model = NULL,
                         embed_provider = raise_default_embed_provider, embed_model = NULL,
                         chat_args = list()) {
  session_path_rv       <- shiny::reactiveVal(NULL)
  state_version         <- shiny::reactiveVal(0L)
  confirm_state         <- shiny::reactiveValues()
  rag_confirm_state     <- shiny::reactiveValues()
  rag_modal_state       <- shiny::reactiveVal(NULL)
  registered_delete_ids <- new.env(parent = emptyenv())

  system_prompt <- load_system_prompt()

  # Mutable reference so a single chat object can serve multiple sessions.
  session_env       <- new.env(parent = emptyenv())
  session_env$path  <- NULL
  chat              <- do.call(make_chat, c(list(session_env, system_prompt,
                                                    provider = provider, model = model),
                                               chat_args))

  inject_ui_notification <- function(msg, contents = list()) {
    if (is.null(session_env$path)) return(invisible(NULL))
    turns    <- chat$get_turns()
    new_turn <- ellmer::Turn("user", c(list(ellmer::ContentText(
      paste0("[UI Notification] ", msg)
    )), contents))
    chat$set_turns(c(turns, list(new_turn)))
    save_chat_history(chat$get_turns(), session_env$path)
  }

  # -- File browser -----------------------------------------------------------
  roots <- c(Home = path.expand("~"), cwd = getwd())
  shinyFiles::shinyDirChoose(input, "browse_session", roots = roots)

  shiny::observeEvent(input$browse_session, {
    chosen <- shinyFiles::parseDirPath(roots, input$browse_session)
    if (length(chosen) > 0 && nzchar(chosen))
      shiny::updateTextInput(session, "session_path_input", value = chosen)
  }, ignoreInit = TRUE)

  # -- Chat module ------------------------------------------------------------
  # shinychat >= 0.5 has its own conversation history; RAISE keeps chat.rds.
  chat_result <- if (utils::packageVersion("shinychat") >= "0.5.0") {
    shinychat::chat_mod_server("chat", client = chat, history = FALSE)
  } else {
    shinychat::chat_mod_server("chat", client = chat)
  }

  shiny::observe({
    chat_result$last_turn()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    save_chat_history(chat$get_turns(), sess)
    state_version(shiny::isolate(state_version()) + 1L)
    session$sendCustomMessage("mathjax-typeset", list())
  })

  # -- Startup helper ---------------------------------------------------------
  # If history exists: restore turns silently, no agent contact.
  # If no history: contact agent for greeting.
  open_session <- function(sess_path) {
    dir.create(figures_dir(sess_path), showWarnings = FALSE)
    shiny::addResourcePath("figures", normalizePath(figures_dir(sess_path)))
    dir.create(file.path(sess_path, "materials"), showWarnings = FALSE)
    shiny::addResourcePath("materials", normalizePath(file.path(sess_path, "materials")))
    for (r in list_rags(sess_path)) {
      rag_confirm_state[[r$rag_id]] <- FALSE
    }

    history <- load_chat_history(sess_path)

    if (length(history) > 0L) {
      chat$set_turns(history)
      chat_result$clear(client_history = "keep")
      msgs <- shinychat::contents_shinychat(chat)
      for (msg in msgs) {
        if (!is.character(msg$content) || !grepl("^\\[SYSTEM STARTUP", msg$content)) {
          content <- if (is.character(msg$content)) {
            msg$content
          } else {
            paste(vapply(msg$content, function(item) {
              if (is.character(item)) item
              else sprintf("\n\n````````{=html}\n%s\n````````\n\n",
                          as.character(htmltools::as.tags(item)))
            }, character(1L)), collapse = "")
          }
          shinychat::chat_append_message(
            "chat-chat",
            list(role = msg$role, content = content),
            chunk = FALSE,
            session = session
          )
          session$sendCustomMessage("mathjax-typeset", list())
        }
      }
      state_version(state_version() + 1L)
      return(invisible(NULL))
    }

    # No history: clear everything and send startup message to agent
    chat_result$clear(client_history = "clear")

    studies_json <- tryCatch(jsonlite::fromJSON(list_studies(sess_path),
                                                simplifyVector = FALSE),
                             error = function(e) list())
    qsets_json   <- tryCatch(jsonlite::fromJSON(list_question_sets(sess_path),
                                                simplifyVector = FALSE),
                             error = function(e) list())
    mats         <- tryCatch(jsonlite::fromJSON(list_materials(sess_path)),
                             error = function(e) character(0))
    figs         <- tryCatch(jsonlite::fromJSON(list_figures(sess_path),
                                                simplifyVector = FALSE),
                             error = function(e) list())

    is_new <- length(studies_json) == 0L && length(qsets_json) == 0L &&
              length(mats) == 0L && length(figs) == 0L

    if (is_new) {
      startup_msg <- paste0(
        "[SYSTEM STARTUP -- do not display]\n",
        "Session: ", sess_path, " -- new session, no existing content.\n",
        "Greet the user, explain the two workflows, ask what they want to do."
      )
    } else {
      study_lines <- vapply(studies_json, function(s)
        paste0(s$study_id, " (", s$family, ", status: ", s$status, ")"),
        character(1))
      qset_lines  <- vapply(qsets_json, function(q)
        paste0(q$id, " (", q$topic, ", ", q$n_questions, " questions)"),
        character(1))
      startup_msg <- paste0(
        "[SYSTEM STARTUP -- do not display]\n",
        "Session: ", sess_path, "\n",
        "Studies: [", paste(study_lines, collapse = ", "), "]\n",
        "Question sets: [", paste(qset_lines,  collapse = ", "), "]\n",
        "Materials: [",     paste(mats,         collapse = ", "), "]\n",
        "Figures: [", paste(vapply(figs, function(f)
          paste0(f$file, if (isTRUE(f$unused)) " (unused)" else ""), ""), collapse = ", "), "]\n",
        "Greet the user, summarise existing content, ask what they want to do."
      )
    }

    greeting <- tryCatch(
      chat$chat(startup_msg),
      error = function(e) paste("Session opened. Error during greeting:", e$message)
    )
    chat_result$append(greeting)
    state_version(state_version() + 1L)
  }

  # -- Open session ----------------------------------------------------------
  shiny::observeEvent(input$open_session_btn, {
    raw_path <- trimws(input$session_path_input)
    if (!nzchar(raw_path)) return()

    sess_path <- init_session(raw_path)

    session_env$path <- sess_path
    session_path_rv(sess_path)

    open_session(sess_path)
  })

  # -- Clear chat ------------------------------------------------------------
  output$clear_chat_ui <- shiny::renderUI({
    if (is.null(session_path_rv())) return(NULL)
    shiny::actionButton("clear_chat_btn", "Clear chat",
                        class = "btn-sm btn-outline-secondary",
                        style = "margin-bottom: 4px;")
  })

  shiny::observeEvent(input$clear_chat_btn, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    chat_rds <- file.path(sess, "chat.rds")
    if (file.exists(chat_rds)) file.remove(chat_rds)
    open_session(sess)
  })

  # -- Token / cost usage display --------------------------------------------
  output$usage_ui <- shiny::renderUI({
    state_version()
    if (is.null(session_path_rv())) return(NULL)
    tokens <- chat_token_totals(chat)
    if (is.null(tokens)) return(NULL)
    total_in     <- tokens[["input"]]
    total_out    <- tokens[["output"]]
    total_cached <- tokens[["cached"]]
    total_cost   <- tryCatch(as.numeric(chat$get_cost()), error = function(e) NA_real_)
    fmt <- function(n) format(n, big.mark = ",", scientific = FALSE)
    shiny::tags$div(
      style = paste0(
        "font-size: 0.82em; color: #555; background: #f8f9fa;",
        " border-radius: 4px; padding: 4px 8px; margin-bottom: 4px;"
      ),
      shiny::tags$span(shiny::tags$strong("Usage this session")),
      shiny::tags$br(),
      shiny::tags$span(
        paste0("In ", fmt(total_in),
               " | Out ", fmt(total_out),
               " | Cached ", fmt(total_cached))
      ),
      shiny::tags$br(),
      shiny::tags$span(paste0("Cost: ", if (is.na(total_cost)) "n/a" else paste0("$", sprintf("%.4f", total_cost))))
    )
  })

  # -- Material upload --------------------------------------------------------
  shiny::observeEvent(input$upload_material, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    fi <- input$upload_material
    dest <- file.path(sess, "materials", fi$name)
    file.copy(fi$datapath, dest, overwrite = TRUE)
    inject_ui_notification(paste0("Material '", fi$name, "' was uploaded to the session."))
    state_version(state_version() + 1L)
  })

  # -- Figure upload ----------------------------------------------------------
  shiny::observeEvent(input$upload_figure, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    fi <- input$upload_figure
    for (k in seq_len(nrow(fi))) {
      res <- tryCatch(add_figure(fi$datapath[k], sess, name = fi$name[k]),
                      error = function(e) conditionMessage(e))
      if (is.character(res)) {
        shiny::showNotification(paste0(fi$name[k], ": ", res), type = "error")
        next
      }
      if (res$existed) {
        shiny::showNotification(paste0(fi$name[k], " is already in the session as ", res$file, "."),
                                type = "message")
        next
      }
      inject_ui_notification(
        paste0("Figure '", fi$name[k], "' was uploaded as ", res$path,
               ". Reference it with `", res$markdown, "`. The image follows."),
        list(figure_content(res$file, sess))
      )
    }
    state_version(state_version() + 1L)
  })

  # -- State panel -----------------------------------------------------------
  output$state_panel <- shiny::renderUI({
    state_version()
    sess <- session_path_rv()
    render_state_panel(sess, confirm_state)
  })

  # -- RAG settings UI (inside modal, reacts to provider selection) --------------
  output$rag_settings_ui <- shiny::renderUI({
    ms       <- rag_modal_state()
    provider <- input$rag_provider %||%
                  (if (!is.null(ms) && !is.null(ms$meta)) ms$meta$embed_provider else embed_provider) %||%
                  embed_provider

    meta          <- if (!is.null(ms)) ms$meta else NULL
    orig_prov     <- if (!is.null(meta)) meta$embed_provider %||% "bm25" else NULL
    use_meta_vals <- isTRUE(identical(provider, orig_prov))

    chunk_val <- if (use_meta_vals && !is.null(meta$chunk_size))
                   as.integer(meta$chunk_size)
                 else 500L

    chunk_slider <- shiny::sliderInput(
      "rag_chunk_size", "Chunk size (characters)",
      min = 200L, max = 1000L, value = chunk_val, step = 50L
    )

    if (identical(provider, "bm25")) return(chunk_slider)
    model_val <- if (use_meta_vals && !is.null(meta$embed_model)) meta$embed_model
                 else if (identical(provider, embed_provider) && !is.null(embed_model)) embed_model
                 else default_embed_model(provider)
    shiny::tagList(
      chunk_slider,
      shiny::textInput("rag_embed_model", "Embedding model", value = model_val,
                       placeholder = "provider default")
    )
  })

  # -- Create RAG button ---------------------------------------------------------
  shiny::observeEvent(input$create_rag_btn, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    avail <- list.files(file.path(sess, "materials"))
    avail <- avail[tolower(tools::file_ext(avail)) %in%
                     c("pdf", "md", "markdown", "txt")]
    rag_modal_state(list(mode = "create", rag_id = NULL, meta = NULL))
    .show_rag_create_update_modal("create", avail, provider = embed_provider)
  }, ignoreInit = TRUE)

  # -- RAG modal confirm (build) -------------------------------------------------
  shiny::observeEvent(input$rag_modal_confirm, {
    ms <- rag_modal_state()
    if (is.null(ms)) return()
    sess <- session_path_rv()
    if (is.null(sess)) return()

    rag_label <- trimws(input$rag_label %||% "")
    if (!nzchar(rag_label)) {
      shiny::showNotification("Please enter a label for this RAG store.", type = "warning")
      return()
    }

    selected_files <- input$rag_files %||% character(0)
    if (length(selected_files) == 0L) {
      shiny::showNotification("Please select at least one file.", type = "warning")
      return()
    }

    provider   <- input$rag_provider %||% embed_provider
    chunk_size <- as.integer(input$rag_chunk_size %||% 500L)
    model      <- if (identical(provider, "bm25")) NULL else {
      m <- trimws(input$rag_embed_model %||% "")
      if (nzchar(m)) m else NULL
    }
    settings <- list(chunk_size = chunk_size, model = model)

    if (identical(ms$mode, "create")) {
      rag_id  <- paste0(provider, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      rag_dir <- file.path(sess, "rag", rag_id)
      dir.create(rag_dir, recursive = TRUE, showWarnings = FALSE)
      created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    } else {
      rag_id     <- ms$rag_id
      rag_dir    <- file.path(sess, "rag", rag_id)
      created_at <- ms$meta$created_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    }

    full_paths <- file.path(sess, "materials", selected_files)

    shiny::showModal(shiny::modalDialog(
      shiny::p("Building RAG store, please wait..."),
      footer = NULL, easyClose = FALSE
    ))

    store <- build_rag_store(rag_dir, full_paths, provider, settings)

    if (!is.null(store)) {
      manifest <- make_file_manifest(full_paths)
      meta_out <- list(
        label          = rag_label,
        rag_id         = rag_id,
        embed_provider = provider,
        embed_model    = model,
        chunk_size     = chunk_size,
        created_at     = created_at,
        updated_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        active         = if (identical(ms$mode, "update")) isTRUE(ms$meta$active) else FALSE,
        file_manifest  = manifest
      )
      write_rag_meta(rag_dir, meta_out)
      inject_ui_notification(paste0(
        "RAG store '", rag_label, "' (", provider, ") was ",
        if (identical(ms$mode, "update")) "updated." else "created."
      ))
      rag_confirm_state[[rag_id]] <- FALSE
    } else {
      shiny::showNotification("RAG build failed. See console for details.", type = "error")
      if (identical(ms$mode, "create") && dir.exists(rag_dir) &&
          length(list.files(rag_dir)) == 0L)
        unlink(rag_dir, recursive = TRUE)
    }

    shiny::removeModal()
    rag_modal_state(NULL)
    state_version(state_version() + 1L)
  }, ignoreInit = TRUE)

  output$materials_section <- shiny::renderUI({
    state_version()
    sess <- session_path_rv()

    file_items <- if (is.null(sess)) {
      list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
    } else {
      files <- tryCatch(
        jsonlite::fromJSON(list_materials(sess)),
        error = function(e) character(0)
      )
      if (length(files) == 0L) {
        list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
      } else {
        lapply(files, function(f) {
          safe_id       <- gsub("[^A-Za-z0-9_-]", "_", f)
          is_confirming <- isTRUE(confirm_state[[paste0("mat_", safe_id)]])
          delete_ui <- if (is_confirming) {
            shiny::tagList(
              shiny::tags$span("Delete?",
                               style = "font-size: 0.8em; color: #721c24; margin-right: 4px;"),
              shiny::actionButton(
                paste0("confirm_yes_mat_", safe_id), "Yes",
                class = "btn-danger btn-xs",
                style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
              ),
              shiny::actionButton(
                paste0("confirm_cancel_mat_", safe_id), "Cancel",
                class = "btn-default btn-xs",
                style = "font-size: 0.75em; padding: 1px 6px;"
              )
            )
          } else {
            shiny::tagList(
              shiny::actionButton(
                paste0("show_mat_", safe_id), "Show",
                class = "btn-default btn-xs",
                style = "font-size: 0.75em; padding: 1px 6px;"
              ),
              shiny::actionButton(
                paste0("del_mat_", safe_id), "Delete",
                class = "btn-danger btn-xs",
                style = "font-size: 0.75em; padding: 1px 6px;"
              )
            )
          }
          shiny::tags$div(
            style = "display: flex; align-items: center; gap: 6px; font-size: 0.85em; margin-bottom: 2px;",
            shiny::tags$span(f, title = f,
                             style = "flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"),
            delete_ui
          )
        })
      }
    }

    shiny::tags$details(
      open = "",
      shiny::tags$summary(
        style = "cursor: pointer; list-style: none; user-select: none; padding: 4px 0;",
        shiny::h5("Materials", style = "display: inline; margin: 0;")
      ),
      shiny::tagAppendAttributes(
        shiny::fileInput("upload_material", label = NULL,
                         accept = c(".pdf", ".md", ".txt", ".markdown"),
                         buttonLabel = "Upload...",
                         placeholder = "PDF / Markdown / txt"),
        style = "margin-bottom: 4px; margin-top: 6px;"
      ),
      do.call(shiny::tagList, file_items)
    )
  })

  output$figures_section <- shiny::renderUI({
    state_version()
    sess <- session_path_rv()
    figs <- if (is.null(sess)) list() else tryCatch(
      jsonlite::fromJSON(list_figures(sess), simplifyVector = FALSE),
      error = function(e) list()
    )
    n_unused <- sum(vapply(figs, function(f) isTRUE(f$unused), logical(1L)))
    btn <- "font-size: 0.75em; padding: 1px 6px;"
    items <- if (length(figs) == 0L) {
      list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
    } else lapply(figs, function(f) {
      sid  <- .figure_id(f$file)
      used <- length(f$used_by)
      badge <- if (used == 0L)
        shiny::tags$span("unused", class = "status-badge badge-unknown", style = "margin-left: 0;")
      else shiny::tags$span(paste0(used, " Q"), class = "status-badge badge-coefficients",
                            style = "margin-left: 0;", title = paste(unlist(f$used_by), collapse = ", "))
      delete_ui <- if (isTRUE(confirm_state[[paste0("fig_", sid)]])) {
        shiny::tagList(
          shiny::tags$span(if (used > 0L) paste0("Used by ", used, " Q. Delete?") else "Delete?",
                           style = "font-size: 0.8em; color: #721c24;"),
          shiny::actionButton(paste0("confirm_yes_fig_", sid), "Yes",
                              class = "btn-danger btn-xs", style = btn),
          shiny::actionButton(paste0("confirm_cancel_fig_", sid), "Cancel",
                              class = "btn-default btn-xs", style = btn)
        )
      } else {
        shiny::tagList(
          shiny::actionButton(paste0("show_fig_", sid), "Show", class = "btn-default btn-xs", style = btn),
          shiny::actionButton(paste0("del_fig_", sid), "Delete", class = "btn-danger btn-xs", style = btn)
        )
      }
      shiny::tags$div(
        style = "display: flex; align-items: center; gap: 6px; font-size: 0.85em; margin-bottom: 4px;",
        shiny::tags$img(src = f$path, style = "height: 32px; width: 48px; object-fit: contain; border: 1px solid #dee2e6;"),
        shiny::tags$span(f$file, title = f$file,
                         style = "flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"),
        badge, delete_ui
      )
    })

    shiny::tags$details(
      open = "",
      shiny::tags$summary(
        style = "cursor: pointer; list-style: none; user-select: none; padding: 4px 0;",
        shiny::h5("Figures", style = "display: inline; margin: 0;")
      ),
      shiny::tagAppendAttributes(
        shiny::fileInput("upload_figure", label = NULL, multiple = TRUE,
                         accept = c(".png", ".jpg", ".jpeg"),
                         buttonLabel = "Upload...", placeholder = "PNG / JPEG"),
        style = "margin-bottom: 4px; margin-top: 6px;"
      ),
      do.call(shiny::tagList, items),
      if (n_unused > 0L) {
        if (isTRUE(confirm_state[["fig_unused"]])) {
          shiny::tags$div(
            style = "font-size: 0.85em; margin-top: 4px;",
            shiny::tags$span(paste0("Delete ", n_unused, " unused figure(s)?"),
                             style = "color: #721c24; margin-right: 4px;"),
            shiny::actionButton("confirm_yes_fig_unused", "Yes", class = "btn-danger btn-xs", style = btn),
            shiny::actionButton("confirm_cancel_fig_unused", "Cancel", class = "btn-default btn-xs", style = btn)
          )
        } else {
          shiny::actionButton("del_fig_unused", paste0("Delete unused (", n_unused, ")"),
                              class = "btn-default btn-xs", style = paste(btn, "margin-top: 4px;"))
        }
      }
    )
  })

  shiny::observeEvent(input$del_fig_unused, {
    confirm_state[["fig_unused"]] <- TRUE
    state_version(state_version() + 1L)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$confirm_yes_fig_unused, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    msg <- delete_unused_figures(sess)
    inject_ui_notification(paste0("Unused figures were deleted:\n", msg))
    confirm_state[["fig_unused"]] <- FALSE
    state_version(state_version() + 1L)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$confirm_cancel_fig_unused, {
    confirm_state[["fig_unused"]] <- FALSE
    state_version(state_version() + 1L)
  }, ignoreInit = TRUE)

  # -- Show / delete figure buttons (dynamic) ---------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    for (fname in list.files(figures_dir(sess))) {
      sid <- .figure_id(fname)
      key <- paste0("fig_", sid)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        fn <- fname
        id <- sid
        shiny::observeEvent(input[[paste0("show_fig_", id)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          used <- figure_usage(sess_local)[[fn]] %||% character(0)
          shiny::showModal(shiny::modalDialog(
            title = fn,
            shiny::tags$img(src = file.path("figures", fn), style = "max-width: 100%;"),
            shiny::tags$p(style = "margin-top: 8px; font-size: 0.9em;",
                          shiny::tags$strong("Reference: "),
                          shiny::tags$code(paste0("![...](figures/", fn, ")"))),
            shiny::tags$p(style = "font-size: 0.9em;", shiny::tags$strong("Used by: "),
                          if (length(used) == 0L) "no question" else paste(used, collapse = ", ")),
            footer = shiny::modalButton("Close"), size = "l", easyClose = TRUE
          ))
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("del_fig_", id)]], {
          confirm_state[[paste0("fig_", id)]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("confirm_yes_fig_", id)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          msg <- delete_figures(fn, sess_local, force = TRUE)
          inject_ui_notification(msg)
          confirm_state[[paste0("fig_", id)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("confirm_cancel_fig_", id)]], {
          confirm_state[[paste0("fig_", id)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$rag_section <- shiny::renderUI({
    state_version()
    sess          <- session_path_rv()
    rag_conf_snap <- shiny::isolate(as.list(rag_confirm_state))

    rag_cards <- if (is.null(sess) || !dir.exists(sess)) {
      list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
    } else {
      rag_base  <- file.path(sess, "rag")
      mat_dir   <- file.path(sess, "materials")
      rag_metas <- list_rags(sess)

      if (length(rag_metas) == 0L) {
        list(shiny::p("(none)", style = "color: grey; font-size: 0.9em;"))
      } else {
        cards <- list()
        for (meta in rag_metas) {
          rid      <- meta$rag_id
          provider <- meta$embed_provider %||% "bm25"
          n_files  <- length(meta$file_manifest %||% list())
          rag_dir  <- file.path(rag_base, rid)

          sync_status   <- rag_sync_status(rag_dir, mat_dir)
          is_active     <- isTRUE(meta$active)
          is_confirming <- isTRUE(rag_conf_snap[[rid]])

          active_badge <- if (is_active)
            shiny::tags$span("Active",
              style = "background:#d4edda;color:#155724;padding:1px 5px;border-radius:8px;font-size:0.8em;margin-right:4px;")
          else
            shiny::tags$span("Inactive",
              style = "background:#e2e3e5;color:#383d41;padding:1px 5px;border-radius:8px;font-size:0.8em;margin-right:4px;")

          prov_badge <- shiny::tags$span(
            style = "background:#e2e3e5;color:#383d41;padding:1px 5px;border-radius:8px;font-size:0.8em;margin-right:4px;",
            provider
          )

          sync_badge <- shiny::tags$span(
            class = paste("rag-sync-badge",
                          if (identical(sync_status, "in_sync")) "rag-sync-ok" else "rag-sync-warn"),
            if (identical(sync_status, "in_sync")) "in sync" else "out of sync"
          )

          rag_buttons_ui <- if (is_confirming) {
            shiny::tagList(
              shiny::tags$span("Delete?",
                               style = "font-size: 0.8em; color: #721c24; margin-right: 4px;"),
              shiny::actionButton(
                paste0("confirm_yes_rag_", rid), "Yes",
                class = "btn-danger btn-xs",
                style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
              ),
              shiny::actionButton(
                paste0("confirm_cancel_rag_", rid), "Cancel",
                class = "btn-default btn-xs",
                style = "font-size: 0.75em; padding: 1px 6px;"
              )
            )
          } else {
            activate_btn <- if (is_active)
              shiny::actionButton(
                paste0("deactivate_rag_", rid), "Deactivate",
                class = "btn-default btn-xs",
                style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
              )
            else
              shiny::actionButton(
                paste0("activate_rag_", rid), "Activate",
                class = "btn-success btn-xs",
                style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
              )
            shiny::tagList(
              activate_btn,
              shiny::actionButton(
                paste0("update_rag_", rid), "Show",
                class = "btn-info btn-xs",
                style = "margin-right: 2px; font-size: 0.75em; padding: 1px 6px;"
              ),
              shiny::actionButton(
                paste0("del_rag_", rid), "Delete",
                class = "btn-danger btn-xs",
                style = "font-size: 0.75em; padding: 1px 6px;"
              )
            )
          }

          cards <- c(cards, list(shiny::tags$div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 6px 8px; margin-bottom: 6px;",
            shiny::strong(meta$label %||% rid),
            shiny::tags$div(
              style = "margin-bottom: 4px; margin-top: 2px;",
              active_badge, prov_badge, sync_badge,
              shiny::tags$span(
                paste0(n_files, " file", if (n_files != 1L) "s" else ""),
                style = "font-size: 0.85em; color: #666; margin-left: 4px;"
              )
            ),
            shiny::tags$div(rag_buttons_ui)
          )))
        }
        cards
      }
    }

    shiny::tags$details(
      open = "",
      shiny::tags$summary(
        style = "cursor: pointer; list-style: none; user-select: none; padding: 4px 0;",
        shiny::h5("RAG Stores", style = "display: inline; margin: 0;")
      ),
      shiny::tags$div(style = "margin-top: 6px;", do.call(shiny::tagList, rag_cards)),
      shiny::actionButton("create_rag_btn", "Create RAG",
                          class = "btn-primary btn-sm",
                          style = "width: 100%; margin-top: 4px;")
    )
  })

  # -- Delete material buttons (dynamic) -------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    mat_dir <- file.path(sess, "materials")
    if (!dir.exists(mat_dir)) return()
    for (fname in list.files(mat_dir, all.files = FALSE)) {
      safe_id <- gsub("[^A-Za-z0-9_-]", "_", fname)
      key     <- paste0("mat_", safe_id)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        fn  <- fname
        sid <- safe_id
        shiny::observeEvent(input[[paste0("show_mat_", sid)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          shiny::showModal(shiny::modalDialog(
            title = fn,
            .material_preview(sess_local, fn),
            footer = shiny::modalButton("Close"), size = "xl", easyClose = TRUE
          ))
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("del_mat_", sid)]], {
          confirm_state[[paste0("mat_", sid)]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_yes_mat_", sid)]], {
          val <- input[[paste0("confirm_yes_mat_", sid)]]
          if (is.null(val) || val < 1L) return()
          unlink(file.path(sess, "materials", fn))
          inject_ui_notification(paste0("Material '", fn, "' was deleted."))
          confirm_state[[paste0("mat_", sid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_cancel_mat_", sid)]], {
          val <- input[[paste0("confirm_cancel_mat_", sid)]]
          if (is.null(val) || val < 1L) return()
          confirm_state[[paste0("mat_", sid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Delete study buttons (dynamic) ----------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    studies_dir <- file.path(sess, "studies")
    if (!dir.exists(studies_dir)) return()
    for (study_id in list.dirs(studies_dir, full.names = FALSE, recursive = FALSE)) {
      key <- paste0("study_", study_id)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        sid <- study_id
        shiny::observeEvent(input[[paste0("del_study_", sid)]], {
          confirm_state[[paste0("study_", sid)]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_yes_study_", sid)]], {
          val <- input[[paste0("confirm_yes_study_", sid)]]
          if (is.null(val) || val < 1L) return()
          study_title <- tryCatch({
            d <- jsonlite::read_json(file.path(sess, "studies", sid, "study_design.json"))
            d$title %||% sid
          }, error = function(e) sid)
          unlink(file.path(sess, "studies", sid), recursive = TRUE)
          inject_ui_notification(paste0("Study '", sid, "' (\"", study_title, "\") was deleted."))
          confirm_state[[paste0("study_", sid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_cancel_study_", sid)]], {
          val <- input[[paste0("confirm_cancel_study_", sid)]]
          if (is.null(val) || val < 1L) return()
          confirm_state[[paste0("study_", sid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Show study buttons (dynamic) ------------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    studies_dir <- file.path(sess, "studies")
    if (!dir.exists(studies_dir)) return()
    for (study_id in list.dirs(studies_dir, full.names = FALSE, recursive = FALSE)) {
      key <- paste0("show_study_", study_id)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        sid <- study_id
        shiny::observeEvent(input[[paste0("show_study_", sid)]], {
          sess_local  <- session_path_rv()
          if (is.null(sess_local)) return()
          study_dir   <- file.path(sess_local, "studies", sid)
          design_file <- file.path(study_dir, "study_design.json")
          design <- tryCatch(jsonlite::read_json(design_file), error = function(e) NULL)
          if (is.null(design)) return()
          shiny::showModal(shiny::modalDialog(
            title      = design$title %||% sid,
            .build_study_modal_content(design, study_dir),
            footer     = shiny::modalButton("Close"),
            size       = "l",
            easyClose  = TRUE
          ))
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Show question set buttons (dynamic) -----------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    qset_dir <- file.path(sess, "question_sets")
    if (!dir.exists(qset_dir)) return()
    for (qset_id in tools::file_path_sans_ext(
        list.files(qset_dir, pattern = "\\.json$"))) {
      key <- paste0("show_qset_", qset_id)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        qid <- qset_id
        shiny::observeEvent(input[[paste0("show_qset_", qid)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          qset_file  <- file.path(sess_local, "question_sets", paste0(qid, ".json"))
          data <- tryCatch(jsonlite::read_json(qset_file), error = function(e) NULL)
          if (is.null(data)) return()
          nq    <- length(data$questions %||% list())
          title <- paste0(data$topic %||% qid, " (", nq, " Q", if (nq != 1L) "s" else "", ")")
          shiny::showModal(shiny::modalDialog(
            title     = title,
            .build_qset_modal_content(data),
            footer    = shiny::modalButton("Close"),
            size      = "l",
            easyClose = TRUE
          ))
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Delete question set buttons (dynamic) ---------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    qset_dir <- file.path(sess, "question_sets")
    if (!dir.exists(qset_dir)) return()
    for (qset_id in tools::file_path_sans_ext(
        list.files(qset_dir, pattern = "\\.json$"))) {
      key <- paste0("qset_", qset_id)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        qid <- qset_id
        shiny::observeEvent(input[[paste0("del_qset_", qid)]], {
          confirm_state[[paste0("qset_", qid)]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_yes_qset_", qid)]], {
          val <- input[[paste0("confirm_yes_qset_", qid)]]
          if (is.null(val) || val < 1L) return()
          file.remove(file.path(sess, "question_sets", paste0(qid, ".json")))
          inject_ui_notification(paste0("Question set '", qid, "' was deleted."))
          confirm_state[[paste0("qset_", qid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_cancel_qset_", qid)]], {
          val <- input[[paste0("confirm_cancel_qset_", qid)]]
          if (is.null(val) || val < 1L) return()
          confirm_state[[paste0("qset_", qid)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Export modal -----------------------------------------------------------
  shiny::observeEvent(input$export_btn, {
    sess <- session_path_rv()
    if (is.null(sess)) return()
    qset_dir   <- file.path(sess, "question_sets")
    qset_files <- if (dir.exists(qset_dir))
      list.files(qset_dir, pattern = "\\.json$", full.names = TRUE)
    else character(0L)
    .show_export_modal(qset_files)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$export_modal_confirm, {
    sess <- session_path_rv()
    if (is.null(sess)) return()

    qset_dir <- file.path(sess, "question_sets")
    if (!dir.exists(qset_dir)) return()

    all_qids     <- tools::file_path_sans_ext(list.files(qset_dir, pattern = "\\.json$"))
    selected_ids <- Filter(
      function(qid) isTRUE(input[[paste0("export_modal_sel_", qid)]]),
      all_qids
    )
    if (length(selected_ids) == 0L) {
      shiny::showNotification("Select at least one question set.", type = "warning")
      return()
    }

    export_label <- trimws(input$export_modal_label %||% "")
    if (!nzchar(export_label)) {
      shiny::showNotification("Please enter a label for this export.", type = "warning")
      return()
    }
    destination <- input$export_modal_destination %||% "pdf"
    prec        <- input$export_modal_precision %||% "fixed_2"
    mode        <- if (startsWith(prec, "fixed")) "fixed" else "auto"
    n_digits    <- if (mode == "fixed") as.integer(sub("fixed_", "", prec)) else 2L
    show_note   <- isTRUE(input$export_modal_show_note)
    answers     <- isTRUE(input$export_modal_answers %||% TRUE)

    result <- withCallingHandlers(
      export_question_sets(
        selected_ids, sess,
        label                  = export_label,
        destination            = destination,
        precision_mode         = mode,
        n_digits               = n_digits,
        include_precision_note = show_note,
        answers                = answers
      ),
      warning = function(w) {
        if (!grepl("answers = FALSE is ignored", conditionMessage(w), fixed = TRUE)) return()
        shiny::showNotification(
          paste0("Answers are always included in ",
                 switch(destination, canvas = "Canvas", testvision = "TestVision", destination),
                 " exports; the \"Include answers\" setting was ignored."),
          type = "warning", duration = 10
        )
        invokeRestart("muffleWarning")
      }
    )
    if (grepl("^(Error|Export failed)", result)) {
      shiny::showNotification(result, type = "error", duration = NULL)
      return()
    }

    shiny::removeModal()
    state_version(state_version() + 1L)
  }, ignoreInit = TRUE)

  # -- Show export buttons (dynamic) -----------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    exports_dir <- file.path(sess, "exports")
    if (!dir.exists(exports_dir)) return()
    for (folder_name in list.dirs(exports_dir, full.names = FALSE, recursive = FALSE)) {
      key <- paste0("show_export_", folder_name)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        fld <- folder_name
        shiny::observeEvent(input[[paste0("show_export_", fld)]], {
          folder_path <- file.path(sess, "exports", fld)
          artifact <- export_artifact(folder_path)
          if (is.null(artifact)) return()
          meta <- tryCatch(
            jsonlite::read_json(file.path(folder_path, "meta.json")),
            error = function(e) list()
          )
          qjson <- tryCatch(
            jsonlite::read_json(file.path(folder_path, "combined.json")),
            error = function(e) list()
          )
          questions <- qjson$questions %||% list()
          shiny::showModal(shiny::modalDialog(
            title     = paste0(.parse_export_ts(fld), " ? ",
                               meta$destination %||% tools::file_ext(artifact)),
            .build_export_modal_content(meta, artifact, questions),
            footer    = shiny::modalButton("Close"),
            size      = "l",
            easyClose = TRUE
          ))
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Export artifact download handlers (dynamic) ---------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    exports_dir <- file.path(sess, "exports")
    if (!dir.exists(exports_dir)) return()
    for (folder_name in list.dirs(exports_dir, full.names = FALSE, recursive = FALSE)) {
      local({
        fname       <- folder_name
        folder_path <- file.path(exports_dir, fname)
        art <- export_artifact(folder_path)
        if (is.null(art)) return(invisible(NULL))
        art_ext <- tools::file_ext(art)
        # A Markdown export with figures/ next to it downloads as a zip.
        with_figs <- art_ext == "md" && dir.exists(file.path(folder_path, "figures"))
        mime    <- if (with_figs) "application/zip" else
          switch(art_ext, zip = "application/zip", md = "text/markdown",
                 json = "application/json", "application/pdf")
        output[[paste0("export_dl_", fname)]] <- shiny::downloadHandler(
          filename    = function() if (with_figs) sub("\\.md$", ".zip", basename(art)) else basename(art),
          content     = function(path) {
            if (!with_figs) return(file.copy(art, path))
            files <- c(basename(art), file.path("figures", list.files(file.path(folder_path, "figures"))))
            zip_path <- tempfile(fileext = ".zip")
            withr::with_dir(folder_path, utils::zip(zip_path, files, flags = "-q"))
            file.copy(zip_path, path)
          },
          contentType = mime
        )
      })
    }
  })

  # -- Study CSV download handlers (dynamic) ------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    studies_dir <- file.path(sess, "studies")
    if (!dir.exists(studies_dir)) return()
    for (sid in list.dirs(studies_dir, full.names = FALSE, recursive = FALSE)) {
      local({
        study_id <- sid
        csv_path <- file.path(studies_dir, study_id, "data.csv")
        if (!file.exists(csv_path)) return(invisible(NULL))
        output[[paste0("study_dl_csv_", study_id)]] <- shiny::downloadHandler(
          filename    = function() paste0(study_id, ".csv"),
          content     = function(path) file.copy(csv_path, path),
          contentType = "text/csv"
        )
      })
    }
  })

  # -- Delete export buttons (dynamic) ----------------------------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    exports_dir <- file.path(sess, "exports")
    if (!dir.exists(exports_dir)) return()
    for (folder_name in list.dirs(exports_dir, full.names = FALSE, recursive = FALSE)) {
      key <- paste0("export_", folder_name)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        fld <- folder_name
        shiny::observeEvent(input[[paste0("del_export_", fld)]], {
          confirm_state[[paste0("export_", fld)]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_yes_export_", fld)]], {
          val <- input[[paste0("confirm_yes_export_", fld)]]
          if (is.null(val) || val < 1L) return()
          unlink(file.path(sess, "exports", fld), recursive = TRUE)
          confirm_state[[paste0("export_", fld)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_cancel_export_", fld)]], {
          val <- input[[paste0("confirm_cancel_export_", fld)]]
          if (is.null(val) || val < 1L) return()
          confirm_state[[paste0("export_", fld)]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)
      })
    }
  })

  # -- Activate / Deactivate / Delete RAG buttons (dynamic) ---------------------
  shiny::observe({
    state_version()
    sess <- session_path_rv()
    if (is.null(sess)) return()
    rag_base <- file.path(sess, "rag")
    if (!dir.exists(rag_base)) return()
    for (rid in list.dirs(rag_base, full.names = FALSE, recursive = FALSE)) {
      key <- paste0("rag_", rid)
      if (isTRUE(registered_delete_ids[[key]])) next
      registered_delete_ids[[key]] <- TRUE
      local({
        rag_id <- rid
        shiny::observeEvent(input[[paste0("activate_rag_", rag_id)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          set_rag_active(sess_local, rag_id, TRUE)
          inject_ui_notification(paste0("RAG store '", rag_id, "' was activated."))
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("deactivate_rag_", rag_id)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          set_rag_active(sess_local, rag_id, FALSE)
          inject_ui_notification(paste0("RAG store '", rag_id, "' was deactivated."))
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("del_rag_", rag_id)]], {
          rag_confirm_state[[rag_id]] <- TRUE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_yes_rag_", rag_id)]], {
          val <- input[[paste0("confirm_yes_rag_", rag_id)]]
          if (is.null(val) || val < 1L) return()
          sess_local <- session_path_rv()
          if (!is.null(sess_local)) delete_rags(sess_local, rag_id)
          inject_ui_notification(paste0("RAG store '", rag_id, "' was deleted."))
          rag_confirm_state[[rag_id]] <- NULL
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("confirm_cancel_rag_", rag_id)]], {
          val <- input[[paste0("confirm_cancel_rag_", rag_id)]]
          if (is.null(val) || val < 1L) return()
          rag_confirm_state[[rag_id]] <- FALSE
          state_version(state_version() + 1L)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("update_rag_", rag_id)]], {
          sess_local <- session_path_rv()
          if (is.null(sess_local)) return()
          meta <- read_rag_meta(file.path(sess_local, "rag", rag_id))
          if (is.null(meta)) return()
          avail <- list.files(file.path(sess_local, "materials"))
          avail <- avail[tolower(tools::file_ext(avail)) %in%
                           c("pdf", "md", "markdown", "txt")]
          manifest_files <- vapply(meta$file_manifest %||% list(),
                                   function(e) e$filename, character(1L))
          preselected   <- intersect(manifest_files, avail)
          deleted_files <- setdiff(manifest_files, avail)
          rag_modal_state(list(mode = "update", rag_id = rag_id, meta = meta))
          .show_rag_create_update_modal(
            "update", avail,
            selected_files = preselected,
            deleted_files  = deleted_files,
            provider       = meta$embed_provider %||% "bm25",
            label          = meta$label %||% ""
          )
        }, ignoreInit = TRUE)
      })
    }
  })

  if (!is.null(initial_session_path)) {
    shiny::observeEvent(TRUE, {
      sess_path <- init_session(initial_session_path)
      if (is.null(sess_path)) return()
      shiny::updateTextInput(session, "session_path_input",
                             value = trimws(initial_session_path))
      session_env$path <- sess_path
      session_path_rv(sess_path)
      open_session(sess_path)
    }, once = TRUE)
  }

  # Increment state_version when tools modify disk (via reactiveVal in closure)
  # Tools capture state_version from the server environment via the chat closure.
  # Because tools run synchronously during chat$chat(), state changes propagate
  # automatically on the next renderUI invalidation.
}
