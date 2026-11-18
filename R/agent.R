# Read inst/prompts/system.md from the installed package, or from the source
# tree under devtools::load_all().
load_system_prompt <- function() {
  path <- system.file("prompts", "system.md", package = "RAISE")
  if (!nzchar(path)) path <- file.path("inst", "prompts", "system.md")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# Create an ellmer chat object with all 31 RAISE tools registered.
# session_env is an environment with a mutable $path field so the chat object
# can be created once at startup and reused across sessions.
# provider / model / ...: see new_llm_chat().
make_chat <- function(session_env, system_prompt, provider = raise_default_provider,
                      model = NULL, ...) {
  chat <- new_llm_chat(provider, model = model, system_prompt = system_prompt, ...)

  # Prepend session guard to fn's body while preserving its formals.
  .guard <- function(fn) {
    body(fn) <- bquote({
      if (is.null(session_env$path) || !dir.exists(session_env$path))
        return("No session folder is active. Please open a session folder first.")
      .(body(fn))
    })
    fn
  }

  # -- Study creation tools (step-by-step workflow) --------------------------

  chat$register_tool(ellmer::tool(
    .guard(function(title, context, family, n_subjects, n_reps = 1L,
                    subject_id_name, variables, study_id = NULL) {
      save_description(
        title = title,
        context = context,
        family = family,
        n_subjects = n_subjects,
        n_reps = n_reps,
        subject_id_name = subject_id_name,
        variables = variables,
        session_path = session_env$path,
        study_id = study_id
      )
    }),
    description = paste(
      "Step 2 of the step-by-step study creation workflow.",
      "Saves study title, context, family, subject count, and variable list.",
      "Returns the study_id to use in all subsequent steps.",
      "Call save_correlations next."
    ),
    arguments = list(
      title = ellmer::type_string("Study title, 5-10 words.", required = TRUE),
      context = ellmer::type_string(
        "Context paragraph, 3-5 sentences, written as if reporting a real study.",
        required = TRUE
      ),
      family = ellmer::type_enum(
        "Model family.",
        values   = c("identity+normal", "logit+binomial", "log+poisson"),
        required = TRUE
      ),
      n_subjects = ellmer::type_integer(
        "Number of subjects to simulate (>= 2).",
        required = TRUE
      ),
      n_reps = ellmer::type_integer(
        paste(
          "Repetitions per balanced within-subject condition combination per subject (>= 1).",
          "Use 1 for purely between-subjects designs and standard RM designs."
        ),
        required = FALSE
      ),
      subject_id_name = ellmer::type_string(
        "Column name for the subject ID. Derive from study context (e.g. 'student_id', 'patient_id'). Must be a valid R identifier.",
        required = TRUE
      ),
      variables = ellmer::type_object(
        .description = paste(
          "Study variables. Keys: 'dependent' (outcome), 'between' (array of between-subjects",
          "variables), 'within' (array of within-subjects variables; use [] for purely between).",
          "Each variable: 'name' (<=10 chars, valid R identifier), 'label', 'type'",
          "('continuous', 'binary', or 'categorical').",
          "Continuous: 'distribution' with 'mean' and 'sd'.",
          "Binary/categorical: 'categories' is a NAMED OBJECT mapping string keys to human-readable labels.",
          "Binary example: categories = {'0':'Male','1':'Female'} -- keys MUST be '0' and '1'. NEVER use an array.",
          "Categorical example: categories = {'1':'Library','2':'Home','3':'Cafe'} -- keys MUST be '1','2','3',... NEVER use an array.",
          "Between binary/categorical: also include 'reference' ('0' for binary, '1' for categorical) and 'proportion' as a keyed object summing to 1.0 (e.g. {'0':0.45,'1':0.55}). Never use an array for proportion.",
          "Within binary/categorical: 'reference' and 'assignment' ('balanced' default, or 'random').",
          "Within balanced: omit 'proportion'. Within random: include 'proportion' as a keyed object matching 'categories'.",
          "Within continuous: omit 'assignment'."
        ),
        .required              = TRUE,
        .additional_properties = TRUE
      ),
      study_id = ellmer::type_string(
        "Provide ONLY when redoing Step 2 for an existing study -- overwrites the design in place. Omit when creating a new study.",
        required = FALSE
      )
    ),
    name = "save_description"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id, correlations) {
      save_correlations(
        study_id = study_id,
        correlations = correlations,
        session_path = session_env$path
      )
    }),
    description = paste(
      "Step 3 of the step-by-step study creation workflow.",
      "Saves off-diagonal predictor correlations as a named-pair list.",
      "Eligible variables: between-subjects and within-continuous only.",
      "Within-binary and within-categorical must NOT appear.",
      "Use [] when there are no correlations or fewer than 2 eligible variables.",
      "Call save_coefficients next."
    ),
    arguments = list(
      study_id = ellmer::type_string("Study ID returned by save_description.", required = TRUE),
      correlations = ellmer::type_array(
        items = ellmer::type_object(
          .description           = "One correlation pair: vars (two variable names), value.",
          .additional_properties = TRUE
        ),
        description = paste(
          "Off-diagonal correlations as named pairs.",
          "Each entry: {vars: ['A', 'B'], value: r} where r is in (-1, 1).",
          "Use [] when there are no off-diagonal correlations."
        ),
        required = TRUE
      )
    ),
    name = "save_correlations"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id, coefficients, residual_sd = NULL) {
      save_coefficients(study_id = study_id, coefficients = coefficients,
                        residual_sd = residual_sd,
                        session_path = session_env$path)
    }),
    description = paste(
      "Step 4 of the step-by-step study creation workflow.",
      "Saves fixed-effects coefficients and (for identity+normal) residual SD.",
      "Use the {vars, value} format -- never R model.matrix column names.",
      "Call simulate_study next (or save_random_effects first for within/mixed designs)."
    ),
    arguments = list(
      study_id = ellmer::type_string("Study ID returned by save_description.", required = TRUE),
      coefficients = ellmer::type_array(
        items = ellmer::type_object(
          .description           = "One coefficient entry: vars (variable references), value.",
          .additional_properties = TRUE
        ),
        description = paste(
          "Fixed-effects parameters as a named-pair list.",
          "Required entries (treatment coding):",
          "- Intercept: {vars: [{name: '(Intercept)'}], value: <number>}",
          "- Continuous: {vars: [{name: '<varname>'}], value: <number>}",
          "- Binary/categorical: one entry per non-reference level using the category key,",
          "  e.g. {vars: [{name: 'gender', level: '1'}], value: 8.0}",
          "- Interaction: two entries in vars.",
          "The 'level' field is the category key string ('0','1','2',...).",
          "R resolves variable+level to model.matrix column names internally.",
          "Do NOT include residual_sd here -- use the separate residual_sd argument."
        ),
        required = TRUE
      ),
      residual_sd = ellmer::type_number(
        paste(
          "Residual SD (>= 0). Required for 'identity+normal'; omit for other families.",
          "Set to 0 for within-subjects designs (all residual variance in random effects)."
        ),
        required = FALSE
      )
    ),
    name = "save_coefficients"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id, random_effects) {
      save_random_effects(random_effects = random_effects,
                          session_path = session_env$path,
                          study_id = study_id)
    }),
    description = paste(
      "Step 5 of the step-by-step study creation workflow.",
      "Saves random-effects variances and covariances for a within-subjects or mixed design.",
      "Skip this step for purely between-subjects designs and call simulate_study directly.",
      "Call simulate_study next."
    ),
    arguments = list(
      study_id = ellmer::type_string("Study ID returned by save_description.", required = TRUE),
      random_effects = ellmer::type_object(
        .description = paste(
          "Random-effects covariance structure. Pass a JSON object with a 'type' key.",
          "Three types:",
          "  {type:'sphericity', variance:<sigma^2>} -- all condition variances equal, zero covariances",
          "    in observation space. Use for within-subjects designs when you assume sphericity.",
          "  {type:'compound_symmetry', variance:<sigma^2>, covariance:<sigma_c>} -- equal variances and",
          "    equal covariances. Constraint: covariance > -variance/(k-1) and covariance <= variance.",
          "  {type:'custom', components:[...]} -- fully specified; use for purely between-subjects",
          "    designs or within-continuous-only designs. Use {type:'custom', components:[]} for",
          "    fixed-effects-only (purely between) designs.",
          "Optional 'components' array on sphericity/compound_symmetry: list of {vars, value}",
          "  entries that override or add random slopes (e.g. for within-continuous variables).",
          "  Format: {vars:[{name:'(Intercept)'}], value:<sigma^2>} for variance;",
          "          {vars:[{name:'<within_var>',level:'<key>'}], value:<sigma^2>} for a dummy-code slope.",
          "Between-subjects variables must never appear in random effects."
        ),
        .required              = TRUE,
        .additional_properties = TRUE
      )
    ),
    name = "save_random_effects"
  ))

  # -- Study creation tools (monolithic, rarely used) -------------------------

  chat$register_tool(ellmer::tool(
    .guard(function(study_id, title, context, family, n_subjects, n_reps,
                    subject_id_name, variables, correlations, coefficients,
                    residual_sd = NULL, random_effects) {
      save_study(session_path = session_env$path,
                        study_id = study_id, title = title, context = context,
                        family = family, n_subjects = n_subjects, n_reps = n_reps,
                        subject_id_name = subject_id_name, variables = variables,
                        correlations = correlations, coefficients = coefficients,
                        residual_sd = residual_sd, random_effects = random_effects)
    }),
    description = paste(
      "Save the complete GLMM study design to disk in one call (schema v2).",
      "Rarely used -- prefer the step-by-step tools (save_description, save_correlations,",
      "save_coefficients, save_random_effects, simulate_study) for the standard interactive workflow.",
      "Use this only when the user explicitly wants to skip step-by-step confirmation.",
      "Present the FULL design to the user and obtain explicit confirmation before calling.",
      "Calling again with the same study_id overwrites the design and invalidates any prior simulation."
    ),
    arguments = list(
      study_id = ellmer::type_string(
        "Study ID in the format <slug>_<YYYYMMDD_HHMMSS>. Slug: lowercase, underscores only.",
        required = TRUE
      ),
      title = ellmer::type_string("Study title, 5-10 words.", required = TRUE),
      context = ellmer::type_string(
        "Context paragraph, 3-5 sentences, written as if reporting a real study.",
        required = TRUE
      ),
      family = ellmer::type_enum(
        "Model family and link.",
        values   = c("identity+normal", "logit+binomial", "log+poisson"),
        required = TRUE
      ),
      n_subjects = ellmer::type_integer("Number of subjects (>= 2).", required = TRUE),
      n_reps = ellmer::type_integer(
        paste(
          "Repetitions per balanced within-subject condition combination per subject (>= 1).",
          "Use 1 for standard repeated-measures designs and all between-subjects designs.",
          "Randomly-assigned within variables do not contribute to this count."
        ),
        required = TRUE
      ),
      subject_id_name = ellmer::type_string(
        "Name for the subject ID column. Derive from study context. Must be a valid R identifier.",
        required = TRUE
      ),
      variables = ellmer::type_object(
        .description = paste(
          "Study variables. Keys: 'dependent' (outcome), 'between' (array of between-subjects",
          "variables), 'within' (array of within-subjects variables).",
          "Between binary/categorical: include 'proportion' as a keyed object matching 'categories' (e.g. {'0':0.4,'1':0.6} for binary) summing to 1.0. Never use an array.",
          "Within binary/categorical: include 'assignment' ('balanced' default, or 'random');",
          "omit 'proportion' for balanced; include for random as a keyed object matching 'categories'.",
          "Within continuous: omit 'assignment'."
        ),
        .required              = TRUE,
        .additional_properties = TRUE
      ),
      correlations = ellmer::type_array(
        items = ellmer::type_object(
          .description           = "One correlation pair: vars (two variable names), value.",
          .additional_properties = TRUE
        ),
        description = paste(
          "Off-diagonal correlations between measured variables (between-subjects and within-continuous).",
          "Each entry: {vars: ['A', 'B'], value: r} where r is in (-1, 1).",
          "Within-binary and within-categorical must NOT appear.",
          "Within-continuous variables are independent by default -- only include when substantively motivated.",
          "Use [] when there are no correlations or fewer than 2 eligible variables."
        ),
        required = TRUE
      ),
      coefficients = ellmer::type_array(
        items = ellmer::type_object(
          .description           = "One coefficient entry: vars (variable references), value.",
          .additional_properties = TRUE
        ),
        description = paste(
          "Fixed-effects parameters. Required entries (treatment coding):",
          "- Intercept: {vars: [{name: '(Intercept)'}], value: <number>}",
          "- Continuous: {vars: [{name: '<varname>'}], value: <number>}",
          "- Binary: {vars: [{name: '<varname>', level: '<non-ref-key>'}], value: <number>}",
          "- Categorical: one entry per non-reference level.",
          "- Interaction: {vars: [{...}, {...}], value: <number>}.",
          "The 'level' field is the category key string ('0', '1', '2', ...).",
          "R maps variable+level to model.matrix column names automatically.",
          "Do NOT include residual_sd here -- use the separate residual_sd argument."
        ),
        required = TRUE
      ),
      residual_sd = ellmer::type_number(
        paste(
          "Residual SD (>= 0). Required for 'identity+normal'; omit for other families.",
          "Set to 0 for within-subjects designs (MANOVA approach)."
        ),
        required = FALSE
      ),
      random_effects = ellmer::type_object(
        .description = paste(
          "Random-effects covariance structure. Pass a JSON object with a 'type' key.",
          "Three types:",
          "  {type:'sphericity', variance:<sigma^2>} -- sphericity assumption (equal variances,",
          "    zero covariances in observation space). Use for within-subjects designs.",
          "  {type:'compound_symmetry', variance:<sigma^2>, covariance:<sigma_c>} -- equal variances and",
          "    equal covariances. Constraint: covariance > -variance/(k-1) and covariance <= variance.",
          "  {type:'custom', components:[...]} -- fully specified list of {vars,value} entries.",
          "    Use for purely between-subjects or within-continuous-only designs.",
          "    Use {type:'custom', components:[]} for fixed-effects-only designs.",
          "Optional 'components' on sphericity/compound_symmetry: {vars,value} entries that",
          "  override individual cells or add random slopes for within-continuous variables.",
          "Between-subjects variables must never appear in random effects."
        ),
        .required              = TRUE,
        .additional_properties = TRUE
      )
    ),
    name = "save_study"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id) {
      simulate_study(study_id, session_path = session_env$path)
    }),
    description = paste(
      "Run the R simulation for a study.",
      "Call after save_study or the step-by-step tools have succeeded.",
      "Saves data.csv, regression.txt, and odds_ratios.txt (logit+binomial only).",
      "Returns the full R regression output for the agent to present to the user."
    ),
    arguments = list(
      study_id = ellmer::type_string("Study ID", required = TRUE)
    ),
    name = "simulate_study"
  ))

  # -- Question generation tools ----------------------------------------------

  chat$register_tool(ellmer::tool(
    .guard(function(file_path) {
      load_material(
        file.path(session_env$path, "materials", file_path)
      )
    }),
    description = paste(
      "Read the complete text of a lecture PDF or Markdown file from materials/.",
      "Prefer search_materials when RAG stores are connected.",
      "Use this only when you need the full contents of one specific file."
    ),
    arguments = list(
      file_path = ellmer::type_string("Filename inside materials/.", required = TRUE)
    ),
    name = "load_material"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(query, rag_ids = NULL, n = 5L) {
      ids <- if (is.null(rag_ids) || length(rag_ids) == 0L)
        get_active_rag_ids(session_env$path)
      else
        rag_ids
      if (length(ids) == 0L)
        return(paste(
          "No active RAG stores. Ask the user to activate a store in the state panel,",
          "or provide rag_ids explicitly."
        ))
      retrieve_from_rags(session_env$path, ids, query, n = n)
    }),
    description = paste(
      "Search active RAG stores for content relevant to a query.",
      "Always call this before load_material when RAG stores are active.",
      "Returns the top n chunks from each active store, labelled by source file.",
      "Use load_material only when you need the complete text of one specific file."
    ),
    arguments = list(
      query = ellmer::type_string(
        "Search query phrased as a topic or concept, not a question.",
        required = TRUE
      ),
      rag_ids = ellmer::type_array(
        items       = ellmer::type_string(),
        description = paste(
          "Explicit RAG store IDs to search. Omit to search all active stores.",
          "Use list_rags to see available store IDs."
        ),
        required = FALSE
      ),
      n = ellmer::type_integer(
        "Number of chunks to retrieve per store (default 5).",
        required = FALSE
      )
    ),
    name = "search_materials"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function() {
      rags <- list_rags(session_path = session_env$path)
      jsonlite::toJSON(rags, auto_unbox = TRUE, pretty = TRUE)
    }),
    description = paste(
      "List all RAG stores in the session with their IDs, labels, providers,",
      "active status, and sync status.",
      "Call before search_materials to see which stores are available.",
      "Use the rag_id values when calling search_materials with explicit rag_ids."
    ),
    arguments = list(),
    name = "list_rags"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(rag_ids) {
      delete_rags(session_env$path, rag_ids)
    }),
    description = paste(
      "Delete one or more RAG stores and all their artefacts.",
      "This cannot be undone. Always ask the user to confirm before calling.",
      "Use list_rags to find the exact RAG store IDs if unsure."
    ),
    arguments = list(
      rag_ids = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "RAG store IDs to delete (folder names, e.g. 'bm25_20260509_100000').",
        required    = TRUE
      )
    ),
    name = "delete_rags"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(rag_id, active) {
      rag_dir <- file.path(session_env$path, "rag", rag_id)
      if (!dir.exists(rag_dir))
        return(paste0("RAG not found: ", rag_id))
      set_rag_active(session_env$path, rag_id, active)
      state <- if (isTRUE(active)) "activated" else "deactivated"
      paste0("RAG '", rag_id, "' ", state, ".")
    }),
    description = paste(
      "Activate or deactivate a RAG store.",
      "Active stores are searched automatically by search_materials when no rag_ids are specified.",
      "Use list_rags to see current active status and available store IDs."
    ),
    arguments = list(
      rag_id = ellmer::type_string("RAG store ID to activate or deactivate.", required = TRUE),
      active = ellmer::type_boolean(
        "TRUE to activate the store; FALSE to deactivate it.",
        required = TRUE
      )
    ),
    name = "set_rag_active"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id) {
      load_study_context(study_id, session_path = session_env$path)
    }),
    description = paste(
      "Load an existing study's regression output and context.",
      "Use this before generating questions for a study that was simulated in a previous session."
    ),
    arguments = list(
      study_id = ellmer::type_string("Study ID", required = TRUE)
    ),
    name = "load_study_context"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(question_data) {
      save_question_set(question_data, session_path = session_env$path)
    }),
    description = paste(
      "Validate and save a completed question set as JSON in question_sets/.",
      "Always ask the user for confirmation before calling this tool."
    ),
    arguments = list(
      question_data = ellmer::type_object(
        .description = "Full question set object matching the Question JSON schema in dev/schemas/question.md.",
        .required    = TRUE,
        .additional_properties = TRUE
      )
    ),
    name = "save_question_set"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function() {
      list_materials(session_path = session_env$path)
    }),
    description = paste(
      "List all files currently in the materials/ folder.",
      "Call when the user asks which materials are available, or when they refer",
      "to a file by a partial or ambiguous name.",
      "Do not call at startup -- the startup message already lists uploaded files."
    ),
    arguments = list(),
    name = "list_materials"
  ))

  # -- Discovery tools --------------------------------------------------------

  chat$register_tool(ellmer::tool(
    .guard(function() {
      list_studies(session_path = session_env$path)
    }),
    description = paste(
      "List all studies in the session with their IDs, titles, families,",
      "statuses, and creation timestamps.",
      "Do not call at startup -- the startup message already contains this information."
    ),
    arguments = list(),
    name = "list_studies"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function() {
      list_question_sets(session_path = session_env$path)
    }),
    description = paste(
      "List all question sets in the session with their IDs, topics, linked study IDs,",
      "question counts, and creation timestamps.",
      "Call when the user asks which question sets exist or refers to a set by a partial name."
    ),
    arguments = list(),
    name = "list_question_sets"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(qset_id) {
      load_question_set(qset_id, session_path = session_env$path)
    }),
    description = paste(
      "Load the full contents of a question set by ID, including every question,",
      "its integer ID, choices or correct answer, and explanation.",
      "Always call this before remove_questions or update_questions to discover question IDs."
    ),
    arguments = list(
      qset_id = ellmer::type_string(
        "The question set ID (filename without .json).", required = TRUE
      )
    ),
    name = "load_question_set"
  ))

  # -- Question editing tools -------------------------------------------------

  chat$register_tool(ellmer::tool(
    .guard(function(qset_id, question_ids) {
      remove_questions(qset_id, question_ids, session_path = session_env$path)
    }),
    description = paste(
      "Remove specific questions from a question set by their integer IDs.",
      "Call load_question_set first to discover the IDs."
    ),
    arguments = list(
      qset_id = ellmer::type_string(
        "Question set ID (filename without .json).",
        required = TRUE
      ),
      question_ids = ellmer::type_array(
        items = ellmer::type_integer(),
        description = "Integer IDs of the questions to remove.",
        required = TRUE
      )
    ),
    name = "remove_questions"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(qset_id, questions) {
      update_questions(qset_id, questions, session_path = session_env$path)
    }),
    description = paste(
      "Replace specific questions in a question set (matched by integer 'id').",
      "IMPORTANT: each replacement object must supply ALL fields for its type --",
      "id, type, question, exhibit, explanation, and the type-specific fields",
      "(choices for multiple_choice; correct_answer for numeric).",
      "This is a full replacement, not a partial patch.",
      "Call load_question_set first to discover question IDs and current content."
    ),
    arguments = list(
      qset_id   = ellmer::type_string("Question set ID (filename without .json).",
                                      required = TRUE),
      questions = ellmer::type_array(
        items       = ellmer::type_object(
          .description = paste(
            "Complete question object including 'id' (integer from load_question_set),",
            "type, question, exhibit, explanation, and type-specific fields."
          ),
          .required            = TRUE,
          .additional_properties = TRUE
        ),
        description = "List of complete replacement question objects, each with an integer 'id'.",
        required    = TRUE
      )
    ),
    name = "update_questions"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(qset_id, questions) {
      add_questions(qset_id, questions, session_path = session_env$path)
    }),
    description = paste(
      "Append new questions to an existing question set.",
      "Do NOT include an 'id' field -- IDs are assigned automatically.",
      "Each question must follow the question schema (type, question, exhibit,",
      "explanation, and type-specific fields)."
    ),
    arguments = list(
      qset_id   = ellmer::type_string("Question set ID (filename without .json).",
                                      required = TRUE),
      questions = ellmer::type_array(
        items       = ellmer::type_object(
          .description           = "New question object without an 'id' field.",
          .required              = TRUE,
          .additional_properties = TRUE
        ),
        description = "List of new question objects to append. Omit 'id' -- it is assigned automatically.",
        required    = TRUE
      )
    ),
    name = "add_questions"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(qset_ids) {
      msgs <- vapply(qset_ids, function(id) {
        delete_question_set(id, session_path = session_env$path)
        paste0("'", id, "' deleted.")
      }, character(1L))
      paste(msgs, collapse = "\n")
    }),
    description = paste(
      "Delete one or more question sets (.json files) from the session.",
      "This cannot be undone. Always ask the user to confirm before calling."
    ),
    arguments = list(
      qset_ids = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Question set IDs to delete (filenames without .json).",
        required    = TRUE
      )
    ),
    name = "delete_question_sets"
  ))

  # -- Deletion tools for other artefact types --------------------------------

  chat$register_tool(ellmer::tool(
    .guard(function(material_names) {
      msgs <- vapply(material_names, function(name) {
        delete_material(name, session_path = session_env$path)
      }, character(1L))
      paste(msgs, collapse = "\n")
    }),
    description = paste(
      "Delete one or more material files from the session's materials/ folder.",
      "This cannot be undone. Always ask the user to confirm before calling.",
      "Use list_materials to find the exact filenames if unsure."
    ),
    arguments = list(
      material_names = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Exact filenames inside materials/ to delete (e.g. 'lecture.pdf').",
        required    = TRUE
      )
    ),
    name = "delete_materials"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_ids) {
      msgs <- vapply(study_ids, function(id) {
        delete_study(id, session_path = session_env$path)
      }, character(1L))
      paste(msgs, collapse = "\n")
    }),
    description = paste(
      "Delete one or more study folders and all their simulation artefacts.",
      "This cannot be undone. Always ask the user to confirm before calling.",
      "The tool will report any question sets that still reference the deleted studies."
    ),
    arguments = list(
      study_ids = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Study IDs to delete (folder names, e.g. 'exam_performance_20260506_143012').",
        required    = TRUE
      )
    ),
    name = "delete_studies"
  ))

  # -- Figure tools -----------------------------------------------------------

  chat$register_tool(ellmer::tool(
    .guard(function() {
      list_figures(session_path = session_env$path)
    }),
    description = paste(
      "List the figures in the session's figures/ folder: file name, the path to use",
      "in questions, size, and which questions use each figure (unused = true if none).",
      "Call before referencing, replacing or deleting a figure."
    ),
    arguments = list(),
    name = "list_figures"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(file) {
      figure_tool_result("", file, session_path = session_env$path)
    }),
    description = paste(
      "Look at a figure from figures/ (returns the image).",
      "Call before writing or checking questions about a figure you have not seen",
      "in this conversation."
    ),
    arguments = list(
      file = ellmer::type_string(
        "File name inside figures/ (e.g. 'scatter_hours_score_3fa2c1d9.png'), with or without the 'figures/' prefix.",
        required = TRUE
      )
    ),
    name = "view_figure"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(study_id, type, x, y = NULL, group = NULL, smooth = FALSE,
                    title = NULL, x_label = NULL, y_label = NULL, name = NULL) {
      res <- create_figure(study_id, type, x, y = y, group = group, smooth = smooth,
                           title = title, x_label = x_label, y_label = y_label,
                           name = name, session_path = session_env$path)
      figure_tool_result(
        paste0(if (res$existed) "This figure already existed: " else "Figure saved: ",
               res$path, "\nReference it in a question with: `", res$markdown,
               "`\nThe image follows; check it before using it."),
        res$file, session_path = session_env$path
      )
    }),
    description = paste(
      "Draw a figure from a simulated study's data (studies/<study_id>/data.csv) and save",
      "it to figures/. Fixed plot types, drawn with ggplot2. Returns the path to use in",
      "questions and the image itself. Ask the user which plot they want before calling."
    ),
    arguments = list(
      study_id = ellmer::type_string("ID of a simulated study.", required = TRUE),
      type = ellmer::type_enum(
        paste("Plot type: scatter (x, y numeric), boxplot (x grouping, y numeric),",
              "histogram (x numeric), bar (x grouping; mean of y per group, or counts without y)."),
        values = c("scatter", "boxplot", "histogram", "bar"), required = TRUE
      ),
      x = ellmer::type_string("Column on the x axis (grouping column for boxplot and bar).", required = TRUE),
      y = ellmer::type_string("Column on the y axis (scatter, boxplot; optional for bar).", required = FALSE),
      group = ellmer::type_string("Optional column for colour / fill (e.g. a categorical predictor).", required = FALSE),
      smooth = ellmer::type_boolean("Scatter only: add a least-squares line per group.", required = FALSE),
      title = ellmer::type_string("Optional plot title.", required = FALSE),
      x_label = ellmer::type_string("Optional x axis label (defaults to the column name).", required = FALSE),
      y_label = ellmer::type_string("Optional y axis label.", required = FALSE),
      name = ellmer::type_string("Optional short readable file name, e.g. 'score_by_location'.", required = FALSE)
    ),
    name = "create_figure"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(old, new) {
      replace_figure(old, new, session_path = session_env$path)
    }),
    description = paste(
      "Point every question that uses one figure at another figure (all question sets).",
      "Figures never change once saved: to change a figure, create or upload the new one,",
      "then call this. The old file stays and becomes unused.",
      "Always ask the user to confirm before calling."
    ),
    arguments = list(
      old = ellmer::type_string("File name of the figure to replace.", required = TRUE),
      new = ellmer::type_string("File name of the new figure (must exist in figures/).", required = TRUE)
    ),
    name = "replace_figure"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(files = NULL, unused = FALSE, force = FALSE) {
      if (isTRUE(unused)) return(delete_unused_figures(session_path = session_env$path))
      if (length(files) == 0L) return("Error: give files, or unused = true.")
      delete_figures(files, session_path = session_env$path, force = isTRUE(force))
    }),
    description = paste(
      "Delete figures from figures/. Figures still used by questions are only deleted",
      "with force = true. This cannot be undone. Always ask the user to confirm before calling."
    ),
    arguments = list(
      files = ellmer::type_array(
        items = ellmer::type_string(),
        description = "File names inside figures/ to delete.",
        required = FALSE
      ),
      unused = ellmer::type_boolean("Delete every figure that no question uses (ignores files).", required = FALSE),
      force = ellmer::type_boolean("Also delete figures that questions still use.", required = FALSE)
    ),
    name = "delete_figures"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function() {
      list_exports(session_path = session_env$path)
    }),
    description = paste(
      "List all past exports in the session with their IDs, destination format,",
      "question count, and creation timestamp.",
      "Call when the user asks about previous exports or before deleting one."
    ),
    arguments = list(),
    name = "list_exports"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(export_ids) {
      msgs <- vapply(export_ids, function(id) {
        delete_export(id, session_path = session_env$path)
      }, character(1L))
      paste(msgs, collapse = "\n")
    }),
    description = paste(
      "Delete one or more export folders and all their artefacts.",
      "This cannot be undone. Always ask the user to confirm before calling.",
      "Use list_exports to find the exact export IDs if unsure."
    ),
    arguments = list(
      export_ids = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Export folder names to delete (e.g. 'combined_20260506_143012').",
        required    = TRUE
      )
    ),
    name = "delete_exports"
  ))

  chat$register_tool(ellmer::tool(
    .guard(function(qset_ids, destination, label,
                    include_precision_note = TRUE, answers = TRUE) {
      export_question_sets(
        qset_ids,
        session_path = session_env$path,
        label = label,
        destination = destination,
        include_precision_note = include_precision_note,
        answers = answers
      )
    }),
    description = paste(
      "Export one or more question sets to PDF, TestVision, Canvas, or Markdown.",
      "Merges all questions from the specified sets into a single export.",
      "Always ask the user for a label for the export before calling.",
      "Always confirm the destination format with the user before calling."
    ),
    arguments = list(
      qset_ids    = ellmer::type_array(
        items       = ellmer::type_string(),
        description = "Question set IDs to export.",
        required    = TRUE
      ),
      destination = ellmer::type_enum(
        "Export format", values = c("pdf", "testvision", "canvas", "markdown"), required = TRUE
      ),
      label       = ellmer::type_string(
        description = "Label for the export (e.g. 'Midterm Exam 2026'). Shown on the export card.",
        required    = TRUE
      ),
      include_precision_note = ellmer::type_boolean(
        description = "Whether to include the rounding/precision note below numeric questions. Defaults to TRUE.",
        required    = FALSE
      ),
      answers = ellmer::type_boolean(
        description = "Whether to include correct answers and explanations. Defaults to TRUE. FALSE (a student version) only applies to pdf and markdown; canvas and testvision ignore it and always include the answers.",
        required    = FALSE
      )
    ),
    name = "export_question_sets"
  ))

  chat
}
