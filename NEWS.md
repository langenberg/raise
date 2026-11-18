# RAISE 0.3.0

Not released yet.

# RAISE 0.2.0

Released 2026-11-05.

### New features

* **Any LLM provider.** `run_raise()`, `raise_chat()` and `check_api_key()` take `provider` (an ellmer provider name such as `"openai"`, `"provider/model"`, an ellmer `Chat` object, or a function) and `model`. The default stays Anthropic with `claude-sonnet-4-6`. New `new_llm_chat()`.
* **Any embedding provider for RAG stores.** `run_raise(embed_provider =, embed_model =)` sets the default backend in the "Create RAG store" dialog (`"bm25"` by default); any `ragnar::embed_*()` provider works. New `embed_function()`; `build_rag_store()` has defaults for `provider` and `settings`.
* **Figures in questions.** Session-wide `figures/` folder with content-hash file names; `![alt](figures/<file>)` in any question field; upload in the sidebar (the agent sees uploaded images); agent tools `create_figure` (plots from a study's data), `view_figure`, `list_figures`, `replace_figure`, `delete_figures`; figures in PDF, Canvas, TestVision and Markdown exports. New functions `add_figure()`, `create_figure()`, `list_figures()`, `delete_figures()`, `delete_unused_figures()`, `replace_figure()`.
* **Math** renders in the chat and in all Show modals (LaTeX inside `$...$` is protected from Markdown).
* **Materials:** Show button (PDF page by page at full width with PDF.js; Markdown rendered).
* **Exports:** `answers = FALSE` is ignored with a warning for Canvas/TestVision instead of failing; the app shows a notification, export errors are shown, and the export's Show modal says whether answers are included. Markdown exports with figures download as a ZIP; `questions_to_markdown(figures = "embed")`.
* **Study view:** note explaining correlations of binary/categorical variables (latent score, category order).

### Bug fixes

* Simulation: continuous predictors were stored shifted by their own mean (designed mean 15 became ~30; SD and slopes were right, the intercept of the fitted model was not). Studies simulated before this fix keep the shifted data until re-simulated.
* Question sets: every edit turned `"exhibit": null` into `{}`, after which `load_question_set()` failed. Writes now keep `null`; old files load again.
* RAG stores: with duckdb 2.0, building a store and then searching it in the same R session failed; deleting a store now closes its connection.
* Chat: a figure tool result crashed the stream in shinychat 0.3 ("No method asJSON S3 class: S7_object"); math typesetting no longer stalls after tool calls with images.
* Token/cost display disappeared after the first UI notification (ellmer 0.5 `get_tokens()`).
* Export dialog: checkbox labels use the full width.

### Dependencies

* R 4.6.1; `renv.lock` updated. shinychat pinned to 0.3.0 (0.5.0 evaluated: no math support, see `dev/status.md`). New imports: `DBI`, `base64enc`, `ggplot2`; `magick` suggested (downscales images sent to the model).
