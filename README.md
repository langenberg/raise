# RAISE — R AI Statistics Exam Generator

RAISE is an R package that helps university lecturers generate realistic exam questions grounded in simulated statistical output. An LLM agent guides the lecturer through two workflows: designing a hypothetical study and simulating its dataset, then generating exam questions anchored in the resulting regression output. Questions can include figures and are exported to PDF, Canvas, TestVision and Markdown.

## Requirements

- R >= 4.1 (developed with R 4.6)
- Credentials for an LLM provider supported by [ellmer](https://ellmer.tidyverse.org). The default is Anthropic: set `ANTHROPIC_API_KEY` in `~/.Renviron`. Other providers use their own variables (e.g. `OPENAI_API_KEY`).
- shinychat 0.3.x (pinned in `renv.lock`; shinychat 0.4+ renders the chat differently)
- LaTeX for PDF export

## Setup

```r
install.packages("renv")
renv::restore()        # restores the locked package library (incl. shinychat 0.3.0)
```

## Running the app

```r
devtools::load_all()   # from the package folder; or library(RAISE) once installed
run_raise("my_session")
```

The app opens in your browser on the given session folder (created if needed).

## Choosing the model

```r
run_raise("my_session")                                         # Anthropic, claude-sonnet-4-6
run_raise("my_session", provider = "openai", model = "gpt-4.1") # any ellmer provider
run_raise("my_session", provider = "openai/gpt-4.1")            # same, as one string
run_raise("my_session", provider = ellmer::chat_openai(model = "gpt-4.1", base_url = "..."))
run_raise("my_session", provider = "ollama/qwen3:14b",          # local model and local
          embed_provider = "ollama")                            # embeddings for RAG stores
```

`provider` is a provider name (the suffix of an `ellmer::chat_*()` function), `"provider/model"`, an ellmer `Chat` object, or a function returning one. `embed_provider` sets the default backend for new RAG stores: `"bm25"` (default, keyword search, no setup) or a `ragnar::embed_*()` provider. The same `provider` / `model` arguments work for `raise_chat()` (scripted use) and `check_api_key()`.

## Documentation

See the vignette `vignette("raise-agent", package = "RAISE")`, also linked from the app's sidebar.
