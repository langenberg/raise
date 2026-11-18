raise_ui <- function() {
  shiny::fluidPage(
    title = "RAISE",
    theme = bslib::bs_theme(bootswatch = "zephyr"),
    shiny::withMathJax(),
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        "
        MathJax.Hub.Config({
          tex2jax: {
            inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
            displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
            processEscapes: true
          }
        });
        Shiny.addCustomMessageHandler('mathjax-typeset', function(message) {
          if (window.MathJax) {
            MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          }
        });
      "
      )),
      htmltools::htmlDependency(
        "raise-math", "1.0",
        src = system.file("www", package = "RAISE"),
        script = c("raise-math.js", "raise-pdf.js")
      ),
      shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.min.js"),
      shiny::tags$style(shiny::HTML(
        "
        body { font-family: 'Helvetica Neue', Arial, sans-serif; }
        .status-badge { display: inline-block; padding: 2px 8px; border-radius: 10px;
                        font-size: 0.8em; font-weight: bold; margin-left: 6px; }
        .badge-simulated   { background: #d4edda; color: #155724; }
        .badge-coefficients{ background: #cce5ff; color: #004085; }
        .badge-correlations{ background: #fff3cd; color: #856404; }
        .badge-description { background: #f8d7da; color: #721c24; }
        .badge-unknown     { background: #e2e3e5; color: #383d41; }
        #state_panel { padding-right: 4px; }
        #state_panel_col h5 { position: sticky; top: 0; background: white; z-index: 1; padding: 4px 0; }
shiny-chat-container { width: 100% !important; }
        shiny-chat-container pre { white-space: pre-wrap; word-break: break-word; overflow-x: auto; }
        shiny-chat-messages { margin-right: 0 !important; overflow-x: hidden !important; }
        .rag-sync-badge {
          font-size: 0.75em; padding: 1px 5px; border-radius: 8px; margin-left: 4px;
        }
        .rag-sync-ok   { background: #d4edda; color: #155724; }
        .rag-sync-warn { background: #fff3cd; color: #856404; }
        .raise-choice-md { flex: 1; }
        .raise-choice-md p { margin: 0; }
        .raise-md > p:first-child { margin-top: 0; }
        .raise-md > p:last-child  { margin-bottom: 0; }
        .raise-md img, .raise-choice-md img { max-width: 100%; max-height: 360px; }
        shiny-chat-messages img { max-width: 100%; max-height: 420px; }
      "
      ))
    ),
    shiny::tags$script(shiny::HTML(
      "
      $(document).on('keypress', '#session_path_input', function(e) {
        if (e.which == 13) {
          $('#open_session_btn').click();
        }
      });
    "
    )),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Session"),
        shiny::textInput(
          "session_path_input",
          value = "mysession",
          label = NULL,
          placeholder = "Path to session folder..."
        ),
        shiny::tags$div(
          style = "display: flex; gap: 4px; margin-bottom: 4px;",
          shinyFiles::shinyDirButton(
            "browse_session",
            "Browse...",
            title = "Select session folder",
            class = "btn-default btn-sm"
          ),
          shiny::actionButton(
            "open_session_btn",
            "Open",
            class = "btn-primary btn-sm"
          )
        ),
        shiny::uiOutput("clear_chat_ui"),
        shiny::uiOutput("usage_ui"),
        shiny::tags$hr(),
        shiny::h4("Documentation"),
        shiny::tags$ul(
          style = "padding-left: 16px; margin-bottom: 4px; font-size: 0.9em;",
          shiny::tags$li(
            shiny::tags$a(
              "How to use RAISE",
              href = "docs/raise-agent.html",
              target = "_blank"
            )
          )
        ),
        shiny::tags$hr(),
        shiny::uiOutput("materials_section"),
        shiny::tags$hr(),
        shiny::uiOutput("figures_section"),
        shiny::tags$hr(),
        shiny::uiOutput("rag_section")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::tags$div(
          style = "display: flex; height: 95vh; gap: 0; overflow: hidden;",
          shiny::tags$div(
            class = "html-fill-container",
            style = "flex: 7; min-width: 0; overflow: hidden; display: flex; flex-direction: column;",
            shinychat::chat_mod_ui("chat")
          ),
          shiny::tags$div(
            id = "state_panel_col",
            style = "flex: 5; min-width: 0; overflow-y: auto; overflow-x: hidden; padding-left: 12px;",
            shiny::uiOutput("state_panel")
          )
        )
      )
    )
  )
}
