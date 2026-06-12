# Prototype 1: Tilt UI — sliders in accordion
#
# Run: Rscript dev/prototype-tilts-sliders.R
# Serves on port 3838.

library(shiny)

REGIONS <- c(
  "US" = "us",
  "Europe" = "europe",
  "Switzerland" = "ch",
  "Emerging" = "em",
  "Asia-Developed" = "asia_dev",
  "Japan" = "japan"
)
SECTORS <- c(
  "Technology" = "tech",
  "Healthcare" = "health",
  "Energy" = "energy",
  "Financials" = "financials",
  "Consumer" = "consumer"
)

css <- "
  body { font-family: 'Open Sans', system-ui, sans-serif;
    background: #f9fafb; margin: 0; }
  .tilt-container { max-width: 1100px; margin: 24px auto;
    display: grid; grid-template-columns: 1.2fr 1fr; gap: 32px; }
  h1 { font-size: 20px; margin: 0 0 4px; color: #111827; }
  .subtitle { color: #6b7280; font-size: 13px; margin-bottom: 20px; }
  .card { background: #fff; border: 1px solid #e5e7eb;
    border-radius: 10px; padding: 18px 20px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.03); }
  .section-title { font-size: 11px; font-weight: 600; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.6px;
    margin: 0 0 12px; padding-bottom: 6px;
    border-bottom: 1px solid #e5e7eb; }
  details { margin-bottom: 14px; }
  details[open] summary { margin-bottom: 8px; }
  summary { cursor: pointer; font-size: 13px; font-weight: 600;
    color: #374151; padding: 6px 0; list-style: none; }
  summary::-webkit-details-marker { display: none; }
  summary:before { content: '›'; display: inline-block;
    margin-right: 8px; transform: rotate(0deg);
    transition: transform 0.2s; color: #9ca3af; font-size: 18px;
    line-height: 0.5; }
  details[open] summary:before { transform: rotate(90deg); }

  .tilt-row { display: grid; grid-template-columns: 110px 1fr 52px;
    align-items: center; gap: 10px; margin: 8px 0;
    padding: 4px 0; }
  .tilt-label { font-size: 13px; color: #374151; font-weight: 500; }
  .tilt-slider-wrap { position: relative; }
  .tilt-slider { width: 100%; -webkit-appearance: none;
    appearance: none; height: 6px; border-radius: 3px;
    background: linear-gradient(90deg,
      #fca5a5 0%, #fca5a5 25%,
      #e5e7eb 25%, #e5e7eb 75%,
      #86efac 75%, #86efac 100%);
    outline: none; cursor: pointer; }
  .tilt-slider::-webkit-slider-thumb { -webkit-appearance: none;
    width: 18px; height: 18px; border-radius: 50%;
    background: #fff; border: 2px solid #3b82f6;
    cursor: pointer; box-shadow: 0 1px 3px rgba(0,0,0,0.2); }
  .tilt-slider::-moz-range-thumb { width: 18px; height: 18px;
    border-radius: 50%; background: #fff;
    border: 2px solid #3b82f6; cursor: pointer; }
  .tilt-value { font-size: 12px; font-weight: 600;
    text-align: center; padding: 3px 0; border-radius: 4px;
    min-width: 44px; }
  .tilt-value.neg { color: #991b1b; background: #fee2e2; }
  .tilt-value.pos { color: #166534; background: #dcfce7; }
  .tilt-value.neu { color: #6b7280; background: #f3f4f6; }
  .tilt-ticks { display: flex; justify-content: space-between;
    font-size: 9px; color: #9ca3af; margin-top: 2px;
    padding: 0 2px; }

  .reset-btn { padding: 6px 14px; font-size: 12px;
    border: 1px solid #d1d5db; background: #fff; color: #374151;
    border-radius: 6px; cursor: pointer; margin-top: 8px; }
  .reset-btn:hover { background: #f3f4f6; }

  /* Right panel: readout */
  #readout { display: flex; flex-direction: column; gap: 20px; }
  .readout-section h3 { font-size: 11px; font-weight: 600;
    color: #6b7280; text-transform: uppercase; letter-spacing: 0.6px;
    margin: 0 0 10px; }
  .bar-row { display: grid; grid-template-columns: 110px 1fr;
    gap: 12px; align-items: center; margin: 6px 0; }
  .bar-label { font-size: 12px; color: #374151; }
  .bar-track { position: relative; height: 14px;
    background: #f3f4f6; border-radius: 3px; overflow: hidden; }
  .bar-track:before { content: ''; position: absolute;
    left: 50%; top: 0; bottom: 0; width: 1px; background: #d1d5db; }
  .bar-fill { position: absolute; top: 0; bottom: 0;
    border-radius: 3px; transition: all 0.18s; }
  .bar-fill.pos { left: 50%; background: #22c55e; }
  .bar-fill.neg { right: 50%; background: #ef4444; }
"

js <- "
  function applyTilt(param, value) {
    Shiny.setInputValue('tilt_ctrl',
      {param: param, value: value, t: Date.now()},
      {priority: 'event'});
  }
  $(function() {
    $(document).on('input', '.tilt-slider', function() {
      var v = parseInt(this.value);
      var $val = $(this).closest('.tilt-row').find('.tilt-value');
      var label = v === 0 ? '0' : (v > 0 ? '+' + v : '' + v);
      $val.text(label);
      $val.removeClass('neg pos neu')
        .addClass(v < 0 ? 'neg' : (v > 0 ? 'pos' : 'neu'));
      applyTilt($(this).data('param'), v);
    });
    $(document).on('click', '.reset-btn', function() {
      $('.tilt-slider').each(function() {
        $(this).val(0).trigger('input');
      });
    });
  });
"

tilt_row <- function(id, label, init = 0) {
  klass <- if (init == 0) "neu" else if (init > 0) "pos" else "neg"
  lab <- if (init == 0) "0" else
    if (init > 0) paste0("+", init) else as.character(init)
  div(class = "tilt-row",
    div(class = "tilt-label", label),
    div(class = "tilt-slider-wrap",
      tags$input(type = "range", class = "tilt-slider",
        `data-param` = id, min = -2, max = 2, step = 1, value = init)
    ),
    div(class = paste("tilt-value", klass), lab)
  )
}

ui <- fluidPage(
  tags$head(tags$style(css)),
  tags$div(class = "tilt-container",
    # Left: tilt controls
    tags$div(class = "card",
      tags$h1("Preferences"),
      tags$div(class = "subtitle",
        "Tilt the optimizer toward what you like or avoid."),
      tags$details(open = NA,
        tags$summary("Regional preferences"),
        lapply(names(REGIONS), function(nm) {
          tilt_row(paste0("r_", REGIONS[[nm]]), nm)
        })
      ),
      tags$details(open = NA,
        tags$summary("Sector preferences"),
        lapply(names(SECTORS), function(nm) {
          tilt_row(paste0("s_", SECTORS[[nm]]), nm)
        })
      ),
      tags$div(
        tags$div(class = "tilt-ticks",
          style = "max-width: 400px; margin: 10px auto 4px;",
          tags$span("Strongly avoid"),
          tags$span("Avoid"),
          tags$span("Neutral"),
          tags$span("Prefer"),
          tags$span("Strongly prefer")
        )
      ),
      tags$button(class = "reset-btn", "Reset all preferences")
    ),

    # Right: live readout
    tags$div(class = "card",
      tags$div(id = "readout",
        tags$div(class = "readout-section",
          tags$h3("Active tilts — regions"),
          uiOutput("region_bars")
        ),
        tags$div(class = "readout-section",
          tags$h3("Active tilts — sectors"),
          uiOutput("sector_bars")
        ),
        tags$div(class = "readout-section",
          tags$h3("Summary"),
          textOutput("summary_txt", inline = FALSE)
        )
      )
    )
  ),
  tags$script(HTML(js))
)

server <- function(input, output, session) {
  tilts <- reactiveValues(
    r = setNames(rep(0, length(REGIONS)), REGIONS),
    s = setNames(rep(0, length(SECTORS)), SECTORS)
  )

  observeEvent(input$tilt_ctrl, {
    m <- input$tilt_ctrl
    if (is.null(m)) return()
    key <- substr(m$param, 1, 1)
    name <- substring(m$param, 3)
    v <- as.integer(m$value)
    if (key == "r") tilts$r[[name]] <- v
    if (key == "s") tilts$s[[name]] <- v
  })

  render_bars <- function(vals, labels) {
    rows <- lapply(seq_along(vals), function(i) {
      v <- vals[[i]]
      lab <- names(labels)[i]
      pct <- abs(v) / 2 * 50
      fill <- if (v == 0) "" else if (v > 0) {
        sprintf("<div class='bar-fill pos' style='width:%.1f%%'></div>", pct)
      } else {
        sprintf("<div class='bar-fill neg' style='width:%.1f%%'></div>", pct)
      }
      sprintf(
        "<div class='bar-row'><div class='bar-label'>%s</div>
         <div class='bar-track'>%s</div></div>", lab, fill
      )
    })
    HTML(paste(rows, collapse = ""))
  }

  output$region_bars <- renderUI({
    render_bars(as.list(tilts$r), REGIONS)
  })
  output$sector_bars <- renderUI({
    render_bars(as.list(tilts$s), SECTORS)
  })
  output$summary_txt <- renderText({
    all_vals <- c(tilts$r, tilts$s)
    active <- sum(all_vals != 0)
    if (active == 0) return("No tilts set — optimizer will pick purely on risk/return.")
    pref <- names(all_vals)[all_vals > 0]
    avoid <- names(all_vals)[all_vals < 0]
    parts <- c()
    if (length(pref))
      parts <- c(parts, paste("Leaning toward:", paste(pref, collapse = ", ")))
    if (length(avoid))
      parts <- c(parts, paste("Avoiding:", paste(avoid, collapse = ", ")))
    paste(parts, collapse = ". ")
  })
}

options(shiny.port = 3838, shiny.host = "0.0.0.0")
shinyApp(ui, server)
