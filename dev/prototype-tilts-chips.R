# Prototype 3: Tilt UI — chips (click to cycle)
#
# Each region and sector is a chip. Click cycles through tilt values:
# 0 (neutral) → +1 (prefer) → +2 (strongly prefer) → -2 (strongly avoid)
# → -1 (avoid) → 0. Shift+click reverses.
#
# Run: Rscript dev/prototype-tilts-chips.R
# Serves on port 3838.

library(shiny)

REGIONS <- c(
  "US" = "us", "Europe" = "europe", "Switzerland" = "ch",
  "Emerging" = "em", "Asia-Developed" = "asia_dev", "Japan" = "japan"
)
SECTORS <- c(
  "Technology" = "tech", "Healthcare" = "health", "Energy" = "energy",
  "Financials" = "financials", "Consumer" = "consumer"
)

css <- "
  body { font-family: 'Open Sans', system-ui, sans-serif;
    background: #f9fafb; margin: 0; }
  .tilt-container { max-width: 1100px; margin: 24px auto;
    display: grid; grid-template-columns: 1.1fr 1fr; gap: 32px; }
  h1 { font-size: 20px; margin: 0 0 4px; color: #111827; }
  .subtitle { color: #6b7280; font-size: 13px; margin-bottom: 16px; }
  .hint { color: #9ca3af; font-size: 11px; margin: 6px 0 16px;
    font-style: italic; }
  .card { background: #fff; border: 1px solid #e5e7eb;
    border-radius: 10px; padding: 18px 20px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.03); }

  .section-title { font-size: 11px; font-weight: 600; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.6px;
    margin: 18px 0 10px; padding-bottom: 6px;
    border-bottom: 1px solid #e5e7eb; }
  .section-title:first-child { margin-top: 0; }

  /* Chip grid */
  .chip-grid { display: flex; flex-wrap: wrap; gap: 8px; }
  .chip { display: inline-flex; align-items: center; gap: 8px;
    padding: 8px 14px; border-radius: 18px;
    border: 1.5px solid #e5e7eb; background: #fff;
    font-size: 13px; color: #374151; font-weight: 500;
    cursor: pointer; user-select: none;
    transition: all 0.15s; position: relative; }
  .chip:hover { background: #f9fafb; border-color: #d1d5db;
    transform: translateY(-1px); }
  .chip.pos-1 { background: #dcfce7; border-color: #86efac;
    color: #166534; }
  .chip.pos-2 { background: #22c55e; border-color: #16a34a;
    color: #fff; font-weight: 600;
    box-shadow: 0 2px 6px rgba(34,197,94,0.25); }
  .chip.neg-1 { background: #fee2e2; border-color: #fca5a5;
    color: #991b1b; }
  .chip.neg-2 { background: #ef4444; border-color: #dc2626;
    color: #fff; font-weight: 600;
    box-shadow: 0 2px 6px rgba(239,68,68,0.25); }

  .chip .state-icon { width: 20px; height: 20px; border-radius: 50%;
    background: #f3f4f6; display: inline-flex; align-items: center;
    justify-content: center; font-size: 11px; font-weight: 700;
    color: #9ca3af; transition: all 0.15s; }
  .chip.pos-1 .state-icon { background: rgba(255,255,255,0.7);
    color: #166534; }
  .chip.pos-2 .state-icon { background: rgba(255,255,255,0.95);
    color: #14532d; }
  .chip.neg-1 .state-icon { background: rgba(255,255,255,0.7);
    color: #991b1b; }
  .chip.neg-2 .state-icon { background: rgba(255,255,255,0.95);
    color: #7f1d1d; }

  .reset-btn { padding: 6px 14px; font-size: 12px;
    border: 1px solid #d1d5db; background: #fff; color: #374151;
    border-radius: 6px; cursor: pointer; margin-top: 14px; }
  .reset-btn:hover { background: #f3f4f6; }

  /* Legend */
  .legend { display: flex; gap: 12px; margin: 10px 0 16px;
    font-size: 11px; color: #6b7280; flex-wrap: wrap; }
  .legend-item { display: flex; align-items: center; gap: 4px; }
  .legend-swatch { width: 14px; height: 14px; border-radius: 3px;
    border: 1px solid; }
  .legend-swatch.n2 { background: #ef4444; border-color: #dc2626; }
  .legend-swatch.n1 { background: #fee2e2; border-color: #fca5a5; }
  .legend-swatch.n0 { background: #fff; border-color: #e5e7eb; }
  .legend-swatch.p1 { background: #dcfce7; border-color: #86efac; }
  .legend-swatch.p2 { background: #22c55e; border-color: #16a34a; }

  /* Right panel */
  .readout-section h3 { font-size: 11px; font-weight: 600;
    color: #6b7280; text-transform: uppercase; letter-spacing: 0.6px;
    margin: 0 0 10px; }
  .readout-section { margin-bottom: 20px; }
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
  .summary-txt { font-size: 12px; color: #374151; line-height: 1.5; }
"

js <- "
  function applyTilt(axis, key, val) {
    Shiny.setInputValue('tilt_ctrl',
      {axis: axis, key: key, value: val, t: Date.now()},
      {priority: 'event'});
  }
  function cycleVal(v, back) {
    var seq = back ? [0, -1, -2, 2, 1, 0] : [0, 1, 2, -2, -1, 0];
    var i = seq.indexOf(v);
    return seq[(i + 1) % seq.length];
  }
  function renderChip($el, v) {
    $el.removeClass('pos-1 pos-2 neg-1 neg-2');
    if (v > 0) $el.addClass('pos-' + v);
    else if (v < 0) $el.addClass('neg-' + Math.abs(v));
    var label = v === 0 ? '\u25CB' :
      (v > 0 ? '+' + v : '\u2212' + Math.abs(v));
    $el.find('.state-icon').text(label);
  }
  $(function() {
    $(document).on('click', '.chip', function(e) {
      var cur = parseInt($(this).data('val')) || 0;
      var next = cycleVal(cur, e.shiftKey);
      $(this).data('val', next);
      renderChip($(this), next);
      applyTilt($(this).data('axis'), $(this).data('key'), next);
    });
    $(document).on('click', '.reset-btn', function() {
      $('.chip').each(function() {
        $(this).data('val', 0);
        renderChip($(this), 0);
        applyTilt($(this).data('axis'), $(this).data('key'), 0);
      });
    });
  });
"

chip <- function(axis, key, label, init = 0) {
  klass <- if (init == 0) "" else if (init > 0) paste0("pos-", init)
    else paste0("neg-", abs(init))
  state_label <- if (init == 0) "\u25CB" else
    if (init > 0) paste0("+", init) else paste0("\u2212", abs(init))
  tags$button(class = paste("chip", klass),
    `data-axis` = axis, `data-key` = key, `data-val` = init,
    tags$span(class = "state-icon", state_label),
    label
  )
}

legend_html <- div(class = "legend",
  div(class = "legend-item",
    div(class = "legend-swatch n2"), "Strongly avoid"),
  div(class = "legend-item",
    div(class = "legend-swatch n1"), "Avoid"),
  div(class = "legend-item",
    div(class = "legend-swatch n0"), "Neutral"),
  div(class = "legend-item",
    div(class = "legend-swatch p1"), "Prefer"),
  div(class = "legend-item",
    div(class = "legend-swatch p2"), "Strongly prefer")
)

ui <- fluidPage(
  tags$head(tags$style(css)),
  tags$div(class = "tilt-container",
    tags$div(class = "card",
      tags$h1("Preferences"),
      tags$div(class = "subtitle",
        "Click a chip to cycle: neutral \u2192 prefer \u2192 strongly prefer \u2192 strongly avoid \u2192 avoid. Shift+click goes back."),
      legend_html,

      div(class = "section-title", "Regional preferences"),
      div(class = "chip-grid",
        lapply(names(REGIONS), function(nm) {
          chip("r", REGIONS[[nm]], nm)
        })
      ),

      div(class = "section-title", "Sector preferences"),
      div(class = "chip-grid",
        lapply(names(SECTORS), function(nm) {
          chip("s", SECTORS[[nm]], nm)
        })
      ),

      tags$button(class = "reset-btn", "Reset all preferences")
    ),

    tags$div(class = "card",
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
        div(class = "summary-txt", textOutput("summary_txt"))
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
    v <- as.integer(m$value); key <- m$key
    if (m$axis == "r") tilts$r[[key]] <- v
    if (m$axis == "s") tilts$s[[key]] <- v
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
  output$region_bars <- renderUI(render_bars(as.list(tilts$r), REGIONS))
  output$sector_bars <- renderUI(render_bars(as.list(tilts$s), SECTORS))
  output$summary_txt <- renderText({
    all_vals <- c(tilts$r, tilts$s)
    if (sum(all_vals != 0) == 0)
      return("No tilts set — optimizer will pick purely on risk/return.")
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
