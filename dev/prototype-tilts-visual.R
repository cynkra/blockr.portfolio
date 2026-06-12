# Prototype 2: Tilt UI — visual (world map + sector bars)
#
# Click a region on the map to cycle its tilt: 0 → +1 → +2 → -2 → -1 → 0
# Click a sector bar to cycle its tilt similarly.
# Shift-click to go backwards.
#
# Run: Rscript dev/prototype-tilts-visual.R
# Serves on port 3838.

library(shiny)
library(echarts4r)

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

# Country → region mapping for the map click handler.
COUNTRY_REGION <- list(
  us = c("United States"),
  europe = c("France", "Germany", "United Kingdom", "Spain", "Italy",
    "Netherlands", "Belgium", "Poland", "Austria", "Denmark", "Sweden",
    "Norway", "Finland", "Ireland", "Portugal", "Greece", "Czech Rep.",
    "Hungary", "Slovakia", "Slovenia", "Croatia", "Romania", "Bulgaria",
    "Lithuania", "Latvia", "Estonia", "Luxembourg"),
  ch = c("Switzerland"),
  em = c("China", "India", "Brazil", "South Africa", "Russia",
    "Indonesia", "Malaysia", "Thailand", "Mexico", "Turkey",
    "Vietnam", "Philippines", "Colombia", "Chile", "Peru", "Egypt"),
  asia_dev = c("Australia", "New Zealand", "South Korea", "Singapore"),
  japan = c("Japan")
)

# Flip to country → region lookup
country_to_region <- local({
  out <- character(0)
  for (rg in names(COUNTRY_REGION)) {
    for (co in COUNTRY_REGION[[rg]]) out[[co]] <- rg
  }
  out
})

css <- "
  body { font-family: 'Open Sans', system-ui, sans-serif;
    background: #f9fafb; margin: 0; }
  .tilt-container { max-width: 1200px; margin: 24px auto;
    display: grid; grid-template-columns: 1.3fr 1fr; gap: 32px; }
  h1 { font-size: 20px; margin: 0 0 4px; color: #111827; }
  .subtitle { color: #6b7280; font-size: 13px; margin-bottom: 16px; }
  .hint { color: #9ca3af; font-size: 11px; margin: 4px 0 16px;
    font-style: italic; }
  .card { background: #fff; border: 1px solid #e5e7eb;
    border-radius: 10px; padding: 18px 20px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.03); }

  .section-title { font-size: 11px; font-weight: 600; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.6px;
    margin: 18px 0 10px; padding-bottom: 6px;
    border-bottom: 1px solid #e5e7eb; }

  /* Region chips on the map legend */
  .region-legend { display: flex; flex-wrap: wrap; gap: 6px;
    margin-top: 6px; }
  .region-chip { display: inline-flex; align-items: center; gap: 6px;
    padding: 4px 9px; border-radius: 14px; font-size: 12px;
    border: 1px solid #e5e7eb; background: #fff; color: #374151;
    cursor: pointer; user-select: none;
    transition: all 0.15s; }
  .region-chip:hover { background: #f3f4f6; }
  .region-chip.pos-1 { background: #dcfce7; border-color: #86efac;
    color: #166534; }
  .region-chip.pos-2 { background: #86efac; border-color: #22c55e;
    color: #14532d; font-weight: 600; }
  .region-chip.neg-1 { background: #fee2e2; border-color: #fca5a5;
    color: #991b1b; }
  .region-chip.neg-2 { background: #fca5a5; border-color: #ef4444;
    color: #7f1d1d; font-weight: 600; }
  .region-chip .dot { width: 16px; height: 16px; border-radius: 50%;
    background: #e5e7eb; display: inline-flex; align-items: center;
    justify-content: center; font-size: 10px; font-weight: 700;
    color: #6b7280; }
  .region-chip.pos-1 .dot, .region-chip.pos-2 .dot {
    background: #fff; color: #166534; }
  .region-chip.neg-1 .dot, .region-chip.neg-2 .dot {
    background: #fff; color: #991b1b; }

  /* Sector bars */
  .sector-row { display: grid;
    grid-template-columns: 110px 1fr 40px; gap: 10px;
    align-items: center; margin: 8px 0; cursor: pointer; padding: 3px 0; }
  .sector-row:hover .sector-track { background: #e5e7eb; }
  .sector-label { font-size: 13px; color: #374151; font-weight: 500; }
  .sector-track { position: relative; height: 26px;
    background: #f3f4f6; border-radius: 4px;
    transition: background 0.12s; }
  .sector-track:before { content: ''; position: absolute;
    left: 50%; top: 2px; bottom: 2px; width: 1px; background: #d1d5db; }
  .sector-fill { position: absolute; top: 2px; bottom: 2px;
    border-radius: 3px; transition: all 0.2s ease; }
  .sector-fill.pos { left: 50%; background: linear-gradient(90deg,
    #86efac, #22c55e); }
  .sector-fill.neg { right: 50%; background: linear-gradient(90deg,
    #ef4444, #fca5a5); }
  .sector-val { font-size: 12px; font-weight: 600;
    text-align: center; min-width: 32px; }
  .sector-val.neg { color: #991b1b; }
  .sector-val.pos { color: #166534; }
  .sector-val.neu { color: #9ca3af; }

  .reset-btn { padding: 6px 14px; font-size: 12px;
    border: 1px solid #d1d5db; background: #fff; color: #374151;
    border-radius: 6px; cursor: pointer; margin-top: 8px; }
  .reset-btn:hover { background: #f3f4f6; }

  /* Right panel: readout */
  #readout-scroll { max-height: 580px; overflow-y: auto;
    padding-right: 6px; }
  .readout-section h3 { font-size: 11px; font-weight: 600;
    color: #6b7280; text-transform: uppercase; letter-spacing: 0.6px;
    margin: 0 0 10px; }
  .bar-row { display: grid; grid-template-columns: 110px 1fr;
    gap: 12px; align-items: center; margin: 6px 0; }
  .bar-label { font-size: 12px; color: #374151; }
  .bar-track { position: relative; height: 12px;
    background: #f3f4f6; border-radius: 3px; overflow: hidden; }
  .bar-track:before { content: ''; position: absolute;
    left: 50%; top: 0; bottom: 0; width: 1px; background: #d1d5db; }
  .bar-fill { position: absolute; top: 0; bottom: 0;
    border-radius: 3px; transition: all 0.18s; }
  .bar-fill.pos { left: 50%; background: #22c55e; }
  .bar-fill.neg { right: 50%; background: #ef4444; }
  .summary-txt { font-size: 12px; color: #374151; line-height: 1.5;
    margin-top: 4px; }
"

js <- sprintf("
  var COUNTRY_TO_REGION = %s;
  function applyTilt(axis, key, val) {
    Shiny.setInputValue('tilt_ctrl',
      {axis: axis, key: key, value: val, t: Date.now()},
      {priority: 'event'});
  }
  function cycleVal(v, back) {
    // 0 → +1 → +2 → -2 → -1 → 0
    var seq = back ? [0, -1, -2, 2, 1, 0] : [0, 1, 2, -2, -1, 0];
    var i = seq.indexOf(v);
    return seq[(i + 1) %% seq.length];
  }
  $(function() {
    // Sector click
    $(document).on('click', '.sector-row', function(e) {
      var cur = parseInt($(this).data('val')) || 0;
      var next = cycleVal(cur, e.shiftKey);
      $(this).data('val', next);
      applyTilt('s', $(this).data('key'), next);
      renderSector($(this), next);
    });
    // Region chip click
    $(document).on('click', '.region-chip', function(e) {
      var cur = parseInt($(this).data('val')) || 0;
      var next = cycleVal(cur, e.shiftKey);
      $(this).data('val', next);
      applyTilt('r', $(this).data('key'), next);
      renderChip($(this), next);
    });
    $(document).on('click', '.reset-btn', function() {
      $('.sector-row').each(function() {
        $(this).data('val', 0);
        applyTilt('s', $(this).data('key'), 0);
        renderSector($(this), 0);
      });
      $('.region-chip').each(function() {
        $(this).data('val', 0);
        applyTilt('r', $(this).data('key'), 0);
        renderChip($(this), 0);
      });
    });
  });
  function renderChip($el, v) {
    $el.removeClass('pos-1 pos-2 neg-1 neg-2');
    if (v > 0) $el.addClass('pos-' + v);
    else if (v < 0) $el.addClass('neg-' + Math.abs(v));
    var label = v === 0 ? '○' : (v > 0 ? '+' + v : '' + v);
    $el.find('.dot').text(label);
  }
  function renderSector($el, v) {
    var $fill = $el.find('.sector-fill');
    var $val = $el.find('.sector-val');
    $fill.removeClass('pos neg').css('width', '0%%');
    var pct = Math.abs(v) / 2 * 50;
    if (v > 0) $fill.addClass('pos').css('width', pct + '%%');
    else if (v < 0) $fill.addClass('neg').css('width', pct + '%%');
    var label = v === 0 ? '0' : (v > 0 ? '+' + v : '' + v);
    $val.text(label).removeClass('neg pos neu')
      .addClass(v < 0 ? 'neg' : (v > 0 ? 'pos' : 'neu'));
  }
  // Bind map click handler (set up after echarts renders)
  $(document).on('shiny:connected', function() {
    setTimeout(function() {
      var chart = echarts.getInstanceByDom(
        document.getElementById('world_map'));
      if (chart) {
        chart.on('click', function(params) {
          var region = COUNTRY_TO_REGION[params.name];
          if (!region) return;
          var $chip = $('.region-chip[data-key=\"' + region + '\"]');
          var cur = parseInt($chip.data('val')) || 0;
          var next = cycleVal(cur, params.event && params.event.event
            && params.event.event.shiftKey);
          $chip.data('val', next);
          applyTilt('r', region, next);
          renderChip($chip, next);
          // Also update map coloring
          Shiny.setInputValue('map_tick', Date.now(),
            {priority: 'event'});
        });
      }
    }, 800);
  });
", jsonlite::toJSON(as.list(country_to_region), auto_unbox = TRUE))

region_chip <- function(key, label, init = 0) {
  klass <- if (init == 0) "" else if (init > 0) paste0("pos-", init)
    else paste0("neg-", abs(init))
  dot_label <- if (init == 0) "\u25CB" else
    if (init > 0) paste0("+", init) else as.character(init)
  tags$button(class = paste("region-chip", klass),
    `data-key` = key, `data-val` = init,
    tags$span(class = "dot", dot_label),
    label
  )
}

sector_row <- function(key, label, init = 0) {
  vclass <- if (init == 0) "neu" else if (init > 0) "pos" else "neg"
  vlab <- if (init == 0) "0" else
    if (init > 0) paste0("+", init) else as.character(init)
  fill_class <- if (init == 0) "" else if (init > 0) "pos" else "neg"
  pct <- abs(init) / 2 * 50
  div(class = "sector-row", `data-key` = key, `data-val` = init,
    div(class = "sector-label", label),
    div(class = "sector-track",
      div(class = paste("sector-fill", fill_class),
        style = sprintf("width: %.0f%%;", pct))
    ),
    div(class = paste("sector-val", vclass), vlab)
  )
}

ui <- fluidPage(
  tags$head(tags$style(css)),
  tags$div(class = "tilt-container",
    tags$div(class = "card",
      tags$h1("Preferences"),
      tags$div(class = "subtitle",
        "Click a region on the map or a sector bar to tilt."),

      div(class = "section-title", "Geographic preferences"),
      echarts4rOutput("world_map", height = "320px"),
      tags$div(class = "hint",
        "Click any country — cycles its region 0 \u2192 +1 \u2192 +2 \u2192 -2 \u2192 -1 \u2192 0. Shift+click goes backward."),
      tags$div(class = "region-legend",
        lapply(names(REGIONS), function(nm) {
          region_chip(REGIONS[[nm]], nm)
        })
      ),

      div(class = "section-title", "Sector preferences"),
      lapply(names(SECTORS), function(nm) {
        sector_row(SECTORS[[nm]], nm)
      }),

      tags$button(class = "reset-btn", "Reset all preferences")
    ),

    tags$div(class = "card",
      tags$div(id = "readout-scroll",
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
    v <- as.integer(m$value)
    key <- m$key
    if (m$axis == "r") tilts$r[[key]] <- v
    if (m$axis == "s") tilts$s[[key]] <- v
  })

  output$world_map <- renderEcharts4r({
    # Only include countries with a non-zero tilt — neutral countries
    # fall through to the default areaColor, avoiding visualMap
    # interpolation artifacts.
    all <- data.frame(
      country = names(country_to_region),
      region = unname(country_to_region),
      stringsAsFactors = FALSE
    )
    all$value <- as.numeric(tilts$r[all$region])
    all$value[is.na(all$value)] <- 0

    # If nothing tilted, render with a dummy point that's offscreen
    df <- all[all$value != 0, , drop = FALSE]
    if (nrow(df) == 0) {
      df <- data.frame(country = "__none__", region = "__none__",
        value = 0, stringsAsFactors = FALSE)
    }

    df |>
      e_charts(country) |>
      e_map(value, map = "world",
        roam = FALSE, zoom = 1.15,
        emphasis = list(
          label = list(show = FALSE),
          itemStyle = list(areaColor = "#fef3c7",
            borderColor = "#f59e0b", borderWidth = 1.5)
        ),
        itemStyle = list(
          borderColor = "#e5e7eb", borderWidth = 0.3,
          areaColor = "#f3f4f6"
        )
      ) |>
      e_visual_map(value, type = "piecewise",
        pieces = list(
          list(value = -2, color = "#ef4444", label = "strongly avoid"),
          list(value = -1, color = "#fca5a5", label = "avoid"),
          list(value = 1, color = "#86efac", label = "prefer"),
          list(value = 2, color = "#22c55e", label = "strongly prefer")
        ),
        show = FALSE) |>
      e_tooltip(
        formatter = htmlwidgets::JS("function(p) {
          if (p.data && typeof p.data.value === 'number' && p.data.value !== 0) {
            var v = p.data.value;
            var lab = v > 0 ? '+' + v + ' (prefer)' : v + ' (avoid)';
            return p.name + '<br>' + lab;
          }
          return p.name + '<br>(neutral)';
        }")
      )
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
