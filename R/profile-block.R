#' Investor Profile Block
#'
#' A data block that captures investor preferences and outputs
#' a one-row data frame with currency, risk (0-100), horizon (0-100),
#' investment amount, and tilt columns for regional + sector preferences.
#'
#' Tilt columns are named `tilt_<key>` and hold integer values in -2..+2:
#'
#' Regions: `tilt_us`, `tilt_europe`, `tilt_ch`, `tilt_em`,
#'   `tilt_asia_dev`, `tilt_japan`.
#' Sectors: `tilt_tech`, `tilt_health`, `tilt_energy`,
#'   `tilt_financials`, `tilt_consumer`.
#'
#' @param age Investor age (18-100), used to suggest risk/horizon defaults
#' @param has_dependents Has kids or other dependents (suggested risk)
#' @param amount Investment amount
#' @param currency Base currency: "USD", "CHF", "EUR"
#' @param risk_slider Risk preference 0-100 (NULL = derive from age/family)
#' @param horizon_slider Horizon preference 0-100 (NULL = derive from age)
#' @param region_tilts Named integer vector of regional tilts (-2..+2).
#'   Defaults to all zeros. Names: us, europe, ch, em, asia_dev, japan.
#' @param sector_tilts Named integer vector of sector tilts (-2..+2).
#'   Defaults to all zeros. Names: tech, health, energy, financials, consumer.
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `investor_profile_block`
#' @export
new_investor_profile_block <- function(
    age = 35L,
    has_dependents = FALSE,
    amount = 50000,
    currency = "USD",
    risk_slider = NULL,
    horizon_slider = NULL,
    region_tilts = NULL,
    sector_tilts = NULL,
    ...) {

  init <- pf_derive_profile(age, has_dependents)
  if (is.null(risk_slider)) risk_slider <- init$risk_slider
  if (is.null(horizon_slider)) horizon_slider <- init$horizon_slider

  region_defaults <- stats::setNames(
    rep(0L, length(PF_REGION_KEYS)), PF_REGION_KEYS)
  sector_defaults <- stats::setNames(
    rep(0L, length(PF_SECTOR_KEYS)), PF_SECTOR_KEYS)

  if (is.null(region_tilts)) region_tilts <- region_defaults
  if (is.null(sector_tilts)) sector_tilts <- sector_defaults
  region_tilts <- pf_normalize_tilts(region_tilts, region_defaults)
  sector_tilts <- pf_normalize_tilts(sector_tilts, sector_defaults)

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_age <- shiny::reactiveVal(as.integer(age))
          r_has_dependents <- shiny::reactiveVal(has_dependents)
          r_amount <- shiny::reactiveVal(amount)
          r_currency <- shiny::reactiveVal(currency)
          r_risk_slider <- shiny::reactiveVal(risk_slider)
          r_horizon_slider <- shiny::reactiveVal(horizon_slider)
          r_region_tilts <- shiny::reactiveVal(region_tilts)
          r_sector_tilts <- shiny::reactiveVal(sector_tilts)

          shiny::observeEvent(
            list(r_age(), r_has_dependents()), {
              derived <- pf_derive_profile(
                r_age(), r_has_dependents())
              r_risk_slider(derived$risk_slider)
              r_horizon_slider(derived$horizon_slider)
              session$sendCustomMessage(
                session$ns("sync_sliders"),
                list(
                  risk = derived$risk_slider,
                  horizon = derived$horizon_slider,
                  suggest_risk = derived$risk_slider,
                  suggest_horizon = derived$horizon_slider
                ))
            }, ignoreInit = TRUE)

          shiny::observeEvent(input$profile_ctrl, {
            msg <- input$profile_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              age = r_age(as.integer(msg$value)),
              has_dependents = r_has_dependents(
                isTRUE(msg$value) || msg$value == "true"),
              amount = r_amount(as.numeric(msg$value)),
              currency = r_currency(msg$value),
              risk_slider = r_risk_slider(as.integer(msg$value)),
              horizon_slider = r_horizon_slider(
                as.integer(msg$value)),
              add_tilt = {
                # msg$value = "<axis>:<key>" (e.g. "region:us")
                parts <- strsplit(msg$value, ":", fixed = TRUE)[[1]]
                if (length(parts) != 2) return()
                if (parts[1] == "region") {
                  cur <- r_region_tilts()
                  if (parts[2] %in% names(cur) &&
                      cur[[parts[2]]] == 0L) {
                    cur[[parts[2]]] <- 1L
                    r_region_tilts(cur)
                  }
                } else if (parts[1] == "sector") {
                  cur <- r_sector_tilts()
                  if (parts[2] %in% names(cur) &&
                      cur[[parts[2]]] == 0L) {
                    cur[[parts[2]]] <- 1L
                    r_sector_tilts(cur)
                  }
                }
              },
              set_tilt = {
                # msg$axis, msg$key, msg$value
                v <- max(-2L, min(2L, as.integer(msg$value)))
                if (msg$axis == "region") {
                  cur <- r_region_tilts()
                  if (msg$key %in% names(cur)) {
                    cur[[msg$key]] <- v
                    r_region_tilts(cur)
                  }
                } else if (msg$axis == "sector") {
                  cur <- r_sector_tilts()
                  if (msg$key %in% names(cur)) {
                    cur[[msg$key]] <- v
                    r_sector_tilts(cur)
                  }
                }
              },
              remove_tilt = {
                if (msg$axis == "region") {
                  cur <- r_region_tilts()
                  if (msg$key %in% names(cur)) {
                    cur[[msg$key]] <- 0L
                    r_region_tilts(cur)
                  }
                } else if (msg$axis == "sector") {
                  cur <- r_sector_tilts()
                  if (msg$key %in% names(cur)) {
                    cur[[msg$key]] <- 0L
                    r_sector_tilts(cur)
                  }
                }
              },
              reset_tilts = {
                r_region_tilts(region_defaults)
                r_sector_tilts(sector_defaults)
              }
            )
          })

          # Active tilts list — rendered server-side so we can use R
          # to compute which items are active and what the "add" menu
          # should offer.
          output$tilts_ui <- shiny::renderUI({
            rt <- r_region_tilts()
            st <- r_sector_tilts()
            active_r <- names(rt)[rt != 0]
            active_s <- names(st)[st != 0]
            ip_render_tilt_list(session, rt, st)
          })

          output$tilts_add_menu <- shiny::renderUI({
            rt <- r_region_tilts()
            st <- r_sector_tilts()
            ip_render_add_menu(session, rt, st)
          })

          list(
            expr = shiny::reactive({
              rt <- r_region_tilts()
              st <- r_sector_tilts()
              bquote(data.frame(
                currency = .(r_currency()),
                risk = .(r_risk_slider()),
                horizon = .(r_horizon_slider()),
                amount = .(r_amount()),
                tilt_us = .(rt[["us"]]),
                tilt_europe = .(rt[["europe"]]),
                tilt_ch = .(rt[["ch"]]),
                tilt_em = .(rt[["em"]]),
                tilt_asia_dev = .(rt[["asia_dev"]]),
                tilt_japan = .(rt[["japan"]]),
                tilt_tech = .(st[["tech"]]),
                tilt_health = .(st[["health"]]),
                tilt_energy = .(st[["energy"]]),
                tilt_financials = .(st[["financials"]]),
                tilt_consumer = .(st[["consumer"]]),
                stringsAsFactors = FALSE
              ))
            }),
            state = list(
              age = r_age,
              has_dependents = r_has_dependents,
              amount = r_amount,
              currency = r_currency,
              risk_slider = r_risk_slider,
              horizon_slider = r_horizon_slider,
              region_tilts = r_region_tilts,
              sector_tilts = r_sector_tilts
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$style(shiny::HTML(ip_css())),
        shiny::div(
          class = "ip-layout", id = ns("ip_layout"),

          # === Primary values ===
          shiny::div(class = "ip-section",
            shiny::div(class = "ip-row",
              shiny::div(class = "ip-group ip-flex1",
                shiny::div(class = "ip-label", "Investment Amount"),
                shiny::tags$input(
                  type = "number", class = "ip-numeric",
                  `data-param` = "amount",
                  value = amount, min = 1000, max = 10000000,
                  step = 1000
                )
              ),
              shiny::div(class = "ip-group",
                shiny::div(class = "ip-label", "Currency"),
                shiny::div(class = "ip-radios",
                  ip_radio_chip(ns, "currency", "USD", "USD", currency),
                  ip_radio_chip(ns, "currency", "CHF", "CHF", currency),
                  ip_radio_chip(ns, "currency", "EUR", "EUR", currency)
                )
              )
            ),
            shiny::div(class = "ip-slider-group",
              shiny::div(class = "ip-slider-header",
                shiny::span(class = "ip-label", "Risk Appetite"),
                shiny::span(class = "ip-slider-value",
                  id = ns("risk_label"),
                  pf_risk_label(risk_slider))
              ),
              shiny::div(class = "ip-slider-wrap",
                shiny::tags$input(
                  type = "range", class = "ip-slider",
                  id = ns("risk_slider"),
                  `data-param` = "risk_slider",
                  min = 0, max = 100, value = risk_slider, step = 1
                ),
                shiny::div(class = "ip-suggest-marker",
                  id = ns("risk_suggest"),
                  title = "Suggested based on demographics"
                )
              ),
              shiny::div(class = "ip-slider-ticks",
                shiny::span("Conservative"),
                shiny::span("Moderate"),
                shiny::span("Aggressive")
              )
            ),
            shiny::div(class = "ip-slider-group",
              shiny::div(class = "ip-slider-header",
                shiny::span(class = "ip-label", "Investment Horizon"),
                shiny::span(class = "ip-slider-value",
                  id = ns("horizon_label"),
                  pf_horizon_label(horizon_slider))
              ),
              shiny::div(class = "ip-slider-wrap",
                shiny::tags$input(
                  type = "range", class = "ip-slider",
                  id = ns("horizon_slider"),
                  `data-param` = "horizon_slider",
                  min = 0, max = 100, value = horizon_slider, step = 1
                ),
                shiny::div(class = "ip-suggest-marker",
                  id = ns("horizon_suggest"),
                  title = "Suggested based on demographics"
                )
              ),
              shiny::div(class = "ip-slider-ticks",
                shiny::span("1 yr"),
                shiny::span("10 yrs"),
                shiny::span("30+ yrs")
              )
            )
          ),

          # === Demographics helper ===
          shiny::div(class = "ip-demographics",
            shiny::div(class = "ip-demo-label",
              "Adjust risk & horizon based on your situation"),
            shiny::div(class = "ip-demo-row",
              shiny::div(class = "ip-demo-age",
                shiny::span(class = "ip-demo-hint", "Age"),
                shiny::tags$input(
                  type = "number", class = "ip-demo-input",
                  `data-param` = "age",
                  value = age, min = 18, max = 100, step = 1
                )
              ),
              shiny::tags$label(class = "ip-demo-toggle",
                shiny::tags$input(
                  type = "checkbox",
                  class = "ip-demo-checkbox",
                  `data-param` = "has_dependents",
                  checked = if (has_dependents) NA else NULL
                ),
                shiny::span("Dependents")
              )
            )
          ),

          # === Preferences ===
          shiny::div(class = "ip-prefs", id = ns("ip_prefs"),
            shiny::div(class = "ip-prefs-header",
              shiny::span(class = "ip-label", "Preferences"),
              shiny::tags$button(class = "ip-prefs-reset",
                `data-param` = "reset_tilts",
                "Reset")
            ),
            shiny::div(class = "ip-prefs-add",
              shiny::uiOutput(ns("tilts_add_menu"), inline = FALSE)
            ),
            shiny::uiOutput(ns("tilts_ui"), inline = FALSE)
          )
        ),

        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("ip_layout"), "';
            var ctrlId = '", ns("profile_ctrl"), "';
            var riskLabelId = '", ns("risk_label"), "';
            var horizonLabelId = '", ns("horizon_label"), "';
            var riskSuggestId = '", ns("risk_suggest"), "';
            var horizonSuggestId = '", ns("horizon_suggest"), "';
            var syncMsgId = '", ns("sync_sliders"), "';
            var prefsId = '", ns("ip_prefs"), "';
            var debounceTimer = null;

            function riskLabel(v) {
              if (v < 20) return 'Very Conservative';
              if (v < 40) return 'Conservative';
              if (v < 60) return 'Moderate';
              if (v < 80) return 'Aggressive';
              return 'Very Aggressive';
            }
            function horizonLabel(v) {
              if (v < 15) return '1-3 years';
              if (v < 30) return '3-5 years';
              if (v < 50) return '5-10 years';
              if (v < 70) return '10-20 years';
              if (v < 85) return '20-30 years';
              return '30+ years';
            }
            function positionMarker(markerId, value) {
              var $marker = $('#' + markerId);
              if (!$marker.length) return;
              $marker.css('left', value + '%');
              var $slider = $marker.siblings('.ip-slider');
              if ($slider.length && parseInt($slider.val()) === value) {
                $marker.addClass('ip-hidden');
              } else {
                $marker.removeClass('ip-hidden');
              }
            }
            positionMarker(riskSuggestId, ", risk_slider, ");
            positionMarker(horizonSuggestId, ", horizon_slider, ");

            // Radio chip click
            $(document).on('click', '#' + layoutId + ' .ip-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.ip-radio').removeClass('is-active');
              $(this).addClass('is-active');
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).data('value')
              }, {priority: 'event'});
            });
            $(document).on('change', '#' + layoutId + ' .ip-numeric', function() {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });
            $(document).on('input', '#' + layoutId + ' .ip-demo-input', function() {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });
            $(document).on('change', '#' + layoutId + ' .ip-demo-checkbox', function() {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).is(':checked')
              }, {priority: 'event'});
            });
            $(document).on('input', '#' + layoutId + ' .ip-slider', function() {
              var param = $(this).data('param');
              var value = parseInt($(this).val());
              if (param === 'risk_slider') {
                $('#' + riskLabelId).text(riskLabel(value));
              } else if (param === 'horizon_slider') {
                $('#' + horizonLabelId).text(horizonLabel(value));
              }
              var $marker = $(this).siblings('.ip-suggest-marker');
              var markerPos = parseFloat($marker.css('left')) / $(this).width() * 100;
              $marker.toggleClass('ip-hidden', Math.abs(value - markerPos) < 3);
              clearTimeout(debounceTimer);
              debounceTimer = setTimeout(function() {
                Shiny.setInputValue(ctrlId, {
                  param: param, value: value
                }, {priority: 'event'});
              }, 200);
            });

            // === Preferences ===
            // Add-dropdown change → add_tilt
            $(document).on('change', '#' + prefsId + ' .ip-tilt-add-select', function() {
              var v = $(this).val();
              if (!v) return;
              Shiny.setInputValue(ctrlId, {
                param: 'add_tilt',
                value: v
              }, {priority: 'event'});
              $(this).val('');
            });
            // Tilt row slider change → set_tilt (debounced)
            var tiltDebounce = null;
            $(document).on('input', '#' + prefsId + ' .ip-tilt-slider', function() {
              var axis = $(this).data('axis');
              var key = $(this).data('key');
              var value = parseInt($(this).val());
              var $row = $(this).closest('.ip-tilt-row');
              $row.find('.ip-tilt-val').text(value > 0 ? '+' + value : '' + value)
                .removeClass('pos neg neu')
                .addClass(value > 0 ? 'pos' : (value < 0 ? 'neg' : 'neu'));
              clearTimeout(tiltDebounce);
              tiltDebounce = setTimeout(function() {
                Shiny.setInputValue(ctrlId, {
                  param: 'set_tilt',
                  axis: axis, key: key, value: value
                }, {priority: 'event'});
              }, 150);
            });
            // Remove button
            $(document).on('click', '#' + prefsId + ' .ip-tilt-remove', function(e) {
              e.stopPropagation();
              Shiny.setInputValue(ctrlId, {
                param: 'remove_tilt',
                axis: $(this).data('axis'),
                key: $(this).data('key')
              }, {priority: 'event'});
            });
            // Reset
            $(document).on('click', '#' + prefsId + ' .ip-prefs-reset', function() {
              Shiny.setInputValue(ctrlId, {
                param: 'reset_tilts',
                value: 'true'
              }, {priority: 'event'});
            });

            // Slider sync
            Shiny.addCustomMessageHandler(syncMsgId, function(msg) {
              var $layout = $('#' + layoutId);
              if (msg.risk !== undefined) {
                $layout.find('.ip-slider[data-param=risk_slider]').val(msg.risk);
                $('#' + riskLabelId).text(riskLabel(msg.risk));
              }
              if (msg.horizon !== undefined) {
                $layout.find('.ip-slider[data-param=horizon_slider]').val(msg.horizon);
                $('#' + horizonLabelId).text(horizonLabel(msg.horizon));
              }
              if (msg.suggest_risk !== undefined)
                positionMarker(riskSuggestId, msg.suggest_risk);
              if (msg.suggest_horizon !== undefined)
                positionMarker(horizonSuggestId, msg.suggest_horizon);
            });
          });
        ")))
      )
    },
    external_ctrl = c("age", "has_dependents", "amount", "currency",
      "risk_slider", "horizon_slider",
      "region_tilts", "sector_tilts"),
    # User inputs (sliders/tilts) may be cleared/repopulated from JS after
    # load; relax the readiness requirement on user state so a transient empty
    # value never wedges the block (defensive; all defaults are non-empty).
    allow_empty_state = TRUE,
    class = "investor_profile_block",
    ...
  )
}

# -- Tilt constants ------------------------------------------------------------

PF_REGION_KEYS <- c("us", "europe", "ch", "em", "asia_dev", "japan")
PF_REGION_LABELS <- c("US", "Europe", "Switzerland", "Emerging",
  "Asia-Developed", "Japan")
PF_SECTOR_KEYS <- c("tech", "health", "energy", "financials", "consumer")
PF_SECTOR_LABELS <- c("Technology", "Healthcare", "Energy",
  "Financials", "Consumer")

# -- Helpers -------------------------------------------------------------------

pf_normalize_tilts <- function(x, defaults) {
  out <- defaults
  x <- as.integer(x)
  keys <- intersect(names(x), names(out))
  if (length(keys)) out[keys] <- pmax(-2L, pmin(2L, x[keys]))
  out
}

# -- UI helpers ----------------------------------------------------------------

ip_radio_chip <- function(ns, param, value, label, current) {
  is_active <- identical(current, value)
  shiny::tags$button(
    class = paste("ip-radio", if (is_active) "is-active"),
    `data-param` = param,
    `data-value` = value,
    label
  )
}

ip_render_add_menu <- function(session, region_tilts, sector_tilts) {
  # Build dropdown offering only items not currently tilted
  available_r <- names(region_tilts)[region_tilts == 0]
  available_s <- names(sector_tilts)[sector_tilts == 0]

  region_opts <- lapply(available_r, function(k) {
    i <- match(k, PF_REGION_KEYS)
    shiny::tags$option(value = paste0("region:", k),
      PF_REGION_LABELS[i])
  })
  sector_opts <- lapply(available_s, function(k) {
    i <- match(k, PF_SECTOR_KEYS)
    shiny::tags$option(value = paste0("sector:", k),
      PF_SECTOR_LABELS[i])
  })

  empty <- length(available_r) == 0 && length(available_s) == 0
  shiny::tags$select(class = "ip-tilt-add-select",
    disabled = if (empty) NA else NULL,
    shiny::tags$option(value = "",
      if (empty) "All preferences added"
      else "+ Add preference\u2026"),
    if (length(region_opts))
      shiny::tags$optgroup(label = "Regions", region_opts),
    if (length(sector_opts))
      shiny::tags$optgroup(label = "Sectors", sector_opts)
  )
}

ip_render_tilt_list <- function(session, region_tilts, sector_tilts) {
  rows <- list()
  for (k in PF_REGION_KEYS) {
    v <- region_tilts[[k]]
    if (v != 0)
      rows <- c(rows, list(ip_tilt_row("region", k,
        PF_REGION_LABELS[match(k, PF_REGION_KEYS)], v)))
  }
  for (k in PF_SECTOR_KEYS) {
    v <- sector_tilts[[k]]
    if (v != 0)
      rows <- c(rows, list(ip_tilt_row("sector", k,
        PF_SECTOR_LABELS[match(k, PF_SECTOR_KEYS)], v)))
  }
  if (length(rows) == 0) {
    return(shiny::div(class = "ip-tilt-empty",
      "No preferences set"))
  }
  shiny::div(class = "ip-tilt-list", rows)
}

ip_tilt_row <- function(axis, key, label, value) {
  vlab <- if (value > 0) paste0("+", value) else as.character(value)
  vclass <- if (value > 0) "pos" else if (value < 0) "neg" else "neu"
  shiny::div(class = "ip-tilt-row",
    `data-axis` = axis, `data-key` = key,
    shiny::div(class = "ip-tilt-name", label),
    shiny::div(class = "ip-tilt-slider-wrap",
      shiny::tags$input(type = "range", class = "ip-tilt-slider",
        `data-axis` = axis, `data-key` = key,
        min = -2, max = 2, step = 1, value = value),
      shiny::div(class = "ip-tilt-ticks",
        shiny::span(), shiny::span(), shiny::span(),
        shiny::span(), shiny::span())
    ),
    shiny::div(class = paste("ip-tilt-val", vclass), vlab),
    shiny::tags$button(class = "ip-tilt-remove",
      `data-axis` = axis, `data-key` = key,
      title = "Remove",
      shiny::HTML("&times;"))
  )
}

ip_css <- function() {
  "
  .ip-layout { font-family: 'Open Sans', system-ui, sans-serif; }
  .ip-section { padding: 8px 0; }
  .ip-group { margin-bottom: 10px; }
  .ip-row { display: flex; gap: 12px; align-items: flex-end;
    margin-bottom: 10px; }
  .ip-flex1 { flex: 1; }
  .ip-label { font-size: 11px; font-weight: 500; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }
  .ip-numeric { width: 100%; padding: 6px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; font-size: 14px; font-family: inherit;
    background: #fff; color: #111827; box-sizing: border-box; }
  .ip-numeric:focus { outline: none; border-color: #3b82f6;
    box-shadow: 0 0 0 2px rgba(59,130,246,0.15); }
  .ip-radios { display: flex; flex-wrap: wrap; gap: 4px; }
  .ip-radio { padding: 4px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; background: #fff; color: #374151;
    font-size: 12px; font-family: inherit; cursor: pointer;
    transition: all 0.15s; }
  .ip-radio:hover { background: #f3f4f6; }
  .ip-radio.is-active { background: #dbeafe; border-color: #93c5fd;
    color: #1d4ed8; font-weight: 500; }

  .ip-slider-group { margin-bottom: 14px; }
  .ip-slider-header { display: flex; justify-content: space-between;
    align-items: baseline; margin-bottom: 6px; }
  .ip-slider-value { font-size: 12px; font-weight: 500; color: #3b82f6; }
  .ip-slider-wrap { position: relative; padding: 4px 0; }
  .ip-slider { width: 100%; height: 6px; -webkit-appearance: none;
    appearance: none; background: #e5e7eb; border-radius: 3px;
    outline: none; cursor: pointer; }
  .ip-slider::-webkit-slider-thumb { -webkit-appearance: none;
    width: 18px; height: 18px; border-radius: 50%; background: #3b82f6;
    cursor: pointer; border: 2px solid #fff;
    box-shadow: 0 1px 3px rgba(0,0,0,0.2); position: relative;
    z-index: 2; }
  .ip-slider::-moz-range-thumb { width: 18px; height: 18px;
    border-radius: 50%; background: #3b82f6; cursor: pointer;
    border: 2px solid #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.2); }
  .ip-slider-ticks { display: flex; justify-content: space-between;
    margin-top: 4px; font-size: 10px; color: #9ca3af; }

  .ip-suggest-marker { position: absolute; top: 50%;
    transform: translate(-50%, -50%); width: 0; height: 0;
    border-left: 5px solid transparent;
    border-right: 5px solid transparent;
    border-top: 7px solid #f59e0b;
    z-index: 1; pointer-events: none;
    transition: left 0.3s ease; }
  .ip-suggest-marker.ip-hidden { display: none; }

  .ip-demographics { border-top: 1px solid #e5e7eb; margin-top: 8px;
    padding-top: 10px; }
  .ip-demo-label { font-size: 10px; font-weight: 500; color: #9ca3af;
    text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 6px; }
  .ip-demo-row { display: flex; align-items: center; gap: 10px; }
  .ip-demo-age { display: flex; align-items: center; gap: 4px; }
  .ip-demo-hint { font-size: 11px; color: #9ca3af; }
  .ip-demo-input { width: 50px; padding: 3px 6px;
    border: 1px solid #d1d5db; border-radius: 4px; font-size: 12px;
    font-family: inherit; background: #fff; color: #6b7280;
    text-align: center; }
  .ip-demo-input:focus { outline: none; border-color: #3b82f6; }
  .ip-demo-toggle { display: flex; align-items: center; gap: 5px;
    font-size: 11px; color: #9ca3af; cursor: pointer;
    user-select: none; }
  .ip-demo-checkbox { width: 13px; height: 13px; cursor: pointer;
    accent-color: #3b82f6; }
  .ip-hidden { display: none !important; }

  /* Preferences */
  .ip-prefs { border-top: 1px solid #e5e7eb; margin-top: 14px;
    padding-top: 12px; }
  .ip-prefs-header { display: flex; justify-content: space-between;
    align-items: center; margin-bottom: 8px; }
  .ip-prefs-reset { padding: 2px 10px; font-size: 11px;
    border: 1px solid #d1d5db; background: #fff; color: #6b7280;
    border-radius: 4px; cursor: pointer; font-family: inherit; }
  .ip-prefs-reset:hover { background: #f3f4f6; color: #374151; }
  .ip-prefs-add { margin-bottom: 8px; }
  .ip-tilt-add-select { width: 100%; padding: 6px 10px;
    border: 1px dashed #d1d5db; border-radius: 6px;
    background: #fff; color: #6b7280;
    font-size: 13px; font-family: inherit; cursor: pointer;
    transition: all 0.15s; }
  .ip-tilt-add-select:hover:not(:disabled) { border-color: #93c5fd;
    color: #374151; background: #f9fafb; }
  .ip-tilt-add-select:focus { outline: none; border-color: #3b82f6;
    border-style: solid; }
  .ip-tilt-add-select:disabled { cursor: not-allowed; opacity: 0.6; }

  .ip-tilt-empty { font-size: 12px; color: #9ca3af;
    font-style: italic; padding: 8px 4px; text-align: center;
    border-radius: 6px; background: #f9fafb; }
  .ip-tilt-list { display: flex; flex-direction: column; gap: 6px; }
  .ip-tilt-row { display: grid;
    grid-template-columns: 100px 1fr 42px 24px;
    align-items: center; gap: 8px; padding: 4px 0; }
  .ip-tilt-name { font-size: 13px; color: #374151;
    font-weight: 500; overflow: hidden; text-overflow: ellipsis;
    white-space: nowrap; }
  .ip-tilt-slider-wrap { position: relative; padding: 2px 0; }
  .ip-tilt-slider { width: 100%; -webkit-appearance: none;
    appearance: none; height: 5px; border-radius: 3px;
    background: linear-gradient(90deg,
      #fca5a5 0%, #fca5a5 22%, #fecaca 22%, #fecaca 45%,
      #e5e7eb 45%, #e5e7eb 55%,
      #bbf7d0 55%, #bbf7d0 78%, #86efac 78%, #86efac 100%);
    outline: none; cursor: pointer; }
  .ip-tilt-slider::-webkit-slider-thumb { -webkit-appearance: none;
    width: 14px; height: 14px; border-radius: 50%;
    background: #fff; border: 2px solid #3b82f6;
    cursor: pointer; box-shadow: 0 1px 2px rgba(0,0,0,0.15); }
  .ip-tilt-slider::-moz-range-thumb { width: 14px; height: 14px;
    border-radius: 50%; background: #fff;
    border: 2px solid #3b82f6; cursor: pointer; }
  .ip-tilt-ticks { display: flex; justify-content: space-between;
    margin-top: 2px; pointer-events: none; }
  .ip-tilt-ticks span { display: block; width: 3px; height: 3px;
    border-radius: 50%; background: #d1d5db; }
  .ip-tilt-val { font-size: 12px; font-weight: 600;
    text-align: center; padding: 2px 0; border-radius: 4px; }
  .ip-tilt-val.pos { color: #166534; background: #dcfce7; }
  .ip-tilt-val.neg { color: #991b1b; background: #fee2e2; }
  .ip-tilt-val.neu { color: #6b7280; background: #f3f4f6; }
  .ip-tilt-remove { width: 22px; height: 22px; padding: 0;
    border: none; background: transparent; color: #9ca3af;
    font-size: 18px; line-height: 1; cursor: pointer;
    border-radius: 4px; transition: all 0.15s; font-family: inherit; }
  .ip-tilt-remove:hover { background: #fee2e2; color: #991b1b; }
  "
}
