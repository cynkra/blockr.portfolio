#' Investor Profile Block
#'
#' A data block that captures investor preferences and outputs
#' a one-row data frame with currency, risk (0-100), horizon (0-100),
#' and investment amount.
#'
#' @param age Investor age (18-100), used to suggest risk/horizon defaults
#' @param family Family status, used to suggest risk defaults
#' @param amount Investment amount
#' @param currency Base currency: "USD", "CHF", "EUR"
#' @param risk_slider Risk preference 0-100 (NULL = derive from age/family)
#' @param horizon_slider Horizon preference 0-100 (NULL = derive from age)
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
    ...) {

  init <- pf_derive_profile(age, has_dependents)
  if (is.null(risk_slider)) risk_slider <- init$risk_slider
  if (is.null(horizon_slider)) horizon_slider <- init$horizon_slider

  blockr.core::new_data_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_age <- shiny::reactiveVal(as.integer(age))
          r_has_dependents <- shiny::reactiveVal(has_dependents)
          r_amount <- shiny::reactiveVal(amount)
          r_currency <- shiny::reactiveVal(currency)
          r_risk_slider <- shiny::reactiveVal(risk_slider)
          r_horizon_slider <- shiny::reactiveVal(horizon_slider)

          # When demographics change, update sliders + suggestion markers
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
                as.integer(msg$value))
            )
          })

          list(
            expr = shiny::reactive({
              bquote(data.frame(
                currency = .(r_currency()),
                risk = .(r_risk_slider()),
                horizon = .(r_horizon_slider()),
                amount = .(r_amount()),
                stringsAsFactors = FALSE
              ))
            }),
            state = list(
              age = r_age,
              has_dependents = r_has_dependents,
              amount = r_amount,
              currency = r_currency,
              risk_slider = r_risk_slider,
              horizon_slider = r_horizon_slider
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

            # Amount + Currency on one row
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
                  ip_radio_chip(ns, "currency", "USD", "USD",
                    currency),
                  ip_radio_chip(ns, "currency", "CHF", "CHF",
                    currency),
                  ip_radio_chip(ns, "currency", "EUR", "EUR",
                    currency)
                )
              )
            ),

            # Risk appetite slider
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
                # Suggestion marker (positioned via JS)
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

            # Horizon slider
            shiny::div(class = "ip-slider-group",
              shiny::div(class = "ip-slider-header",
                shiny::span(class = "ip-label",
                  "Investment Horizon"),
                shiny::span(class = "ip-slider-value",
                  id = ns("horizon_label"),
                  pf_horizon_label(horizon_slider))
              ),
              shiny::div(class = "ip-slider-wrap",
                shiny::tags$input(
                  type = "range", class = "ip-slider",
                  id = ns("horizon_slider"),
                  `data-param` = "horizon_slider",
                  min = 0, max = 100, value = horizon_slider,
                  step = 1
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
          )
        ),

        # Client-side JS
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("ip_layout"), "';
            var ctrlId = '", ns("profile_ctrl"), "';
            var riskLabelId = '", ns("risk_label"), "';
            var horizonLabelId = '", ns("horizon_label"), "';
            var riskSuggestId = '", ns("risk_suggest"), "';
            var horizonSuggestId = '", ns("horizon_suggest"), "';
            var syncMsgId = '", ns("sync_sliders"), "';
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
              // Hide marker if it matches the slider position
              var $slider = $marker.siblings('.ip-slider');
              if ($slider.length && parseInt($slider.val()) === value) {
                $marker.addClass('ip-hidden');
              } else {
                $marker.removeClass('ip-hidden');
              }
            }

            // Initial marker positions
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

            // Numeric input change (amount)
            $(document).on('change', '#' + layoutId + ' .ip-numeric', function(e) {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });

            // Demographics: age input (continuous update)
            $(document).on('input', '#' + layoutId + ' .ip-demo-input', function(e) {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });

            // Demographics: dependents checkbox
            $(document).on('change', '#' + layoutId + ' .ip-demo-checkbox', function(e) {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).is(':checked')
              }, {priority: 'event'});
            });

            // Slider input (debounced)
            $(document).on('input', '#' + layoutId + ' .ip-slider', function(e) {
              var param = $(this).data('param');
              var value = parseInt($(this).val());
              if (param === 'risk_slider') {
                $('#' + riskLabelId).text(riskLabel(value));
                positionMarker(riskSuggestId, parseInt($('#' + riskSuggestId).css('left')) || 0);
              } else if (param === 'horizon_slider') {
                $('#' + horizonLabelId).text(horizonLabel(value));
              }
              // Hide/show marker based on match
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

            // Sync sliders + suggestion markers from server
            Shiny.addCustomMessageHandler(syncMsgId, function(msg) {
              var $layout = $('#' + layoutId);
              if (msg.risk !== undefined) {
                var $risk = $layout.find('.ip-slider[data-param=risk_slider]');
                $risk.val(msg.risk);
                $('#' + riskLabelId).text(riskLabel(msg.risk));
              }
              if (msg.horizon !== undefined) {
                var $horizon = $layout.find('.ip-slider[data-param=horizon_slider]');
                $horizon.val(msg.horizon);
                $('#' + horizonLabelId).text(horizonLabel(msg.horizon));
              }
              if (msg.suggest_risk !== undefined) {
                positionMarker(riskSuggestId, msg.suggest_risk);
              }
              if (msg.suggest_horizon !== undefined) {
                positionMarker(horizonSuggestId, msg.suggest_horizon);
              }
            });
          });
        ")))
      )
    },
    external_ctrl = c("age", "has_dependents", "amount", "currency",
      "risk_slider", "horizon_slider"),
    class = "investor_profile_block",
    ...
  )
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

  /* Slider */
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

  /* Suggestion marker */
  .ip-suggest-marker {
    position: absolute; top: 50%; transform: translate(-50%, -50%);
    width: 0; height: 0;
    border-left: 5px solid transparent;
    border-right: 5px solid transparent;
    border-top: 7px solid #f59e0b;
    z-index: 1; pointer-events: none;
    transition: left 0.3s ease;
  }
  .ip-suggest-marker.ip-hidden { display: none; }

  /* Demographics helper */
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
  "
}

# S3 methods: use default data_block output (html_table_preview via blockr.extra)
