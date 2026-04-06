#' Investor Profile Block
#'
#' A data block that captures investor personal details and outputs
#' a one-row data frame with risk/horizon as continuous 0-100 values.
#'
#' @param age Investor age (18-100)
#' @param family Family status: "single", "married", "married_kids", "retired"
#' @param amount Investment amount
#' @param currency Base currency: "USD", "CHF", "EUR"
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `investor_profile_block`
#' @export
new_investor_profile_block <- function(
    age = 35L,
    family = "single",
    amount = 50000,
    currency = "USD",
    risk_slider = NULL,
    horizon_slider = NULL,
    ...) {

  # Compute initial slider defaults from age/family if not provided
  init <- pf_derive_profile(age, family, amount)
  if (is.null(risk_slider)) risk_slider <- init$risk_slider
  if (is.null(horizon_slider)) horizon_slider <- init$horizon_slider

  blockr.core::new_data_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_age <- shiny::reactiveVal(as.integer(age))
          r_family <- shiny::reactiveVal(family)
          r_amount <- shiny::reactiveVal(amount)
          r_currency <- shiny::reactiveVal(currency)
          r_risk_slider <- shiny::reactiveVal(risk_slider)
          r_horizon_slider <- shiny::reactiveVal(horizon_slider)

          # When age/family/amount change, update slider recommendations
          shiny::observeEvent(
            list(r_age(), r_family(), r_amount()), {
              derived <- pf_derive_profile(
                r_age(), r_family(), r_amount())
              r_risk_slider(derived$risk_slider)
              r_horizon_slider(derived$horizon_slider)
              session$sendCustomMessage(
                session$ns("sync_sliders"),
                list(risk = derived$risk_slider,
                  horizon = derived$horizon_slider))
            }, ignoreInit = TRUE)

          shiny::observeEvent(input$profile_ctrl, {
            msg <- input$profile_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              age = r_age(as.integer(msg$value)),
              family = r_family(msg$value),
              amount = r_amount(as.numeric(msg$value)),
              currency = r_currency(msg$value),
              risk_slider = r_risk_slider(as.integer(msg$value)),
              horizon_slider = r_horizon_slider(
                as.integer(msg$value))
            )
          })

          list(
            expr = shiny::reactive({
              derived <- pf_derive_profile(
                r_age(), r_family(), r_amount())
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
              family = r_family,
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

          shiny::div(class = "ip-section",
            shiny::div(class = "ip-title", "Your Profile"),

            # Age
            shiny::div(class = "ip-group",
              shiny::div(class = "ip-label", "Age"),
              shiny::tags$input(
                type = "number", class = "ip-numeric",
                `data-param` = "age",
                value = age, min = 18, max = 100, step = 1
              )
            ),

            # Family
            shiny::div(class = "ip-group",
              shiny::div(class = "ip-label", "Family Status"),
              shiny::div(class = "ip-radios",
                ip_radio_chip(ns, "family", "single", "Single",
                  family),
                ip_radio_chip(ns, "family", "married", "Married",
                  family),
                ip_radio_chip(ns, "family", "married_kids", "+Kids",
                  family),
                ip_radio_chip(ns, "family", "retired", "Retired",
                  family)
              )
            ),

            # Amount
            shiny::div(class = "ip-group",
              shiny::div(class = "ip-label", "Investment Amount"),
              shiny::tags$input(
                type = "number", class = "ip-numeric",
                `data-param` = "amount",
                value = amount, min = 1000, max = 10000000, step = 1000
              )
            ),

            # Currency
            shiny::div(class = "ip-group",
              shiny::div(class = "ip-label", "Base Currency"),
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

          # Risk & Horizon sliders
          shiny::div(class = "ip-section ip-sliders",

            # Risk appetite slider
            shiny::div(class = "ip-slider-group",
              shiny::div(class = "ip-slider-header",
                shiny::span(class = "ip-label", "Risk Appetite"),
                shiny::span(class = "ip-slider-value",
                  id = ns("risk_label"),
                  pf_risk_label(risk_slider))
              ),
              shiny::tags$input(
                type = "range", class = "ip-slider",
                id = ns("risk_slider"),
                `data-param` = "risk_slider",
                min = 0, max = 100, value = risk_slider,
                step = 1
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
              shiny::tags$input(
                type = "range", class = "ip-slider",
                id = ns("horizon_slider"),
                `data-param` = "horizon_slider",
                min = 0, max = 100, value = horizon_slider,
                step = 1
              ),
              shiny::div(class = "ip-slider-ticks",
                shiny::span("1 yr"),
                shiny::span("10 yrs"),
                shiny::span("30+ yrs")
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

            // Numeric input change
            $(document).on('change', '#' + layoutId + ' .ip-numeric', function(e) {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });

            // Slider input (debounced)
            $(document).on('input', '#' + layoutId + ' .ip-slider', function(e) {
              var param = $(this).data('param');
              var value = parseInt($(this).val());
              // Update label immediately
              if (param === 'risk_slider') {
                $('#' + riskLabelId).text(riskLabel(value));
              } else if (param === 'horizon_slider') {
                $('#' + horizonLabelId).text(horizonLabel(value));
              }
              // Debounce server update
              clearTimeout(debounceTimer);
              debounceTimer = setTimeout(function() {
                Shiny.setInputValue(ctrlId, {
                  param: param, value: value
                }, {priority: 'event'});
              }, 200);
            });

            // Sync sliders from server (when age/family changes)
            Shiny.addCustomMessageHandler(syncMsgId, function(msg) {
              var $risk = $('#' + layoutId + ' .ip-slider[data-param=risk_slider]');
              var $horizon = $('#' + layoutId + ' .ip-slider[data-param=horizon_slider]');
              if (msg.risk !== undefined) {
                $risk.val(msg.risk);
                $('#' + riskLabelId).text(riskLabel(msg.risk));
              }
              if (msg.horizon !== undefined) {
                $horizon.val(msg.horizon);
                $('#' + horizonLabelId).text(horizonLabel(msg.horizon));
              }
            });
          });
        ")))
      )
    },
    external_ctrl = c("age", "family", "amount", "currency",
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
  .ip-sliders { border-top: 1px solid #e5e7eb; margin-top: 4px;
    padding-top: 12px; }
  .ip-title { font-size: 14px; font-weight: 600; color: #111827;
    margin-bottom: 12px; }
  .ip-group { margin-bottom: 10px; }
  .ip-label { font-size: 11px; font-weight: 500; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }
  .ip-numeric { width: 100%; padding: 6px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; font-size: 14px; font-family: inherit;
    background: #fff; color: #111827; }
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
  .ip-slider { width: 100%; height: 6px; -webkit-appearance: none;
    appearance: none; background: #e5e7eb; border-radius: 3px;
    outline: none; cursor: pointer; }
  .ip-slider::-webkit-slider-thumb { -webkit-appearance: none;
    width: 18px; height: 18px; border-radius: 50%; background: #3b82f6;
    cursor: pointer; border: 2px solid #fff;
    box-shadow: 0 1px 3px rgba(0,0,0,0.2); }
  .ip-slider::-moz-range-thumb { width: 18px; height: 18px;
    border-radius: 50%; background: #3b82f6; cursor: pointer;
    border: 2px solid #fff; box-shadow: 0 1px 3px rgba(0,0,0,0.2); }
  .ip-slider-ticks { display: flex; justify-content: space-between;
    margin-top: 4px; font-size: 10px; color: #9ca3af; }
  "
}

# S3 methods: use default data_block output (html_table_preview via blockr.extra)
