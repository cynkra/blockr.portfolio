#' Investor Profile Block
#'
#' A data block that captures investor personal details and outputs
#' a one-row data frame with derived financial parameters.
#'
#' @param age Investor age (18-100)
#' @param family Family status: "single", "married", "married_kids", "retired"
#' @param amount Investment amount in dollars
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `investor_profile_block`
#' @export
new_investor_profile_block <- function(
    age = 35L,
    family = "single",
    amount = 50000,
    ...) {

  blockr.core::new_data_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_age <- shiny::reactiveVal(as.integer(age))
          r_family <- shiny::reactiveVal(family)
          r_amount <- shiny::reactiveVal(amount)

          # Override reactive vals (NULL = use derived)
          r_risk_override <- shiny::reactiveVal(NULL)
          r_horizon_override <- shiny::reactiveVal(NULL)
          r_strategy_override <- shiny::reactiveVal(NULL)

          # Handle profile control messages from client
          shiny::observeEvent(input$profile_ctrl, {
            msg <- input$profile_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              age = r_age(as.integer(msg$value)),
              family = r_family(msg$value),
              amount = r_amount(as.numeric(msg$value)),
              risk = r_risk_override(msg$value),
              horizon = r_horizon_override(msg$value),
              strategy = r_strategy_override(msg$value)
            )
          })

          list(
            expr = shiny::reactive({
              derived <- pf_derive_profile(r_age(), r_family(), r_amount())
              risk <- r_risk_override() %||% derived$risk
              horizon <- r_horizon_override() %||% derived$horizon
              strategy <- r_strategy_override() %||% derived$strategy
              min_positions <- derived$min_positions

              bquote(data.frame(
                age = .(r_age()),
                family = .(r_family()),
                amount = .(r_amount()),
                risk = .(risk),
                horizon = .(horizon),
                strategy = .(strategy),
                min_positions = .(min_positions),
                stringsAsFactors = FALSE
              ))
            }),
            state = list(
              age = r_age,
              family = r_family,
              amount = r_amount
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
                ip_radio_chip(ns, "family", "single", "Single", family),
                ip_radio_chip(ns, "family", "married", "Married", family),
                ip_radio_chip(ns, "family", "married_kids", "+Kids",
                  family),
                ip_radio_chip(ns, "family", "retired", "Retired", family)
              )
            ),

            # Amount
            shiny::div(class = "ip-group",
              shiny::div(class = "ip-label", "Investment Amount ($)"),
              shiny::tags$input(
                type = "number", class = "ip-numeric",
                `data-param` = "amount",
                value = amount, min = 1000, max = 10000000, step = 1000
              )
            ),

            # Summary
            shiny::div(class = "ip-summary", id = ns("summary"),
              shiny::uiOutput(ns("summary_text"))
            )
          ),

          # Override section
          shiny::div(class = "ip-override-section",
            shiny::tags$button(
              class = "ip-override-toggle", id = ns("override_toggle"),
              shiny::HTML("&#9654; Override")
            ),
            shiny::div(
              class = "ip-override-controls ip-hidden",
              id = ns("override_controls"),
              shiny::div(class = "ip-group",
                shiny::div(class = "ip-label", "Risk"),
                shiny::div(class = "ip-radios",
                  ip_radio_chip(ns, "risk", "conservative", "Con", ""),
                  ip_radio_chip(ns, "risk", "moderate", "Mod", ""),
                  ip_radio_chip(ns, "risk", "aggressive", "Agg", "")
                )
              ),
              shiny::div(class = "ip-group",
                shiny::div(class = "ip-label", "Horizon"),
                shiny::div(class = "ip-radios",
                  ip_radio_chip(ns, "horizon", "short", "Short", ""),
                  ip_radio_chip(ns, "horizon", "medium", "Medium", ""),
                  ip_radio_chip(ns, "horizon", "long", "Long", "")
                )
              ),
              shiny::div(class = "ip-group",
                shiny::div(class = "ip-label", "Strategy"),
                shiny::div(class = "ip-radios",
                  ip_radio_chip(ns, "strategy", "mean_variance",
                    "Mean-Var", ""),
                  ip_radio_chip(ns, "strategy", "equal_weight",
                    "Equal Wt", ""),
                  ip_radio_chip(ns, "strategy", "min_vol",
                    "Min Vol", ""),
                  ip_radio_chip(ns, "strategy", "risk_parity",
                    "Risk Par", "")
                )
              )
            )
          )
        ),

        # Client-side JS
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("ip_layout"), "';
            var ctrlId = '", ns("profile_ctrl"), "';
            var toggleId = '", ns("override_toggle"), "';
            var controlsId = '", ns("override_controls"), "';

            // Radio chip click
            $(document).on('click', '#' + layoutId + ' .ip-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.ip-radio').removeClass('is-active');
              $(this).addClass('is-active');
              var param = $(this).data('param');
              var value = $(this).data('value');
              Shiny.setInputValue(ctrlId, {param: param, value: value}, {priority: 'event'});
            });

            // Numeric input change
            $(document).on('change', '#' + layoutId + ' .ip-numeric', function(e) {
              var param = $(this).data('param');
              var value = $(this).val();
              Shiny.setInputValue(ctrlId, {param: param, value: value}, {priority: 'event'});
            });

            // Override toggle
            $(document).on('click', '#' + toggleId, function() {
              var $controls = $('#' + controlsId);
              $controls.toggleClass('ip-hidden');
              var isOpen = !$controls.hasClass('ip-hidden');
              $(this).html(isOpen ? '&#9660; Override' : '&#9654; Override');
            });
          });
        ")))
      )
    },
    external_ctrl = c("age", "family", "amount"),
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
  .ip-summary { font-size: 13px; color: #059669; font-style: italic;
    padding: 8px 0; }
  .ip-override-section { border-top: 1px solid #e5e7eb; padding-top: 8px;
    margin-top: 4px; }
  .ip-override-toggle { background: none; border: none; color: #6b7280;
    font-size: 12px; cursor: pointer; padding: 4px 0;
    font-family: inherit; }
  .ip-override-toggle:hover { color: #374151; }
  .ip-override-controls { padding-top: 8px; }
  .ip-hidden { display: none; }
  "
}

# -- S3 methods ----------------------------------------------------------------

#' @importFrom blockr.core block_ui
#' @method block_ui investor_profile_block
#' @export
block_ui.investor_profile_block <- function(id, x, ...) {
  shiny::tagList()
}

#' @importFrom blockr.core block_output
#' @method block_output investor_profile_block
#' @export
block_output.investor_profile_block <- function(x, result, session) {
  DT::renderDT(result, options = list(dom = "t", pageLength = 1))
}
