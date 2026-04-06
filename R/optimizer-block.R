#' Portfolio Optimizer Block
#'
#' A transform block with two inputs (data dm + investor profile data frame).
#' Runs portfolio optimization and returns an enriched dm with result tables.
#' Always runs all 4 strategies for comparison.
#'
#' @param max_weight Maximum position weight (0-1), NA = no limit
#' @param max_positions Maximum number of ETFs, NA = no limit
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `portfolio_optimizer_block`
#' @export
new_portfolio_optimizer_block <- function(
    strategy = "mean_variance",
    max_weight = NA_real_,
    max_positions = -1L,
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data, profile) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_strategy <- shiny::reactiveVal(strategy)
          r_max_weight <- shiny::reactiveVal(max_weight)
          r_max_positions <- shiny::reactiveVal(max_positions)

          shiny::observeEvent(input$opt_ctrl, {
            msg <- input$opt_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              strategy = r_strategy(msg$value),
              max_weight_toggle = r_max_weight(
                if (isTRUE(msg$value)) 0.25 else NA_real_),
              max_weight = r_max_weight(
                as.numeric(msg$value) / 100),
              max_positions_mode = {
                mode <- msg$value
                if (mode == "auto") r_max_positions(-1L)
                else if (mode == "custom") r_max_positions(10L)
                else r_max_positions(NA_integer_)
              },
              max_positions = r_max_positions(
                if (is.null(msg$value) || msg$value == "")
                  NA_integer_ else as.integer(msg$value))
            )
          })

          list(
            expr = shiny::reactive({
              bbquote(
                blockr.portfolio:::pf_run_optimizer(
                  .(data), .(profile),
                  strategy = .(strat),
                  max_weight = .(mw),
                  max_positions = .(mp)
                ),
                list(
                  strat = r_strategy(),
                  mw = r_max_weight(),
                  mp = r_max_positions()
                )
              )
            }),
            state = list(
              strategy = r_strategy,
              max_weight = r_max_weight,
              max_positions = r_max_positions
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$style(shiny::HTML(opt_css())),
        shiny::div(
          class = "opt-layout", id = ns("opt_layout"),

          # Strategy
          shiny::div(class = "opt-group",
            shiny::div(class = "opt-label", "Strategy"),
            shiny::div(class = "opt-radios",
              opt_radio_chip(ns, "strategy", "mean_variance",
                "Mean-Variance", strategy),
              opt_radio_chip(ns, "strategy", "min_vol",
                "Min Volatility", strategy),
              opt_radio_chip(ns, "strategy", "risk_parity",
                "Risk Parity", strategy),
              opt_radio_chip(ns, "strategy", "equal_weight",
                "Equal Weight", strategy)
            )
          ),

          # Constraints
          shiny::div(class = "opt-group",
            shiny::div(class = "opt-label", "Constraints"),

            # Max weight toggle + input
            shiny::div(class = "opt-constraint-row",
              shiny::tags$label(class = "opt-toggle-label",
                shiny::tags$input(
                  type = "checkbox",
                  class = "opt-toggle-checkbox",
                  `data-param` = "max_weight_toggle",
                  checked = if (!is.na(max_weight)) NA else NULL
                ),
                "Max position weight"
              ),
              shiny::tags$input(
                type = "number",
                class = "opt-numeric opt-constraint-input",
                `data-param` = "max_weight",
                value = if (!is.na(max_weight))
                  round(max_weight * 100) else 25,
                min = 5, max = 100, step = 5,
                disabled = if (is.na(max_weight)) NA else NULL
              ),
              shiny::span(class = "opt-unit", "%")
            ),

            # Max positions: Auto / Custom / Off
            shiny::div(class = "opt-constraint-row",
              shiny::span(class = "opt-constraint-label",
                "Max ETFs"),
              shiny::div(class = "opt-radios opt-small-radios",
                opt_radio_chip(ns, "max_positions_mode", "auto",
                  "Auto",
                  if (identical(max_positions, -1L)) "auto"
                  else if (!is.na(max_positions)) "custom"
                  else "off"),
                opt_radio_chip(ns, "max_positions_mode", "custom",
                  "Custom",
                  if (!is.na(max_positions) &&
                    max_positions != -1L) "custom" else ""),
                opt_radio_chip(ns, "max_positions_mode", "off",
                  "Off",
                  if (is.na(max_positions)) "off" else "")
              ),
              shiny::tags$input(
                type = "number",
                class = "opt-numeric opt-constraint-input",
                `data-param` = "max_positions",
                value = if (!is.na(max_positions) &&
                  max_positions > 0) max_positions else 10,
                min = 3, max = 34, step = 1,
                disabled = if (is.na(max_positions) ||
                  identical(max_positions, -1L)) NA else NULL
              )
            )
          )
        ),

        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("opt_layout"), "';
            var ctrlId = '", ns("opt_ctrl"), "';

            // Radio chips (strategy + max_positions_mode)
            $(document).on('click', '#' + layoutId + ' .opt-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.opt-radio').removeClass('is-active');
              $(this).addClass('is-active');
              var param = $(this).data('param');
              var value = $(this).data('value');
              Shiny.setInputValue(ctrlId, {
                param: param, value: value
              }, {priority: 'event'});
              // Enable/disable numeric input for max_positions_mode
              if (param === 'max_positions_mode') {
                var $input = $(this).closest('.opt-constraint-row').find('.opt-constraint-input');
                $input.prop('disabled', value !== 'custom');
              }
            });

            // Max weight toggle checkbox
            $(document).on('change', '#' + layoutId + ' .opt-toggle-checkbox', function(e) {
              var param = $(this).data('param');
              var isOn = $(this).is(':checked');
              var $input = $(this).closest('.opt-constraint-row').find('.opt-constraint-input');
              $input.prop('disabled', !isOn);
              Shiny.setInputValue(ctrlId, {
                param: param, value: isOn
              }, {priority: 'event'});
            });

            $(document).on('change', '#' + layoutId + ' .opt-numeric', function(e) {
              if ($(this).prop('disabled')) return;
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });
          });
        ")))
      )
    },
    dat_valid = function(data, profile) {
      if (!inherits(data, "dm")) {
        stop("First input must be a dm object")
      }
      tbls <- dm::dm_get_tables(data)
      if (!all(c("metadata", "returns") %in% names(tbls))) {
        stop("dm must contain 'metadata' and 'returns' tables")
      }
      if (!is.data.frame(profile) || nrow(profile) < 1) {
        stop("Second input must be a data frame with at least 1 row")
      }
      needed <- c("risk", "horizon")
      if (!all(needed %in% colnames(profile))) {
        stop("Profile must have columns: ",
          paste(needed, collapse = ", "))
      }
    },
    expr_type = "bquoted",
    allow_empty_state = c("max_weight", "max_positions"),
    external_ctrl = c("strategy", "max_weight", "max_positions"),
    class = c("portfolio_optimizer_block", "dm_block"),
    ...
  )
}

opt_css <- function() {
  "
  .opt-layout { font-family: 'Open Sans', system-ui, sans-serif; }
  .opt-group { margin-bottom: 10px; }
  .opt-label { font-size: 11px; font-weight: 500; color: #6b7280;
    text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }
  .opt-radios { display: flex; flex-wrap: wrap; gap: 4px; }
  .opt-radio { padding: 4px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; background: #fff; color: #374151;
    font-size: 12px; font-family: inherit; cursor: pointer;
    transition: all 0.15s; }
  .opt-radio:hover { background: #f3f4f6; }
  .opt-radio.is-active { background: #dbeafe; border-color: #93c5fd;
    color: #1d4ed8; font-weight: 500; }
  .opt-numeric { width: 60px; padding: 6px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; font-size: 14px; font-family: inherit;
    background: #fff; color: #111827; }
  .opt-numeric:focus { outline: none; border-color: #3b82f6;
    box-shadow: 0 0 0 2px rgba(59,130,246,0.15); }
  .opt-constraint-row { display: flex; align-items: center; gap: 8px;
    margin-bottom: 6px; }
  .opt-toggle-label { display: flex; align-items: center; gap: 6px;
    font-size: 12px; color: #374151; cursor: pointer; flex: 1;
    user-select: none; }
  .opt-toggle-checkbox { width: 14px; height: 14px; cursor: pointer;
    accent-color: #3b82f6; }
  .opt-constraint-label { font-size: 12px; color: #374151;
    min-width: 60px; }
  .opt-small-radios .opt-radio { padding: 2px 8px; font-size: 11px; }
  .opt-constraint-input { width: 60px; }
  .opt-constraint-input:disabled { opacity: 0.4; background: #f3f4f6; }
  .opt-unit { font-size: 12px; color: #6b7280; }
  "
}

opt_radio_chip <- function(ns, param, value, label, current) {
  is_active <- identical(current, value)
  shiny::tags$button(
    class = paste("opt-radio", if (is_active) "is-active"),
    `data-param` = param,
    `data-value` = value,
    label
  )
}

# S3 methods: inherit from dm_block for output (clickable dm diagram)
