#' Portfolio Optimizer Block
#'
#' A transform block with two inputs (data dm + investor profile data frame).
#' Runs portfolio optimization and returns an enriched dm with result tables.
#'
#' @param benchmark Benchmark: "60_40", "equal_weight", "sp500"
#' @param max_weight Maximum position weight (0-1)
#' @param compare Character vector of strategy IDs for comparison
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `portfolio_optimizer_block`
#' @export
new_portfolio_optimizer_block <- function(
    benchmark = "60_40",
    max_weight = 0.25,
    compare = character(0),
    ...) {

  blockr.core::new_transform_block(
    # Two named inputs: data (dm) and profile (data.frame)
    server = function(id, data, profile) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_benchmark <- shiny::reactiveVal(benchmark)
          r_max_weight <- shiny::reactiveVal(max_weight)
          r_compare <- shiny::reactiveVal(compare)

          # Handle control messages from client
          shiny::observeEvent(input$opt_ctrl, {
            msg <- input$opt_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              benchmark = r_benchmark(msg$value),
              max_weight = r_max_weight(
                as.numeric(msg$value) / 100),
              compare = r_compare(
                if (is.null(msg$value)) character(0)
                else as.character(unlist(msg$value))
              )
            )
          })

          list(
            expr = shiny::reactive({
              bbquote(
                blockr.portfolio:::pf_run_optimizer(
                  .(data), .(profile),
                  benchmark = .(bm),
                  max_weight = .(mw),
                  compare = .(cmp)
                ),
                list(
                  bm = r_benchmark(),
                  mw = r_max_weight(),
                  cmp = r_compare()
                )
              )
            }),
            state = list(
              benchmark = r_benchmark,
              max_weight = r_max_weight,
              compare = r_compare
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

          # Benchmark
          shiny::div(class = "opt-group",
            shiny::div(class = "opt-label", "Benchmark"),
            shiny::div(class = "opt-radios",
              opt_radio_chip(ns, "benchmark", "60_40", "60/40",
                benchmark),
              opt_radio_chip(ns, "benchmark", "equal_weight",
                "Equal Wt", benchmark),
              opt_radio_chip(ns, "benchmark", "sp500",
                "S&P 500", benchmark)
            )
          ),

          # Max weight
          shiny::div(class = "opt-group",
            shiny::div(class = "opt-label", "Max Position (%)"),
            shiny::tags$input(
              type = "number", class = "opt-numeric",
              `data-param` = "max_weight",
              value = round(max_weight * 100),
              min = 5, max = 100, step = 5
            )
          ),

          # Compare
          shiny::div(class = "opt-group",
            shiny::div(class = "opt-label", "Compare Strategies"),
            shiny::div(class = "opt-compare-chips",
              opt_compare_chip("mean_variance", "Mean-Var", compare),
              opt_compare_chip("risk_parity", "Risk Parity", compare),
              opt_compare_chip("equal_weight", "Equal Wt", compare),
              opt_compare_chip("min_vol", "Min Vol", compare)
            )
          )
        ),

        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("opt_layout"), "';
            var ctrlId = '", ns("opt_ctrl"), "';

            $(document).on('click', '#' + layoutId + ' .opt-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.opt-radio').removeClass('is-active');
              $(this).addClass('is-active');
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).data('value')
              }, {priority: 'event'});
            });

            $(document).on('change', '#' + layoutId + ' .opt-numeric', function(e) {
              Shiny.setInputValue(ctrlId, {
                param: $(this).data('param'),
                value: $(this).val()
              }, {priority: 'event'});
            });

            $(document).on('click', '#' + layoutId + ' .opt-compare-chip', function(e) {
              e.stopPropagation();
              $(this).toggleClass('is-active');
              var active = [];
              $(this).closest('.opt-compare-chips')
                .find('.opt-compare-chip.is-active').each(function() {
                  active.push($(this).data('value'));
                });
              Shiny.setInputValue(ctrlId, {
                param: 'compare', value: active
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
      needed <- c("risk", "horizon", "strategy", "min_positions")
      if (!all(needed %in% colnames(profile))) {
        stop("Profile must have columns: ", paste(needed, collapse = ", "))
      }
    },
    expr_type = "bquoted",
    allow_empty_state = "compare",
    external_ctrl = c("benchmark", "max_weight", "compare"),
    class = c("portfolio_optimizer_block", "dm_block"),
    ...
  )
}

# -- UI helpers ----------------------------------------------------------------

opt_radio_chip <- function(ns, param, value, label, current) {
  is_active <- identical(current, value)
  shiny::tags$button(
    class = paste("opt-radio", if (is_active) "is-active"),
    `data-param` = param,
    `data-value` = value,
    label
  )
}

opt_compare_chip <- function(value, label, current) {
  is_active <- value %in% current
  shiny::tags$button(
    class = paste("opt-compare-chip",
      if (is_active) "is-active"),
    `data-value` = value,
    label
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
  .opt-numeric { width: 80px; padding: 6px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; font-size: 14px; font-family: inherit;
    background: #fff; color: #111827; }
  .opt-numeric:focus { outline: none; border-color: #3b82f6;
    box-shadow: 0 0 0 2px rgba(59,130,246,0.15); }
  .opt-compare-chips { display: flex; flex-wrap: wrap; gap: 4px; }
  .opt-compare-chip { padding: 4px 10px; border: 1px solid #d1d5db;
    border-radius: 6px; background: #fff; color: #374151;
    font-size: 12px; font-family: inherit; cursor: pointer;
    transition: all 0.15s; }
  .opt-compare-chip:hover { background: #f3f4f6; }
  .opt-compare-chip.is-active { background: #dbeafe; border-color: #93c5fd;
    color: #1d4ed8; font-weight: 500; }
  "
}

# S3 methods: inherit from dm_block for output (clickable dm diagram)
