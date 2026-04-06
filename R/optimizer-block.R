#' Portfolio Optimizer Block
#'
#' A transform block with two inputs (data dm + investor profile data frame).
#' Uses ridge-regularized ROI solver. Per-ticker limits use DEoptim.
#'
#' @param strategy Optimization strategy
#' @param max_weight Maximum position weight (NA = no limit)
#' @param max_positions Maximum ETFs (-1 = auto, NA = off)
#' @param ticker_limits_json JSON string of per-ticker limits
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `portfolio_optimizer_block`
#' @export
new_portfolio_optimizer_block <- function(
    strategy = "mean_variance",
    max_weight = NA_real_,
    max_positions = -1L,
    ticker_limits_json = "{}",
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data, profile) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Individual reactiveVals — blockr.core tracks each one
          r_strategy <- shiny::reactiveVal(strategy)
          r_max_weight <- shiny::reactiveVal(max_weight)
          r_max_positions <- shiny::reactiveVal(max_positions)
          r_ticker_limits_json <- shiny::reactiveVal(ticker_limits_json)

          # Send ticker metadata to JS when data arrives
          shiny::observeEvent(data(), {
            dm_obj <- tryCatch(data(), error = function(e) NULL)
            if (!inherits(dm_obj, "dm")) return()
            tbls <- dm::dm_get_tables(dm_obj)
            if (!"metadata" %in% names(tbls)) return()
            meta <- as.data.frame(tbls[["metadata"]])
            meta <- meta[meta$type != "FX", , drop = FALSE]
            ticker_list <- lapply(seq_len(nrow(meta)), function(i) {
              list(
                ticker = meta$ticker[i],
                name = meta$name[i],
                region = meta$region[i]
              )
            })
            session$sendCustomMessage("optimizer-tickers",
              list(id = ns("opt_input"), tickers = ticker_list))
          })

          # JS → R: user changed state
          shiny::observeEvent(input$opt_input, {
            val <- input$opt_input
            if (!is.list(val)) return()

            r_strategy(val$strategy %||% "mean_variance")

            r_max_weight(
              if (is.null(val$max_weight) ||
                identical(val$max_weight, "null"))
                NA_real_ else as.numeric(val$max_weight))

            r_max_positions(
              if (is.null(val$max_positions) ||
                identical(val$max_positions, "null"))
                NA_integer_ else as.integer(val$max_positions))

            tl <- val$ticker_limits
            if (is.list(tl) && length(tl) > 0) {
              r_ticker_limits_json(jsonlite::toJSON(
                tl, auto_unbox = TRUE))
            } else {
              r_ticker_limits_json("{}")
            }
          })

          list(
            expr = shiny::reactive({
              tl_json <- r_ticker_limits_json()
              tl <- if (nzchar(tl_json) && tl_json != "{}") {
                jsonlite::fromJSON(tl_json, simplifyVector = FALSE)
              } else {
                list()
              }
              message("[EXPR] strategy=", r_strategy(),
                " tl_json=", tl_json,
                " tl=", paste(names(tl), "=", tl, collapse=", "))
              make_optimizer_expr(
                r_strategy(),
                r_max_weight(),
                r_max_positions(),
                tl
              )
            }),
            state = list(
              strategy = r_strategy,
              max_weight = r_max_weight,
              max_positions = r_max_positions,
              ticker_limits_json = r_ticker_limits_json
            )
          )
        }
      )
    },
    ui = function(id) {
      init_state <- list(
        strategy = strategy,
        max_weight = if (is.na(max_weight)) NULL else max_weight,
        max_positions = max_positions,
        ticker_limits = if (nzchar(ticker_limits_json) &&
          ticker_limits_json != "{}") {
          jsonlite::fromJSON(ticker_limits_json,
            simplifyVector = FALSE)
        } else {
          structure(list(), names = character(0))
        }
      )
      state_json <- jsonlite::toJSON(init_state, auto_unbox = TRUE,
        null = "null")

      shiny::tagList(
        optimizer_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = shiny::NS(id, "opt_input"),
            class = "opt-container",
            `data-state` = as.character(state_json)
          )
        )
      )
    },
    dat_valid = function(data, profile) {
      if (!inherits(data, "dm")) {
        stop("First input must be a dm object")
      }
      if (!is.data.frame(profile) || nrow(profile) < 1) {
        stop("Second input must be a data frame")
      }
    },
    expr_type = "bquoted",
    allow_empty_state = c("max_weight", "max_positions",
      "ticker_limits_json"),
    external_ctrl = c("strategy", "max_weight", "max_positions",
      "ticker_limits_json"),
    class = c("portfolio_optimizer_block", "dm_block"),
    ...
  )
}

#' Build optimizer expression (following blockr.dplyr pattern)
#' @noRd
make_optimizer_expr <- function(strategy, max_weight, max_positions,
                                 ticker_limits) {
  if (is.list(ticker_limits) && length(ticker_limits) > 0) {
    tl_args <- lapply(names(ticker_limits), function(nm) {
      ticker_limits[[nm]]
    })
    names(tl_args) <- names(ticker_limits)
    tl_call <- as.call(c(quote(list), tl_args))
  } else {
    tl_call <- quote(list())
  }

  blockr.core::bbquote(
    blockr.portfolio:::pf_run_optimizer(
      .(data), .(profile),
      strategy = .(strat),
      max_weight = .(mw),
      max_positions = .(mp),
      ticker_limits = .(tl)
    ),
    list(
      strat = strategy,
      mw = max_weight,
      mp = max_positions,
      tl = tl_call
    )
  )
}

#' HTML dependencies for the optimizer block
#' @noRd
optimizer_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "optimizer-block-js",
      version = utils::packageVersion("blockr.portfolio"),
      src = system.file("js", package = "blockr.portfolio"),
      script = "optimizer-block.js"
    ),
    htmltools::htmlDependency(
      name = "optimizer-block-css",
      version = utils::packageVersion("blockr.portfolio"),
      src = system.file("css", package = "blockr.portfolio"),
      stylesheet = "optimizer-block.css"
    )
  )
}
