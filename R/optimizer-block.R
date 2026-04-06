#' Portfolio Optimizer Block
#'
#' A transform block with two inputs (data dm + investor profile data frame).
#' Uses the JS-first pattern: single state object flows bidirectionally.
#'
#' @param state List with strategy, max_weight, max_positions, ticker_limits
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `portfolio_optimizer_block`
#' @export
new_portfolio_optimizer_block <- function(
    state = list(
      strategy = "mean_variance",
      max_weight = NA_real_,
      max_positions = -1L,
      ticker_limits = list()
    ),
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data, profile) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Central reactive state
          r_state <- shiny::reactiveVal(state)

          # Bidirectional sync tracking
          self_write <- new.env(parent = emptyenv())
          self_write$active <- FALSE

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
            message("[OPT] input$opt_input received: ",
              paste(utils::capture.output(str(input$opt_input)),
                collapse = " "))
            self_write$active <- TRUE
            val <- input$opt_input
            if (is.list(val)) {
              # Normalize: convert NA strings, ensure correct types
              s <- list(
                strategy = val$strategy %||% "mean_variance",
                max_weight = if (is.null(val$max_weight) ||
                  identical(val$max_weight, "null"))
                  NA_real_ else as.numeric(val$max_weight),
                max_positions = if (is.null(val$max_positions) ||
                  identical(val$max_positions, "null"))
                  NA_integer_ else as.integer(val$max_positions),
                ticker_limits = if (is.list(val$ticker_limits) &&
                  length(val$ticker_limits) > 0)
                  val$ticker_limits else list()
              )
              r_state(s)
            }
            self_write$active <- FALSE
          })

          # R → JS: external update (e.g., from AI ctrl)
          shiny::observeEvent(r_state(), {
            if (!self_write$active) {
              session$sendCustomMessage("optimizer-update",
                list(id = ns("opt_input"), state = r_state()))
            }
          })

          list(
            expr = shiny::reactive({
              s <- r_state()
              blockr.portfolio:::make_optimizer_expr(
                s$strategy %||% "mean_variance",
                s$max_weight,
                s$max_positions,
                s$ticker_limits
              )
            }),
            state = list(state = r_state)
          )
        }
      )
    },
    ui = function(id) {
      # Ensure ticker_limits serializes as {} not []
      state_for_json <- state
      if (length(state_for_json$ticker_limits) == 0) {
        state_for_json$ticker_limits <- structure(list(),
          names = character(0))
      }
      state_json <- jsonlite::toJSON(state_for_json, auto_unbox = TRUE,
        null = "null", na = "null")

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
    external_ctrl = TRUE,
    allow_empty_state = "state",
    class = c("portfolio_optimizer_block", "dm_block"),
    ...
  )
}

#' Build optimizer expression (following blockr.dplyr pattern)
#' @noRd
make_optimizer_expr <- function(strategy, max_weight, max_positions,
                                 ticker_limits) {
  # Build ticker_limits as a proper R call: list(GLD = 0.1, EWT = 0.05)
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

# S3 methods: inherit from dm_block for output (clickable dm diagram)
