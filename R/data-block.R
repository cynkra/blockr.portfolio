#' Portfolio Data Block
#'
#' A data block that returns a bundled dm object containing ETF metadata
#' and monthly returns for portfolio analysis.
#'
#' The returned dm has two tables:
#' - `metadata`: ticker, name, asset_class, region, sub_class
#' - `returns`: date, ticker, return (long format)
#'
#' Primary key on `metadata.ticker`, foreign key from `returns.ticker`.
#'
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("portfolio_data_block", "dm_block")`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.portfolio)
#'   serve(new_portfolio_data_block())
#' }
#'
#' @export
new_portfolio_data_block <- function(...) {
  rds_path <- system.file(
    "extdata", "portfolio_dm.rds",
    package = "blockr.portfolio",
    mustWork = TRUE
  )

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = shiny::reactive({
              substitute(
                readRDS(path),
                list(path = rds_path)
              )
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          style = "padding: 8px 12px; font-size: 13px; color: #6b7280;",
          "Portfolio Universe (34 ETFs, 10yr monthly returns, global coverage)"
        )
      )
    },
    class = c("portfolio_data_block", "dm_block"),
    ...
  )
}

# S3 methods: inherit from dm_block for clickable dm diagram output
