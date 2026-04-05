#' Create a Portfolio Advisor Board
#'
#' Four-block pipeline: data + profile -> optimizer -> dashboard
#'
#' @param ... Forwarded to the dock board constructor
#' @return A dock board
#' @export
new_portfolio_board <- function(...) {
  board_fn <- if (requireNamespace("blockr.dock", quietly = TRUE)) {
    blockr.dock::new_dock_board
  } else {
    blockr.core::new_board
  }
  board_fn(
    blocks = c(
      data = new_portfolio_data_block(),
      profile = new_investor_profile_block(),
      optimizer = new_portfolio_optimizer_block(),
      dashboard = new_portfolio_dashboard_block()
    ),
    links = blockr.core::links(
      from = c("data", "profile", "optimizer"),
      to = c("optimizer", "optimizer", "dashboard"),
      input = c("data", "profile", "data")
    ),
    ...
  )
}

#' Create a Share Explorer Board
#'
#' Two-block pipeline: ticker_data -> explorer
#'
#' @param tickers Character vector of initial tickers
#' @param ... Forwarded to the dock board constructor
#' @return A dock board
#' @export
new_explorer_board <- function(tickers = c("AAPL", "MSFT", "GOOG"), ...) {
  board_fn <- if (requireNamespace("blockr.dock", quietly = TRUE)) {
    blockr.dock::new_dock_board
  } else {
    blockr.core::new_board
  }
  board_fn(
    blocks = c(
      data = new_ticker_data_block(tickers = tickers),
      explorer = new_share_explorer_block()
    ),
    links = blockr.core::links(
      from = "data",
      to = "explorer"
    ),
    ...
  )
}
