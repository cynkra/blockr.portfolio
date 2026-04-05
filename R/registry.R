#' Register Portfolio Blocks
#'
#' Registers all portfolio blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_portfolio_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_portfolio_data_block",
      "new_investor_profile_block",
      "new_portfolio_optimizer_block",
      "new_portfolio_dashboard_block",
      "new_ticker_data_block",
      "new_share_explorer_block"
    ),
    name = c(
      "Portfolio Data",
      "Investor Profile",
      "Portfolio Optimizer",
      "Portfolio Dashboard",
      "Ticker Data",
      "Share Explorer"
    ),
    description = c(
      "Bundled ETF/stock universe with metadata, returns, and OHLC",
      "Personal investor profile with auto-derived risk parameters",
      "Portfolio optimization with configurable benchmark and constraints",
      "Interactive portfolio analysis panels",
      "Fetch OHLC price data for stocks and ETFs",
      "Explore individual stocks with candlestick charts and metrics"
    ),
    category = rep("structured", 6),
    icon = c("briefcase", "person", "calculator", "graph-up-arrow",
      "cloud-download", "search"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
