# Share Explorer Demo
#
# Pipeline:
#   ticker_data → explorer
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE
)

board <- blockr.dock::new_dock_board(
  blocks = c(
    data = new_ticker_data_block(
      tickers = c("AAPL", "MSFT", "GOOG", "AMZN")
    ),
    explorer = new_share_explorer_block()
  ),
  links = blockr.core::links(
    from = "data",
    to = "explorer"
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board, "explorer_demo")
