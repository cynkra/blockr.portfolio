# Share Explorer Demo
#
# Pipeline:
#   ticker_data → explorer
#
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE
)

board <- new_explorer_board(
  tickers = c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA"),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board, "explorer_demo")
