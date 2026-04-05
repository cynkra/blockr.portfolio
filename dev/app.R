# Portfolio Advisor Demo
#
# Pipeline:
#   data + profile → optimizer → dashboard
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE
)

board <- new_portfolio_board(
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board, "portfolio_demo")
