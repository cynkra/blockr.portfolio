# Portfolio Advisor Demo
#
# Pipeline:
#   data → filter → optimizer ← profile
#                          ↓
#                      dashboard
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.lazy_eval = FALSE
  # shiny.port = 3838,
  # shiny.host = "0.0.0.0"
)

board <- blockr.dock::new_dock_board(
  blocks = c(
    data = new_portfolio_data_block(),
    filter = blockr.dm::new_dm_filter_block(table = "metadata"),
    profile = new_investor_profile_block(),
    optimizer = new_portfolio_optimizer_block(),
    dashboard = new_portfolio_dashboard_block()
  ),
  links = blockr.core::links(
    from = c("data", "filter", "profile", "optimizer"),
    to = c("filter", "optimizer", "optimizer", "dashboard"),
    input = c("data", "data", "profile", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board, "portfolio_demo")
