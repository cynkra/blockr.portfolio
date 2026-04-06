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

board <- blockr.dock::new_dock_board(
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
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board, "portfolio_demo")
