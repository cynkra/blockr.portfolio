# Portfolio Advisor Demo (from JSON)
#
# Pipeline:
#   data + profile → optimizer → dashboard
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE
)

json_path <- system.file(
  "extdata", "Portfolio_Optimizer.json",
  package = "blockr.portfolio"
)

serve(json_path, plugins = custom_plugins(blockr.session::manage_project()))
