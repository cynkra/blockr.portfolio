# Share Explorer Demo (from JSON)
#
# Pipeline:
#   ticker_data → explorer
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.portfolio")

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE
)

json_path <- system.file(
  "extdata", "Share_Explorer.json",
  package = "blockr.portfolio"
)

serve(json_path, plugins = custom_plugins(blockr.session::manage_project()))
