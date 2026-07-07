# Share Explorer Demo
#
# Pipeline:
#   ticker_data → explorer
#
# Layout: a single "Explorer" screen — the ticker / date selection on the LEFT,
# the share explorer filling the RIGHT. A second "Workflow" tab shows the live
# block graph. Editing the ticker selection recomputes the explorer.
#
# Run with:
#   source(system.file("examples/app-explorer.R", package = "blockr.portfolio"))
# then open http://127.0.0.1:3838/

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/app-explorer.R wrapper -- to load every blockr package from its LOCAL
# source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.dock",
  "blockr.extra",
  "blockr.dag",
  "blockr.session",
  "blockr.portfolio"
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.dock_is_locked = FALSE
)

board_blocks <- c(
  data = new_ticker_data_block(
    tickers = c("AAPL", "MSFT", "GOOG", "AMZN")
  ),
  explorer = new_share_explorer_block()
)

# Both panels render their own UI (controls / charts) — hide the redundant
# data-frame output preview accordion so each panel reads cleanly.
for (.id in c("data", "explorer")) {
  attr(board_blocks[[.id]], "visible") <- "inputs"
}

board <- blockr.dock::new_dock_board(
  blocks = board_blocks,

  links = blockr.core::links(
    from = "data",
    to = "explorer"
  ),

  # Blocks grouped into coloured STACKS by role — these drive the Workflow graph.
  stacks = list(
    input = new_dock_stack(
      c("data"),
      name = "Tickers", color = "#2162B7"),
    view = new_dock_stack(
      c("explorer"),
      name = "Explorer", color = "#E8843C")
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layouts = list(
    # Explorer cockpit: ticker / date selection LEFT (narrow), the share
    # explorer RIGHT (wide).
    Explorer = dock_layout(
      "data",
      "explorer",
      orientation = "horizontal", sizes = c(1, 2.5),
      name = "Explorer"),
    # Workflow: the live block graph.
    Workflow = dock_layout(
      "ext_panel-dag_extension",
      name = "Workflow")
  ),
  active = "Explorer"
)

# `manage_project()` (blockr.session) adds project save / restore to the toolbar,
# appended to the dock board's default plugins.
serve(
  board, "explorer_demo",
  plugins = custom_plugins(c(manage_project()))
)
