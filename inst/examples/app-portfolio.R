# Portfolio Advisor Demo
#
# Pipeline:
#   data → optimizer ← profile
#              ↓
#          dashboard
#
# Layout: a single "Advisor" screen — the investor settings on the LEFT
# (profile on top, the optimizer strategy below), the portfolio dashboard
# filling the RIGHT. A second "Workflow" tab shows the live block graph.
# Editing a setting on the left recomputes the dashboard.
#
# Run with:
#   source(system.file("examples/app-portfolio.R", package = "blockr.portfolio"))
# then open http://127.0.0.1:3838/

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/app-portfolio.R wrapper -- to load every blockr package from its LOCAL
# source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.dock",
  "blockr.dplyr",
  "blockr.extra",
  "blockr.dag",
  "blockr.session",
  "blockr.portfolio"
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# ---- Curate the block browser ----------------------------------------------
# Keep ONLY `dataset` and `glue` from blockr.core; drop its low-level / noise
# blocks (subset, merge, rbind, head, scatter, csv, filebrowser, upload) via
# unregister_blocks(), selecting by the registry `package` attribute so only
# core blocks are affected.
core_keep <- c("dataset_block", "glue_block")
core_drop <- setdiff(
  names(Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    available_blocks()
  )),
  core_keep
)
unregister_blocks(core_drop)

options(
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.lazy_eval = FALSE,
  blockr.dock_is_locked = FALSE
)

board_blocks <- c(
  data = new_portfolio_data_block(),
  # Low default risk appetite (Conservative) → high risk-aversion in the
  # solver → a diversified, many-asset allocation (a more colourful dashboard).
  profile = new_investor_profile_block(risk_slider = 20L),
  optimizer = new_portfolio_optimizer_block(),
  dashboard = new_portfolio_dashboard_block()
)

# Settings panels show CONTROLS only — hide the redundant data-frame / dm output
# preview accordion so the left column reads as a clean settings pane. The
# dashboard renders in its own UI section, so it is "inputs" too.
for (.id in c("profile", "optimizer", "dashboard")) {
  attr(board_blocks[[.id]], "visible") <- "inputs"
}

board <- blockr.dock::new_dock_board(
  blocks = board_blocks,

  links = blockr.core::links(
    from = c("data", "profile", "optimizer"),
    to = c("optimizer", "optimizer", "dashboard"),
    input = c("data", "profile", "data")
  ),

  # Blocks grouped into coloured STACKS by role — these drive the Workflow graph
  # so the data flow reads at a glance.
  stacks = list(
    universe = new_dock_stack(
      c("data"),
      name = "Universe", color = "#2162B7"),
    settings = new_dock_stack(
      c("profile", "optimizer"),
      name = "Settings", color = "#1FA06E"),
    result = new_dock_stack(
      c("dashboard"),
      name = "Dashboard", color = "#E8843C")
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layouts = list(
    # Advisor cockpit: settings LEFT (narrow), dashboard RIGHT (wide). The left
    # column stacks the investor profile on top with the optimizer strategy
    # below it (a nested `list()` flips orientation to vertical).
    Advisor = dock_layout(
      list("profile", "optimizer"),
      "dashboard",
      orientation = "horizontal", sizes = c(1, 2.2),
      name = "Advisor"),
    # Workflow: the live block graph, blocks grouped into coloured stacks.
    Workflow = dock_layout(
      "ext_panel-dag_extension",
      name = "Workflow")
  ),
  active = "Advisor"
)

# `manage_project()` (blockr.session) adds project save / restore to the toolbar,
# appended to the dock board's default plugins.
serve(
  board, "portfolio_demo",
  plugins = custom_plugins(c(manage_project()))
)
