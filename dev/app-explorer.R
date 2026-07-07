# Run the Share Explorer board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/app-explorer.R:
# it just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.portfolio/dev/app-explorer.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/app-explorer.R", package = "blockr.portfolio")))

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.portfolio/inst/examples/app-explorer.R")
