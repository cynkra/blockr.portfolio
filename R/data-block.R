#' Portfolio Data Block
#'
#' Returns the bundled ETF universe as a `dm` object. Filters (distribution
#' policy, TER, AUM, region, sector, theme) are applied downstream via a
#' standard [blockr.dm::new_dm_filter_block()] wired to this block's output.
#'
#' The returned dm has two tables:
#' - `metadata`: ticker + descriptive columns (name, asset_class, region,
#'   region_tag, sector, theme, expense_ratio, distribution_policy,
#'   domicile, aum, type, …)
#' - `returns`: date, ticker, return (long format)
#'
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("portfolio_data_block", "dm_block")`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.portfolio)
#'   serve(new_portfolio_data_block())
#' }
#'
#' @export
new_portfolio_data_block <- function(...) {
  rds_path <- system.file(
    "extdata", "portfolio_dm.rds",
    package = "blockr.portfolio",
    mustWork = TRUE
  )

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = shiny::reactive({
              substitute(readRDS(path), list(path = rds_path))
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          style = "padding: 8px 12px; font-size: 13px; color: #6b7280;",
          "Portfolio universe \u2014 bundled ETF metadata + monthly returns."
        )
      )
    },
    class = c("portfolio_data_block", "dm_block"),
    ...
  )
}

#' Filter a portfolio universe by common investor preferences
#'
#' Programmatic helper (no longer used by the data block, which now relies
#' on [blockr.dm::new_dm_filter_block()] for in-app filtering). Useful from
#' R scripts or tests.
#'
#' FX pseudo-tickers (.CHFUSD, .EURUSD) are always preserved.
#'
#' @param dm_obj A portfolio dm with `metadata` + `returns` tables.
#' @param accumulating_only Logical.
#' @param max_ter Numeric (decimal; 0.005 = 0.5%).
#' @param min_aum Numeric (USD).
#' @param exclude_regions Character vector of `region_tag` values.
#' @param exclude_sectors Character vector of `sector` values.
#' @return A filtered dm.
#' @export
pf_filter_universe <- function(dm_obj,
    accumulating_only = FALSE,
    max_ter = Inf,
    min_aum = 0,
    exclude_regions = character(0),
    exclude_sectors = character(0)) {
  tbls <- dm::dm_get_tables(dm_obj)
  meta <- as.data.frame(tbls[["metadata"]])
  rets <- as.data.frame(tbls[["returns"]])

  is_fx <- meta$type == "FX"
  keep <- !logical(nrow(meta))

  if (isTRUE(accumulating_only)) {
    keep <- keep & (is_fx |
      (!is.na(meta$distribution_policy) &
        meta$distribution_policy == "accumulating"))
  }
  if (is.finite(max_ter)) {
    keep <- keep & (is_fx | is.na(meta$expense_ratio) |
      meta$expense_ratio <= max_ter)
  }
  if (is.finite(min_aum) && min_aum > 0) {
    keep <- keep & (is_fx | is.na(meta$aum) | meta$aum >= min_aum)
  }
  if (length(exclude_regions) > 0) {
    keep <- keep & (is_fx | is.na(meta$region_tag) |
      !meta$region_tag %in% exclude_regions)
  }
  if (length(exclude_sectors) > 0) {
    keep <- keep & (is_fx | is.na(meta$sector) |
      !meta$sector %in% exclude_sectors)
  }

  meta_f <- meta[keep, , drop = FALSE]
  rets_f <- rets[rets$ticker %in% meta_f$ticker, , drop = FALSE]

  dm::dm(metadata = meta_f, returns = rets_f) |>
    dm::dm_add_pk(metadata, ticker) |>
    dm::dm_add_fk(returns, ticker, metadata)
}
