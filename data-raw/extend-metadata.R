# Extend metadata with preference-related columns.
#
# Adds: region_tag, sector, theme, expense_ratio, distribution_policy,
# domicile, aum. Hand-filled for the existing 34-ETF universe.
# Regenerates inst/extdata/portfolio_dm.rds in-place (no re-fetch).
#
# Run: Rscript data-raw/extend-metadata.R

library(dm)

rds_in <- "inst/extdata/portfolio_dm.rds"
stopifnot(file.exists(rds_in))

old_dm <- readRDS(rds_in)
old_meta <- as.data.frame(dm::dm_get_tables(old_dm)[["metadata"]])
returns <- as.data.frame(dm::dm_get_tables(old_dm)[["returns"]])

cat("Old metadata columns:", paste(names(old_meta), collapse = ", "), "\n")
cat("Rows:", nrow(old_meta), "\n")

# --- New per-ticker attributes ---
#
# region_tag   : one of us, europe, ch, em, asia_dev, japan, NA (no tilt)
# sector       : GICS tag or NA (broad)
# theme        : short tag or NA
# expense_ratio: decimal
# distribution_policy: "accumulating" or "distributing"
# domicile     : ISO code
# aum          : fund size in USD
#
# Values approximated from public factsheets (2024-ish) — good enough
# for demo purposes. Refresh via justETF scrape in a follow-up.

extras <- data.frame(
  ticker = c(
    # Global Equity — Broad
    "VT", "VTI", "VEA", "VWO",
    # Regional Equity — Developed
    "EWL", "EWG", "EWO", "VGK", "EWU", "EWJ", "EWA", "EWC",
    # Regional Equity — Emerging
    "EEM", "MCHI", "EWZ", "INDA", "EWT",
    # US Equity — Style/Size
    "VUG", "VTV", "VO", "VB",
    # Fixed Income
    "AGG", "SHY", "IEF", "TLT", "TIP", "LQD", "BNDX", "EMB",
    # Alternatives
    "VNQ", "VNQI", "GLD", "DBC", "BITO",
    # FX pseudo-tickers (inherit safe defaults)
    ".CHFUSD", ".EURUSD"
  ),
  region_tag = c(
    NA, "us", NA, "em",
    "ch", "europe", "europe", "europe", "europe", "japan", "asia_dev", NA,
    "em", "em", "em", "em", "em",
    "us", "us", "us", "us",
    "us", "us", "us", "us", "us", "us", NA, "em",
    "us", NA, NA, NA, NA,
    NA, NA
  ),
  sector = NA_character_,
  theme = NA_character_,
  expense_ratio = c(
    0.0007, 0.0003, 0.0005, 0.0008,
    0.0050, 0.0050, 0.0050, 0.0011, 0.0050, 0.0050, 0.0050, 0.0050,
    0.0068, 0.0059, 0.0059, 0.0064, 0.0059,
    0.0004, 0.0004, 0.0004, 0.0005,
    0.0003, 0.0015, 0.0015, 0.0015, 0.0019, 0.0014, 0.0007, 0.0039,
    0.0012, 0.0012, 0.0040, 0.0085, 0.0095,
    NA, NA
  ),
  distribution_policy = c(
    rep("distributing", 34),
    NA, NA
  ),
  domicile = c(
    rep("US", 34),
    NA, NA
  ),
  aum = c(
    45000, 420000, 120000, 78000,
    1500, 1800, 90, 18000, 2500, 12000, 1700, 3200,
    30000, 6500, 5500, 8500, 4200,
    190000, 175000, 65000, 55000,
    115000, 28000, 32000, 50000, 18000, 32000, 56000, 14000,
    35000, 4200, 70000, 1300, 1800,
    NA, NA
  ) * 1e6,  # convert M USD → USD
  stringsAsFactors = FALSE
)

stopifnot(all(extras$ticker %in% old_meta$ticker))
stopifnot(nrow(extras) == nrow(old_meta))

# Merge — preserve column order of old_meta, append new columns
merged <- merge(old_meta, extras, by = "ticker", sort = FALSE)
# Restore original row order
merged <- merged[match(old_meta$ticker, merged$ticker), , drop = FALSE]
rownames(merged) <- NULL

cat("New metadata columns:", paste(names(merged), collapse = ", "), "\n")
cat("Rows:", nrow(merged), "\n")

new_dm <- dm::dm(metadata = merged, returns = returns) |>
  dm::dm_add_pk(metadata, ticker) |>
  dm::dm_add_fk(returns, ticker, metadata)

saveRDS(new_dm, rds_in)
cat("Saved", rds_in, "\n")

# Quick sanity print
cat("\nSample rows:\n")
print(merged[c(1, 5, 18, 22, 32), c("ticker", "name", "region_tag",
  "expense_ratio", "distribution_policy", "aum")])
