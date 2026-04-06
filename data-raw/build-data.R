# Build bundled ETF data for blockr.portfolio
#
# Fetches real monthly returns from Yahoo Finance for ~38 ETFs
# plus FX rates for currency adjustment. Output: dm with metadata + returns.

library(dm)
library(quantmod)

# --- ETF Universe ---

tickers <- c(
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
  "VNQ", "VNQI", "GLD", "DBC", "BITO"
)

# FX pairs for currency adjustment
fx_pairs <- c("CHFUSD=X", "EURUSD=X")

# --- Metadata ---

metadata <- data.frame(
  ticker = c(tickers, ".CHFUSD", ".EURUSD"),
  name = c(
    # Global Equity — Broad
    "Vanguard Total World Stock", "Vanguard Total US Stock",
    "Vanguard FTSE Developed ex-US", "Vanguard FTSE Emerging",
    # Regional Equity — Developed
    "iShares MSCI Switzerland", "iShares MSCI Germany",
    "iShares MSCI Austria", "Vanguard FTSE Europe",
    "iShares MSCI United Kingdom", "iShares MSCI Japan",
    "iShares MSCI Australia", "iShares MSCI Canada",
    # Regional Equity — Emerging
    "iShares MSCI Emerging", "iShares MSCI China",
    "iShares MSCI Brazil", "iShares MSCI India",
    "iShares MSCI Taiwan",
    # US Equity — Style/Size
    "Vanguard Growth", "Vanguard Value",
    "Vanguard Mid-Cap", "Vanguard Small-Cap",
    # Fixed Income
    "iShares US Agg Bond", "iShares 1-3yr Treasury",
    "iShares 7-10yr Treasury", "iShares 20+yr Treasury",
    "iShares TIPS", "iShares Inv Grade Corp",
    "Vanguard Intl Bond", "iShares EM Bond",
    # Alternatives
    "Vanguard US Real Estate", "Vanguard Intl Real Estate",
    "SPDR Gold", "Invesco DB Commodity", "ProShares Bitcoin",
    # FX
    "CHF per USD", "EUR per USD"
  ),
  asset_class = c(
    rep("Equity", 4), rep("Equity", 8), rep("Equity", 5),
    rep("Equity", 4),
    rep("Bond", 8),
    "Real Estate", "Real Estate", "Commodity", "Commodity", "Crypto",
    "FX", "FX"
  ),
  region = c(
    "Global", "US", "International", "Emerging",
    "Switzerland", "Germany", "Austria", "Europe", "UK",
    "Japan", "Australia", "Canada",
    "Emerging", "China", "Brazil", "India", "Taiwan",
    "US", "US", "US", "US",
    rep("US", 6), "International", "Emerging",
    "US", "International", "Global", "Global", "Global",
    "Global", "Global"
  ),
  sub_class = c(
    "Total Market", "Total Market", "Developed", "Emerging",
    "Country", "Country", "Country", "Regional",
    "Country", "Country", "Country", "Country",
    "Emerging", "Country", "Country", "Country", "Country",
    "Growth", "Value", "Mid Cap", "Small Cap",
    "Aggregate", "Short Duration", "Medium Duration",
    "Long Duration", "Inflation Protected",
    "Investment Grade", "Aggregate", "Emerging Debt",
    "REIT", "REIT", "Precious Metals", "Broad Commodities",
    "Cryptocurrency",
    "Currency", "Currency"
  ),
  type = c(rep("ETF", length(tickers)), "FX", "FX"),
  stringsAsFactors = FALSE
)

# --- Fetch real monthly returns ---

cat("Fetching monthly returns from Yahoo Finance...\n")
from_date <- "2016-01-01"
to_date <- as.character(Sys.Date())

# ETF returns
returns_list <- lapply(tickers, function(tkr) {
  cat("  ", tkr, "... ")
  tryCatch({
    raw <- getSymbols(tkr, src = "yahoo", from = from_date, to = to_date,
      periodicity = "monthly", auto.assign = FALSE)
    # Monthly return from adjusted close
    adj <- Ad(raw)
    ret <- diff(log(adj))[-1]  # log returns, drop first NA
    result <- data.frame(
      date = zoo::index(ret),
      ticker = tkr,
      return = as.numeric(ret),
      stringsAsFactors = FALSE
    )
    cat(nrow(result), "months\n")
    result
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
    NULL
  })
})

# FX returns
fx_list <- lapply(seq_along(fx_pairs), function(i) {
  pair <- fx_pairs[i]
  pseudo <- c(".CHFUSD", ".EURUSD")[i]
  cat("  ", pair, "... ")
  tryCatch({
    raw <- getSymbols(pair, src = "yahoo", from = from_date, to = to_date,
      periodicity = "monthly", auto.assign = FALSE)
    adj <- Cl(raw)  # FX uses close, not adjusted
    ret <- diff(log(adj))[-1]
    result <- data.frame(
      date = zoo::index(ret),
      ticker = pseudo,
      return = as.numeric(ret),
      stringsAsFactors = FALSE
    )
    cat(nrow(result), "months\n")
    result
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
    NULL
  })
})

# Combine
all_returns <- do.call(rbind,
  Filter(Negate(is.null), c(returns_list, fx_list)))
cat("Total return rows:", nrow(all_returns), "\n")
cat("Tickers with data:", length(unique(all_returns$ticker)), "\n")

# --- Build dm ---

portfolio_dm <- dm(metadata = metadata, returns = all_returns) |>
  dm_add_pk(metadata, ticker) |>
  dm_add_fk(returns, ticker, metadata)

saveRDS(portfolio_dm, "inst/extdata/portfolio_dm.rds")
cat("Saved portfolio_dm.rds\n")
