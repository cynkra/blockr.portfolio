# Build bundled data for blockr.portfolio
#
# Creates a dm object with three tables:
#   metadata  — ticker info (36 rows: 16 ETFs + 20 stocks)
#   returns   — monthly returns for ETFs (synthetic, for portfolio optimization)
#   ohlc      — daily OHLC for all tickers (real from Yahoo, for explorer)

library(dm)

# --- Ticker universes ---
etf_tickers <- c(
  "SPY", "QQQ", "IWM",
  "EFA", "VEA",
  "VWO", "EEM",
  "AGG", "BND", "TLT",
  "BNDX", "TIP",
  "VNQ",
  "GLD", "DBC",
  "BITO"
)

stock_tickers <- c(
  "AAPL", "MSFT", "GOOG", "AMZN", "NVDA", "META", "TSLA",
  "JPM", "V", "MA", "BAC",
  "JNJ", "UNH", "PFE",
  "WMT", "PG", "HD", "KO", "PEP",
  "XOM"
)

all_tickers <- c(etf_tickers, stock_tickers)

# --- Metadata ---
metadata <- data.frame(
  ticker = all_tickers,
  name = c(
    "S&P 500", "Nasdaq 100", "Russell 2000",
    "MSCI EAFE", "FTSE Developed",
    "FTSE Emerging", "MSCI Emerging",
    "US Agg Bond", "US Total Bond", "20+ Year Treasury",
    "Intl Bond", "TIPS",
    "US Real Estate",
    "Gold", "Commodities", "Bitcoin ETF",
    "Apple", "Microsoft", "Alphabet", "Amazon", "NVIDIA", "Meta", "Tesla",
    "JPMorgan Chase", "Visa", "Mastercard", "Bank of America",
    "Johnson & Johnson", "UnitedHealth", "Pfizer",
    "Walmart", "Procter & Gamble", "Home Depot", "Coca-Cola", "PepsiCo",
    "ExxonMobil"
  ),
  asset_class = c(
    rep("Equity", 7), rep("Bond", 5), "Real Estate",
    rep("Commodity", 2), "Crypto",
    rep("Equity", 20)
  ),
  region = c(
    rep("US", 3), rep("International", 2), rep("Emerging", 2),
    rep("US", 3), "International", "US", "US", rep("Global", 2), "Global",
    rep("US", 20)
  ),
  sub_class = c(
    "Large Cap", "Large Cap Growth", "Small Cap",
    "Developed", "Developed", "Emerging", "Emerging",
    "Aggregate", "Aggregate", "Long Duration",
    "Aggregate", "Inflation Protected",
    "REIT", "Precious Metals", "Broad Commodities", "Cryptocurrency",
    rep("Technology", 7), rep("Financials", 4),
    rep("Healthcare", 3), rep("Consumer", 5), "Energy"
  ),
  type = c(rep("ETF", 16), rep("Stock", 20)),
  stringsAsFactors = FALSE
)

# --- Synthetic monthly returns for ETFs (10 years) ---
# Used by the portfolio optimizer. Synthetic ensures reproducibility.
assumptions <- list(
  SPY  = list(mu = 0.10,  vol = 0.15),
  QQQ  = list(mu = 0.13,  vol = 0.20),
  IWM  = list(mu = 0.08,  vol = 0.20),
  EFA  = list(mu = 0.05,  vol = 0.16),
  VEA  = list(mu = 0.05,  vol = 0.16),
  VWO  = list(mu = 0.04,  vol = 0.20),
  EEM  = list(mu = 0.04,  vol = 0.22),
  AGG  = list(mu = 0.03,  vol = 0.04),
  BND  = list(mu = 0.03,  vol = 0.04),
  TLT  = list(mu = 0.04,  vol = 0.14),
  BNDX = list(mu = 0.02,  vol = 0.05),
  TIP  = list(mu = 0.03,  vol = 0.05),
  VNQ  = list(mu = 0.07,  vol = 0.18),
  GLD  = list(mu = 0.06,  vol = 0.16),
  DBC  = list(mu = 0.01,  vol = 0.18),
  BITO = list(mu = 0.15,  vol = 0.70)
)

set.seed(42)
n_months <- 120
dates <- seq.Date(as.Date("2016-02-01"), by = "month", length.out = n_months)
n_etfs <- length(etf_tickers)

cor_mat <- diag(n_etfs)
rownames(cor_mat) <- colnames(cor_mat) <- etf_tickers

equity_tickers <- c("SPY", "QQQ", "IWM", "EFA", "VEA", "VWO", "EEM")
bond_tickers <- c("AGG", "BND", "TLT", "BNDX", "TIP")

for (i in seq_along(equity_tickers)) {
  for (j in seq_along(equity_tickers)) {
    if (i != j) cor_mat[equity_tickers[i], equity_tickers[j]] <- 0.7
  }
}
for (i in seq_along(bond_tickers)) {
  for (j in seq_along(bond_tickers)) {
    if (i != j) cor_mat[bond_tickers[i], bond_tickers[j]] <- 0.65
  }
}
for (eq in equity_tickers) {
  for (bd in bond_tickers) {
    cor_mat[eq, bd] <- -0.2
    cor_mat[bd, eq] <- -0.2
  }
}
for (eq in equity_tickers) {
  cor_mat["VNQ", eq] <- 0.5
  cor_mat[eq, "VNQ"] <- 0.5
}
for (t in etf_tickers) {
  if (!t %in% c("GLD", "DBC", "BITO")) {
    cor_mat["GLD", t] <- 0.05
    cor_mat[t, "GLD"] <- 0.05
    cor_mat["DBC", t] <- 0.15
    cor_mat[t, "DBC"] <- 0.15
  }
}
cor_mat["GLD", "DBC"] <- 0.3
cor_mat["DBC", "GLD"] <- 0.3
for (eq in equity_tickers) {
  cor_mat["BITO", eq] <- 0.4
  cor_mat[eq, "BITO"] <- 0.4
}
cor_mat["BITO", "GLD"] <- 0.15
cor_mat["GLD", "BITO"] <- 0.15

eigen_vals <- eigen(cor_mat)$values
if (any(eigen_vals <= 0)) {
  cor_mat <- as.matrix(Matrix::nearPD(cor_mat, corr = TRUE)$mat)
}

monthly_vol <- sapply(etf_tickers, function(t) assumptions[[t]]$vol / sqrt(12))
monthly_mu <- sapply(etf_tickers, function(t) assumptions[[t]]$mu / 12)
cov_mat <- diag(monthly_vol) %*% cor_mat %*% diag(monthly_vol)
L <- chol(cov_mat)
Z <- matrix(rnorm(n_months * n_etfs), nrow = n_months, ncol = n_etfs)
returns_mat <- Z %*% L
returns_mat <- sweep(returns_mat, 2, monthly_mu, "+")
colnames(returns_mat) <- etf_tickers

bito_start <- which(dates >= as.Date("2021-10-01"))[1]
if (!is.na(bito_start) && bito_start > 1) {
  returns_mat[1:(bito_start - 1), "BITO"] <- NA
}

returns_df <- data.frame(
  date = rep(dates, n_etfs),
  ticker = rep(etf_tickers, each = n_months),
  return = as.numeric(returns_mat),
  stringsAsFactors = FALSE
)
returns_df <- returns_df[!is.na(returns_df$return), ]

# --- Daily OHLC for all tickers (real data from Yahoo) ---
cat("Fetching daily OHLC data from Yahoo Finance...\n")
ohlc_list <- lapply(all_tickers, function(tkr) {
  cat("  ", tkr, "... ")
  tryCatch({
    raw <- quantmod::getSymbols(
      tkr, src = "yahoo",
      from = Sys.Date() - 365, to = Sys.Date(),
      periodicity = "daily", auto.assign = FALSE
    )
    result <- data.frame(
      date = zoo::index(raw),
      ticker = tkr,
      open = as.numeric(quantmod::Op(raw)),
      high = as.numeric(quantmod::Hi(raw)),
      low = as.numeric(quantmod::Lo(raw)),
      close = as.numeric(quantmod::Cl(raw)),
      volume = as.numeric(quantmod::Vo(raw)),
      adjusted = as.numeric(quantmod::Ad(raw)),
      stringsAsFactors = FALSE
    )
    cat(nrow(result), "rows\n")
    result
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
    NULL
  })
})
ohlc_df <- do.call(rbind, Filter(Negate(is.null), ohlc_list))
cat("Total OHLC rows:", nrow(ohlc_df), "\n")

# --- Build dm ---
portfolio_dm <- dm(metadata = metadata, returns = returns_df, ohlc = ohlc_df) |>
  dm_add_pk(metadata, ticker) |>
  dm_add_fk(returns, ticker, metadata) |>
  dm_add_fk(ohlc, ticker, metadata)

saveRDS(portfolio_dm, "inst/extdata/portfolio_dm.rds")
cat("Saved portfolio_dm.rds\n")
