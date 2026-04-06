# Build comprehensive global ticker catalog for the share explorer
#
# Sources:
#   - US: TTR::stockSymbols() (NASDAQ, NYSE, AMEX) — no API key needed
#   - International: EODHD Exchanges API (free tier) — needs API key
#
# Output: inst/extdata/ticker_catalog.rds
#
# Run manually to refresh:
#   EODHD_API_KEY=your_key Rscript data-raw/build-ticker-catalog.R
#
# Without EODHD_API_KEY, only US tickers are fetched.

library(TTR)

eodhd_key <- Sys.getenv("EODHD_API_KEY", "")

# --- EODHD helper ---

fetch_eodhd_exchange <- function(exchange_code, api_key) {
  url <- sprintf(
    "https://eodhd.com/api/exchange-symbol-list/%s?api_token=%s&fmt=json",
    exchange_code, api_key
  )
  tryCatch({
    raw <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    data.frame(
      ticker   = paste0(raw$Code, ".", exchange_code),
      name     = raw$Name,
      exchange = exchange_code,
      country  = raw$Country,
      type     = raw$Type,
      isin     = if ("Isin" %in% names(raw)) raw$Isin else NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("    FAILED:", e$message, "\n")
    NULL
  })
}

# --- 1. US tickers via TTR (no key needed) ---

cat("== US tickers (TTR) ==\n")
syms <- stockSymbols(exchange = c("NASDAQ", "NYSE", "AMEX"))
us <- syms[!syms$Test.Issue, ]
us_catalog <- data.frame(
  ticker   = us$Symbol,
  name     = us$Name,
  exchange = us$Exchange,
  country  = "USA",
  type     = ifelse(us$ETF, "ETF", "Common Stock"),
  isin     = NA_character_,
  stringsAsFactors = FALSE
)
cat("  US tickers:", nrow(us_catalog), "\n")

# --- 2. International tickers via EODHD ---

intl_catalog <- NULL

if (nzchar(eodhd_key)) {
  # Priority order: DACH → Europe → Asia → Other
  exchanges <- c(
    # DACH
    "SW", "VI", "XETRA",
    # Europe
    "LSE", "PA", "AS", "BR", "LS", "MC", "ST", "OL", "HE", "CO", "IR",
    # Major Asian
    "TO", "HK", "SHG", "SHE"
  )

  cat("\n== International tickers (EODHD) ==\n")
  intl_parts <- lapply(exchanges, function(ex) {
    cat("  ", ex, "... ")
    df <- fetch_eodhd_exchange(ex, eodhd_key)
    if (!is.null(df)) cat(nrow(df), "tickers\n") else cat("0\n")
    df
  })
  intl_catalog <- do.call(rbind, Filter(Negate(is.null), intl_parts))
  cat("  International total:", nrow(intl_catalog), "\n")
} else {
  cat("\n== Skipping international (no EODHD_API_KEY) ==\n")
}

# --- 3. Combine and deduplicate ---

catalog <- rbind(us_catalog, intl_catalog)
catalog <- catalog[!duplicated(catalog$ticker), ]
catalog <- catalog[order(catalog$exchange, catalog$ticker), ]
rownames(catalog) <- NULL

cat("\n== Summary ==\n")
cat("  Total tickers:", nrow(catalog), "\n")
cat("  Exchanges:", length(unique(catalog$exchange)), "\n")
cat("  By exchange:\n")
counts <- sort(table(catalog$exchange), decreasing = TRUE)
for (i in seq_along(counts)) {
  cat("    ", names(counts)[i], ":", counts[i], "\n")
}

# --- 4. Save ---

outpath <- file.path("inst", "extdata", "ticker_catalog.rds")
saveRDS(catalog, outpath)
cat("\n  Saved:", outpath, "(", round(file.size(outpath) / 1024), "KB )\n")
