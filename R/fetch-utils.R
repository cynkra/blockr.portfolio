#' Fetch OHLC data for one or more tickers
#'
#' @param tickers Character vector of ticker symbols
#' @param from Start date
#' @param to End date
#' @param periodicity "daily", "weekly", or "monthly"
#' @param source "yahoo", "av", or "tiingo"
#' @return Data frame with date, ticker, open, high, low, close, volume, adjusted
#' @noRd
pf_fetch_tickers <- function(tickers, from, to, periodicity = "daily",
                              source = "yahoo") {
  # Fetch each ticker individually so one failure doesn't kill the batch
  dfs <- lapply(tickers, function(tkr) {
    tryCatch({
      raw <- quantmod::getSymbols(
        tkr, src = source, from = from, to = to,
        periodicity = periodicity, auto.assign = FALSE
      )
      data.frame(
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
    }, error = function(e) {
      # Try bundled fallback for this ticker
      pf_bundled_ohlc(tkr, from, to)
    })
  })
  result <- do.call(rbind, Filter(function(d) nrow(d) > 0, dfs))
  if (is.null(result) || nrow(result) == 0) {
    # All failed — return empty frame
    data.frame(
      date = as.Date(character(0)), ticker = character(0),
      open = numeric(0), high = numeric(0), low = numeric(0),
      close = numeric(0), volume = numeric(0), adjusted = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    result
  }
}

#' Read bundled OHLC data as fallback
#'
#' @param tickers Character vector of ticker symbols
#' @param from Start date
#' @param to End date
#' @return Data frame with OHLC columns
#' @noRd
pf_bundled_ohlc <- function(tickers, from, to) {
  rds_path <- system.file("extdata", "portfolio_dm.rds",
    package = "blockr.portfolio")
  if (rds_path == "") {
    warning("No bundled data available")
    return(data.frame(
      date = as.Date(character(0)), ticker = character(0),
      open = numeric(0), high = numeric(0), low = numeric(0),
      close = numeric(0), volume = numeric(0), adjusted = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  dm_obj <- readRDS(rds_path)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"ohlc" %in% names(tbls)) {
    warning("No OHLC table in bundled data")
    return(data.frame(
      date = as.Date(character(0)), ticker = character(0),
      open = numeric(0), high = numeric(0), low = numeric(0),
      close = numeric(0), volume = numeric(0), adjusted = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  ohlc <- as.data.frame(tbls[["ohlc"]])
  ohlc <- ohlc[ohlc$ticker %in% tickers &
    ohlc$date >= as.Date(from) &
    ohlc$date <= as.Date(to), , drop = FALSE]
  if (nrow(ohlc) == 0) {
    warning("No bundled data for tickers: ", paste(tickers, collapse = ", "))
  }
  ohlc
}

#' Get bundled ticker metadata (portfolio ETFs only)
#'
#' @return Data frame with ticker, name, type columns
#' @noRd
pf_bundled_tickers <- function() {
  rds_path <- system.file("extdata", "portfolio_dm.rds",
    package = "blockr.portfolio")
  if (rds_path == "") return(data.frame(
    ticker = character(0), name = character(0), type = character(0),
    stringsAsFactors = FALSE
  ))
  dm_obj <- readRDS(rds_path)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"metadata" %in% names(tbls)) return(data.frame(
    ticker = character(0), name = character(0), type = character(0),
    stringsAsFactors = FALSE
  ))
  meta <- as.data.frame(tbls[["metadata"]])
  cols <- intersect(c("ticker", "name", "type", "sub_class"), names(meta))
  meta[, cols, drop = FALSE]
}

#' Comprehensive global ticker catalog
#'
#' Loaded once into memory from inst/extdata/ticker_catalog.rds.
#' Contains ~42K tickers across 23 exchanges (US, Europe, Asia).
#'
#' @return Data frame with ticker, name, exchange, country, type, isin
#' @noRd
pf_ticker_catalog <- function() {
  cache <- .pf_catalog_env$catalog
  if (!is.null(cache)) return(cache)
  rds_path <- system.file("extdata", "ticker_catalog.rds",
    package = "blockr.portfolio")
  if (rds_path == "") return(data.frame(
    ticker = character(0), name = character(0), exchange = character(0),
    country = character(0), type = character(0), isin = character(0),
    stringsAsFactors = FALSE
  ))
  .pf_catalog_env$catalog <- readRDS(rds_path)
  .pf_catalog_env$catalog
}

# Package-level cache for the ticker catalog
.pf_catalog_env <- new.env(parent = emptyenv())

#' Search for tickers via catalog + Yahoo Finance fallback
#'
#' Searches the global ticker catalog (~42K tickers) first.
#' Falls back to Yahoo Finance for anything not in the catalog.
#'
#' @param query Search string (e.g., "apple", "roche", "RO.SW")
#' @param limit Max results
#' @return Data frame with ticker, name, sector, source columns
#' @noRd
pf_search_tickers <- function(query, limit = 20) {
  if (is.null(query) || !nzchar(trimws(query))) return(NULL)
  query_lower <- tolower(trimws(query))

  # Search the comprehensive catalog
  catalog <- pf_ticker_catalog()
  if (nrow(catalog) > 0) {
    tickers_lower <- tolower(catalog$ticker)
    names_lower <- tolower(catalog$name)
    countries_lower <- tolower(catalog$country)

    # Score: 1 = exact ticker, 2 = ticker starts with query,
    #        3 = word-boundary match in name, 4 = substring match
    exact_ticker <- tickers_lower == query_lower
    starts_ticker <- startsWith(tickers_lower, query_lower) |
      startsWith(tickers_lower, paste0(query_lower, "."))
    # Word boundary: query appears after start-of-string or non-letter
    word_pat <- paste0("(^|[^a-z])", query_lower)
    word_name <- grepl(word_pat, names_lower)
    substr_match <- grepl(query_lower, tickers_lower, fixed = TRUE) |
      grepl(query_lower, names_lower, fixed = TRUE) |
      grepl(query_lower, countries_lower, fixed = TRUE)

    hit <- exact_ticker | starts_ticker | word_name | substr_match
    if (!any(hit)) hit <- rep(FALSE, nrow(catalog))

    score <- ifelse(exact_ticker, 1L,
      ifelse(starts_ticker, 2L,
        ifelse(word_name, 3L, 4L)))

    matches <- catalog[hit, , drop = FALSE]
    scores <- score[hit]
    matches <- matches[order(scores, matches$ticker), ]
    matches <- utils::head(matches, limit)
    if (nrow(matches) > 0) {
      result <- data.frame(
        ticker = matches$ticker,
        name = matches$name,
        sector = ifelse(
          matches$type == "ETF", "ETF",
          ifelse(nzchar(matches$country), matches$country, matches$exchange)
        ),
        source = "catalog",
        stringsAsFactors = FALSE
      )
      return(result)
    }
  }

  # Fallback: Yahoo Finance live search
  tryCatch({
    url <- paste0(
      "https://query2.finance.yahoo.com/v1/finance/search?q=",
      utils::URLencode(query),
      "&quotesCount=", limit,
      "&newsCount=0"
    )
    resp <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
    if (is.null(resp$quotes) || nrow(resp$quotes) == 0) return(NULL)
    q <- resp$quotes
    data.frame(
      ticker = q$symbol,
      name = q$shortname %||% q$symbol,
      sector = q$quoteType %||% "EQUITY",
      source = "yahoo",
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

#' Build a dm from OHLC data and metadata
#'
#' @param ohlc_df Data frame with date, ticker, OHLC columns
#' @param metadata Data frame with ticker metadata (or NULL to generate minimal)
#' @return dm object with metadata and ohlc tables
#' @noRd
pf_ticker_dm <- function(ohlc_df, metadata = NULL) {
  tickers <- unique(ohlc_df$ticker)
  if (is.null(metadata)) {
    metadata <- data.frame(
      ticker = tickers,
      name = tickers,
      asset_class = "Equity",
      region = "US",
      sub_class = "Unknown",
      type = "Stock",
      stringsAsFactors = FALSE
    )
  } else {
    metadata <- metadata[metadata$ticker %in% tickers, , drop = FALSE]
  }
  dm::dm(metadata = metadata, ohlc = ohlc_df) |>
    dm::dm_add_pk(metadata, ticker) |>
    dm::dm_add_fk(ohlc, ticker, metadata)
}
