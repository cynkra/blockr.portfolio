#' Build a per-ticker μ tilt vector from investor preferences.
#'
#' Each ticker's tilt is the sum of its region tilt × k_region and its
#' sector tilt × k_sector, where region/sector tags come from
#' `metadata$region_tag` and `metadata$sector`. Broad ETFs with NA tags
#' get zero tilt contribution for that axis.
#'
#' Constants `k_region = 0.004` and `k_sector = 0.003` per step. On
#' monthly returns, ±2 on a region slider ≈ ±0.8%/month ≈ ±10%/yr
#' bias in expected return.
#'
#' @param metadata Data frame with ticker, region_tag, sector columns.
#' @param region_tilts Named integer vector (keys: us, europe, ch, em,
#'   asia_dev, japan).
#' @param sector_tilts Named integer vector (keys: tech, health, energy,
#'   financials, consumer).
#' @return Named numeric vector indexed by ticker, each value the μ shift.
#' @noRd
pf_build_tilt_vector <- function(metadata, region_tilts, sector_tilts,
    k_region = 0.004, k_sector = 0.003) {
  if (nrow(metadata) == 0) return(numeric(0))
  region_tilts <- if (is.null(region_tilts)) integer(0) else region_tilts
  sector_tilts <- if (is.null(sector_tilts)) integer(0) else sector_tilts

  r_tag <- if ("region_tag" %in% names(metadata))
    metadata$region_tag else rep(NA_character_, nrow(metadata))
  s_tag <- if ("sector" %in% names(metadata))
    metadata$sector else rep(NA_character_, nrow(metadata))

  r_vals <- as.numeric(region_tilts[r_tag])
  s_vals <- as.numeric(sector_tilts[s_tag])
  r_vals[is.na(r_vals)] <- 0
  s_vals[is.na(s_vals)] <- 0

  out <- k_region * r_vals + k_sector * s_vals
  names(out) <- metadata$ticker
  out
}

#' Build portfolio constraints from advisor inputs
#'
#' Maps risk appetite, horizon, and position limits to PortfolioAnalytics
#' constraint parameters.
#'
#' @param risk Character: "conservative", "moderate", or "aggressive"
#' @param horizon Character: "short", "medium", or "long"
#' @param max_weight Numeric: maximum weight per asset (0-1)
#' @param min_positions Integer: minimum number of assets
#' @param metadata Data frame with ticker and asset_class columns
#' @param tilt_vector Optional named numeric vector of μ shifts per ticker.
#' @return List of constraint parameters
#' @noRd
pf_build_constraints <- function(risk_val, horizon_val, max_weight,
                                  metadata, ticker_limits = NULL,
                                  tilt_vector = NULL) {
  # Continuous mapping from 0-100 values to optimization parameters
  # Risk aversion: exponential scale (0→20, 50→2, 100→0.1)
  risk_val <- max(0, min(100, as.numeric(risk_val)))
  horizon_val <- max(0, min(100, as.numeric(horizon_val)))

  risk_aversion <- 20 * exp(-4.6 * risk_val / 100)

  # Equity cap: 0.30 (conservative) to 1.0 (aggressive)
  equity_max <- 0.30 + 0.70 * risk_val / 100

  # Alternatives cap: 0.05 (conservative) to 0.50 (aggressive)
  alt_max <- 0.05 + 0.45 * risk_val / 100

  # Horizon adjustment: -0.20 (short) to +0.20 (long)
  horizon_adj <- -0.20 + 0.40 * horizon_val / 100
  equity_max <- min(1, equity_max + horizon_adj)
  alt_max <- min(1, max(0, alt_max + horizon_adj))

  tickers <- metadata$ticker
  equity_idx <- which(metadata$asset_class == "Equity")
  bond_idx <- which(metadata$asset_class == "Bond")
  alt_idx <- which(metadata$asset_class %in%
    c("Real Estate", "Commodity", "Crypto"))


  # NA means no explicit global constraint.

  # When per-ticker limits exist, use 0.50 as the default cap for

  # unconstrained tickers — having some at 0.10 and others at 1.0
  # creates numerically ill-conditioned problems for the QP solver.
  # Without per-ticker limits, use 1.0 (unconstrained).
  effective_max_weight <- if (!is.na(max_weight)) {
    max_weight
  } else if (!is.null(ticker_limits) && length(ticker_limits) > 0) {
    0.50
  } else {
    1.0
  }

  list(
    risk_aversion = risk_aversion,
    max_weight = effective_max_weight,
    ticker_limits = ticker_limits,
    groups = list(equity = equity_idx, bonds = bond_idx,
      alternatives = alt_idx),
    group_max = c(equity_max, 1.0, alt_max),
    group_min = c(0, 0, 0),
    tilt_vector = tilt_vector
  )
}

#' Read tilt values from a profile data frame.
#'
#' @param profile One-row profile data frame.
#' @param axis "region" or "sector".
#' @return Named integer vector keyed by the axis's tilt keys, with any
#'   missing columns defaulting to 0.
#' @noRd
pf_read_profile_tilts <- function(profile, axis) {
  keys <- switch(axis,
    region = c("us", "europe", "ch", "em", "asia_dev", "japan"),
    sector = c("tech", "health", "energy", "financials", "consumer"),
    stop("Unknown tilt axis: ", axis))
  out <- stats::setNames(integer(length(keys)), keys)
  for (k in keys) {
    col <- paste0("tilt_", k)
    if (col %in% names(profile)) {
      v <- suppressWarnings(as.integer(profile[[col]][1]))
      if (!is.na(v)) out[[k]] <- v
    }
  }
  out
}

#' Ledoit-Wolf-style shrinkage covariance
#'
#' Shrinks the sample covariance toward a constant-correlation target.
#' Stable for N up to ~200 with monthly data (where the sample covariance
#' breaks down near N = T / 3).
#'
#' @param R Matrix or xts of returns.
#' @param delta Shrinkage intensity in [0, 1]. Default 0.3 is a sensible
#'   prior for monthly equity + bond returns; full Ledoit-Wolf optimal-delta
#'   derivation is a future enhancement.
#' @return Numeric covariance matrix.
#' @noRd
pf_shrinkage_cov <- function(R, delta = 0.5) {
  S <- stats::cov(R, use = "pairwise.complete.obs")
  n <- ncol(S)
  if (n < 2) return(S)
  vars <- diag(S)
  sds <- sqrt(pmax(vars, 1e-12))
  C <- S / tcrossprod(sds)
  off <- C[lower.tri(C)]
  avg_cor <- if (length(off) > 0) mean(off, na.rm = TRUE) else 0
  if (!is.finite(avg_cor)) avg_cor <- 0
  target <- tcrossprod(sds) * avg_cor
  diag(target) <- vars
  out <- delta * target + (1 - delta) * S
  # Larger diagonal jitter — combined with per-ticker box constraints
  # and group constraints, the QP solver is more robust.
  diag(out) <- diag(out) + mean(vars) * 0.02
  out
}

#' Run portfolio optimization
#'
#' @param returns_xts xts object of asset returns.
#' @param strategy Character: optimization strategy.
#' @param constraints List from `pf_build_constraints`. May include
#'   `tilt_vector` (named numeric vector of μ shifts per ticker).
#' @return List with weights element (named numeric vector).
#' @noRd
pf_optimize <- function(returns_xts, strategy, constraints) {
  funds <- colnames(returns_xts)

  if (!"package:ROI" %in% search())
    suppressPackageStartupMessages(require(ROI, quietly = TRUE))
  if (!"package:ROI.plugin.quadprog" %in% search())
    suppressPackageStartupMessages(require(ROI.plugin.quadprog, quietly = TRUE))
  if (!"package:PortfolioAnalytics" %in% search())
    suppressPackageStartupMessages(require(PortfolioAnalytics, quietly = TRUE))

  pspec <- PortfolioAnalytics::portfolio.spec(assets = funds)
  pspec <- PortfolioAnalytics::add.constraint(pspec,
    type = "full_investment")

  max_vec <- rep(constraints$max_weight, length(funds))
  names(max_vec) <- funds
  if (!is.null(constraints$ticker_limits)) {
    for (tkr in names(constraints$ticker_limits)) {
      if (tkr %in% funds) {
        max_vec[tkr] <- constraints$ticker_limits[[tkr]]
      }
    }
  }
  min_vec <- rep(0, length(funds))
  names(min_vec) <- funds
  pspec <- PortfolioAnalytics::add.constraint(pspec, type = "box",
    min = min_vec, max = max_vec)

  has_ticker_limits <- !is.null(constraints$ticker_limits) &&
    length(constraints$ticker_limits) > 0

  non_empty <- lengths(constraints$groups) > 0
  if (any(non_empty)) {
    pspec <- PortfolioAnalytics::add.constraint(pspec, type = "group",
      groups = constraints$groups[non_empty],
      group_min = constraints$group_min[non_empty],
      group_max = constraints$group_max[non_empty])
  }

  # Moments function: shrinkage covariance + optional μ tilt.
  tilt_vec <- constraints$tilt_vector
  tilted_moments <- function(R, portfolio) {
    mu <- colMeans(R, na.rm = TRUE)
    if (!is.null(tilt_vec)) {
      tv <- tilt_vec[colnames(R)]
      tv[is.na(tv)] <- 0
      mu <- mu + tv
    }
    list(mu = mu, sigma = pf_shrinkage_cov(R))
  }

  # Solver routing: DEoptim only when per-ticker limits are in effect
  # (ROI handles box + group constraints fine; DEoptim is slower but
  # robust to the ill-conditioning that per-ticker caps introduce).
  use_deoptim <- has_ticker_limits

  result <- tryCatch(
    switch(strategy,
      mean_variance = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "return", name = "mean")
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk", name = "StdDev",
          risk_aversion = constraints$risk_aversion)
        if (use_deoptim) {
          PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
            optimize_method = "DEoptim",
            momentFUN = tilted_moments,
            search_size = min(3000, 200 * length(funds)),
            trace = FALSE)
        } else {
          PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
            optimize_method = "ROI", momentFUN = tilted_moments)
        }
      },
      min_vol = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk", name = "var")
        if (use_deoptim) {
          PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
            optimize_method = "DEoptim",
            momentFUN = tilted_moments,
            search_size = min(3000, 200 * length(funds)),
            trace = FALSE)
        } else {
          PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
            optimize_method = "ROI", momentFUN = tilted_moments)
        }
      },
      risk_parity = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk_budget", name = "StdDev",
          min_concentration = TRUE)
        PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
          optimize_method = "DEoptim",
          search_size = 2000, trace = FALSE)
      },
      equal_weight = {
        w <- rep(1 / length(funds), length(funds))
        names(w) <- funds
        list(weights = w)
      },
      {
        stop("Unknown strategy: ", strategy)
      }
    ),
    error = function(e) {
      stop(paste0(
        "Optimization failed: ", conditionMessage(e), "\n",
        "Try relaxing your constraints (increase max position weight, ",
        "remove individual position limits, or use a different strategy)."
      ))
    }
  )

  # ROI occasionally returns NA weights at certain constraint-boundary
  # combinations even when a valid portfolio exists. Fall back to DEoptim
  # (slower but robust) before giving up.
  if (!is.null(result$weights) && any(is.na(result$weights)) &&
      !use_deoptim && strategy %in% c("mean_variance", "min_vol")) {
    message("[OPTIMIZE] ROI returned NAs; falling back to DEoptim")
    result <- tryCatch(
      switch(strategy,
        mean_variance = PortfolioAnalytics::optimize.portfolio(
          returns_xts, pspec, optimize_method = "DEoptim",
          momentFUN = tilted_moments,
          search_size = min(3000, 200 * length(funds)), trace = FALSE),
        min_vol = PortfolioAnalytics::optimize.portfolio(
          returns_xts, pspec, optimize_method = "DEoptim",
          momentFUN = tilted_moments,
          search_size = min(3000, 200 * length(funds)), trace = FALSE)
      ),
      error = function(e) {
        stop("Fallback optimization also failed: ", conditionMessage(e))
      }
    )
  }

  if (!is.null(result$weights) && any(is.na(result$weights))) {
    stop(paste0(
      "Optimization could not find a valid portfolio with the current ",
      "constraints. Try relaxing position limits or increasing the ",
      "maximum position weight."
    ))
  }

  result
}

#' Compute portfolio backtest metrics
#'
#' @param returns_xts xts object of asset returns
#' @param weights Named numeric vector of portfolio weights
#' @return List with returns, cumulative, and performance metrics
#' @noRd
pf_backtest <- function(returns_xts, weights) {
  # Align weights to available columns
  w <- weights[colnames(returns_xts)]
  w[is.na(w)] <- 0
  if (sum(w) > 0) w <- w / sum(w)

  port_ret <- PerformanceAnalytics::Return.portfolio(returns_xts,
    weights = w)

  list(
    returns = port_ret,
    cumulative = cumprod(1 + port_ret) - 1,
    ann_return = as.numeric(
      PerformanceAnalytics::Return.annualized(port_ret)),
    ann_vol = as.numeric(
      PerformanceAnalytics::StdDev.annualized(port_ret)),
    sharpe = as.numeric(
      PerformanceAnalytics::SharpeRatio.annualized(port_ret)),
    max_dd = as.numeric(
      PerformanceAnalytics::maxDrawdown(port_ret)),
    var_95 = as.numeric(
      PerformanceAnalytics::VaR(port_ret, p = 0.95,
        method = "historical")),
    drawdown = PerformanceAnalytics::Drawdowns(port_ret)
  )
}

#' Construct benchmark weight vector
#'
#' @param benchmark Character: "60_40", "equal_weight", or "sp500"
#' @param metadata Data frame with ticker column
#' @param returns_xts xts object (used for available tickers)
#' @return Named numeric vector of weights
#' @noRd
pf_benchmark_weights <- function(benchmark, metadata, returns_xts) {
  tickers <- colnames(returns_xts)
  w <- rep(0, length(tickers))
  names(w) <- tickers

  switch(benchmark,
    "60_40" = {
      vti_idx <- match("VTI", tickers)
      agg_idx <- match("AGG", tickers)
      if (!is.na(vti_idx)) w[vti_idx] <- 0.6
      if (!is.na(agg_idx)) w[agg_idx] <- 0.4
      if (sum(w) == 0) w <- rep(1 / length(w), length(w))
      w <- w / sum(w)
    },
    global_60_40 = {
      vt_idx <- match("VT", tickers)
      agg_idx <- match("AGG", tickers)
      if (!is.na(vt_idx)) w[vt_idx] <- 0.6
      if (!is.na(agg_idx)) w[agg_idx] <- 0.4
      if (sum(w) == 0) w <- rep(1 / length(w), length(w))
      w <- w / sum(w)
    },
    equal_weight = {
      w <- rep(1 / length(tickers), length(tickers))
      names(w) <- tickers
    },
    us_market = {
      vti_idx <- match("VTI", tickers)
      if (!is.na(vti_idx)) {
        w[vti_idx] <- 1
      } else {
        w <- rep(1 / length(tickers), length(tickers))
      }
    }
  )
  w
}

#' Generate efficient frontier data
#'
#' @param returns_xts xts object of asset returns
#' @param constraints List from pf_build_constraints
#' @return List with frontier and assets data frames
#' @noRd
pf_efficient_frontier <- function(returns_xts, constraints) {
  funds <- colnames(returns_xts)
  pspec <- PortfolioAnalytics::portfolio.spec(assets = funds)
  pspec <- PortfolioAnalytics::add.constraint(pspec,
    type = "full_investment")
  pspec <- PortfolioAnalytics::add.constraint(pspec, type = "box",
    min = 0, max = constraints$max_weight)
  pspec <- PortfolioAnalytics::add.objective(pspec,
    type = "return", name = "mean")
  pspec <- PortfolioAnalytics::add.objective(pspec,
    type = "risk", name = "StdDev")

  ef <- tryCatch(
    PortfolioAnalytics::create.EfficientFrontier(
      returns_xts, pspec, type = "mean-StdDev", n.portfolios = 25),
    error = function(e) NULL
  )

  if (is.null(ef)) {
    # Fallback: generate simple frontier from individual assets
    asset_risk <- apply(returns_xts, 2, function(x) stats::sd(x, na.rm = TRUE) * sqrt(12))
    asset_ret <- apply(returns_xts, 2, function(x) mean(x, na.rm = TRUE) * 12)
    return(list(
      frontier = data.frame(
        risk = seq(min(asset_risk), max(asset_risk), length.out = 25),
        return = seq(min(asset_ret), max(asset_ret), length.out = 25)
      ),
      assets = data.frame(
        ticker = funds,
        risk = asset_risk,
        return = asset_ret
      )
    ))
  }

  list(
    frontier = data.frame(
      risk = ef$frontier[, "StdDev"],
      return = ef$frontier[, "mean"]
    ),
    assets = data.frame(
      ticker = funds,
      risk = apply(returns_xts, 2,
        function(x) stats::sd(x, na.rm = TRUE) * sqrt(12)),
      return = apply(returns_xts, 2,
        function(x) mean(x, na.rm = TRUE) * 12)
    )
  )
}

#' Convert long-format returns to wide xts
#'
#' @param returns_df Data frame with date, ticker, return columns
#' @return xts object with tickers as columns
#' @noRd
pf_to_xts <- function(returns_df) {
  wide <- stats::reshape(returns_df,
    idvar = "date", timevar = "ticker",
    direction = "wide", v.names = "return")
  dates <- as.Date(wide$date)
  mat <- as.matrix(wide[, -1, drop = FALSE])
  colnames(mat) <- sub("^return\\.", "", colnames(mat))

  # Drop tickers with ALL NAs, but keep rows with some NAs
  # (short-history tickers like BITO shouldn't limit the date range)
  keep <- apply(mat, 2, function(x) !all(is.na(x)))
  mat <- mat[, keep, drop = FALSE]

  # Drop rows where ALL tickers are NA (no data at all)
  any_data <- apply(mat, 1, function(x) !all(is.na(x)))
  mat <- mat[any_data, , drop = FALSE]
  dates <- dates[any_data]

  xts::xts(mat, order.by = dates)
}

#' Clean near-zero weights and renormalize
#' @param weights Named numeric vector
#' @return Cleaned weights
#' @noRd
pf_clean_weights <- function(weights) {
  weights[is.na(weights)] <- 0
  weights[weights < 1e-6] <- 0
  s <- sum(weights)
  if (!is.na(s) && s > 0) weights <- weights / s
  weights
}

#' Adjust returns to a target currency
#'
#' Converts USD-denominated returns to the investor's base currency.
#' The FX returns are stored in the returns table as pseudo-tickers
#' (.CHFUSD, .EURUSD).
#'
#' @param returns_xts xts with asset returns (USD-denominated)
#' @param fx_xts xts with FX returns (e.g., .CHFUSD column)
#' @param currency Target currency: "USD", "CHF", or "EUR"
#' @return xts with currency-adjusted returns
#' @noRd
pf_adjust_currency <- function(returns_xts, fx_xts, currency) {
  if (currency == "USD" || is.null(fx_xts)) return(returns_xts)

  fx_col <- switch(currency,
    CHF = ".CHFUSD",
    EUR = ".EURUSD",
    NULL
  )
  if (is.null(fx_col) || !fx_col %in% colnames(fx_xts)) {
    return(returns_xts)
  }

  fx_ret <- fx_xts[, fx_col]

  # Align by year-month (monthly data may have different day-of-month)
  ret_ym <- format(zoo::index(returns_xts), "%Y-%m")
  fx_ym <- format(zoo::index(fx_ret), "%Y-%m")
  common_ym <- intersect(ret_ym, fx_ym)

  ret_idx <- which(ret_ym %in% common_ym)
  fx_idx <- which(fx_ym %in% common_ym)

  # Match in order
  ret_match <- match(ret_ym[ret_idx], fx_ym[fx_idx])
  valid <- !is.na(ret_match)
  ret_idx <- ret_idx[valid]
  fx_idx <- fx_idx[ret_match[valid]]

  adjusted <- returns_xts[ret_idx, ]
  fx_vec <- as.numeric(fx_ret[fx_idx, ])

  # Adjust: r_local = (1 + r_usd) * (1 + r_fx) - 1
  for (col in colnames(adjusted)) {
    vals <- as.numeric(adjusted[, col])
    # Only adjust non-NA values
    non_na <- !is.na(vals)
    vals[non_na] <- (1 + vals[non_na]) * (1 + fx_vec[non_na]) - 1
    adjusted[, col] <- vals
  }
  adjusted
}

#' Run the full optimization pipeline
#'
#' Orchestrates constraint building, optimization, backtesting, frontier,
#' and risk contribution. Returns an enriched dm with result tables.
#'
#' @param dm_data dm object with metadata and returns tables
#' @param profile One-row data frame with risk, horizon, strategy, min_positions, currency
#' @param benchmark Character: "60_40", "equal_weight", or "sp500"
#' @param max_weight Numeric: max position weight (0-1)
#' @param compare Character vector of strategy IDs for comparison
#' @return dm object enriched with result tables
#' @noRd
pf_run_optimizer <- function(dm_data, profile, strategy = "mean_variance",
                              max_weight = NA_real_,
                              max_positions = NA_integer_,
                              ticker_limits = list()) {
  if (length(ticker_limits) == 0) ticker_limits <- NULL
  tbls <- dm::dm_get_tables(dm_data)
  metadata <- as.data.frame(tbls[["metadata"]])
  ret_df <- as.data.frame(tbls[["returns"]])

  # Separate FX returns from asset returns
  fx_tickers <- metadata$ticker[metadata$type == "FX"]
  asset_ret_df <- ret_df[!ret_df$ticker %in% fx_tickers, , drop = FALSE]
  fx_ret_df <- ret_df[ret_df$ticker %in% fx_tickers, , drop = FALSE]

  returns_xts <- pf_to_xts(asset_ret_df)
  fx_xts <- if (nrow(fx_ret_df) > 0) pf_to_xts(fx_ret_df) else NULL

  # Filter metadata to ETFs only (exclude FX)
  metadata <- metadata[metadata$type != "FX" &
    metadata$ticker %in% colnames(returns_xts), , drop = FALSE]

  risk_val <- as.numeric(profile$risk[1])
  horizon_val <- as.numeric(profile$horizon[1])
  if (is.na(risk_val)) risk_val <- 50
  if (is.na(horizon_val)) horizon_val <- 50
  amount <- if ("amount" %in% colnames(profile))
    as.numeric(profile$amount[1]) else NA_real_

  # Auto-derive max_positions from amount if set to -1 (auto mode)
  if (!is.na(max_positions) && max_positions == -1L && !is.na(amount)) {
    max_positions <- if (amount < 10000) 3L
      else if (amount < 50000) 5L
      else if (amount < 200000) 8L
      else if (amount < 500000) 12L
      else NA_integer_  # no limit for large portfolios
  }
  currency <- if ("currency" %in% colnames(profile))
    profile$currency[1] else "USD"

  # Adjust returns to target currency
  returns_xts <- pf_adjust_currency(returns_xts, fx_xts, currency)

  # Drop tickers with insufficient data and fill remaining NAs
  enough_data <- apply(returns_xts, 2, function(x) sum(!is.na(x)) >= 24)
  returns_xts <- returns_xts[, enough_data, drop = FALSE]
  metadata <- metadata[metadata$ticker %in% colnames(returns_xts), ,
    drop = FALSE]
  returns_xts[is.na(returns_xts)] <- 0

  # Over-filter safety: bail out cleanly if the surviving universe is
  # too small for a meaningful optimization.
  if (ncol(returns_xts) < 3) {
    stop(paste0(
      "Only ", ncol(returns_xts), " ETF(s) survive the filters — need at ",
      "least 3. Try loosening the TER cap, the minimum AUM, or the region/",
      "sector exclusions."))
  }

  # Build tilt vector from profile columns (tilt_us, tilt_tech, ...)
  region_tilts <- pf_read_profile_tilts(profile, "region")
  sector_tilts <- pf_read_profile_tilts(profile, "sector")
  tilt_vector <- pf_build_tilt_vector(metadata, region_tilts, sector_tilts)
  # Tilts only bite on mean_variance — μ is ignored otherwise.
  if (!identical(strategy, "mean_variance")) tilt_vector <- NULL

  constraints <- pf_build_constraints(risk_val, horizon_val,
    max_weight, metadata, ticker_limits = ticker_limits,
    tilt_vector = tilt_vector)
  opt <- pf_optimize(returns_xts, strategy, constraints)
  weights <- pf_clean_weights(opt$weights)

  # Limit to max_positions: keep top N by weight, zero the rest
  if (!is.na(max_positions) && sum(weights > 0) > max_positions) {
    ranked <- order(weights, decreasing = TRUE)
    keep_idx <- ranked[seq_len(max_positions)]
    weights[-keep_idx] <- 0
    # Renormalize, then re-enforce per-ticker limits iteratively
    for (iter in 1:5) {
      s <- sum(weights)
      if (s > 0) weights <- weights / s
      if (is.null(ticker_limits)) break
      excess <- 0
      capped_tickers <- character(0)
      for (tkr in names(ticker_limits)) {
        if (tkr %in% names(weights) &&
            weights[tkr] > ticker_limits[[tkr]]) {
          excess <- excess + weights[tkr] - ticker_limits[[tkr]]
          weights[tkr] <- ticker_limits[[tkr]]
          capped_tickers <- c(capped_tickers, tkr)
        }
      }
      if (excess == 0) break
      # Redistribute excess to uncapped positions proportionally
      uncapped <- setdiff(names(weights[weights > 0]), capped_tickers)
      if (length(uncapped) > 0) {
        uw <- weights[uncapped]
        weights[uncapped] <- uw + excess * uw / sum(uw)
      }
    }
    weights[weights < 1e-6] <- 0
  }

  bt <- pf_backtest(returns_xts, weights)
  frontier_data <- pf_efficient_frontier(returns_xts, constraints)
  risk_contrib_data <- pf_risk_contribution(returns_xts, weights)

  weights_df <- data.frame(
    ticker = names(weights), weight = as.numeric(weights),
    asset_class = metadata$asset_class[match(names(weights),
      metadata$ticker)],
    stringsAsFactors = FALSE
  )
  backtest_df <- data.frame(
    date = zoo::index(bt$returns),
    return = as.numeric(bt$returns),
    cumulative = as.numeric(bt$cumulative),
    drawdown = as.numeric(bt$drawdown),
    stringsAsFactors = FALSE
  )
  metrics_df <- data.frame(
    strategy = strategy,
    ann_return = bt$ann_return, ann_vol = bt$ann_vol,
    sharpe = bt$sharpe, max_dd = bt$max_dd, var_95 = bt$var_95,
    stringsAsFactors = FALSE
  )

  result_dm <- dm_data |>
    dm::dm(
      weights = weights_df, backtest = backtest_df,
      metrics = metrics_df,
      frontier = frontier_data$frontier, assets = frontier_data$assets,
      risk_contrib = risk_contrib_data
    ) |>
    dm::dm_add_fk(weights, ticker, metadata) |>
    dm::dm_add_fk(risk_contrib, ticker, metadata) |>
    dm::dm_add_fk(assets, ticker, metadata)

  result_dm
}

#' Compute risk contribution per asset
#'
#' @param returns_xts xts object of asset returns
#' @param weights Named numeric vector of portfolio weights
#' @return Data frame with ticker and contribution columns
#' @noRd
pf_risk_contribution <- function(returns_xts, weights) {
  w <- weights[colnames(returns_xts)]
  w[is.na(w)] <- 0
  if (sum(w) == 0) {
    return(data.frame(
      ticker = names(w),
      contribution = rep(0, length(w))
    ))
  }

  cov_mat <- stats::cov(returns_xts, use = "pairwise.complete.obs")
  port_var <- as.numeric(t(w) %*% cov_mat %*% w)
  if (port_var <= 0) {
    return(data.frame(
      ticker = names(w),
      contribution = rep(0, length(w))
    ))
  }
  marginal <- as.numeric(cov_mat %*% w)
  contrib <- w * marginal / sqrt(port_var)
  total <- sum(abs(contrib))
  pct_contrib <- if (total > 0) contrib / total * 100 else rep(0, length(w))

  data.frame(
    ticker = names(w),
    contribution = pct_contrib
  )
}
