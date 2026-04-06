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
#' @return List of constraint parameters
#' @noRd
pf_build_constraints <- function(risk_val, horizon_val, max_weight,
                                  metadata, ticker_limits = NULL) {
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
    group_min = c(0, 0, 0)
  )
}

#' Run portfolio optimization
#'
#' @param returns_xts xts object of asset returns
#' @param strategy Character: optimization strategy
#' @param constraints List from pf_build_constraints
#' @return List with weights element (named numeric vector)
#' @noRd
pf_optimize <- function(returns_xts, strategy, constraints) {
  funds <- colnames(returns_xts)

  # PortfolioAnalytics needs these in the search path (not just namespace)
  if (!"package:ROI" %in% search())
    suppressPackageStartupMessages(require(ROI, quietly = TRUE))
  if (!"package:ROI.plugin.quadprog" %in% search())
    suppressPackageStartupMessages(require(ROI.plugin.quadprog, quietly = TRUE))
  if (!"package:PortfolioAnalytics" %in% search())
    suppressPackageStartupMessages(require(PortfolioAnalytics, quietly = TRUE))

  pspec <- PortfolioAnalytics::portfolio.spec(assets = funds)
  pspec <- PortfolioAnalytics::add.constraint(pspec,
    type = "full_investment")

  # Per-ticker box constraints
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

  # Ridge-regularized moments for numerical stability.
  # Adding a small diagonal term to the covariance matrix prevents
  # the QP solver from failing at certain risk_aversion values
  # when per-ticker box constraints create ill-conditioned Hessians.
  ridge_moments <- function(R, portfolio) {
    list(
      mu = colMeans(R, na.rm = TRUE),
      sigma = stats::cov(R) + 0.05 * diag(ncol(R))
    )
  }

  result <- tryCatch(
    switch(strategy,
      mean_variance = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "return", name = "mean")
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk", name = "StdDev",
          risk_aversion = constraints$risk_aversion)
        PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
          optimize_method = "ROI", momentFUN = ridge_moments)
      },
      min_vol = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk", name = "var")
        PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
          optimize_method = "ROI", momentFUN = ridge_moments)
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

  # Guard: if optimizer returned NA weights, report error
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
  message("[OPTIMIZER] Called with strategy=", strategy,
    " max_weight=", max_weight,
    " ticker_limits=",
    if (length(ticker_limits) == 0) "none"
    else paste(names(ticker_limits), "=",
      unlist(ticker_limits), collapse=", "))
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

  constraints <- pf_build_constraints(risk_val, horizon_val,
    max_weight, metadata, ticker_limits = ticker_limits)
  message("[OPTIMIZER] constraints$max_weight: ", constraints$max_weight)
  message("[OPTIMIZER] constraints$ticker_limits: ",
    if (is.null(constraints$ticker_limits)) "NULL"
    else paste(names(constraints$ticker_limits), "=",
      constraints$ticker_limits, collapse = ", "))
  opt <- pf_optimize(returns_xts, strategy, constraints)
  weights <- pf_clean_weights(opt$weights)
  # Log constrained ticker weights
  if (!is.null(ticker_limits)) {
    for (tkr in names(ticker_limits)) {
      message("[OPTIMIZER] ", tkr, " weight: ",
        round(weights[tkr], 4), " (limit: ", ticker_limits[[tkr]], ")")
    }
  }

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
