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
pf_build_constraints <- function(risk, horizon, max_weight, min_positions,
                                  metadata) {
  risk_aversion <- switch(risk,
    conservative = 10,
    moderate = 2,
    aggressive = 0.5,
    2
  )

  equity_max <- switch(risk,
    conservative = 0.40,
    moderate = 0.70,
    aggressive = 1.0,
    0.70
  )
  alt_max <- switch(risk,
    conservative = 0.10,
    moderate = 0.25,
    aggressive = 0.50,
    0.25
  )

  horizon_adj <- switch(horizon,
    short = -0.10,
    medium = 0,
    long = 0.10,
    0
  )
  equity_max <- min(1, equity_max + horizon_adj)
  alt_max <- min(1, alt_max + horizon_adj)

  tickers <- metadata$ticker
  equity_idx <- which(metadata$asset_class == "Equity")
  bond_idx <- which(metadata$asset_class == "Bond")
  alt_idx <- which(metadata$asset_class %in%
    c("Real Estate", "Commodity", "Crypto"))

  list(
    risk_aversion = risk_aversion,
    max_weight = max_weight,
    min_positions = min_positions,
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
  pspec <- PortfolioAnalytics::portfolio.spec(assets = funds)
  pspec <- PortfolioAnalytics::add.constraint(pspec,
    type = "full_investment")
  pspec <- PortfolioAnalytics::add.constraint(pspec, type = "box",
    min = 0, max = constraints$max_weight)

  # Group constraints
  non_empty <- lengths(constraints$groups) > 0
  if (any(non_empty)) {
    pspec <- PortfolioAnalytics::add.constraint(pspec, type = "group",
      groups = constraints$groups[non_empty],
      group_min = constraints$group_min[non_empty],
      group_max = constraints$group_max[non_empty])
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
          optimize_method = "ROI")
      },
      min_vol = {
        pspec <- PortfolioAnalytics::add.objective(pspec,
          type = "risk", name = "var")
        PortfolioAnalytics::optimize.portfolio(returns_xts, pspec,
          optimize_method = "ROI")
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
        w <- rep(1 / length(funds), length(funds))
        names(w) <- funds
        list(weights = w)
      }
    ),
    error = function(e) {
      w <- rep(1 / length(funds), length(funds))
      names(w) <- funds
      list(weights = w)
    }
  )

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
      spy_idx <- match("SPY", tickers)
      agg_idx <- match("AGG", tickers)
      if (!is.na(spy_idx)) w[spy_idx] <- 0.6
      if (!is.na(agg_idx)) w[agg_idx] <- 0.4
      if (sum(w) == 0) w <- rep(1 / length(w), length(w))
      w <- w / sum(w)
    },
    equal_weight = {
      w <- rep(1 / length(tickers), length(tickers))
      names(w) <- tickers
    },
    sp500 = {
      spy_idx <- match("SPY", tickers)
      if (!is.na(spy_idx)) {
        w[spy_idx] <- 1
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

  keep <- apply(mat, 2, function(x) !all(is.na(x)))
  mat <- mat[, keep, drop = FALSE]

  complete <- stats::complete.cases(mat)
  mat <- mat[complete, , drop = FALSE]
  dates <- dates[complete]

  xts::xts(mat, order.by = dates)
}

#' Clean near-zero weights and renormalize
#' @param weights Named numeric vector
#' @return Cleaned weights
#' @noRd
pf_clean_weights <- function(weights) {
  weights[weights < 1e-6] <- 0
  if (sum(weights) > 0) weights <- weights / sum(weights)
  weights
}

#' Run the full optimization pipeline
#'
#' Orchestrates constraint building, optimization, backtesting, frontier,
#' and risk contribution. Returns an enriched dm with result tables.
#'
#' @param dm_data dm object with metadata and returns tables
#' @param profile One-row data frame with risk, horizon, strategy, min_positions
#' @param benchmark Character: "60_40", "equal_weight", or "sp500"
#' @param max_weight Numeric: max position weight (0-1)
#' @param compare Character vector of strategy IDs for comparison
#' @return dm object enriched with result tables
#' @noRd
pf_run_optimizer <- function(dm_data, profile, benchmark = "60_40",
                              max_weight = 0.25,
                              compare = character(0)) {
  tbls <- dm::dm_get_tables(dm_data)
  metadata <- as.data.frame(tbls[["metadata"]])
  ret_df <- as.data.frame(tbls[["returns"]])
  returns_xts <- pf_to_xts(ret_df)

  metadata <- metadata[metadata$ticker %in% colnames(returns_xts), ,
    drop = FALSE]

  risk <- profile$risk[1]
  horizon <- profile$horizon[1]
  strategy <- profile$strategy[1]
  min_positions <- as.integer(profile$min_positions[1])

  constraints <- pf_build_constraints(risk, horizon, max_weight,
    min_positions, metadata)
  opt <- pf_optimize(returns_xts, strategy, constraints)
  weights <- pf_clean_weights(opt$weights)

  bt <- pf_backtest(returns_xts, weights)
  bw <- pf_benchmark_weights(benchmark, metadata, returns_xts)
  bench_bt <- pf_backtest(returns_xts, bw)
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
  bench_backtest_df <- data.frame(
    date = zoo::index(bench_bt$returns),
    return = as.numeric(bench_bt$returns),
    cumulative = as.numeric(bench_bt$cumulative),
    drawdown = as.numeric(bench_bt$drawdown),
    stringsAsFactors = FALSE
  )
  metrics_df <- data.frame(
    strategy = strategy,
    ann_return = bt$ann_return, ann_vol = bt$ann_vol,
    sharpe = bt$sharpe, max_dd = bt$max_dd, var_95 = bt$var_95,
    benchmark = benchmark,
    bench_ann_return = bench_bt$ann_return,
    bench_ann_vol = bench_bt$ann_vol,
    bench_sharpe = bench_bt$sharpe,
    bench_max_dd = bench_bt$max_dd,
    stringsAsFactors = FALSE
  )

  comparison_df <- NULL
  if (length(compare) > 0) {
    comp_list <- lapply(compare, function(strat) {
      comp_opt <- pf_optimize(returns_xts, strat, constraints)
      comp_w <- pf_clean_weights(comp_opt$weights)
      comp_bt <- pf_backtest(returns_xts, comp_w)
      data.frame(
        date = zoo::index(comp_bt$returns), strategy = strat,
        return = as.numeric(comp_bt$returns),
        cumulative = as.numeric(comp_bt$cumulative),
        ann_return = comp_bt$ann_return, ann_vol = comp_bt$ann_vol,
        sharpe = comp_bt$sharpe, max_dd = comp_bt$max_dd,
        var_95 = comp_bt$var_95,
        stringsAsFactors = FALSE
      )
    })
    comparison_df <- do.call(rbind, comp_list)
  }

  result_dm <- dm_data |>
    dm::dm(
      weights = weights_df, backtest = backtest_df,
      bench_backtest = bench_backtest_df, metrics = metrics_df,
      frontier = frontier_data$frontier, assets = frontier_data$assets,
      risk_contrib = risk_contrib_data
    ) |>
    dm::dm_add_fk(weights, ticker, metadata) |>
    dm::dm_add_fk(risk_contrib, ticker, metadata) |>
    dm::dm_add_fk(assets, ticker, metadata)

  if (!is.null(comparison_df)) {
    result_dm <- dm::dm(result_dm, comparison_backtest = comparison_df)
  }

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
