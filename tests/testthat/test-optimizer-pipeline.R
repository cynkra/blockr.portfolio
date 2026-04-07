test_that("pf_build_constraints maps risk and horizon correctly", {
  meta <- data.frame(
    ticker = c("VTI", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )

  # Conservative (risk=20) + short horizon (horizon=10)
  cons <- blockr.portfolio:::pf_build_constraints(
    20, 10, 0.25, meta)
  expect_true(cons$risk_aversion > 5) # high aversion
  expect_true(cons$group_max[1] < 0.50) # low equity cap

  # Aggressive (risk=90) + long horizon (horizon=90)
  cons2 <- blockr.portfolio:::pf_build_constraints(
    90, 90, 0.5, meta)
  expect_true(cons2$risk_aversion < 1) # low aversion
  expect_equal(cons2$max_weight, 0.5)
})

test_that("pf_optimize returns valid weights", {
  skip_if_not_installed("PortfolioAnalytics")

  set.seed(42)
  n <- 60
  mat <- matrix(rnorm(n * 3, mean = 0.005, sd = 0.04), nrow = n)
  colnames(mat) <- c("VTI", "AGG", "GLD")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2020-01-01"), by = "month", length.out = n))

  meta <- data.frame(
    ticker = c("VTI", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )

  constraints <- blockr.portfolio:::pf_build_constraints(
    50, 50, 0.5, meta)

  # Equal weight
  opt_ew <- blockr.portfolio:::pf_optimize(returns_xts, "equal_weight",
    constraints)
  expect_true(all(abs(opt_ew$weights - 1/3) < 1e-6))

  # Mean-variance
  opt_mv <- blockr.portfolio:::pf_optimize(returns_xts, "mean_variance",
    constraints)
  w <- opt_mv$weights
  expect_true(abs(sum(w) - 1) < 1e-4)
  expect_true(all(w >= -1e-6))
})

test_that("pf_backtest computes expected metrics", {
  set.seed(42)
  n <- 60
  mat <- matrix(rnorm(n * 2, mean = 0.005, sd = 0.04), nrow = n)
  colnames(mat) <- c("A", "B")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2020-01-01"), by = "month", length.out = n))

  w <- c(A = 0.6, B = 0.4)
  bt <- blockr.portfolio:::pf_backtest(returns_xts, w)

  expect_true(is.numeric(bt$ann_return))
  expect_true(is.numeric(bt$ann_vol))
  expect_true(is.numeric(bt$sharpe))
  expect_true(is.numeric(bt$max_dd))
  expect_true(bt$max_dd >= 0)
  expect_true(bt$ann_vol > 0)
  expect_s3_class(bt$returns, "xts")
  expect_s3_class(bt$drawdown, "xts")
})

test_that("pf_benchmark_weights sums to 1", {
  meta <- data.frame(
    ticker = c("VTI", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(0, nrow = 10, ncol = 3)
  colnames(mat) <- c("VTI", "AGG", "GLD")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2024-01-01"), by = "month", length.out = 10))

  bw_6040 <- blockr.portfolio:::pf_benchmark_weights("60_40", meta,
    returns_xts)
  expect_equal(sum(bw_6040), 1)
  expect_equal(bw_6040["VTI"], c(VTI = 0.6))

  bw_ew <- blockr.portfolio:::pf_benchmark_weights("equal_weight", meta,
    returns_xts)
  expect_equal(sum(bw_ew), 1)
  expect_true(all(abs(bw_ew - 1/3) < 1e-6))

  bw_us <- blockr.portfolio:::pf_benchmark_weights("us_market", meta,
    returns_xts)
  expect_equal(bw_us["VTI"], c(VTI = 1))
})

test_that("pf_risk_contribution sums approximately to 100", {
  set.seed(42)
  n <- 60
  mat <- matrix(rnorm(n * 3, mean = 0.005, sd = 0.04), nrow = n)
  colnames(mat) <- c("A", "B", "C")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2020-01-01"), by = "month", length.out = n))

  w <- c(A = 0.5, B = 0.3, C = 0.2)
  rc <- blockr.portfolio:::pf_risk_contribution(returns_xts, w)

  expect_equal(nrow(rc), 3)
  expect_true(abs(sum(rc$contribution) - 100) < 5) # ~100%
})

test_that("per-ticker limits are enforced after max_positions", {
  skip_if_not_installed("PortfolioAnalytics")

  dm_obj <- readRDS(system.file("extdata", "portfolio_dm.rds",
    package = "blockr.portfolio"))
  profile <- data.frame(currency = "USD", risk = 81, horizon = 81,
    amount = 50000, stringsAsFactors = FALSE)

  w <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
    strategy = "mean_variance",
    ticker_limits = list(VUG = 0.10))

  # Result should be a dm
  expect_s3_class(w, "dm")
  wdf <- as.data.frame(dm::dm_get_tables(w)[["weights"]])
  vug <- wdf$weight[wdf$ticker == "VUG"]
  expect_true(length(vug) > 0)
  expect_true(vug <= 0.101) # respects 10% limit
})
