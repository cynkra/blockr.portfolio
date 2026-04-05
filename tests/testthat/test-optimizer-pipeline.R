test_that("pf_build_constraints maps risk and horizon correctly", {
  meta <- data.frame(
    ticker = c("SPY", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )

  cons <- blockr.portfolio:::pf_build_constraints(
    "conservative", "short", 0.25, 5L, meta)
  expect_equal(cons$risk_aversion, 10)
  expect_true(cons$group_max[1] <= 0.40) # equity max for conservative+short

  cons2 <- blockr.portfolio:::pf_build_constraints(
    "aggressive", "long", 0.5, 3L, meta)
  expect_equal(cons2$risk_aversion, 0.5)
  expect_equal(cons2$max_weight, 0.5)
})

test_that("pf_optimize returns valid weights", {
  skip_if_not_installed("PortfolioAnalytics")

  set.seed(42)
  n <- 60
  mat <- matrix(rnorm(n * 3, mean = 0.005, sd = 0.04), nrow = n)
  colnames(mat) <- c("SPY", "AGG", "GLD")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2020-01-01"), by = "month", length.out = n))

  meta <- data.frame(
    ticker = c("SPY", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )

  constraints <- blockr.portfolio:::pf_build_constraints(
    "moderate", "medium", 0.5, 1L, meta)

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
    ticker = c("SPY", "AGG", "GLD"),
    asset_class = c("Equity", "Bond", "Commodity"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(0, nrow = 10, ncol = 3)
  colnames(mat) <- c("SPY", "AGG", "GLD")
  returns_xts <- xts::xts(mat, order.by = seq.Date(
    as.Date("2024-01-01"), by = "month", length.out = 10))

  bw_6040 <- blockr.portfolio:::pf_benchmark_weights("60_40", meta,
    returns_xts)
  expect_equal(sum(bw_6040), 1)
  expect_equal(bw_6040["SPY"], c(SPY = 0.6))

  bw_ew <- blockr.portfolio:::pf_benchmark_weights("equal_weight", meta,
    returns_xts)
  expect_equal(sum(bw_ew), 1)
  expect_true(all(abs(bw_ew - 1/3) < 1e-6))

  bw_sp <- blockr.portfolio:::pf_benchmark_weights("sp500", meta,
    returns_xts)
  expect_equal(bw_sp["SPY"], c(SPY = 1))
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
