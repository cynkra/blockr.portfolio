# Constraint Debug — pure R, no Shiny
#
# Question: Do per-ticker constraints actually work with our optimizer?

library(blockr.portfolio)
library(dm)

dm_obj <- readRDS(system.file("extdata", "portfolio_dm.rds",
  package = "blockr.portfolio"))

# Default profile: age 35, moderate risk
profile <- data.frame(
  currency = "USD",
  risk = 50,       # moderate
  horizon = 50,    # medium
  amount = 250000,
  stringsAsFactors = FALSE
)

# --- Test 1: No constraints ---
cat("=== Test 1: No constraints ===\n")
res1 <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
  strategy = "mean_variance")
w1 <- as.data.frame(dm::dm_get_tables(res1)[["weights"]])
w1 <- w1[w1$weight > 0.01, ]
cat("Top positions:\n")
print(w1[order(-w1$weight), c("ticker", "weight")], row.names = FALSE)
cat("GLD:", round(w1$weight[w1$ticker == "GLD"] * 100, 1), "%\n")
cat("EWT:", round(w1$weight[w1$ticker == "EWT"] * 100, 1), "%\n\n")

# --- Test 2: Cap GLD at 10% ---
cat("=== Test 2: GLD capped at 10% ===\n")
res2 <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
  strategy = "mean_variance",
  ticker_limits = list(GLD = 0.10))
w2 <- as.data.frame(dm::dm_get_tables(res2)[["weights"]])
w2 <- w2[w2$weight > 0.01, ]
cat("Top positions:\n")
print(w2[order(-w2$weight), c("ticker", "weight")], row.names = FALSE)
gld2 <- w2$weight[w2$ticker == "GLD"]
cat("GLD:", round(gld2 * 100, 1), "% — constraint",
  if (gld2 <= 0.101) "BINDING" else "NOT BINDING", "\n\n")

# --- Test 3: Cap EWT at 5% ---
cat("=== Test 3: EWT capped at 5% ===\n")
res3 <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
  strategy = "mean_variance",
  ticker_limits = list(EWT = 0.05))
w3 <- as.data.frame(dm::dm_get_tables(res3)[["weights"]])
w3 <- w3[w3$weight > 0.01, ]
cat("Top positions:\n")
print(w3[order(-w3$weight), c("ticker", "weight")], row.names = FALSE)
ewt3 <- w3$weight[w3$ticker == "EWT"]
cat("EWT:", round(ewt3 * 100, 1), "% — constraint",
  if (ewt3 <= 0.051) "BINDING" else "NOT BINDING", "\n\n")

# --- Test 4: Both GLD 10% + EWT 5% ---
cat("=== Test 4: GLD 10% + EWT 5% ===\n")
res4 <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
  strategy = "mean_variance",
  ticker_limits = list(GLD = 0.10, EWT = 0.05))
w4 <- as.data.frame(dm::dm_get_tables(res4)[["weights"]])
w4 <- w4[w4$weight > 0.01, ]
cat("Top positions:\n")
print(w4[order(-w4$weight), c("ticker", "weight")], row.names = FALSE)
cat("GLD:", round(w4$weight[w4$ticker == "GLD"] * 100, 1), "%\n")
cat("EWT:", round(w4$weight[w4$ticker == "EWT"] * 100, 1), "%\n\n")

# --- Test 5: Global max 25% + GLD 10% ---
cat("=== Test 5: Global max 25% + GLD 10% ===\n")
res5 <- blockr.portfolio:::pf_run_optimizer(dm_obj, profile,
  strategy = "mean_variance",
  max_weight = 0.25,
  ticker_limits = list(GLD = 0.10))
w5 <- as.data.frame(dm::dm_get_tables(res5)[["weights"]])
w5 <- w5[w5$weight > 0.01, ]
cat("Top positions:\n")
print(w5[order(-w5$weight), c("ticker", "weight")], row.names = FALSE)
cat("GLD:", round(w5$weight[w5$ticker == "GLD"] * 100, 1), "%\n")
cat("Max position:", round(max(w5$weight) * 100, 1), "%\n\n")

# --- Test 6: Sweep risk levels with GLD 10% ---
cat("=== Test 6: GLD 10% across risk levels ===\n")
for (risk_val in c(20, 40, 60, 80, 95)) {
  p <- data.frame(currency = "USD", risk = risk_val, horizon = 50,
    amount = 250000, stringsAsFactors = FALSE)
  res <- tryCatch({
    blockr.portfolio:::pf_run_optimizer(dm_obj, p,
      strategy = "mean_variance",
      ticker_limits = list(GLD = 0.10))
  }, error = function(e) { cat("  risk=", risk_val, "ERROR:",
    e$message, "\n"); NULL })
  if (!is.null(res)) {
    w <- as.data.frame(dm::dm_get_tables(res)[["weights"]])
    gld <- w$weight[w$ticker == "GLD"]
    n <- sum(w$weight > 0.01)
    cat(sprintf("  risk=%2d: GLD=%4.1f%%, positions=%d\n",
      risk_val, gld * 100, n))
  }
}
