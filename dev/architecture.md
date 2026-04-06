# Portfolio Optimizer — Architecture

## Block responsibilities

The portfolio optimizer is built from four blocks, each with a clear responsibility.

### 1. Portfolio Data

**What it does:** Provides the investment universe — a bundled dataset of ~34 ETFs with 10 years of real monthly returns from Yahoo Finance, plus FX rates for currency adjustment.

**Output:** A dm object with two tables:
- `metadata` — ticker, name, asset_class, region, sub_class, type
- `returns` — date, ticker, return (monthly log returns)

FX rates (CHF/USD, EUR/USD) are stored as pseudo-tickers in the returns table with type "FX".

**No user input needed.** This block is pure data. In principle, a user could swap in a different dataset (e.g., a custom ETF universe) by replacing this block.

### 2. Investor Profile

**What it does:** Captures *who the investor is*. These are personal facts that determine how the portfolio should be constructed.

**Parameters:**
- **Age** — drives risk tolerance and time horizon
- **Family status** — single, married, married with kids, retired
- **Investment amount** — how much to invest
- **Base currency** — USD, CHF, or EUR (determines how returns are measured)

**Derived parameters:**
- **Risk appetite** (conservative / moderate / aggressive) — derived from age + family. A 25-year-old single person can take more risk than a 60-year-old with dependents.
- **Investment horizon** (short / medium / long) — derived from age. Younger investors have longer horizons.

**What does NOT belong here:** Strategy choice (mean-variance vs risk parity), position constraints (max weight, max positions), benchmark selection. These are algorithm/display concerns, not personal attributes.

**Output:** A one-row data frame with: age, family, amount, currency, risk, horizon.

Note: earlier versions derived `strategy` from `amount` in the profile. This was removed — strategy is an algorithm choice that belongs in the optimizer.

### 3. Portfolio Optimizer

**What it does:** Takes the data universe + investor profile and runs the portfolio optimization math. This is where algorithmic choices live.

**Inputs:**
- Data (dm from the data block)
- Profile (data frame from the profile block)

**Parameters (algorithm tuning):**
- **Strategy** — which optimization method to use (see below). This is an algorithm choice, not a personal preference. Changing it re-triggers optimization.
- **Constraints** — optional toggles:
  - Max position weight (e.g., no single ETF > 25%)
  - Max number of ETFs in portfolio (e.g., limit to 7 for simplicity)

**Available strategies:**

1. **Mean-Variance** — classic Markowitz optimization. Maximizes expected return for a given level of risk. Uses a risk aversion parameter derived from the investor's risk appetite. Best for investors who believe historical returns predict future returns.

2. **Minimum Volatility** — finds the portfolio with the lowest possible risk regardless of return. Good for conservative investors who prioritize stability above all.

3. **Risk Parity** — allocates so that each asset contributes equally to total portfolio risk. Tends to overweight bonds relative to mean-variance. Popular in institutional investing.

4. **Equal Weight** — simple 1/N allocation across all assets. No optimization. Surprisingly hard to beat in practice. Serves as a baseline.

The optimizer runs the selected strategy and computes:
- Optimal weights per ETF
- Backtest (cumulative returns, drawdown over the historical period)
- Performance metrics (annualized return, volatility, Sharpe ratio, max drawdown, VaR)
- Efficient frontier (risk-return tradeoff curve)
- Risk contribution per asset

**Currency adjustment:** Before optimization, all returns are adjusted to the investor's base currency. A CHF investor sees USD-denominated assets as more volatile (because of exchange rate risk), which naturally tilts the optimizer toward Swiss and European assets.

**Output:** An enriched dm with result tables (weights, backtest, metrics, frontier, risk contribution). The dashboard reads whatever it needs from this dm.

Strategy comparison (running a second strategy side-by-side) could be added as an optional toggle in the optimizer — but it's not enabled by default since it doubles computation time.

### 4. Portfolio Dashboard

**What it does:** Pure visualization. Takes the optimizer's output dm and renders interactive charts. No computation — just display.

**Parameters (display choices):**
- **Benchmark** — what to compare against (US 60/40, Global 60/40, Equal Weight, US Market). This doesn't affect the optimization, only the comparison lines on charts.
- **Panel selection** — which charts to show (allocation treemap, efficient frontier, equity curve, drawdown, risk contribution, risk metrics, strategy comparison)
- **Per-panel settings** — treemap vs doughnut view, benchmark overlay toggle, etc.

**Panels available:**
- **Allocation** — treemap or doughnut showing portfolio weights grouped by asset class
- **Efficient Frontier** — scatter plot of risk vs return with the frontier curve and "you are here" marker
- **Equity Curve** — cumulative backtest returns vs benchmark over time
- **Drawdown** — underwater chart showing peak-to-trough losses
- **Risk Contribution** — horizontal bar chart showing each ETF's contribution to total portfolio risk
- **Risk Metrics** — KPI cards with annualized return, volatility, Sharpe ratio, max drawdown, VaR
- **Strategy Comparison** — side-by-side metrics table for all four strategies

## Data flow

```
Portfolio Data ──→ Optimizer ──→ Dashboard
                      ↑
Investor Profile ─────┘
```

- Profile changes (age, risk, currency) → optimizer re-runs all strategies → dashboard updates
- Constraint changes (max weight, max positions) → optimizer re-runs → dashboard updates
- Benchmark/panel changes → only dashboard updates (no re-optimization)

## Currency adjustment

Returns from Yahoo Finance are in USD. For a non-USD investor, holding USD assets carries exchange rate risk. The optimizer adjusts returns before optimization:

```
r_local = (1 + r_usd) × (1 + r_fx) - 1
```

Where `r_fx` is the monthly change in the investor's currency per USD. This changes the covariance matrix the optimizer sees:

- **Swiss stocks (EWL)** appear less volatile for a CHF investor (no FX risk)
- **US stocks (VTI)** appear more volatile for a CHF investor (USD/CHF adds noise)

This naturally tilts the optimal portfolio toward the investor's home market — the well-known "home bias" effect, but here it's mathematically justified rather than behavioral.

## ETF universe

34 ETFs covering:

| Category | ETFs | Purpose |
|---|---|---|
| Global equity | VT, VTI, VEA, VWO | Broad market exposure |
| Regional developed | EWL, EWG, EWO, VGK, EWU, EWJ, EWA, EWC | Geographic tilts (CH, DE, AT, Europe, UK, JP, AU, CA) |
| Regional emerging | EEM, MCHI, EWZ, INDA, EWT | EM exposure (broad, China, Brazil, India, Taiwan) |
| US style/size | VUG, VTV, VO, VB | Growth/Value/Mid/Small cap tilts |
| Fixed income | AGG, SHY, IEF, TLT, TIP, LQD, BNDX, EMB | Full bond spectrum |
| Alternatives | VNQ, VNQI, GLD, DBC, BITO | Real estate, gold, commodities, crypto |

Monthly returns, 10 years (2016–2026). Real data from Yahoo Finance, fetched at package build time.
