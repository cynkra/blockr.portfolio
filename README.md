# blockr.portfolio

<!-- badges: start -->
![Status: experimental](https://img.shields.io/badge/status-experimental-orange)
<!-- badges: end -->

> [!WARNING]
> **Experimental.** This package is an early prototype and will change a
> lot — blocks, APIs, and bundled data may break without notice. Do not
> rely on it for anything.

Portfolio management and share exploration blocks for
[blockr](https://blockr-org.github.io/blockr.site/). Extends
`blockr.core` with an investor-profile block, a portfolio optimizer, a
rich analysis dashboard, and a share explorer with candlestick charts.
All visualizations use [echarts4r](https://echarts4r.john-coene.com/).

## Blocks

| Block | Constructor | What it does |
|---|---|---|
| **Portfolio Data** | `new_portfolio_data_block()` | Bundled universe of ~34 ETFs with 10 years of monthly returns plus FX rates, as a `dm` object |
| **Investor Profile** | `new_investor_profile_block()` | Captures age, family status, amount and base currency; auto-derives risk appetite and horizon |
| **Portfolio Optimizer** | `new_portfolio_optimizer_block()` | Runs mean-variance, min-volatility, risk-parity or equal-weight optimization with optional constraints and currency adjustment |
| **Portfolio Dashboard** | `new_portfolio_dashboard_block()` | Interactive analysis panels: allocation, efficient frontier, equity curve, drawdown, risk contribution, risk metrics |
| **Ticker Data** | `new_ticker_data_block()` | Fetches OHLC price data for stocks and ETFs from Yahoo Finance |
| **Share Explorer** | `new_share_explorer_block()` | Explores individual stocks with candlestick charts and key metrics |

The optimizer pipeline builds on `blockr.dm`, so blocks pass a `dm`
(linked tables) downstream:

```
Portfolio Data ──→ Optimizer ──→ Dashboard
                      ↑
Investor Profile ─────┘
```

See [`dev/architecture.md`](dev/architecture.md) for the full design —
block responsibilities, optimization strategies, currency adjustment,
and the ETF universe.

## Installation

```r
# install.packages("pak")
pak::pak("cynkra/blockr.portfolio")
```

## Usage

Register the blocks, then use them in any blockr board:

```r
library(blockr.core)
library(blockr.portfolio)

register_portfolio_blocks()
```

Two example boards ship with the package as saved JSON in
`inst/extdata/`. To run the portfolio optimizer demo:

```r
library(blockr.core)
library(blockr.dock)
library(blockr.dm)
library(blockr.portfolio)

register_portfolio_blocks()

serve(
  system.file("extdata", "Portfolio_Optimizer.json", package = "blockr.portfolio")
)
```

Swap in `"Share_Explorer.json"` for the share-explorer demo. Ready-to-run
launch scripts are in [`dev/`](dev/) (`app-portfolio-json.R`,
`app-explorer-json.R`).

## Data

The bundled returns are real monthly data from Yahoo Finance (2016–2026),
fetched at package build time; the scripts that build the datasets live in
[`data-raw/`](data-raw/). No API key is required to use the package. The
international ticker catalog build (`data-raw/build-ticker-catalog.R`) can
optionally use an EODHD free-tier key via the `EODHD_API_KEY` environment
variable, but this is only for regenerating bundled data, not for normal
use.

## License

GPL (>= 3). See [LICENSE.md](LICENSE.md).
