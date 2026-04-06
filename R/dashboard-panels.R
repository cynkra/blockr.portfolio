#' Portfolio panel registry
#'
#' Returns all available panel definitions for the portfolio advisor block.
#'
#' @return Named list of panel definitions
#' @noRd
portfolio_panels <- function() {
  list(
    allocation = allocation_panel(),
    frontier = frontier_panel(),
    equity_curve = equity_curve_panel(),
    drawdown = drawdown_panel(),
    risk_contribution = risk_contribution_panel(),
    risk_metrics = risk_metrics_panel(),
    strategy_comparison = strategy_comparison_panel()
  )
}

#' Default panel selection
#' @noRd
pf_default_panels <- function() {
  c("allocation", "frontier", "equity_curve", "risk_metrics")
}

# -- Allocation panel ----------------------------------------------------------

allocation_panel <- function() {
  list(
    id = "allocation",
    label = "Portfolio Allocation",
    category = "Composition",
    icon = "pie-chart",
    color = "#3b82f6",
    description = "Asset allocation grouped by class",
    controls = list(
      view = list(
        type = "radio", label = "View",
        choices = c("Treemap" = "treemap", "Doughnut" = "doughnut"),
        default = "treemap"
      )
    ),
    render = function(results, settings) {
      w <- results$weights
      meta <- results$metadata
      if (is.null(w) || all(w == 0)) {
        return(pf_empty_chart("No allocation data"))
      }

      # Filter to non-zero weights
      active <- w[w > 1e-6]
      if (length(active) == 0) return(pf_empty_chart("No positions"))

      df <- data.frame(
        ticker = names(active),
        weight = as.numeric(active),
        stringsAsFactors = FALSE
      )
      df <- merge(df, meta[, c("ticker", "name", "asset_class")],
        by = "ticker", all.x = TRUE)

      view <- settings$view %||% "treemap"

      if (view == "doughnut") {
        # Doughnut chart
        series_data <- lapply(seq_len(nrow(df)), function(i) {
          list(
            name = paste0(df$ticker[i], " (", df$name[i], ")"),
            value = round(df$weight[i] * 100, 1)
          )
        })
        echarts4r::e_charts(height = 400) |>
          echarts4r::e_list(list(
            backgroundColor = "transparent",
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(p){return p.name+': '+p.value+'%';}"
              )
            ),
            legend = list(
              orient = "vertical", right = 10, top = 20,
              textStyle = list(fontSize = 11)
            ),
            series = list(list(
              type = "pie",
              radius = c("40%", "70%"),
              center = c("40%", "50%"),
              data = series_data,
              label = list(show = FALSE),
              emphasis = list(
                label = list(show = TRUE, fontSize = 14,
                  fontWeight = "bold")
              )
            ))
          ))
      } else {
        # Treemap
        classes <- unique(df$asset_class)
        class_colors <- pf_class_colors()
        children <- lapply(classes, function(cls) {
          sub <- df[df$asset_class == cls, ]
          list(
            name = cls,
            value = round(sum(sub$weight) * 100, 1),
            itemStyle = list(
              borderColor = "#fff", borderWidth = 2,
              color = unname(class_colors[cls] %||% "#6b7280")
            ),
            children = lapply(seq_len(nrow(sub)), function(i) {
              etf_name <- sub$name[i]
              if (is.na(etf_name)) etf_name <- sub$ticker[i]
              list(
                name = paste0(sub$ticker[i], "\n", etf_name,
                  "\n", round(sub$weight[i] * 100, 1), "%"),
                value = round(sub$weight[i] * 100, 1)
              )
            })
          )
        })

        echarts4r::e_charts(height = 400) |>
          echarts4r::e_list(list(
            backgroundColor = "transparent",
            tooltip = list(
              formatter = htmlwidgets::JS(
                "function(p){var parts=p.name.split('\\n'); var ticker=parts[0]||''; var name=parts[1]||''; var weight=p.value; return '<b>'+ticker+'</b><br>'+name+'<br>'+weight+'%';}"
              )
            ),
            series = list(list(
              type = "treemap",
              data = children,
              roam = FALSE,
              nodeClick = FALSE,
              breadcrumb = list(show = FALSE),
              label = list(
                show = TRUE, fontSize = 11,
                formatter = "{b}"
              ),
              levels = list(
                list(
                  itemStyle = list(
                    borderColor = "#fff", borderWidth = 3,
                    gapWidth = 3
                  )
                ),
                list(
                  itemStyle = list(
                    borderColor = "#fff", borderWidth = 1,
                    gapWidth = 1
                  ),
                  colorSaturation = c(0.3, 0.6)
                )
              )
            ))
          ))
      }
    }
  )
}

# -- Efficient Frontier panel --------------------------------------------------

frontier_panel <- function() {
  list(
    id = "frontier",
    label = "Efficient Frontier",
    category = "Analysis",
    icon = "graph-up",
    color = "#8b5cf6",
    description = "Risk vs. return with optimal portfolio position",
    controls = list(
      show_assets = list(
        type = "toggle", label = "Show assets", default = TRUE
      )
    ),
    render = function(results, settings) {
      frontier <- results$frontier
      if (is.null(frontier)) return(pf_empty_chart("No frontier data"))

      show_assets <- settings$show_assets %||% TRUE
      series <- list()

      # Frontier curve
      frontier_data <- lapply(seq_len(nrow(frontier$frontier)), function(i) {
        list(round(frontier$frontier$risk[i] * 100, 2),
          round(frontier$frontier$return[i] * 100, 2))
      })
      series <- c(series, list(list(
        type = "line",
        name = "Efficient Frontier",
        data = frontier_data,
        smooth = TRUE,
        symbol = "none",
        lineStyle = list(color = "#8b5cf6", width = 2.5),
        itemStyle = list(color = "#8b5cf6"),
        z = 2
      )))

      # Individual assets
      if (isTRUE(show_assets) && !is.null(frontier$assets)) {
        asset_data <- lapply(seq_len(nrow(frontier$assets)), function(i) {
          list(
            value = list(
              round(frontier$assets$risk[i] * 100, 2),
              round(frontier$assets$return[i] * 100, 2)
            ),
            name = frontier$assets$ticker[i]
          )
        })
        series <- c(series, list(list(
          type = "scatter",
          name = "Assets",
          data = asset_data,
          symbolSize = 10,
          itemStyle = list(color = "#9ca3af", borderColor = "#fff",
            borderWidth = 2),
          label = list(
            show = TRUE, position = "right",
            formatter = htmlwidgets::JS(
              "function(p){return p.data.name||'';}"
            ),
            fontSize = 10, color = "#6b7280"
          ),
          z = 1
        )))
      }

      # Portfolio position
      if (!is.null(results$backtest)) {
        port_risk <- results$backtest$ann_vol * 100
        port_ret <- results$backtest$ann_return * 100
        series <- c(series, list(list(
          type = "effectScatter",
          name = "Portfolio",
          data = list(list(
            value = list(round(port_risk, 2), round(port_ret, 2)),
            name = "Your Portfolio"
          )),
          symbolSize = 16,
          itemStyle = list(color = "#059669"),
          rippleEffect = list(brushType = "stroke", scale = 3),
          z = 3
        )))
      }

      echarts4r::e_charts(height = 400) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pf_tooltip(),
          legend = list(
            show = TRUE, bottom = 0,
            textStyle = list(fontSize = 11)
          ),
          grid = list(left = 60, right = 30, top = 20, bottom = 40),
          xAxis = list(
            type = "value",
            name = "Annualized Risk (%)",
            nameLocation = "center", nameGap = 25,
            nameTextStyle = list(fontSize = 11, color = "#6b7280"),
            axisLabel = list(fontSize = 11, color = "#9ca3af"),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          yAxis = list(
            type = "value",
            name = "Annualized Return (%)",
            nameLocation = "center", nameGap = 40,
            nameTextStyle = list(fontSize = 11, color = "#6b7280"),
            axisLabel = list(fontSize = 11, color = "#9ca3af"),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          series = series
        ))
    }
  )
}

# -- Equity Curve panel --------------------------------------------------------

equity_curve_panel <- function() {
  list(
    id = "equity_curve",
    label = "Equity Curve",
    category = "Performance",
    icon = "graph-up-arrow",
    color = "#059669",
    description = "Cumulative backtest returns vs. benchmark",
    controls = list(
      show_benchmark = list(
        type = "toggle", label = "Benchmark", default = TRUE
      )
    ),
    render = function(results, settings) {
      bt <- results$backtest
      if (is.null(bt)) return(pf_empty_chart("No backtest data"))

      show_bm <- settings$show_benchmark %||% TRUE
      series <- list()

      # Portfolio equity curve
      cum <- bt$cumulative
      port_data <- lapply(seq_len(nrow(cum)), function(i) {
        list(
          as.numeric(zoo::index(cum)[i]) * 86400000,
          round(as.numeric(cum[i, 1]) * 100, 2)
        )
      })
      series <- c(series, list(list(
        type = "line",
        name = "Portfolio",
        data = port_data,
        smooth = TRUE,
        symbol = "none",
        lineStyle = list(color = "#059669", width = 2.5),
        areaStyle = list(color = list(
          type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
          colorStops = list(
            list(offset = 0, color = "rgba(5,150,105,0.15)"),
            list(offset = 1, color = "rgba(5,150,105,0)")
          )
        )),
        itemStyle = list(color = "#059669")
      )))

      # Benchmark
      if (isTRUE(show_bm) && !is.null(results$benchmark)) {
        bm_cum <- results$benchmark$cumulative
        bm_data <- lapply(seq_len(nrow(bm_cum)), function(i) {
          list(
            as.numeric(zoo::index(bm_cum)[i]) * 86400000,
            round(as.numeric(bm_cum[i, 1]) * 100, 2)
          )
        })
        series <- c(series, list(list(
          type = "line",
          name = "Benchmark",
          data = bm_data,
          smooth = TRUE,
          symbol = "none",
          lineStyle = list(
            color = "#9ca3af", width = 1.5, type = "dashed"),
          itemStyle = list(color = "#9ca3af")
        )))
      }

      # Comparison strategies
      if (!is.null(results$comparison)) {
        comp_colors <- c("#f59e0b", "#dc2626", "#0891b2", "#8b5cf6")
        strategy_labels <- c(
          mean_variance = "Mean-Variance",
          risk_parity = "Risk Parity",
          equal_weight = "Equal Weight",
          min_vol = "Min Volatility"
        )
        ci <- 1
        for (sname in names(results$comparison)) {
          comp <- results$comparison[[sname]]
          if (is.null(comp$backtest$cumulative)) next
          c_cum <- comp$backtest$cumulative
          c_data <- lapply(seq_len(nrow(c_cum)), function(i) {
            list(
              as.numeric(zoo::index(c_cum)[i]) * 86400000,
              round(as.numeric(c_cum[i, 1]) * 100, 2)
            )
          })
          series <- c(series, list(list(
            type = "line",
            name = strategy_labels[sname] %||% sname,
            data = c_data,
            smooth = TRUE,
            symbol = "none",
            lineStyle = list(
              color = comp_colors[ci],
              width = 1.5, type = "dotted"
            ),
            itemStyle = list(color = comp_colors[ci])
          )))
          ci <- ci + 1
        }
      }

      echarts4r::e_charts(height = 400) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = list(
            trigger = "axis",
            confine = TRUE,
            formatter = htmlwidgets::JS(
              "function(params){
                var html = params[0].axisValueLabel + '<br/>';
                params.forEach(function(p){
                  html += p.marker + ' ' + p.seriesName + ': ' +
                    p.value[1].toFixed(1) + '%<br/>';
                });
                return html;
              }"
            )
          ),
          legend = list(
            show = TRUE, bottom = 0,
            textStyle = list(fontSize = 11)
          ),
          grid = list(left = 60, right = 20, top = 10, bottom = 40),
          xAxis = list(
            type = "time",
            axisLabel = list(fontSize = 11, color = "#9ca3af"),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          yAxis = list(
            type = "value",
            name = "Cumulative Return (%)",
            nameTextStyle = list(fontSize = 11, color = "#6b7280"),
            axisLabel = list(
              fontSize = 11, color = "#9ca3af",
              formatter = htmlwidgets::JS(
                "function(v){return v.toFixed(0)+'%';}"
              )
            ),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          series = series
        ))
    }
  )
}

# -- Drawdown panel ------------------------------------------------------------

drawdown_panel <- function() {
  list(
    id = "drawdown",
    label = "Drawdown",
    category = "Risk",
    icon = "arrow-down-circle",
    color = "#dc2626",
    description = "Underwater chart showing drawdown depth over time",
    controls = list(
      show_benchmark = list(
        type = "toggle", label = "Benchmark", default = FALSE
      )
    ),
    render = function(results, settings) {
      bt <- results$backtest
      if (is.null(bt) || is.null(bt$drawdown)) {
        return(pf_empty_chart("No drawdown data"))
      }

      show_bm <- settings$show_benchmark %||% FALSE
      series <- list()

      dd <- bt$drawdown
      dd_data <- lapply(seq_len(nrow(dd)), function(i) {
        list(
          as.numeric(zoo::index(dd)[i]) * 86400000,
          round(as.numeric(dd[i, 1]) * 100, 2)
        )
      })
      series <- c(series, list(list(
        type = "line",
        name = "Portfolio",
        data = dd_data,
        symbol = "none",
        lineStyle = list(color = "#dc2626", width = 1.5),
        areaStyle = list(color = "rgba(220,38,38,0.12)"),
        itemStyle = list(color = "#dc2626")
      )))

      if (isTRUE(show_bm) && !is.null(results$benchmark$drawdown)) {
        bm_dd <- results$benchmark$drawdown
        bm_dd_data <- lapply(seq_len(nrow(bm_dd)), function(i) {
          list(
            as.numeric(zoo::index(bm_dd)[i]) * 86400000,
            round(as.numeric(bm_dd[i, 1]) * 100, 2)
          )
        })
        series <- c(series, list(list(
          type = "line",
          name = "Benchmark",
          data = bm_dd_data,
          symbol = "none",
          lineStyle = list(
            color = "#9ca3af", width = 1, type = "dashed"),
          itemStyle = list(color = "#9ca3af")
        )))
      }

      echarts4r::e_charts(height = 300) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = list(
            trigger = "axis",
            formatter = htmlwidgets::JS(
              "function(params){
                var html = params[0].axisValueLabel + '<br/>';
                params.forEach(function(p){
                  html += p.marker + ' ' + p.seriesName + ': ' +
                    p.value[1].toFixed(1) + '%<br/>';
                });
                return html;
              }"
            )
          ),
          legend = list(
            show = TRUE, bottom = 0,
            textStyle = list(fontSize = 11)
          ),
          grid = list(left = 60, right = 20, top = 10, bottom = 40),
          xAxis = list(
            type = "time",
            axisLabel = list(fontSize = 11, color = "#9ca3af"),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          yAxis = list(
            type = "value",
            name = "Drawdown (%)",
            nameTextStyle = list(fontSize = 11, color = "#6b7280"),
            max = 0,
            axisLabel = list(
              fontSize = 11, color = "#9ca3af",
              formatter = htmlwidgets::JS(
                "function(v){return v.toFixed(0)+'%';}"
              )
            ),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          series = series
        ))
    }
  )
}

# -- Risk Contribution panel ---------------------------------------------------

risk_contribution_panel <- function() {
  list(
    id = "risk_contribution",
    label = "Risk Contribution",
    category = "Risk",
    icon = "bar-chart",
    color = "#f59e0b",
    description = "Each asset's contribution to portfolio risk",
    controls = NULL,
    render = function(results, settings) {
      rc <- results$risk_contrib
      if (is.null(rc) || nrow(rc) == 0) {
        return(pf_empty_chart("No risk contribution data"))
      }

      # Filter to non-zero and sort
      rc <- rc[abs(rc$contribution) > 0.1, , drop = FALSE]
      if (nrow(rc) == 0) return(pf_empty_chart("No risk contributors"))
      rc <- rc[order(rc$contribution, decreasing = TRUE), ]

      # Merge with metadata for colors
      meta <- results$metadata
      rc <- merge(rc, meta[, c("ticker", "asset_class")],
        by = "ticker", all.x = TRUE)
      rc <- rc[order(rc$contribution, decreasing = TRUE), ]
      class_colors <- pf_class_colors()

      bar_data <- lapply(seq_len(nrow(rc)), function(i) {
        list(
          value = round(rc$contribution[i], 1),
          itemStyle = list(
            color = unname(
              class_colors[rc$asset_class[i]] %||% "#6b7280"
            )
          )
        )
      })

      chart_height <- max(200, nrow(rc) * 30 + 60)

      echarts4r::e_charts(height = chart_height) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = list(
            trigger = "axis",
            axisPointer = list(type = "shadow"),
            formatter = htmlwidgets::JS(
              "function(p){return p[0].name+': '+p[0].value.toFixed(1)+'%';}"
            )
          ),
          grid = list(left = 80, right = 30, top = 10, bottom = 20),
          xAxis = list(
            type = "value",
            name = "Risk Contribution (%)",
            nameTextStyle = list(fontSize = 11, color = "#6b7280"),
            axisLabel = list(fontSize = 11, color = "#9ca3af"),
            splitLine = list(lineStyle = list(
              color = "#e5e7eb", type = "dashed"))
          ),
          yAxis = list(
            type = "category",
            data = rc$ticker,
            inverse = TRUE,
            axisLabel = list(fontSize = 11, color = "#374151"),
            axisLine = list(show = FALSE),
            axisTick = list(show = FALSE)
          ),
          series = list(list(
            type = "bar",
            data = bar_data,
            barWidth = 18,
            itemStyle = list(borderRadius = c(0, 3, 3, 0))
          ))
        ))
    }
  )
}

# -- Risk Metrics panel --------------------------------------------------------

risk_metrics_panel <- function() {
  list(
    id = "risk_metrics",
    label = "Risk Metrics",
    category = "Summary",
    icon = "speedometer2",
    color = "#6366f1",
    description = "Key portfolio performance indicators",
    controls = NULL,
    render = function(results, settings) {
      bt <- results$backtest
      bm <- results$benchmark
      if (is.null(bt)) return(pf_empty_chart("No metrics available"))

      metrics <- list(
        list(
          label = "Ann. Return",
          value = bt$ann_return,
          benchmark = if (!is.null(bm)) bm$ann_return,
          higher_is_better = TRUE
        ),
        list(
          label = "Ann. Volatility",
          value = bt$ann_vol,
          benchmark = if (!is.null(bm)) bm$ann_vol,
          higher_is_better = FALSE
        ),
        list(
          label = "Sharpe Ratio",
          value = bt$sharpe,
          benchmark = if (!is.null(bm)) bm$sharpe,
          higher_is_better = TRUE,
          is_ratio = TRUE
        ),
        list(
          label = "Max Drawdown",
          value = bt$max_dd,
          benchmark = if (!is.null(bm)) bm$max_dd,
          higher_is_better = FALSE
        ),
        list(
          label = "VaR (95%)",
          value = abs(bt$var_95),
          benchmark = if (!is.null(bm)) abs(bm$var_95),
          higher_is_better = FALSE
        )
      )

      cards <- lapply(metrics, function(m) {
        val <- m$value
        is_ratio <- isTRUE(m$is_ratio)
        val_str <- if (is_ratio) pf_fmt_num(val) else pf_fmt_pct(val)

        # Color: compare to benchmark
        val_class <- "pd-kpi-neutral"
        bm_str <- NULL
        if (!is.null(m$benchmark)) {
          bm_val <- m$benchmark
          bm_str <- if (is_ratio) pf_fmt_num(bm_val) else pf_fmt_pct(bm_val)
          is_better <- if (m$higher_is_better) val > bm_val else val < bm_val
          val_class <- if (is_better) "pd-kpi-positive" else "pd-kpi-negative"
        }

        bm_html <- ""
        if (!is.null(bm_str)) {
          bm_html <- sprintf(
            '<div class="pd-kpi-benchmark">vs. %s benchmark</div>',
            bm_str
          )
        }

        sprintf(
          paste0(
            '<div class="pd-kpi-card">',
            '<div class="pd-kpi-label">%s</div>',
            '<div class="pd-kpi-value %s">%s</div>',
            '%s',
            '</div>'
          ),
          m$label, val_class, val_str, bm_html
        )
      })

      shiny::div(
        class = "pd-kpi-grid",
        shiny::HTML(paste(cards, collapse = ""))
      )
    }
  )
}

# -- Strategy Comparison panel -------------------------------------------------

strategy_comparison_panel <- function() {
  list(
    id = "strategy_comparison",
    label = "Strategy Comparison",
    category = "Analysis",
    icon = "columns-gap",
    color = "#0891b2",
    description = "Side-by-side comparison of optimization strategies",
    controls = NULL,
    render = function(results, settings) {
      comp <- results$comparison
      if (is.null(comp) || length(comp) == 0) {
        return(shiny::div(
          class = "pf-empty-state",
          shiny::p(class = "pf-empty-state-text",
            "No strategies selected for comparison"),
          shiny::p(class = "pf-empty-state-hint",
            "Use the Compare control in the sidebar to add strategies")
        ))
      }

      strategy_labels <- c(
        mean_variance = "Mean-Variance",
        risk_parity = "Risk Parity",
        equal_weight = "Equal Weight",
        min_vol = "Min Volatility"
      )
      comp_colors <- c("#3b82f6", "#f59e0b", "#dc2626", "#0891b2")

      # Metrics table
      header <- '<tr><th>Metric</th>'
      for (s in names(comp)) {
        header <- paste0(header, '<th>', strategy_labels[s] %||% s,
          '</th>')
      }
      header <- paste0(header, '</tr>')

      metric_names <- c("Ann. Return", "Ann. Volatility", "Sharpe",
        "Max Drawdown", "VaR (95%)")
      rows <- ""
      for (mn in metric_names) {
        row <- paste0('<tr><td>', mn, '</td>')
        for (s in names(comp)) {
          bt <- comp[[s]]$backtest
          val <- switch(mn,
            "Ann. Return" = pf_fmt_pct(bt$ann_return),
            "Ann. Volatility" = pf_fmt_pct(bt$ann_vol),
            "Sharpe" = pf_fmt_num(bt$sharpe),
            "Max Drawdown" = pf_fmt_pct(bt$max_dd),
            "VaR (95%)" = pf_fmt_pct(abs(bt$var_95)),
            ""
          )
          row <- paste0(row, '<td>', val, '</td>')
        }
        rows <- paste0(rows, row, '</tr>')
      }

      table_html <- sprintf(
        '<table class="pf-compare-table"><thead>%s</thead><tbody>%s</tbody></table>',
        header, rows
      )

      shiny::div(
        class = "pf-compare-container",
        shiny::HTML(table_html)
      )
    }
  )
}
