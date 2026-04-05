#' Explorer panel registry
#' @return Named list of panel definitions
#' @noRd
explorer_panels <- function() {
  list(
    candlestick = candlestick_panel(),
    key_metrics = key_metrics_panel(),
    return_history = return_history_panel(),
    price_history = price_history_panel()
  )
}

#' Default explorer panel selection
#' @noRd
se_default_panels <- function() {
  c("candlestick", "key_metrics")
}

# -- Candlestick panel ---------------------------------------------------------

candlestick_panel <- function() {
  list(
    id = "candlestick",
    label = "Candlestick Chart",
    category = "Price",
    icon = "graph-up",
    color = "#3b82f6",
    description = "OHLC candlestick with moving averages",
    controls = list(
      show_ma = list(type = "toggle", label = "Moving Averages",
        default = FALSE),
      show_volume = list(type = "toggle", label = "Volume",
        default = FALSE)
    ),
    render = function(results, settings) {
      d <- results$ticker_data
      if (is.null(d) || nrow(d) == 0) {
        return(pf_empty_chart("No data for selected ticker"))
      }

      d <- d[order(d$date), ]
      show_ma <- isTRUE(settings$show_ma)
      show_volume <- isTRUE(settings$show_volume)

      # Build echarts options manually
      dates <- format(d$date, "%Y-%m-%d")
      ohlc_data <- lapply(seq_len(nrow(d)), function(i) {
        c(d$open[i], d$close[i], d$low[i], d$high[i])
      })

      series <- list(
        list(
          type = "candlestick", name = "OHLC",
          data = ohlc_data,
          itemStyle = list(
            color = "#059669", color0 = "#dc2626",
            borderColor = "#059669", borderColor0 = "#dc2626"
          )
        )
      )

      # Moving averages
      if (show_ma && nrow(d) >= 20) {
        ma20 <- stats::filter(d$close, rep(1/20, 20), sides = 1)
        series[[length(series) + 1]] <- list(
          type = "line", name = "MA20",
          data = as.list(round(as.numeric(ma20), 2)),
          smooth = TRUE,
          lineStyle = list(width = 1, color = "#f59e0b"),
          showSymbol = FALSE
        )
        if (nrow(d) >= 50) {
          ma50 <- stats::filter(d$close, rep(1/50, 50), sides = 1)
          series[[length(series) + 1]] <- list(
            type = "line", name = "MA50",
            data = as.list(round(as.numeric(ma50), 2)),
            smooth = TRUE,
            lineStyle = list(width = 1, color = "#8b5cf6"),
            showSymbol = FALSE
          )
        }
      }

      grid <- list(list(
        left = 60, right = 25,
        top = 40, bottom = if (show_volume) "35%" else 60
      ))
      x_axis <- list(list(
        type = "category", data = as.list(dates),
        boundaryGap = TRUE,
        axisLine = list(lineStyle = list(color = "#d1d5db")),
        axisLabel = list(color = "#6b7280", fontSize = 11)
      ))
      y_axis <- list(list(
        scale = TRUE,
        axisLine = list(lineStyle = list(color = "#d1d5db")),
        axisLabel = list(color = "#6b7280", fontSize = 11)
      ))
      data_zoom <- list(
        list(type = "inside", xAxisIndex = 0, start = 70, end = 100),
        list(type = "slider", xAxisIndex = 0, start = 70, end = 100,
          bottom = if (show_volume) "15%" else 10)
      )

      opts <- list(
        backgroundColor = "transparent",
        tooltip = pf_tooltip(),
        legend = list(
          data = if (show_ma) list("OHLC", "MA20", "MA50")
          else list("OHLC"),
          top = 10
        ),
        grid = grid,
        xAxis = x_axis,
        yAxis = y_axis,
        dataZoom = data_zoom,
        series = series
      )

      # Volume subplot
      if (show_volume && "volume" %in% names(d)) {
        vol_data <- lapply(seq_len(nrow(d)), function(i) {
          color <- if (d$close[i] >= d$open[i]) "#059669" else "#dc2626"
          list(value = d$volume[i],
            itemStyle = list(color = color, opacity = 0.5))
        })
        opts$grid[[2]] <- list(
          left = 60, right = 25, top = "72%", bottom = "15%"
        )
        opts$xAxis[[2]] <- list(
          type = "category", data = as.list(dates),
          gridIndex = 1, boundaryGap = TRUE,
          axisLabel = list(show = FALSE),
          axisLine = list(lineStyle = list(color = "#d1d5db"))
        )
        opts$yAxis[[2]] <- list(
          gridIndex = 1, scale = TRUE,
          axisLabel = list(show = FALSE),
          axisLine = list(show = FALSE),
          splitLine = list(show = FALSE)
        )
        opts$dataZoom[[1]]$xAxisIndex <- c(0, 1)
        opts$dataZoom[[2]]$xAxisIndex <- c(0, 1)
        opts$series[[length(opts$series) + 1]] <- list(
          type = "bar", name = "Volume",
          data = vol_data, xAxisIndex = 1, yAxisIndex = 1,
          barWidth = "60%"
        )
      }

      echarts4r::e_charts(height = if (show_volume) 500 else 400) |>
        echarts4r::e_list(opts)
    }
  )
}

# -- Key Metrics panel ---------------------------------------------------------

key_metrics_panel <- function() {
  list(
    id = "key_metrics",
    label = "Key Metrics",
    category = "Summary",
    icon = "speedometer2",
    color = "#6366f1",
    description = "Core performance and risk indicators",
    controls = NULL,
    render = function(results, settings) {
      m <- results$metrics
      if (is.null(m)) return(pf_empty_chart("No metrics available"))

      cards <- list(
        list(label = "Last Close",
          value = paste0("$", pf_fmt_num(m$last_close, 2)),
          color = "neutral"),
        list(label = "52-Week High",
          value = paste0("$", pf_fmt_num(m$high_52w, 2)),
          color = "neutral"),
        list(label = "52-Week Low",
          value = paste0("$", pf_fmt_num(m$low_52w, 2)),
          color = "neutral"),
        list(label = "YTD Return",
          value = pf_fmt_pct(m$ytd_return),
          color = if (m$ytd_return >= 0) "positive" else "negative"),
        list(label = "Ann. Volatility",
          value = pf_fmt_pct(m$ann_vol),
          color = "neutral")
      )

      card_tags <- lapply(cards, function(card) {
        shiny::div(class = "se-kpi-card",
          shiny::div(class = "se-kpi-label", card$label),
          shiny::div(class = paste("se-kpi-value",
            paste0("se-kpi-", card$color)),
            card$value
          )
        )
      })

      shiny::div(class = "se-kpi-grid", card_tags)
    }
  )
}

# -- Return History panel ------------------------------------------------------

return_history_panel <- function() {
  list(
    id = "return_history",
    label = "Monthly Returns",
    category = "Performance",
    icon = "bar-chart",
    color = "#059669",
    description = "Monthly return bar chart",
    controls = NULL,
    render = function(results, settings) {
      monthly <- results$monthly_returns
      if (is.null(monthly) || nrow(monthly) == 0) {
        return(pf_empty_chart("No return data"))
      }

      bar_data <- lapply(seq_len(nrow(monthly)), function(i) {
        val <- round(monthly$return[i] * 100, 2)
        color <- if (val >= 0) "#059669" else "#dc2626"
        list(value = val, itemStyle = list(color = color))
      })

      echarts4r::e_charts(height = 300) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = list(
            trigger = "axis",
            formatter = htmlwidgets::JS(
              "function(p){return p[0].name+': '+p[0].value+'%';}"
            )
          ),
          grid = list(left = 60, right = 25, top = 20, bottom = 40),
          xAxis = list(
            type = "category",
            data = as.list(monthly$month),
            axisLabel = list(rotate = 45, fontSize = 10,
              color = "#6b7280")
          ),
          yAxis = list(
            type = "value",
            axisLabel = list(
              formatter = htmlwidgets::JS("function(v){return v+'%';}"),
              color = "#6b7280", fontSize = 11
            )
          ),
          series = list(list(
            type = "bar", data = bar_data, barWidth = "60%"
          ))
        ))
    }
  )
}

# -- Price History panel -------------------------------------------------------

price_history_panel <- function() {
  list(
    id = "price_history",
    label = "Price History",
    category = "Performance",
    icon = "graph-up-arrow",
    color = "#0891b2",
    description = "Daily closing price line chart",
    controls = list(
      log_scale = list(type = "toggle", label = "Log scale",
        default = FALSE)
    ),
    render = function(results, settings) {
      d <- results$ticker_data
      if (is.null(d) || nrow(d) == 0) {
        return(pf_empty_chart("No price data"))
      }

      d <- d[order(d$date), ]
      log_scale <- isTRUE(settings$log_scale)

      echarts4r::e_charts(height = 350) |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          tooltip = pf_tooltip(),
          grid = list(left = 60, right = 25, top = 20, bottom = 60),
          xAxis = list(
            type = "category",
            data = as.list(format(d$date, "%Y-%m-%d")),
            axisLabel = list(color = "#6b7280", fontSize = 11)
          ),
          yAxis = list(
            type = if (log_scale) "log" else "value",
            scale = TRUE,
            axisLabel = list(color = "#6b7280", fontSize = 11)
          ),
          dataZoom = list(
            list(type = "inside", start = 0, end = 100),
            list(type = "slider", start = 0, end = 100, bottom = 10)
          ),
          series = list(list(
            type = "line", name = "Close",
            data = as.list(round(d$close, 2)),
            showSymbol = FALSE,
            lineStyle = list(width = 1.5, color = "#3b82f6"),
            areaStyle = list(
              color = list(
                type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
                colorStops = list(
                  list(offset = 0, color = "rgba(59,130,246,0.15)"),
                  list(offset = 1, color = "rgba(59,130,246,0.02)")
                )
              )
            )
          ))
        ))
    }
  )
}
