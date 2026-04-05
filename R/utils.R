#' Format percentage
#' @param x Numeric value (0-1 scale)
#' @param digits Number of decimal places
#' @return Character string
#' @noRd
pf_fmt_pct <- function(x, digits = 1) {
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

#' Format number with sign
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Character string
#' @noRd
pf_fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

#' Generate colored square icon HTML for sidebar cards
#'
#' Creates a 40x40 colored square with a Bootstrap Icon SVG inside.
#'
#' @param icon_name Icon identifier
#' @param color Hex color for icon fill and background tint
#' @return HTML string
#' @noRd
pf_icon_html <- function(icon_name, color) {
  icons <- list(
    `pie-chart` = paste0(
      '<path d="M7.5 1.018a7 7 0 1 0 4.95 11.95l.707.707A8.001 8.001 ',
      '0 1 1 7 0zm1.354-.619a.5.5 0 0 1 .146.354v6.764a.5.5 0 0 1-.854',
      '.354L4.293 4.018a.5.5 0 0 1 0-.708 7.03 7.03 0 0 1 4.56-1.907z"/>',
      '<path d="M8.5.066a.5.5 0 0 1 .5.434c.003.03.005.06.007.09A7 7 0 0 ',
      '1 14.5 7.5a.5.5 0 0 1-.5.5H8.5a.5.5 0 0 1-.5-.5z"/>'
    ),
    `graph-up` = paste0(
      '<path fill-rule="evenodd" d="M0 0h1v15h15v1H0zm14.817 3.113a.5',
      '.5 0 0 1 .07.704l-4.5 5.5a.5.5 0 0 1-.74.037L7.06 6.767l-3.656',
      ' 5.027a.5.5 0 0 1-.808-.588l4-5.5a.5.5 0 0 1 .758-.06l2.609 ',
      '2.61 4.15-5.073a.5.5 0 0 1 .704-.07"/>'
    ),
    `graph-up-arrow` = paste0(
      '<path fill-rule="evenodd" d="M0 0h1v15h15v1H0zm10 3.5a.5.5 0 0 ',
      '1 .5-.5h4a.5.5 0 0 1 .5.5v4a.5.5 0 0 1-1 0V4.9l-3.613 4.417a',
      '.5.5 0 0 1-.74.037L7.06 6.767l-3.656 5.027a.5.5 0 0 1-.808-',
      '.588l4-5.5a.5.5 0 0 1 .758-.06l2.609 2.61L13.445 4H10.5a.5.5 ',
      '0 0 1-.5-.5"/>'
    ),
    `arrow-down-circle` = paste0(
      '<path fill-rule="evenodd" d="M1 8a7 7 0 1 0 14 0A7 7 0 0 0 1 8',
      'm15 0A8 8 0 1 1 0 8a8 8 0 0 1 16 0M8.5 4.5a.5.5 0 0 0-1 0v5',
      '.793L5.354 8.146a.5.5 0 1 0-.708.708l3 3a.5.5 0 0 0 .708 0l3-3',
      'a.5.5 0 0 0-.708-.708L8.5 10.293z"/>'
    ),
    `bar-chart` = paste0(
      '<path d="M4 11H2v3h2zm5-4H7v7h2zm5-5h-2v12h2zm-2-1a1 1 0 0 0',
      '-1 1v12a1 1 0 0 0 1 1h2a1 1 0 0 0 1-1V2a1 1 0 0 0-1-1zM6 7a1',
      ' 1 0 0 1 1-1h2a1 1 0 0 1 1 1v7a1 1 0 0 1-1 1H7a1 1 0 0 1-1-1',
      'zm-5 4a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v3a1 1 0 0 1-1 1H2a1 1 0 ',
      '0 1-1-1z"/>'
    ),
    speedometer2 = paste0(
      '<path d="M8 4a.5.5 0 0 1 .5.5V6a.5.5 0 0 1-1 0V4.5A.5.5 0 0 ',
      '1 8 4M3.732 5.732a.5.5 0 0 1 .707 0l.915.914a.5.5 0 1 1-.708',
      '.708l-.914-.915a.5.5 0 0 1 0-.707M2 10a.5.5 0 0 1 .5-.5h1.586',
      'a.5.5 0 0 1 0 1H2.5A.5.5 0 0 1 2 10m9.5 0a.5.5 0 0 1 .5-.5h',
      '1.5a.5.5 0 0 1 0 1H12a.5.5 0 0 1-.5-.5m.754-4.246a.39.39 0 0 ',
      '0-.527-.02L7.547 9.31a.91.91 0 1 0 1.302 1.258l3.434-4.297a.39',
      '.39 0 0 0-.029-.518z"/>',
      '<path fill-rule="evenodd" d="M0 10a8 8 0 1 1 15.547 2.661c-.442',
      ' 1.253-1.845 1.602-2.932 1.25C11.309 13.488 9.475 13 8 13c-1.474',
      ' 0-3.31.488-4.615.911-1.087.352-2.49.003-2.932-1.25A8 8 0 0 1 0',
      ' 10m8-7a7 7 0 0 0-6.603 9.329c.203.575.923.876 1.68.63C4.397 ',
      '12.533 6.358 12 8 12s3.604.532 4.923.96c.757.245 1.477-.056 ',
      '1.68-.631A7 7 0 0 0 8 3"/>'
    ),
    `columns-gap` = paste0(
      '<path d="M6 1v3H1V1zM1 0a1 1 0 0 0-1 1v3a1 1 0 0 0 1 1h5a1 1 ',
      '0 0 0 1-1V1a1 1 0 0 0-1-1zm14 12v3h-5v-3zm-5-1a1 1 0 0 0-1 ',
      '1v3a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1v-3a1 1 0 0 0-1-1zM6 8v7H1',
      'V8zM1 7a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h5a1 1 0 0 0 1-1V8a1 1 ',
      '0 0 0-1-1zm14-6v7h-5V1zm-5-1a1 1 0 0 0-1 1v7a1 1 0 0 0 1 1h5',
      'a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1z"/>'
    )
  )

  svg_path <- icons[[icon_name]]
  if (is.null(svg_path)) svg_path <- icons[["graph-up"]]

  hex <- sub("^#", "", color)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  bg_rgba <- sprintf("rgba(%d,%d,%d,0.15)", r, g, b)

  sprintf(
    paste0(
      '<div style="width:40px;height:40px;border-radius:7px;background:%s;',
      'display:flex;align-items:center;justify-content:center;flex-shrink:0">',
      '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" ',
      'fill="%s" viewBox="0 0 16 16">%s</svg></div>'
    ),
    bg_rgba, color, svg_path
  )
}

#' Empty chart placeholder
#' @param msg Message to display
#' @return echarts4r widget
#' @noRd
pf_empty_chart <- function(msg) {
  echarts4r::e_charts(height = 80) |>
    echarts4r::e_list(list(
      title = list(
        text = msg,
        left = "center", top = "center",
        textStyle = list(fontSize = 13, color = "#9ca3af", fontWeight = 400)
      ),
      xAxis = list(show = FALSE),
      yAxis = list(show = FALSE)
    ))
}

#' Shared tooltip config
#' @return List for echarts tooltip option
#' @noRd
pf_tooltip <- function() {
  list(
    trigger = "axis",
    confine = TRUE,
    backgroundColor = "rgba(255,255,255,0.98)",
    borderColor = "#d1d5db",
    borderWidth = 1,
    textStyle = list(color = "#1f2937", fontSize = 12),
    extraCssText = paste0(
      "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
      "border-radius: 6px; padding: 8px 12px;"
    )
  )
}

#' Asset class color palette
#' @param asset_class Character vector of asset class names
#' @return Named character vector of colors
#' @noRd
pf_class_colors <- function(asset_class = NULL) {
  colors <- c(
    Equity = "#3b82f6",
    Bond = "#059669",
    `Real Estate` = "#d97706",
    Commodity = "#f59e0b",
    Crypto = "#8b5cf6"
  )
  if (is.null(asset_class)) return(colors)
  colors[asset_class]
}
