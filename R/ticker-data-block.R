#' Ticker Data Block
#'
#' A data block that fetches OHLC price data for one or more tickers
#' via quantmod. Falls back to bundled data when offline.
#' Includes a searchable ticker table with Yahoo Finance autocomplete.
#'
#' @param tickers Character vector of ticker symbols
#' @param from Start date
#' @param to End date
#' @param periodicity "daily", "weekly", or "monthly"
#' @param source "yahoo", "av", or "tiingo"
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `ticker_data_block`
#' @export
new_ticker_data_block <- function(
    tickers = "AAPL",
    from = Sys.Date() - 365,
    to = Sys.Date(),
    periodicity = "daily",
    source = "yahoo",
    ...) {

  # Build initial ticker list from bundled metadata
  bundled <- pf_bundled_tickers()
  initial_tickers <- lapply(seq_len(nrow(bundled)), function(i) {
    list(
      ticker = bundled$ticker[i],
      name = bundled$name[i],
      sector = if ("sub_class" %in% names(bundled)) bundled$sub_class[i]
        else "",
      source = "bundled"
    )
  })

  blockr.core::new_data_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_tickers <- shiny::reactiveVal(tickers)
          r_from <- shiny::reactiveVal(from)
          r_to <- shiny::reactiveVal(to)
          r_periodicity <- shiny::reactiveVal(periodicity)
          r_source <- shiny::reactiveVal(source)

          # Ticker table selection → r_tickers
          shiny::observeEvent(input$ticker_table, {
            val <- input$ticker_table
            if (!is.null(val) && length(val) > 0) {
              r_tickers(val)
            }
          }, ignoreNULL = FALSE)

          # Server-side search: Yahoo Finance lookup
          shiny::observeEvent(input$ticker_table_search, {
            query <- input$ticker_table_search
            if (is.null(query) || nchar(query) < 2) return()
            results <- pf_search_tickers(query, limit = 15)
            if (nrow(results) == 0) return()
            ticker_list <- lapply(seq_len(nrow(results)), function(i) {
              list(
                ticker = results$ticker[i],
                name = results$name[i],
                sector = if ("type" %in% names(results))
                  results$type[i] else "",
                source = "yahoo"
              )
            })
            session$sendCustomMessage("ticker-table-results", list(
              id = session$ns("ticker_table"),
              tickers = ticker_list
            ))
          })

          # Handle control messages for periodicity/source
          shiny::observeEvent(input$td_ctrl, {
            msg <- input$td_ctrl
            if (is.null(msg)) return()
            switch(msg$param,
              periodicity = r_periodicity(msg$value),
              source = r_source(msg$value)
            )
          })

          list(
            expr = shiny::reactive({
              bquote(blockr.portfolio:::pf_fetch_tickers(
                tickers = .(r_tickers()),
                from = .(as.character(r_from())),
                to = .(as.character(r_to())),
                periodicity = .(r_periodicity()),
                source = .(r_source())
              ))
            }),
            state = list(
              tickers = r_tickers,
              from = r_from,
              to = r_to,
              periodicity = r_periodicity,
              source = r_source
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      # Register resource paths for JS/CSS
      pkg_path <- system.file(package = "blockr.portfolio")
      if (nzchar(pkg_path)) {
        shiny::addResourcePath("blockr-portfolio-js",
          file.path(pkg_path, "js"))
        shiny::addResourcePath("blockr-portfolio-css",
          file.path(pkg_path, "css"))
      }

      # Build initial state JSON for the ticker table
      state_json <- jsonlite::toJSON(list(
        selected = as.list(tickers),
        tickers = initial_tickers
      ), auto_unbox = TRUE)

      shiny::tagList(
        shiny::tags$link(rel = "stylesheet",
          href = "blockr-portfolio-css/ticker-table.css"),
        shiny::tags$script(
          src = "blockr-portfolio-js/ticker-table.js"),
        shiny::tags$style(shiny::HTML("
          .td-layout { font-family: 'Open Sans', system-ui, sans-serif; }
          .td-group { margin-bottom: 10px; }
          .td-label { font-size: 11px; font-weight: 500; color: #6b7280;
            text-transform: uppercase; letter-spacing: 0.5px;
            margin-bottom: 4px; }
          .td-radios { display: flex; flex-wrap: wrap; gap: 4px; }
          .td-radio { padding: 4px 10px; border: 1px solid #d1d5db;
            border-radius: 6px; background: #fff; color: #374151;
            font-size: 12px; font-family: inherit; cursor: pointer;
            transition: all 0.15s; }
          .td-radio:hover { background: #f3f4f6; }
          .td-radio.is-active { background: #dbeafe;
            border-color: #93c5fd; color: #1d4ed8; font-weight: 500; }
        ")),
        shiny::div(
          class = "td-layout", id = ns("td_layout"),

          # Ticker table
          shiny::div(class = "td-group",
            shiny::div(class = "td-label", "Tickers"),
            shiny::div(
              id = ns("ticker_table"),
              class = "tt-container",
              `data-state` = as.character(state_json)
            )
          ),

          # Periodicity
          shiny::div(class = "td-group",
            shiny::div(class = "td-label", "Periodicity"),
            shiny::div(class = "td-radios",
              td_radio_chip(ns, "periodicity", "daily", "Daily",
                periodicity),
              td_radio_chip(ns, "periodicity", "weekly", "Weekly",
                periodicity),
              td_radio_chip(ns, "periodicity", "monthly", "Monthly",
                periodicity)
            )
          ),

          # Source
          shiny::div(class = "td-group",
            shiny::div(class = "td-label", "Source"),
            shiny::div(class = "td-radios",
              td_radio_chip(ns, "source", "yahoo", "Yahoo",
                source),
              td_radio_chip(ns, "source", "av", "Alpha Vantage",
                source),
              td_radio_chip(ns, "source", "tiingo", "Tiingo",
                source)
            )
          )
        ),

        # JS for radio chips
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("td_layout"), "';
            var ctrlId = '", ns("td_ctrl"), "';

            $(document).on('click', '#' + layoutId + ' .td-radio',
              function(e) {
                e.stopPropagation();
                $(this).siblings('.td-radio').removeClass('is-active');
                $(this).addClass('is-active');
                Shiny.setInputValue(ctrlId, {
                  param: $(this).data('param'),
                  value: $(this).data('value')
                }, {priority: 'event'});
              });
          });
        ")))
      )
    },
    external_ctrl = c("tickers", "from", "to", "periodicity"),
    class = "ticker_data_block",
    ...
  )
}

td_radio_chip <- function(ns, param, value, label, current) {
  is_active <- identical(current, value)
  shiny::tags$button(
    class = paste("td-radio", if (is_active) "is-active"),
    `data-param` = param,
    `data-value` = value,
    label
  )
}
