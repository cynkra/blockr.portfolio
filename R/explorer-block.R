#' Share Explorer Block
#'
#' A rich dual-pane block for exploring individual ticker data.
#' Sidebar with ticker selector + panel picker, scrollable chart area.
#'
#' @param selected_ticker Initial ticker to display (NULL = first available)
#' @param selected Initial panel IDs to show
#' @param panel_settings Named list of per-panel settings
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `share_explorer_block`
#' @export
new_share_explorer_block <- function(
    selected_ticker = NULL,
    selected = NULL,
    panel_settings = list(),
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_selected_ticker <- shiny::reactiveVal(selected_ticker)
          r_selected <- shiny::reactiveVal(selected)
          r_panel_settings <- shiny::reactiveVal(panel_settings)

          all_panels <- explorer_panels()

          # Init defaults
          init_done <- shiny::reactiveVal(FALSE)
          shiny::observe({
            data()
            if (!init_done()) {
              if (is.null(r_selected()) || length(r_selected()) == 0) {
                r_selected(se_default_panels())
              }
              settings <- r_panel_settings()
              for (p in all_panels) {
                if (is.null(settings[[p$id]])) {
                  ctrls <- p$controls
                  if (!is.null(ctrls)) {
                    settings[[p$id]] <- lapply(ctrls, `[[`, "default")
                  } else {
                    settings[[p$id]] <- list()
                  }
                }
              }
              r_panel_settings(settings)
              init_done(TRUE)
            }
          })

          # Extract data (plain data.frame with OHLC columns)
          r_ohlc <- shiny::reactive({
            d <- data()
            shiny::req(is.data.frame(d), nrow(d) > 0)
            d
          })

          r_metadata <- shiny::reactive(NULL)

          r_available_tickers <- shiny::reactive({
            sort(unique(r_ohlc()$ticker))
          })

          # Auto-select first ticker, or re-select if current is gone
          shiny::observe({
            tickers <- r_available_tickers()
            current <- r_selected_ticker()
            if (length(tickers) > 0 &&
                (is.null(current) || !current %in% tickers)) {
              r_selected_ticker(tickers[1])
            }
          })

          # Filtered data for selected ticker
          r_ticker_data <- shiny::reactive({
            shiny::req(r_selected_ticker())
            ohlc <- r_ohlc()
            d <- ohlc[ohlc$ticker == r_selected_ticker(), , drop = FALSE]
            d[order(d$date), ]
          })

          # Computed metrics
          r_metrics <- shiny::reactive({
            d <- r_ticker_data()
            shiny::req(nrow(d) > 0)
            last_close <- utils::tail(d$close, 1)
            high_52w <- max(d$high, na.rm = TRUE)
            low_52w <- min(d$low, na.rm = TRUE)
            first_close <- utils::head(d$close, 1)
            ytd_return <- (last_close / first_close) - 1
            daily_returns <- diff(log(d$close))
            ann_vol <- stats::sd(daily_returns, na.rm = TRUE) * sqrt(252)
            list(
              last_close = last_close, high_52w = high_52w,
              low_52w = low_52w, ytd_return = ytd_return,
              ann_vol = ann_vol
            )
          })

          # Monthly returns
          r_monthly_returns <- shiny::reactive({
            d <- r_ticker_data()
            shiny::req(nrow(d) > 1)
            d <- d[order(d$date), ]
            # Compute return between consecutive observations
            closes <- d$close
            returns <- closes[-1] / closes[-length(closes)] - 1
            data.frame(
              month = format(d$date[-1], "%Y-%m"),
              return = returns,
              stringsAsFactors = FALSE
            )
          })

          # Compute summary info for ALL tickers
          r_all_ticker_info <- shiny::reactive({
            ohlc <- r_ohlc()
            shiny::req(nrow(ohlc) > 0)
            tickers_avail <- r_available_tickers()
            lapply(stats::setNames(tickers_avail, tickers_avail),
              function(tkr) {
                d <- ohlc[ohlc$ticker == tkr, , drop = FALSE]
                d <- d[order(d$date), ]
                if (nrow(d) == 0) return(NULL)
                last_close <- utils::tail(d$close, 1)
                prev_close <- if (nrow(d) >= 2)
                  d$close[nrow(d) - 1] else last_close
                change <- last_close - prev_close
                change_pct <- if (prev_close != 0)
                  change / prev_close else 0
                list(ticker = tkr, name = tkr,
                  price = last_close, change = change,
                  change_pct = change_pct)
              })
          })

          # Input handlers
          shiny::observeEvent(input$select_ticker, {
            r_selected_ticker(input$select_ticker)
          })

          shiny::observeEvent(input$toggle_panel, {
            panel_id <- input$toggle_panel
            sel <- r_selected()
            if (panel_id %in% sel) {
              r_selected(setdiff(sel, panel_id))
            } else {
              r_selected(c(sel, panel_id))
            }
          })

          shiny::observeEvent(input$reorder_panel, {
            new_order <- input$reorder_panel
            if (is.null(new_order)) return()
            new_order <- as.character(unlist(new_order))
            cur <- r_selected()
            if (setequal(new_order, cur)) r_selected(new_order)
          })

          shiny::observeEvent(input$panel_ctrl, {
            msg <- input$panel_ctrl
            if (is.null(msg)) return()
            settings <- r_panel_settings()
            if (is.null(settings[[msg$panel_id]])) {
              settings[[msg$panel_id]] <- list()
            }
            settings[[msg$panel_id]][[msg$param]] <- msg$value
            r_panel_settings(settings)
          })

          shiny::observe({
            session$sendCustomMessage(
              session$ns("sync_selected"), r_selected()
            )
          })

          # Sidebar: ticker card list
          output$ticker_section <- shiny::renderUI({
            all_info <- r_all_ticker_info()
            sel <- r_selected_ticker()
            if (length(all_info) == 0) return(NULL)

            # Build a card for each ticker
            cards <- lapply(all_info, function(info) {
              if (is.null(info)) return(NULL)
              is_active <- identical(info$ticker, sel)
              change_class <- if (info$change >= 0) "positive"
                else "negative"
              change_sign <- if (info$change >= 0) "+" else ""
              shiny::div(
                class = paste("se-tkr-card",
                  if (is_active) "is-active"),
                `data-ticker` = info$ticker,
                shiny::div(class = "se-tkr-card-header",
                  shiny::span(class = "se-tkr-symbol",
                    info$ticker),
                  shiny::span(class = "se-tkr-price",
                    paste0("$", pf_fmt_num(info$price, 2)))
                ),
                shiny::div(
                  class = paste("se-tkr-change", change_class),
                  paste0(change_sign, pf_fmt_num(info$change, 2),
                    " (", change_sign,
                    pf_fmt_pct(info$change_pct), ")")
                )
              )
            })
            cards <- Filter(Negate(is.null), cards)
            shiny::div(class = "se-tkr-list", cards)
          })

          # Sidebar: panel cards
          output$sidebar_cards <- shiny::renderUI({
            sel <- shiny::isolate(r_selected())
            if (length(all_panels) == 0) return(NULL)

            build_card <- function(p, is_sel) {
              shiny::div(
                class = paste("se-card",
                  if (is_sel) "is-selected"),
                `data-panel-id` = p$id,
                `data-category` = p$category,
                `data-search-text` = paste(p$label, p$description,
                  p$category),
                shiny::div(class = "se-card-main",
                  shiny::div(class = "se-card-icon",
                    shiny::HTML(pf_icon_html(p$icon, p$color))),
                  shiny::div(class = "se-card-content",
                    shiny::tags$p(class = "se-card-title", p$label),
                    shiny::tags$p(class = "se-card-description",
                      p$description)),
                  shiny::div(class = "se-card-check",
                    shiny::HTML(paste0(
                      '<svg xmlns="http://www.w3.org/2000/svg" ',
                      'width="12" height="12" fill="currentColor" ',
                      'viewBox="0 0 16 16">',
                      '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7',
                      'a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-',
                      '.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 ',
                      '0z"/></svg>'))
                  )
                )
              )
            }

            active_ids <- intersect(sel, names(all_panels))
            active_cards <- lapply(active_ids, function(pid) {
              build_card(all_panels[[pid]], is_sel = TRUE)
            })
            unsel_panels <- all_panels[setdiff(names(all_panels), sel)]
            categories <- unique(vapply(all_panels, `[[`,
              character(1L), "category"))
            category_groups <- lapply(categories, function(cat) {
              cpanels <- Filter(function(p) p$category == cat,
                unsel_panels)
              if (length(cpanels) == 0) return(NULL)
              shiny::div(class = "se-category-group",
                `data-category` = cat,
                shiny::div(class = "se-category-header",
                  shiny::tags$span(toupper(cat))),
                lapply(cpanels, function(p) {
                  build_card(p, is_sel = FALSE)
                }))
            })
            category_groups <- Filter(Negate(is.null), category_groups)

            shiny::tagList(
              shiny::div(class = "se-active-section",
                shiny::div(class = "se-section-header", "SELECTED"),
                shiny::div(class = paste("se-active-hint",
                  if (length(active_cards) < 2) "is-hidden"),
                  "Drag to reorder"),
                shiny::div(class = "se-active-list", active_cards,
                  shiny::div(class = paste("se-active-empty",
                    if (length(active_cards) > 0) "is-hidden"),
                    "Click a card below to add it here"))
              ),
              shiny::div(class = "se-available-section",
                shiny::div(class = "se-section-header", "AVAILABLE"),
                category_groups)
            )
          })

          # Chart area
          output$chart_area <- shiny::renderUI({
            sel <- r_selected()
            all_settings <- r_panel_settings()
            active_ids <- intersect(sel, names(all_panels))

            if (length(active_ids) == 0) {
              return(shiny::div(class = "se-empty-state",
                shiny::p(class = "se-empty-state-text",
                  "No panels selected"),
                shiny::p(class = "se-empty-state-hint",
                  "Click cards in the sidebar to add charts")))
            }

            results <- tryCatch(list(
              ticker_data = r_ticker_data(),
              metrics = r_metrics(),
              monthly_returns = r_monthly_returns(),
              selected_ticker = r_selected_ticker()
            ), error = function(e) NULL)

            if (is.null(results)) {
              return(shiny::div(class = "se-empty-state",
                shiny::p(class = "se-empty-state-text",
                  "Loading data...")))
            }

            se_controls_ui <- function(panel, panel_id, settings) {
              controls <- panel$controls
              if (is.null(controls)) return(NULL)
              tags <- lapply(names(controls), function(param) {
                ctrl <- controls[[param]]
                cur_val <- settings[[param]] %||% ctrl$default
                if (ctrl$type == "toggle") {
                  is_on <- isTRUE(cur_val)
                  shiny::div(class = "se-ctrl-group",
                    shiny::span(class = "se-ctrl-label", ctrl$label),
                    shiny::tags$button(
                      class = paste("se-ctrl-toggle",
                        if (is_on) "is-on"),
                      `data-panel-id` = panel_id,
                      `data-param` = param,
                      shiny::span(class = "se-ctrl-toggle-track",
                        shiny::span(class = "se-ctrl-toggle-thumb"))))
                } else NULL
              })
              tags <- Filter(Negate(is.null), tags)
              if (length(tags) == 0) return(NULL)
              shiny::div(class = "se-chart-controls", tags)
            }

            chart_tags <- lapply(active_ids, function(panel_id) {
              panel <- all_panels[[panel_id]]
              psettings <- all_settings[[panel_id]] %||% list()
              chart <- tryCatch(
                panel$render(results, psettings),
                error = function(e) pf_empty_chart(
                  paste("Error:", conditionMessage(e))))
              shiny::div(class = "se-chart-panel",
                shiny::div(class = "se-chart-header",
                  shiny::div(class = "se-chart-title", panel$label),
                  se_controls_ui(panel, panel_id, psettings),
                  shiny::div(class = "se-chart-category",
                    panel$category)),
                shiny::div(class = "se-chart-body", chart))
            })

            # Resize echarts after DOM settles (fixes initial width)
            resize_js <- shiny::tags$script(shiny::HTML(
              "setTimeout(function(){window.dispatchEvent(new Event('resize'))}, 200);"
            ))

            shiny::tagList(chart_tags, resize_js)
          })

          list(
            expr = shiny::reactive(quote(identity(data))),
            state = list(
              selected_ticker = r_selected_ticker,
              selected = r_selected,
              panel_settings = r_panel_settings
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$head(shiny::tags$link(
          rel = "stylesheet",
          href = paste0("https://fonts.googleapis.com/css2?",
            "family=Open+Sans:wght@400;500;600&display=swap")
        )),
        se_explorer_css(),
        shiny::div(
          class = "se-layout", id = ns("se_layout"),
          shiny::div(
            class = "se-sidebar", id = ns("se_sidebar"),
            shiny::div(class = "se-sidebar-header",
              shiny::tags$h3(class = "se-sidebar-title",
                "Share Explorer"),
              shiny::tags$button(class = "se-pin-btn",
                id = ns("pin_btn"), title = "Toggle sidebar",
                shiny::HTML(paste0(
                  '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                  'height="16" fill="currentColor" viewBox="0 0 16 16">',
                  '<path d="M4.146.146A.5.5 0 0 1 4.5 0h7a.5.5 0 0 1 ',
                  '.5.5c0 .68-.342 1.174-.646 1.479-.126.125-.25.224-',
                  '.354.298v4.431l.078.048c.203.127.476.314.751.555',
                  'C12.36 7.775 13 8.527 13 9.5a.5.5 0 0 1-.5.5h-4v4.5',
                  'a.5.5 0 0 1-1 0V10h-4A.5.5 0 0 1 3 9.5c0-.973.64-',
                  '1.725 1.17-2.189A6 6 0 0 1 5 6.708V2.277a3 3 0 0 ',
                  '1-.354-.298C4.342 1.674 4 1.179 4 .5a.5.5 0 0 1 ',
                  '.146-.354z"/></svg>'))
              )
            ),
            shiny::div(class = "se-ticker-section",
              shiny::uiOutput(ns("ticker_section"))),
            shiny::div(class = "se-sidebar-divider"),
            shiny::div(class = "se-sidebar-search",
              shiny::div(class = "se-sidebar-search-wrapper",
                shiny::span(class = "se-sidebar-search-icon",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                    'height="16" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h',
                    '-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-',
                    '1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 ',
                    '5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></svg>'))),
                shiny::tags$input(type = "text",
                  class = "se-sidebar-search-input",
                  id = ns("search"),
                  placeholder = "Search panels..."))
            ),
            shiny::div(class = "se-sidebar-content",
              shiny::uiOutput(ns("sidebar_cards")))
          ),
          shiny::tags$button(class = "se-expand-btn",
            id = ns("expand_btn"), title = "Show sidebar",
            shiny::HTML(paste0(
              '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
              'height="16" fill="currentColor" viewBox="0 0 16 16">',
              '<path fill-rule="evenodd" d="M1 8a.5.5 0 0 1 .5-.5h11',
              '.793l-3.147-3.146a.5.5 0 0 1 .708-.708l4 4a.5.5 0 0 ',
              '1 0 .708l-4 4a.5.5 0 0 1-.708-.708L13.293 8.5H1.5A.5',
              '.5 0 0 1 1 8z"/></svg>'))),
          shiny::div(class = "se-chart-area",
            shiny::uiOutput(ns("chart_area")))
        ),

        # JS
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("se_layout"), "';
            var sidebarId = '", ns("se_sidebar"), "';
            var searchId = '", ns("search"), "';
            var pinBtnId = '", ns("pin_btn"), "';
            var expandBtnId = '", ns("expand_btn"), "';
            var toggleInputId = '", ns("toggle_panel"), "';
            var panelCtrlInputId = '", ns("panel_ctrl"), "';
            var syncMsgId = '", ns("sync_selected"), "';
            var reorderInputId = '", ns("reorder_panel"), "';
            var selectTickerId = '", ns("select_ticker"), "';

            // Ticker card click
            $(document).on('click', '#' + layoutId + ' .se-tkr-card', function(e) {
              e.stopPropagation();
              var ticker = $(this).data('ticker');
              if (!ticker) return;
              $(this).closest('.se-tkr-list').find('.se-tkr-card').removeClass('is-active');
              $(this).addClass('is-active');
              Shiny.setInputValue(selectTickerId, ticker, {priority: 'event'});
            });

            // Panel card click
            $(document).on('click', '#' + layoutId + ' .se-card', function(e) {
              var panelId = $(this).data('panel-id');
              if (panelId) Shiny.setInputValue(toggleInputId, panelId, {priority: 'event'});
            });

            // Panel toggle control
            $(document).on('click', '#' + layoutId + ' .se-ctrl-toggle', function(e) {
              e.stopPropagation();
              $(this).toggleClass('is-on');
              Shiny.setInputValue(panelCtrlInputId, {
                panel_id: $(this).data('panel-id'),
                param: $(this).data('param'),
                value: $(this).hasClass('is-on')
              }, {priority: 'event'});
            });

            // Search
            $(document).on('input', '#' + searchId, function() {
              var query = $(this).val().toLowerCase();
              var sidebar = $(this).closest('.se-sidebar');
              sidebar.find('.se-card').each(function() {
                var text = ($(this).data('search-text') || '').toLowerCase();
                $(this).toggle(!query || text.indexOf(query) >= 0);
              });
              sidebar.find('.se-category-group').each(function() {
                $(this).toggle($(this).find('.se-card:visible').length > 0);
              });
            });

            // Pin/expand sidebar
            $(document).on('click', '#' + pinBtnId, function() {
              document.getElementById(sidebarId).classList.toggle('collapsed');
              document.getElementById(layoutId).classList.toggle('sidebar-collapsed');
              $(this).toggleClass('is-unpinned');
            });
            $(document).on('click', '#' + expandBtnId, function() {
              document.getElementById(sidebarId).classList.remove('collapsed');
              document.getElementById(layoutId).classList.remove('sidebar-collapsed');
              $('#' + pinBtnId).removeClass('is-unpinned');
            });

            // Sync selected
            Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
              if (!selected) selected = [];
              if (typeof selected === 'string') selected = [selected];
              var $layout = $('#' + layoutId);
              var $activeList = $layout.find('.se-active-list');
              var $availSection = $layout.find('.se-available-section');
              var cardMap = {};
              $layout.find('.se-card').each(function() {
                var pid = $(this).data('panel-id');
                if (pid) cardMap[pid] = $(this);
              });
              var $hint = $activeList.find('.se-active-empty');
              for (var i = 0; i < selected.length; i++) {
                var $card = cardMap[selected[i]];
                if ($card && $card.length) {
                  $card.addClass('is-selected').attr('draggable', 'true');
                  $hint.before($card);
                }
              }
              Object.keys(cardMap).forEach(function(pid) {
                if (selected.indexOf(pid) >= 0) return;
                var $card = cardMap[pid];
                $card.removeClass('is-selected').removeAttr('draggable');
                var cat = $card.data('category');
                var $group = $availSection.find('.se-category-group[data-category=' + JSON.stringify(cat) + ']');
                if (!$group.length) {
                  $group = $('<div class=se-category-group data-category=' + JSON.stringify(cat) + '><div class=se-category-header><span>' + cat.toUpperCase() + '</span></div></div>');
                  $availSection.append($group);
                }
                $group.append($card);
              });
              $hint.toggleClass('is-hidden', selected.length > 0);
              $layout.find('.se-active-hint').toggleClass('is-hidden', selected.length < 2);
              $availSection.find('.se-category-group').each(function() {
                $(this).toggle($(this).find('.se-card').length > 0);
              });
            });

            // Drag and drop
            var dragActive = false;
            var $doc = $(document);
            $doc.on('dragstart', '#' + layoutId + ' .se-active-list .se-card', function(e) {
              dragActive = true; $(this).addClass('is-dragging');
              e.originalEvent.dataTransfer.effectAllowed = 'move';
              e.originalEvent.dataTransfer.setData('text/plain', $(this).data('panel-id'));
            });
            $doc.on('dragover', '#' + layoutId + ' .se-active-list .se-card', function(e) {
              if (!dragActive) return; e.preventDefault();
              var rect = this.getBoundingClientRect();
              if (e.originalEvent.clientY < rect.top + rect.height / 2) {
                $(this).addClass('drop-above').removeClass('drop-below');
              } else { $(this).addClass('drop-below').removeClass('drop-above'); }
            });
            $doc.on('dragleave', '#' + layoutId + ' .se-active-list .se-card', function() {
              $(this).removeClass('drop-above drop-below');
            });
            $doc.on('drop', '#' + layoutId + ' .se-active-list .se-card', function(e) {
              e.preventDefault();
              var draggedId = e.originalEvent.dataTransfer.getData('text/plain');
              var $target = $(this); $target.removeClass('drop-above drop-below');
              if (draggedId === $target.data('panel-id')) return;
              var $dragged = $target.closest('.se-active-list').find('.se-card[data-panel-id=' + JSON.stringify(draggedId) + ']');
              if (!$dragged.length) return;
              var rect = this.getBoundingClientRect();
              if (e.originalEvent.clientY < rect.top + rect.height / 2) { $dragged.insertBefore($target); }
              else { $dragged.insertAfter($target); }
              var newOrder = [];
              $dragged.closest('.se-active-list').find('.se-card').each(function() { newOrder.push($(this).data('panel-id')); });
              Shiny.setInputValue(reorderInputId, newOrder, {priority: 'event'});
            });
            $doc.on('dragend', '#' + layoutId + ' .se-active-list .se-card', function() {
              $(this).removeClass('is-dragging');
              $(this).closest('.se-active-list').find('.se-card').removeClass('drop-above drop-below');
              setTimeout(function() { dragActive = false; }, 0);
            });
            $doc.on('dragover', '#' + layoutId + ' .se-active-list', function(e) {
              if (dragActive) e.preventDefault();
            });
          });
        ")))
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame with OHLC columns")
      }
      needed <- c("date", "ticker", "close")
      if (!all(needed %in% colnames(data))) {
        stop("Data frame must have columns: ",
          paste(needed, collapse = ", "))
      }
    },
    allow_empty_state = "panel_settings",
    external_ctrl = c("selected_ticker", "selected", "panel_settings"),
    class = "share_explorer_block",
    ...
  )
}

#' @importFrom blockr.core block_ui
#' @method block_ui share_explorer_block
#' @export
block_ui.share_explorer_block <- function(id, x, ...) {
  shiny::tagList()
}

#' @importFrom blockr.core block_output
#' @method block_output share_explorer_block
#' @export
block_output.share_explorer_block <- function(x, result, session) {
  shiny::renderUI(NULL)
}
