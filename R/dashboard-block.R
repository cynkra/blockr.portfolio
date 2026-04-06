#' Portfolio Dashboard Block
#'
#' A transform block that renders portfolio analysis panels from a
#' portfolio dm (output of the optimizer block). Pure visualization.
#'
#' @param selected Initial panel IDs to show
#' @param panel_settings Named list of per-panel settings
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `portfolio_dashboard_block`
#' @export
new_portfolio_dashboard_block <- function(
    selected = NULL,
    panel_settings = list(),
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_selected <- shiny::reactiveVal(selected)
          r_panel_settings <- shiny::reactiveVal(panel_settings)

          all_panels <- portfolio_panels()

          # Init default selection
          init_done <- shiny::reactiveVal(FALSE)
          shiny::observe({
            data()
            if (!init_done()) {
              cur <- r_selected()
              if (is.null(cur) || length(cur) == 0) {
                r_selected(pf_default_panels())
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

          # Extract result tables from dm
          r_results <- shiny::reactive({
            dm_obj <- data()
            shiny::req(inherits(dm_obj, "dm"))
            tbls <- dm::dm_get_tables(dm_obj)

            # Required tables
            shiny::req(
              "weights" %in% names(tbls),
              "backtest" %in% names(tbls),
              "metrics" %in% names(tbls)
            )

            # Convert dm tables to the format panels expect
            weights_df <- as.data.frame(tbls[["weights"]])
            bt_df <- as.data.frame(tbls[["backtest"]])
            metrics_df <- as.data.frame(tbls[["metrics"]])
            meta <- if ("metadata" %in% names(tbls))
              as.data.frame(tbls[["metadata"]]) else NULL

            # Weights as named numeric vector
            w <- stats::setNames(weights_df$weight, weights_df$ticker)

            # Backtest as list with xts objects
            bt_dates <- as.Date(bt_df$date)
            backtest_list <- list(
              returns = xts::xts(bt_df$return, order.by = bt_dates),
              cumulative = xts::xts(bt_df$cumulative,
                order.by = bt_dates),
              ann_return = metrics_df$ann_return[1],
              ann_vol = metrics_df$ann_vol[1],
              sharpe = metrics_df$sharpe[1],
              max_dd = metrics_df$max_dd[1],
              var_95 = metrics_df$var_95[1],
              drawdown = xts::xts(bt_df$drawdown, order.by = bt_dates)
            )

            # Frontier
            frontier_data <- NULL
            if ("frontier" %in% names(tbls) &&
                "assets" %in% names(tbls)) {
              frontier_data <- list(
                frontier = as.data.frame(tbls[["frontier"]]),
                assets = as.data.frame(tbls[["assets"]])
              )
            }

            list(
              weights = w,
              backtest = backtest_list,
              benchmark = NULL,
              frontier = frontier_data,
              risk_contrib = if ("risk_contrib" %in% names(tbls))
                as.data.frame(tbls[["risk_contrib"]]) else NULL,
              metadata = meta,
              comparison = NULL,
              strategy = metrics_df$strategy[1],
              returns_xts = NULL
            )
          })

          # Input handlers
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
            if (is.character(new_order) && length(new_order) == 1) {
              new_order <- list(new_order)
            }
            new_order <- as.character(unlist(new_order))
            cur <- r_selected()
            if (setequal(new_order, cur)) {
              r_selected(new_order)
            }
          })

          shiny::observeEvent(input$panel_ctrl, {
            msg <- input$panel_ctrl
            if (is.null(msg)) return()
            panel_id <- msg$panel_id
            param <- msg$param
            value <- msg$value
            if (is.null(panel_id) || is.null(param)) return()
            settings <- r_panel_settings()
            if (is.null(settings[[panel_id]])) {
              settings[[panel_id]] <- list()
            }
            settings[[panel_id]][[param]] <- value
            r_panel_settings(settings)
          })

          # Sync selected panels to client
          shiny::observe({
            sel <- r_selected()
            session$sendCustomMessage(
              session$ns("sync_selected"), sel
            )
          })

          # Sidebar rendering
          output$sidebar_cards <- shiny::renderUI({
            sel <- shiny::isolate(r_selected())
            if (length(all_panels) == 0) return(NULL)

            build_card <- function(p, is_sel) {
              shiny::div(
                class = paste("pd-card",
                  if (is_sel) "is-selected"),
                `data-panel-id` = p$id,
                `data-category` = p$category,
                `data-search-text` = paste(
                  p$label, p$description, p$category),
                shiny::div(class = "pd-card-main",
                  shiny::div(class = "pd-card-icon",
                    shiny::HTML(pf_icon_html(p$icon, p$color))
                  ),
                  shiny::div(class = "pd-card-content",
                    shiny::tags$p(class = "pd-card-title", p$label),
                    shiny::tags$p(class = "pd-card-description",
                      p$description)
                  ),
                  shiny::div(class = "pd-card-check",
                    shiny::HTML(paste0(
                      '<svg xmlns="http://www.w3.org/2000/svg" ',
                      'width="12" height="12" fill="currentColor" ',
                      'viewBox="0 0 16 16">',
                      '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7',
                      'a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-',
                      '.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 ',
                      '0z"/></svg>'
                    ))
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
              cpanels <- Filter(
                function(p) p$category == cat, unsel_panels)
              if (length(cpanels) == 0) return(NULL)
              shiny::div(
                class = "pd-category-group",
                `data-category` = cat,
                shiny::div(class = "pd-category-header",
                  shiny::tags$span(toupper(cat))
                ),
                lapply(cpanels, function(p) {
                  build_card(p, is_sel = FALSE)
                })
              )
            })
            category_groups <- Filter(Negate(is.null), category_groups)

            shiny::tagList(
              shiny::div(class = "pd-active-section",
                shiny::div(class = "pd-section-header", "SELECTED"),
                shiny::div(
                  class = paste("pd-active-hint",
                    if (length(active_cards) < 2) "is-hidden"),
                  "Drag to reorder"
                ),
                shiny::div(class = "pd-active-list",
                  active_cards,
                  shiny::div(
                    class = paste("pd-active-empty",
                      if (length(active_cards) > 0) "is-hidden"),
                    "Click a card below to add it here"
                  )
                )
              ),
              shiny::div(class = "pd-available-section",
                shiny::div(
                  class = "pd-section-header pd-section-header-available",
                  "AVAILABLE"
                ),
                category_groups
              )
            )
          })

          # Per-panel controls builder
          pd_controls_ui <- function(panel, panel_id, settings) {
            controls <- panel$controls
            if (is.null(controls) || length(controls) == 0) {
              return(NULL)
            }

            tags <- lapply(names(controls), function(param) {
              ctrl <- controls[[param]]
              cur_val <- settings[[param]] %||% ctrl$default

              if (ctrl$type == "toggle") {
                is_on <- isTRUE(cur_val)
                shiny::div(class = "pd-ctrl-group",
                  shiny::span(class = "pd-ctrl-label", ctrl$label),
                  shiny::tags$button(
                    class = paste("pd-ctrl-toggle",
                      if (is_on) "is-on"),
                    `data-panel-id` = panel_id,
                    `data-param` = param,
                    shiny::span(class = "pd-ctrl-toggle-track",
                      shiny::span(class = "pd-ctrl-toggle-thumb")
                    )
                  )
                )
              } else if (ctrl$type == "radio") {
                choices <- ctrl$choices
                if (is.null(cur_val)) cur_val <- choices[1]
                choice_names <- names(choices) %||% choices
                btns <- lapply(seq_along(choices), function(ci) {
                  is_active <- choices[ci] == cur_val
                  shiny::tags$button(
                    class = paste("pd-ctrl-radio",
                      if (is_active) "is-active"),
                    `data-panel-id` = panel_id,
                    `data-param` = param,
                    `data-value` = choices[ci],
                    choice_names[ci]
                  )
                })
                shiny::div(class = "pd-ctrl-group",
                  shiny::span(class = "pd-ctrl-label", ctrl$label),
                  shiny::div(class = "pd-ctrl-radios", btns)
                )
              } else {
                NULL
              }
            })
            tags <- Filter(Negate(is.null), tags)
            if (length(tags) == 0) return(NULL)
            shiny::div(class = "pd-chart-controls", tags)
          }

          # Chart area rendering
          output$chart_area <- shiny::renderUI({
            sel <- r_selected()
            all_settings <- r_panel_settings()

            active_ids <- intersect(sel, names(all_panels))
            if (length(active_ids) == 0) {
              return(shiny::div(class = "pd-empty-state",
                shiny::p(class = "pd-empty-state-text",
                  "No panels selected"),
                shiny::p(class = "pd-empty-state-hint",
                  "Click cards in the sidebar to add charts")
              ))
            }

            results <- tryCatch(r_results(), error = function(e) NULL)
            if (is.null(results)) {
              return(shiny::div(class = "pd-empty-state",
                shiny::p(class = "pd-empty-state-text",
                  "Computing portfolio..."),
                shiny::p(class = "pd-empty-state-hint",
                  "Optimization in progress")
              ))
            }

            chart_tags <- lapply(active_ids, function(panel_id) {
              panel <- all_panels[[panel_id]]
              psettings <- all_settings[[panel_id]] %||% list()

              chart <- tryCatch(
                panel$render(results, psettings),
                error = function(e) pf_empty_chart(
                  paste("Error:", conditionMessage(e))
                )
              )

              controls_ui <- pd_controls_ui(panel, panel_id, psettings)

              shiny::div(class = "pd-chart-panel",
                shiny::div(class = "pd-chart-header",
                  shiny::div(class = "pd-chart-title", panel$label),
                  controls_ui,
                  shiny::div(class = "pd-chart-category",
                    panel$category)
                ),
                shiny::div(class = "pd-chart-body", chart)
              )
            })

            shiny::tagList(chart_tags)
          })

          # Return
          list(
            expr = shiny::reactive({
              quote(identity(data))
            }),
            state = list(
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
        shiny::tags$head(
          shiny::tags$link(
            rel = "stylesheet",
            href = paste0(
              "https://fonts.googleapis.com/css2?",
              "family=Open+Sans:wght@400;500;600&display=swap"
            )
          )
        ),
        pf_dashboard_css(),
        shiny::div(
          class = "pd-layout", id = ns("pd_layout"),

          # Left sidebar (panel picker only)
          shiny::div(
            class = "pd-sidebar", id = ns("pd_sidebar"),

            shiny::div(class = "pd-sidebar-header",
              shiny::tags$h3(class = "pd-sidebar-title",
                "Portfolio Dashboard"),
              shiny::tags$button(
                class = "pd-pin-btn", id = ns("pin_btn"),
                title = "Toggle sidebar",
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
                  '.146-.354z"/></svg>'
                ))
              )
            ),

            # Search
            shiny::div(class = "pd-sidebar-search",
              shiny::div(class = "pd-sidebar-search-wrapper",
                shiny::span(class = "pd-sidebar-search-icon",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                    'height="16" fill="currentColor" viewBox="0 0 16 16">',
                    '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h',
                    '-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-',
                    '1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 ',
                    '5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></svg>'
                  ))
                ),
                shiny::tags$input(
                  type = "text",
                  class = "pd-sidebar-search-input",
                  id = ns("search"),
                  placeholder = "Search panels..."
                )
              )
            ),

            # Panel cards
            shiny::div(class = "pd-sidebar-content",
              shiny::uiOutput(ns("sidebar_cards"))
            )
          ),

          # Expand button
          shiny::tags$button(
            class = "pd-expand-btn", id = ns("expand_btn"),
            title = "Show sidebar",
            shiny::HTML(paste0(
              '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
              'height="16" fill="currentColor" viewBox="0 0 16 16">',
              '<path fill-rule="evenodd" d="M1 8a.5.5 0 0 1 .5-.5h11',
              '.793l-3.147-3.146a.5.5 0 0 1 .708-.708l4 4a.5.5 0 0 ',
              '1 0 .708l-4 4a.5.5 0 0 1-.708-.708L13.293 8.5H1.5A.5',
              '.5 0 0 1 1 8z"/></svg>'
            ))
          ),

          # Chart area
          shiny::div(class = "pd-chart-area",
            shiny::uiOutput(ns("chart_area"))
          )
        ),

        # Client-side JS (same as advisor but with pd- prefix)
        shiny::tags$script(shiny::HTML(paste0("
          $(function() {
            var layoutId = '", ns("pd_layout"), "';
            var sidebarId = '", ns("pd_sidebar"), "';
            var searchId = '", ns("search"), "';
            var pinBtnId = '", ns("pin_btn"), "';
            var expandBtnId = '", ns("expand_btn"), "';
            var toggleInputId = '", ns("toggle_panel"), "';
            var panelCtrlInputId = '", ns("panel_ctrl"), "';
            var syncMsgId = '", ns("sync_selected"), "';
            var reorderInputId = '", ns("reorder_panel"), "';

            var dragActive = false;

            $(document).on('click', '#' + layoutId + ' .pd-card', function(e) {
              if (dragActive) return;
              var panelId = $(this).data('panel-id');
              if (!panelId) return;
              Shiny.setInputValue(toggleInputId, panelId, {priority: 'event'});
            });

            $(document).on('click', '#' + layoutId + ' .pd-ctrl-toggle', function(e) {
              e.stopPropagation();
              $(this).toggleClass('is-on');
              Shiny.setInputValue(panelCtrlInputId, {
                panel_id: $(this).data('panel-id'),
                param: $(this).data('param'),
                value: $(this).hasClass('is-on')
              }, {priority: 'event'});
            });

            $(document).on('click', '#' + layoutId + ' .pd-ctrl-radio', function(e) {
              e.stopPropagation();
              $(this).siblings('.pd-ctrl-radio').removeClass('is-active');
              $(this).addClass('is-active');
              Shiny.setInputValue(panelCtrlInputId, {
                panel_id: $(this).data('panel-id'),
                param: $(this).data('param'),
                value: $(this).data('value')
              }, {priority: 'event'});
            });

            $(document).on('input', '#' + searchId, function() {
              var query = $(this).val().toLowerCase();
              var sidebar = $(this).closest('.pd-sidebar');
              sidebar.find('.pd-card').each(function() {
                var text = ($(this).data('search-text') || '').toLowerCase();
                $(this).toggle(!query || text.indexOf(query) >= 0);
              });
              sidebar.find('.pd-category-group').each(function() {
                $(this).toggle($(this).find('.pd-card:visible').length > 0);
              });
            });

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

            Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
              if (!selected) selected = [];
              if (typeof selected === 'string') selected = [selected];
              var $layout = $('#' + layoutId);
              var $activeList = $layout.find('.pd-active-list');
              var $availSection = $layout.find('.pd-available-section');
              var cardMap = {};
              $layout.find('.pd-card').each(function() {
                var pid = $(this).data('panel-id');
                if (pid) cardMap[pid] = $(this);
              });
              var $hint = $activeList.find('.pd-active-empty');
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
                var $group = $availSection
                  .find('.pd-category-group[data-category=' + JSON.stringify(cat) + ']');
                if (!$group.length) {
                  $group = $('<div class=pd-category-group data-category=' +
                    JSON.stringify(cat) + '>' +
                    '<div class=pd-category-header><span>' +
                    cat.toUpperCase() + '</span></div></div>');
                  $availSection.append($group);
                }
                $group.append($card);
              });
              $hint.toggleClass('is-hidden', selected.length > 0);
              $layout.find('.pd-active-hint').toggleClass('is-hidden', selected.length < 2);
              $availSection.find('.pd-category-group').each(function() {
                $(this).toggle($(this).find('.pd-card').length > 0);
              });
            });

            // Drag and Drop
            var $doc = $(document);
            $doc.on('dragstart', '#' + layoutId + ' .pd-active-list .pd-card', function(e) {
              dragActive = true;
              $(this).addClass('is-dragging');
              e.originalEvent.dataTransfer.effectAllowed = 'move';
              e.originalEvent.dataTransfer.setData('text/plain', $(this).data('panel-id'));
            });
            $doc.on('dragover', '#' + layoutId + ' .pd-active-list .pd-card', function(e) {
              if (!dragActive) return;
              e.preventDefault();
              var rect = this.getBoundingClientRect();
              var midY = rect.top + rect.height / 2;
              if (e.originalEvent.clientY < midY) {
                $(this).addClass('drop-above').removeClass('drop-below');
              } else {
                $(this).addClass('drop-below').removeClass('drop-above');
              }
            });
            $doc.on('dragleave', '#' + layoutId + ' .pd-active-list .pd-card', function() {
              $(this).removeClass('drop-above drop-below');
            });
            $doc.on('drop', '#' + layoutId + ' .pd-active-list .pd-card', function(e) {
              e.preventDefault();
              var draggedId = e.originalEvent.dataTransfer.getData('text/plain');
              var $target = $(this);
              $target.removeClass('drop-above drop-below');
              if (draggedId === $target.data('panel-id')) return;
              var $dragged = $target.closest('.pd-active-list')
                .find('.pd-card[data-panel-id=' + JSON.stringify(draggedId) + ']');
              if (!$dragged.length) return;
              var rect = this.getBoundingClientRect();
              if (e.originalEvent.clientY < rect.top + rect.height / 2) {
                $dragged.insertBefore($target);
              } else {
                $dragged.insertAfter($target);
              }
              var newOrder = [];
              $dragged.closest('.pd-active-list').find('.pd-card').each(function() {
                newOrder.push($(this).data('panel-id'));
              });
              Shiny.setInputValue(reorderInputId, newOrder, {priority: 'event'});
            });
            $doc.on('dragend', '#' + layoutId + ' .pd-active-list .pd-card', function() {
              $(this).removeClass('is-dragging');
              $(this).closest('.pd-active-list').find('.pd-card').removeClass('drop-above drop-below');
              setTimeout(function() { dragActive = false; }, 0);
            });
            $doc.on('dragover', '#' + layoutId + ' .pd-active-list', function(e) {
              if (!dragActive) return;
              e.preventDefault();
            });
          });
        ")))
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
      tbls <- dm::dm_get_tables(data)
      if (!"weights" %in% names(tbls)) {
        stop("dm must contain 'weights' table (from optimizer)")
      }
    },
    allow_empty_state = "panel_settings",
    external_ctrl = c("selected", "panel_settings"),
    class = c("portfolio_dashboard_block", "dm_block"),
    ...
  )
}

# -- S3 methods ----------------------------------------------------------------

#' @importFrom blockr.core block_ui
#' @method block_ui portfolio_dashboard_block
#' @export
block_ui.portfolio_dashboard_block <- function(id, x, ...) {
  shiny::tagList()
}

#' @importFrom blockr.core block_output
#' @method block_output portfolio_dashboard_block
#' @export
block_output.portfolio_dashboard_block <- function(x, result, session) {
  shiny::renderUI(NULL)
}
