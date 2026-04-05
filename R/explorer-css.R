#' Share Explorer CSS
#' @return shiny tags$style element
#' @noRd
se_explorer_css <- function() {
  shiny::tags$style(shiny::HTML('
.se-layout {
  display: flex; min-height: 500px;
  background: #ffffff; font-family: "Open Sans", system-ui, sans-serif;
  position: relative; overflow: hidden;
  margin: -16px -16px -10px -16px;
  width: calc(100% + 32px);
}
.se-sidebar {
  width: 300px; flex-shrink: 0; border-right: 1px solid #e5e7eb;
  display: flex; flex-direction: column; background: #f9fafb;
  transition: width 0.25s, opacity 0.2s; overflow: hidden;
}
.se-sidebar.collapsed { width: 0; opacity: 0; pointer-events: none; }
.se-sidebar-header {
  display: flex; align-items: center; justify-content: space-between;
  padding: 12px 16px 8px; border-bottom: 1px solid #e5e7eb;
}
.se-sidebar-title { font-size: 15px; font-weight: 600; color: #111827; margin: 0; }
.se-pin-btn {
  background: none; border: none; color: #9ca3af; cursor: pointer;
  padding: 4px; border-radius: 4px; display: flex; align-items: center;
}
.se-pin-btn:hover { color: #6b7280; background: #f3f4f6; }
.se-expand-btn {
  position: absolute; left: 0; top: 50%; transform: translateY(-50%);
  background: #f9fafb; border: 1px solid #e5e7eb; border-left: none;
  border-radius: 0 6px 6px 0; padding: 8px 6px; cursor: pointer;
  color: #6b7280; display: none; z-index: 10;
}
.se-layout.sidebar-collapsed .se-expand-btn { display: flex; }
.se-ticker-section { padding: 12px 16px; border-bottom: 1px solid #e5e7eb; }
.se-ticker-select {
  width: 100%; padding: 8px 10px; border: 1px solid #d1d5db;
  border-radius: 6px; font-size: 14px; font-family: inherit;
  background: #fff; color: #111827; cursor: pointer;
}
.se-ticker-select:focus { outline: none; border-color: #3b82f6;
  box-shadow: 0 0 0 2px rgba(59,130,246,0.15); }
.se-ticker-card {
  margin-top: 10px; padding: 10px 12px; background: #fff;
  border: 1px solid #e5e7eb; border-radius: 8px;
}
.se-ticker-name { font-size: 14px; font-weight: 600; color: #111827; }
.se-ticker-price { font-size: 20px; font-weight: 600; color: #111827;
  margin-top: 4px; }
.se-ticker-change { font-size: 12px; margin-top: 2px; }
.se-ticker-change.positive { color: #059669; }
.se-ticker-change.negative { color: #dc2626; }
.se-ticker-meta { font-size: 11px; color: #6b7280; margin-top: 4px; }
.se-sidebar-divider { height: 1px; background: #e5e7eb; margin: 0 16px; }
.se-sidebar-search { padding: 8px 16px; }
.se-sidebar-search-wrapper {
  position: relative; display: flex; align-items: center;
}
.se-sidebar-search-icon {
  position: absolute; left: 10px; color: #9ca3af;
  display: flex; pointer-events: none;
}
.se-sidebar-search-input {
  width: 100%; padding: 7px 10px 7px 32px; border: 1px solid #d1d5db;
  border-radius: 6px; font-size: 13px; font-family: inherit;
  background: #fff; color: #111827;
}
.se-sidebar-search-input:focus { outline: none; border-color: #3b82f6; }
.se-sidebar-content { flex: 1; overflow-y: auto; padding: 8px 16px 16px; }
.se-card {
  display: flex; align-items: center; padding: 8px 10px;
  margin-bottom: 4px; border-radius: 6px; cursor: pointer;
  border: 1px solid transparent; transition: all 0.15s;
}
.se-card:hover { background: #f3f4f6; }
.se-card.is-selected { background: #eff6ff; border-color: #bfdbfe; }
.se-card-main { display: flex; align-items: center; gap: 10px; width: 100%; }
.se-card-icon { flex-shrink: 0; }
.se-card-content { flex: 1; min-width: 0; }
.se-card-title { font-size: 13px; font-weight: 500; color: #111827;
  margin: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.se-card-description { font-size: 11px; color: #6b7280; margin: 2px 0 0; }
.se-card-check { flex-shrink: 0; color: #3b82f6; opacity: 0; }
.se-card.is-selected .se-card-check { opacity: 1; }
.se-active-section, .se-available-section { margin-bottom: 12px; }
.se-section-header { font-size: 10px; font-weight: 600; color: #9ca3af;
  letter-spacing: 1px; margin-bottom: 6px; }
.se-active-list { min-height: 30px; }
.se-active-hint { font-size: 11px; color: #9ca3af; margin-bottom: 6px; }
.se-active-empty { font-size: 12px; color: #9ca3af; padding: 8px 0; }
.is-hidden { display: none !important; }
.se-category-group { margin-bottom: 8px; }
.se-category-header { padding: 4px 0; }
.se-category-header span { font-size: 10px; font-weight: 600;
  color: #9ca3af; letter-spacing: 1px; }
.se-chart-area {
  flex: 1; overflow-y: auto; padding: 16px 20px;
  background: #ffffff; min-width: 0;
}
.se-chart-panel {
  margin-bottom: 16px; border: 1px solid #e5e7eb;
  border-radius: 8px; overflow: hidden;
}
.se-chart-header {
  display: flex; align-items: center; justify-content: space-between;
  padding: 10px 14px; border-bottom: 1px solid #f3f4f6;
  background: #fafafa;
}
.se-chart-title { font-size: 13px; font-weight: 600; color: #111827; }
.se-chart-category { font-size: 10px; color: #9ca3af;
  text-transform: uppercase; letter-spacing: 0.5px; }
.se-chart-controls { display: flex; align-items: center; gap: 12px; }
.se-chart-body { padding: 12px; }
.se-empty-state { display: flex; flex-direction: column;
  align-items: center; justify-content: center;
  height: 300px; color: #9ca3af; }
.se-empty-state-text { font-size: 16px; font-weight: 500; margin: 0; }
.se-empty-state-hint { font-size: 13px; margin-top: 4px; }
.se-ctrl-group { display: flex; align-items: center; gap: 6px; }
.se-ctrl-label { font-size: 11px; color: #6b7280; }
.se-ctrl-toggle {
  position: relative; width: 32px; height: 18px;
  background: #d1d5db; border: none; border-radius: 9px;
  cursor: pointer; transition: background 0.2s;
}
.se-ctrl-toggle.is-on { background: #3b82f6; }
.se-ctrl-toggle-track { display: block; width: 100%; height: 100%; }
.se-ctrl-toggle-thumb {
  position: absolute; top: 2px; left: 2px;
  width: 14px; height: 14px; background: #fff;
  border-radius: 50%; transition: transform 0.2s;
}
.se-ctrl-toggle.is-on .se-ctrl-toggle-thumb { transform: translateX(14px); }
.se-ctrl-radio {
  padding: 3px 8px; border: 1px solid #d1d5db; border-radius: 5px;
  background: #fff; color: #374151; font-size: 11px;
  font-family: inherit; cursor: pointer; transition: all 0.15s;
}
.se-ctrl-radio.is-active { background: #dbeafe; border-color: #93c5fd;
  color: #1d4ed8; }
.se-kpi-grid { display: flex; flex-wrap: wrap; gap: 12px; }
.se-kpi-card {
  flex: 1 1 140px; padding: 12px 14px; background: #f9fafb;
  border: 1px solid #e5e7eb; border-radius: 8px;
}
.se-kpi-label { font-size: 11px; color: #6b7280; font-weight: 500;
  text-transform: uppercase; letter-spacing: 0.3px; }
.se-kpi-value { font-size: 20px; font-weight: 600; margin-top: 4px;
  color: #111827; }
.se-kpi-positive { color: #059669; }
.se-kpi-negative { color: #dc2626; }
.se-kpi-neutral { color: #111827; }
.se-card.is-dragging { opacity: 0.4; }
.se-card.drop-above { box-shadow: inset 0 3px 0 0 #3b82f6; }
.se-card.drop-below { box-shadow: inset 0 -3px 0 0 #3b82f6; }
'))
}
