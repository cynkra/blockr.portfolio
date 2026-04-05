#' Portfolio Dashboard CSS
#'
#' Returns inline CSS for the portfolio dashboard block.
#' Uses pd- prefix classes following the patient-profile pattern.
#'
#' @return shiny tags$style element
#' @noRd
pf_dashboard_css <- function() {
  shiny::tags$style(shiny::HTML('
/* ============================================
   Portfolio Dashboard Block -- Sidebar + Layout
   ============================================ */

/* --- Layout -------------------------------- */
.pd-layout {
  display: flex;
  width: 100%;
  min-height: 500px;
  background: #ffffff;
  border-radius: 0;
  overflow: hidden;
  font-family: "Open Sans", system-ui, -apple-system, sans-serif;
  font-size: 14px;
  color: #111827;
  margin: -16px -16px -10px -16px;
  width: calc(100% + 32px);
}

/* --- Sidebar ------------------------------- */
.pd-sidebar {
  width: 320px;
  min-width: 320px;
  flex-shrink: 0;
  display: flex;
  flex-direction: column;
  background: #ffffff;
  border-right: 1px solid #e5e7eb;
  transition: width 0.25s ease, min-width 0.25s ease, opacity 0.25s ease;
  overflow: hidden;
}

.pd-sidebar.collapsed {
  width: 0;
  min-width: 0;
  border-right: none;
  opacity: 0;
  pointer-events: none;
}

/* --- Sidebar Header ------------------------ */
.pd-sidebar-header {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 16px;
  border-bottom: 1px solid #e5e7eb;
  flex-shrink: 0;
  background: #ffffff;
}

.pd-sidebar-title {
  font-size: 15px;
  font-weight: 600;
  color: #111827;
  margin: 0;
  white-space: nowrap;
}

.pd-pin-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 30px;
  height: 30px;
  padding: 0;
  margin-left: auto;
  background: transparent;
  border: none;
  border-radius: 6px;
  color: #9ca3af;
  cursor: pointer;
  transition: background-color 0.15s, color 0.15s;
  flex-shrink: 0;
}
.pd-pin-btn:hover { background: #f3f4f6; color: #374151; }
.pd-pin-btn.is-unpinned { color: #3b82f6; }

/* --- Advisor Controls Section -------------- */
.pd-advisor-controls {
  padding: 12px 16px;
  border-bottom: 1px solid #e5e7eb;
  overflow-y: auto;
  max-height: 420px;
  flex-shrink: 0;
}

.pd-advisor-group {
  margin-bottom: 12px;
}
.pd-advisor-group:last-child { margin-bottom: 0; }

.pd-advisor-label {
  font-size: 10px;
  font-weight: 600;
  color: #9ca3af;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 6px;
}

/* Radio chips for advisor controls */
.pd-advisor-radios {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
}

.pd-advisor-radio {
  display: inline-flex;
  align-items: center;
  padding: 4px 10px;
  font-size: 12px;
  font-family: inherit;
  font-weight: 400;
  color: #6b7280;
  background: #f3f4f6;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.12s ease;
  white-space: nowrap;
}
.pd-advisor-radio:hover { border-color: #d1d5db; background: #e5e7eb; }
.pd-advisor-radio.is-active {
  color: #1d4ed8;
  background: #dbeafe;
  border-color: #93c5fd;
  font-weight: 500;
}

/* Compare multi-select chips */
.pd-compare-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
}

.pd-compare-chip {
  display: inline-flex;
  align-items: center;
  padding: 4px 10px;
  font-size: 12px;
  font-family: inherit;
  font-weight: 400;
  color: #6b7280;
  background: #f3f4f6;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.12s ease;
  white-space: nowrap;
}
.pd-compare-chip:hover { border-color: #d1d5db; background: #e5e7eb; }
.pd-compare-chip.is-active {
  color: #0369a1;
  background: #e0f2fe;
  border-color: #7dd3fc;
  font-weight: 500;
}

/* Numeric inputs */
.pd-advisor-numeric {
  width: 80px;
  padding: 4px 8px;
  font-size: 12px;
  font-family: inherit;
  background: #f9fafb;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  color: #111827;
}
.pd-advisor-numeric:focus {
  outline: none;
  border-color: #3b82f6;
  box-shadow: 0 0 0 3px #dbeafe;
}

/* Reset button */
.pd-reset-btn {
  display: inline-flex;
  align-items: center;
  padding: 4px 12px;
  font-size: 11px;
  font-family: inherit;
  color: #6b7280;
  background: transparent;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.12s ease;
}
.pd-reset-btn:hover { background: #f3f4f6; color: #374151; }

/* --- Sidebar Divider ----------------------- */
.pd-sidebar-divider {
  border-top: 1px solid #e5e7eb;
  margin: 0;
}

/* --- Search -------------------------------- */
.pd-sidebar-search {
  padding: 12px 16px;
  border-bottom: 1px solid #e5e7eb;
  flex-shrink: 0;
}

.pd-sidebar-search-wrapper { position: relative; }

.pd-sidebar-search-icon {
  position: absolute;
  left: 12px;
  top: 50%;
  transform: translateY(-50%);
  color: #9ca3af;
  pointer-events: none;
}

.pd-sidebar-search-input {
  width: 100%;
  padding: 9px 12px 9px 38px;
  font-size: 13px;
  font-family: inherit;
  background: #f9fafb;
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  color: #111827;
  transition: border-color 0.15s ease, box-shadow 0.15s ease;
  box-sizing: border-box;
}
.pd-sidebar-search-input:focus {
  outline: none;
  border-color: #3b82f6;
  box-shadow: 0 0 0 3px #dbeafe;
}
.pd-sidebar-search-input::placeholder { color: #9ca3af; }

/* --- Sidebar Content (panel cards) --------- */
.pd-sidebar-content {
  flex: 1;
  overflow-y: auto;
  padding: 8px 12px 12px;
}

/* --- Section Headers ----------------------- */
.pd-section-header {
  font-size: 10px;
  font-weight: 600;
  color: #9ca3af;
  text-transform: uppercase;
  letter-spacing: 0.6px;
  padding: 8px 4px 6px;
}
.pd-section-header-available {
  border-top: 1px solid #e5e7eb;
  margin-top: 8px;
  padding-top: 12px;
}

/* --- Active List (drop target) ------------- */
.pd-active-list { min-height: 32px; }

.pd-active-hint {
  font-size: 10px;
  color: #d1d5db;
  padding: 2px 4px 6px;
  letter-spacing: 0.3px;
}
.pd-active-hint.is-hidden { display: none; }

.pd-active-empty {
  font-size: 12px;
  font-style: italic;
  color: #d1d5db;
  padding: 8px 4px;
}
.pd-active-empty.is-hidden { display: none; }

/* Drag handle for active cards */
.pd-active-list .pd-card-main::before {
  content: "\\2261";
  font-size: 16px;
  color: #d1d5db;
  cursor: grab;
  flex-shrink: 0;
  width: 16px;
  text-align: center;
  line-height: 1;
  transition: color 0.15s;
}
.pd-active-list .pd-card:hover .pd-card-main::before { color: #9ca3af; }

/* Drag states */
.pd-card.is-dragging { opacity: 0.4; }
.pd-card.drop-above {
  box-shadow: inset 0 3px 0 0 #3b82f6;
  border-top-color: #3b82f6;
}
.pd-card.drop-below {
  box-shadow: inset 0 -3px 0 0 #3b82f6;
  border-bottom-color: #3b82f6;
}

/* --- Category Headers ---------------------- */
.pd-category-group { margin-bottom: 4px; }
.pd-category-header { padding: 8px 4px 6px; margin-top: 4px; }
.pd-category-group:first-child .pd-category-header { margin-top: 0; }
.pd-category-header span {
  font-size: 11px;
  font-weight: 500;
  color: #6b7280;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* --- Cards --------------------------------- */
.pd-card {
  position: relative;
  display: flex;
  flex-direction: column;
  margin-bottom: 4px;
  border-radius: 8px;
  border: 1px solid transparent;
  background: transparent;
  transition: all 150ms ease;
  cursor: pointer;
  user-select: none;
}
.pd-card:hover {
  border-color: #e5e7eb;
  background: rgba(249, 250, 251, 0.6);
}
.pd-card.is-selected {
  border-color: #3b82f6;
  background: rgba(59, 130, 246, 0.04);
}

.pd-card-main {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 44px 10px 10px;
  min-width: 0;
}

.pd-card-icon {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 40px;
  height: 40px;
  flex-shrink: 0;
}

.pd-card-content { flex: 1; min-width: 0; }
.pd-card-title {
  font-weight: 500;
  font-size: 13px;
  color: #111827;
  margin: 0;
  line-height: 1.4;
}
.pd-card-description {
  font-size: 11px;
  color: #6b7280;
  margin: 2px 0 0 0;
  line-height: 1.4;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

/* --- Card Check Circle --------------------- */
.pd-card-check {
  position: absolute;
  right: 10px;
  top: 50%;
  transform: translateY(-50%);
  width: 22px;
  height: 22px;
  border-radius: 50%;
  background: #e5e7eb;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.15s ease;
}
.pd-card-check svg { width: 12px; height: 12px; color: transparent; transition: color 0.15s ease; }
.pd-card.is-selected .pd-card-check { background: #3b82f6; }
.pd-card.is-selected .pd-card-check svg { color: #ffffff; }

/* --- Chart Area ---------------------------- */
.pd-chart-area {
  flex: 1;
  min-width: 0;
  overflow-y: auto;
  padding: 16px 20px;
  background: #f9fafb;
}

/* --- Chart Panels -------------------------- */
.pd-chart-panel {
  margin-bottom: 16px;
  background: #ffffff;
  border-radius: 8px;
  border: 1px solid #e5e7eb;
  overflow: hidden;
}
.pd-chart-panel:last-child { margin-bottom: 0; }

.pd-chart-header {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 10px 16px;
  border-bottom: 1px solid #f3f4f6;
}

.pd-chart-title {
  font-size: 12px;
  font-weight: 500;
  color: #374151;
  margin: 0;
}

.pd-chart-category {
  font-size: 11px;
  color: #9ca3af;
  margin-left: auto;
}

.pd-chart-body { padding: 0; }

/* --- Chart Controls Toolbar ---------------- */
.pd-chart-controls {
  display: flex;
  align-items: center;
  gap: 12px;
  flex: 1;
  min-width: 0;
  overflow-x: auto;
  padding: 0 4px;
}

.pd-ctrl-group {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-shrink: 0;
}

.pd-ctrl-label {
  font-size: 10px;
  font-weight: 500;
  color: #9ca3af;
  text-transform: uppercase;
  letter-spacing: 0.3px;
  white-space: nowrap;
}

/* Toggle control */
.pd-ctrl-toggle {
  display: inline-flex;
  align-items: center;
  padding: 0;
  background: none;
  border: none;
  cursor: pointer;
}
.pd-ctrl-toggle-track {
  display: inline-flex;
  align-items: center;
  width: 28px;
  height: 16px;
  background: #d1d5db;
  border-radius: 8px;
  padding: 2px;
  transition: background 0.15s ease;
}
.pd-ctrl-toggle.is-on .pd-ctrl-toggle-track { background: #3b82f6; }
.pd-ctrl-toggle-thumb {
  display: block;
  width: 12px;
  height: 12px;
  background: #ffffff;
  border-radius: 50%;
  transition: transform 0.15s ease;
  box-shadow: 0 1px 2px rgba(0,0,0,0.15);
}
.pd-ctrl-toggle.is-on .pd-ctrl-toggle-thumb { transform: translateX(12px); }

/* Radio controls for panels */
.pd-ctrl-radios {
  display: flex;
  align-items: center;
  gap: 0;
  border: 1px solid #e5e7eb;
  border-radius: 4px;
  overflow: hidden;
}
.pd-ctrl-radio {
  display: inline-flex;
  align-items: center;
  padding: 2px 8px;
  font-size: 11px;
  font-family: inherit;
  font-weight: 400;
  color: #6b7280;
  background: #f9fafb;
  border: none;
  border-right: 1px solid #e5e7eb;
  cursor: pointer;
  transition: all 0.12s ease;
  white-space: nowrap;
  line-height: 1.4;
}
.pd-ctrl-radio:last-child { border-right: none; }
.pd-ctrl-radio:hover { background: #f3f4f6; }
.pd-ctrl-radio.is-active {
  color: #1d4ed8;
  background: #dbeafe;
  font-weight: 500;
}

/* --- KPI Grid ------------------------------ */
.pd-kpi-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  padding: 16px;
}

.pd-kpi-card {
  flex: 1;
  min-width: 120px;
  max-width: 200px;
  padding: 12px 16px;
  background: #f9fafb;
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  text-align: center;
}

.pd-kpi-label {
  font-size: 10px;
  font-weight: 600;
  color: #9ca3af;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 4px;
}

.pd-kpi-value {
  font-size: 22px;
  font-weight: 600;
  color: #111827;
  line-height: 1.3;
}
.pd-kpi-positive { color: #059669; }
.pd-kpi-negative { color: #dc2626; }
.pd-kpi-neutral { color: #111827; }

.pd-kpi-benchmark {
  font-size: 11px;
  color: #9ca3af;
  margin-top: 2px;
}

/* --- Comparison Table ---------------------- */
.pd-compare-container { padding: 16px; }
.pd-compare-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 13px;
}
.pd-compare-table th, .pd-compare-table td {
  padding: 8px 12px;
  text-align: left;
  border-bottom: 1px solid #e5e7eb;
}
.pd-compare-table th {
  font-weight: 600;
  color: #374151;
  background: #f9fafb;
  font-size: 12px;
}
.pd-compare-table td { color: #111827; }

/* --- Empty state --------------------------- */
.pd-empty-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 60px 24px;
  text-align: center;
  color: #9ca3af;
}
.pd-empty-state-text {
  font-size: 14px;
  color: #6b7280;
  margin: 0 0 4px;
}
.pd-empty-state-hint {
  font-size: 12px;
  color: #9ca3af;
  margin: 0;
}

/* --- Expand button (when sidebar collapsed) */
.pd-expand-btn {
  position: absolute;
  left: 8px;
  top: 8px;
  z-index: 2;
  display: none;
  align-items: center;
  justify-content: center;
  width: 32px;
  height: 32px;
  padding: 0;
  background: #ffffff;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  color: #6b7280;
  cursor: pointer;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  transition: background-color 0.15s, color 0.15s;
}
.pd-expand-btn:hover { background: #f9fafb; color: #111827; }
.pd-layout.sidebar-collapsed .pd-expand-btn { display: flex; }

/* --- Scrollbar styling --------------------- */
.pd-sidebar-content::-webkit-scrollbar,
.pd-chart-area::-webkit-scrollbar,
.pd-advisor-controls::-webkit-scrollbar { width: 6px; }

.pd-sidebar-content::-webkit-scrollbar-track,
.pd-chart-area::-webkit-scrollbar-track,
.pd-advisor-controls::-webkit-scrollbar-track { background: transparent; }

.pd-sidebar-content::-webkit-scrollbar-thumb,
.pd-chart-area::-webkit-scrollbar-thumb,
.pd-advisor-controls::-webkit-scrollbar-thumb {
  background: #d1d5db;
  border-radius: 3px;
}

.pd-sidebar-content::-webkit-scrollbar-thumb:hover,
.pd-chart-area::-webkit-scrollbar-thumb:hover,
.pd-advisor-controls::-webkit-scrollbar-thumb:hover { background: #9ca3af; }
  '))
}
