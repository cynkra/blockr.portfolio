/**
 * TickerTable — searchable table for selecting tickers
 * Follows the blockr.task subtask-table pattern:
 *   vanilla DOM, Shiny InputBinding, server-side search via custom messages.
 *
 * UX: selected tickers shown as removable chips above the search box.
 * Empty search shows selected tickers; typing switches to catalog search.
 */
class TickerTable {
  constructor(el, state) {
    this.el = el;
    this.selected = new Set(state.selected || []);
    this.tickers = state.tickers || []; // [{ticker, name, sector, source}]
    this.searchTerm = '';
    this.sortCol = null;
    this.sortDir = 'none'; // asc | desc | none
    this._debounce = null;
    this._callback = null;
    this._build();
    this._renderChips();
    this._renderRows();
  }

  _build() {
    this.el.innerHTML = '';
    this.el.classList.add('tt-wrap');

    // Chips container (selected tickers)
    this.chipsEl = document.createElement('div');
    this.chipsEl.className = 'tt-chips';
    this.el.appendChild(this.chipsEl);

    // Search row
    const searchRow = document.createElement('div');
    searchRow.className = 'tt-search-row';
    const searchIcon = document.createElement('span');
    searchIcon.className = 'tt-search-icon';
    searchIcon.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></svg>';
    this.searchInput = document.createElement('input');
    this.searchInput.type = 'text';
    this.searchInput.className = 'tt-search-input';
    this.searchInput.placeholder = 'Search 42,000+ tickers...';
    this.searchInput.addEventListener('input', () => this._onSearch());
    searchRow.appendChild(searchIcon);
    searchRow.appendChild(this.searchInput);
    this.el.appendChild(searchRow);

    // Table wrapper (scrollable)
    const wrap = document.createElement('div');
    wrap.className = 'tt-table-wrap';

    const table = document.createElement('table');
    table.className = 'tt-table';

    // Header
    const thead = document.createElement('thead');
    const hr = document.createElement('tr');
    const cols = [
      { key: 'selected', label: '', sortable: false },
      { key: 'ticker', label: 'Ticker', sortable: true },
      { key: 'name', label: 'Name', sortable: true },
      { key: 'sector', label: 'Sector', sortable: true }
    ];
    cols.forEach(col => {
      const th = document.createElement('th');
      th.textContent = col.label;
      if (col.sortable) {
        th.classList.add('tt-sortable');
        th.dataset.col = col.key;
        th.addEventListener('click', () => this._onSort(col.key));
      }
      hr.appendChild(th);
    });
    thead.appendChild(hr);
    table.appendChild(thead);

    this.tbody = document.createElement('tbody');
    table.appendChild(this.tbody);
    wrap.appendChild(table);
    this.el.appendChild(wrap);
  }

  _renderChips() {
    this.chipsEl.innerHTML = '';
    if (this.selected.size === 0) {
      this.chipsEl.style.display = 'none';
      return;
    }
    this.chipsEl.style.display = 'flex';
    this.selected.forEach(ticker => {
      const chip = document.createElement('span');
      chip.className = 'tt-chip';
      chip.dataset.ticker = ticker;

      const label = document.createElement('span');
      label.className = 'tt-chip-label';
      label.textContent = ticker;
      chip.appendChild(label);

      const removeBtn = document.createElement('button');
      removeBtn.className = 'tt-chip-remove';
      removeBtn.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10" fill="currentColor" viewBox="0 0 16 16"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>';
      removeBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._onChipRemove(ticker);
      });
      chip.appendChild(removeBtn);

      this.chipsEl.appendChild(chip);
    });
  }

  _renderRows() {
    this.tbody.innerHTML = '';
    var isSearching = !!this.searchTerm;
    var items;

    if (isSearching) {
      // Search mode: show matching tickers from loaded data
      var q = this.searchTerm.toLowerCase();
      items = this.tickers.filter(function(t) {
        return (t.ticker || '').toLowerCase().includes(q) ||
          (t.name || '').toLowerCase().includes(q) ||
          (t.sector || '').toLowerCase().includes(q);
      });
    } else {
      // Idle mode: show only selected tickers
      items = this.tickers.filter(t => this.selected.has(t.ticker));
    }

    // Sort
    if (this.sortCol && this.sortDir !== 'none') {
      const dir = this.sortDir === 'asc' ? 1 : -1;
      const col = this.sortCol;
      items.sort((a, b) => {
        const va = (a[col] || '').toLowerCase();
        const vb = (b[col] || '').toLowerCase();
        return va < vb ? -dir : va > vb ? dir : 0;
      });
    }

    // In search mode, put selected first
    if (isSearching) {
      items.sort((a, b) => {
        const sa = this.selected.has(a.ticker) ? 0 : 1;
        const sb = this.selected.has(b.ticker) ? 0 : 1;
        return sa - sb;
      });
    }

    if (items.length === 0) {
      const tr = document.createElement('tr');
      const td = document.createElement('td');
      td.colSpan = 4;
      td.className = 'tt-empty';
      td.textContent = isSearching
        ? 'No matches — try a different query'
        : 'No tickers selected — use search to add';
      tr.appendChild(td);
      this.tbody.appendChild(tr);
      return;
    }

    items.forEach(t => {
      const tr = document.createElement('tr');
      const isSel = this.selected.has(t.ticker);
      if (isSel) tr.classList.add('tt-selected');
      tr.dataset.ticker = t.ticker;
      tr.addEventListener('click', () => this._onRowClick(t.ticker));

      // Checkmark
      const tdCheck = document.createElement('td');
      tdCheck.className = 'tt-col-check';
      tdCheck.innerHTML = isSel
        ? '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0z"/></svg>'
        : '';
      tr.appendChild(tdCheck);

      // Ticker
      const tdTicker = document.createElement('td');
      tdTicker.className = 'tt-col-ticker';
      tdTicker.textContent = t.ticker;
      tr.appendChild(tdTicker);

      // Name
      const tdName = document.createElement('td');
      tdName.className = 'tt-col-name';
      tdName.textContent = t.name || '';
      tr.appendChild(tdName);

      // Sector
      const tdSector = document.createElement('td');
      tdSector.className = 'tt-col-sector';
      tdSector.textContent = t.sector || '';
      tr.appendChild(tdSector);

      this.tbody.appendChild(tr);
    });
  }

  _onSearch() {
    this.searchTerm = this.searchInput.value.trim().toLowerCase();
    this._renderRows();

    // Debounced server-side search
    clearTimeout(this._debounce);
    if (this.searchInput.value.trim().length >= 2) {
      this._debounce = setTimeout(() => {
        const id = this.el.id;
        if (id) {
          Shiny.setInputValue(id + '_search',
            this.searchInput.value.trim(), { priority: 'event' });
        }
      }, 400);
    }
  }

  _onSort(col) {
    if (this.sortCol === col) {
      this.sortDir = this.sortDir === 'asc' ? 'desc'
        : this.sortDir === 'desc' ? 'none' : 'asc';
    } else {
      this.sortCol = col;
      this.sortDir = 'asc';
    }
    // Update header indicators
    this.el.querySelectorAll('.tt-sortable').forEach(th => {
      th.classList.remove('tt-sort-asc', 'tt-sort-desc');
      if (th.dataset.col === this.sortCol) {
        if (this.sortDir === 'asc') th.classList.add('tt-sort-asc');
        if (this.sortDir === 'desc') th.classList.add('tt-sort-desc');
      }
    });
    this._renderRows();
  }

  _onRowClick(ticker) {
    if (this.selected.has(ticker)) {
      this.selected.delete(ticker);
    } else {
      this.selected.add(ticker);
    }
    this._renderChips();
    this._renderRows();
    this._submit();
  }

  _onChipRemove(ticker) {
    this.selected.delete(ticker);
    this._renderChips();
    this._renderRows();
    this._submit();
  }

  _submit() {
    if (this._callback) this._callback();
  }

  addTickers(newTickers) {
    const existing = new Set(this.tickers.map(t => t.ticker));
    newTickers.forEach(t => {
      if (!existing.has(t.ticker)) {
        this.tickers.push(t);
        existing.add(t.ticker);
      }
    });
    this._renderRows();
  }

  getSelected() {
    return Array.from(this.selected);
  }

  setState(state) {
    if (state.selected) this.selected = new Set(state.selected);
    if (state.tickers) this.tickers = state.tickers;
    this._renderChips();
    this._renderRows();
  }
}

// --- Shiny InputBinding ---
var tickerTableBinding = new Shiny.InputBinding();
$.extend(tickerTableBinding, {
  find: function(scope) {
    return $(scope).find('.tt-container');
  },
  getValue: function(el) {
    if (!el._tickerTable) return null;
    return el._tickerTable.getSelected();
  },
  setValue: function(el, value) {
    if (!el._tickerTable) return;
    el._tickerTable.setState({ selected: value });
  },
  subscribe: function(el, callback) {
    if (!el._tickerTable) {
      var state = JSON.parse(el.dataset.state || '{}');
      el._tickerTable = new TickerTable(el, state);
    }
    el._tickerTable._callback = callback;
  },
  unsubscribe: function(el) {
    if (el._tickerTable) el._tickerTable._callback = null;
  }
});
Shiny.inputBindings.register(tickerTableBinding, 'blockr.portfolio.tickerTable');

// --- Custom message handler for search results ---
$(function() {
  Shiny.addCustomMessageHandler('ticker-table-results', function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._tickerTable) {
      el._tickerTable.addTickers(msg.tickers);
    }
  });
});
