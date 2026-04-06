/**
 * PositionLimits — search + add per-ticker max weight constraints.
 * Shows only constrained positions in a compact table.
 * Search to find ETFs, click to add, set max %, click × to remove.
 */
class PositionLimits {
  constructor(el, state) {
    this.el = el;
    this.limits = state.limits || {};
    this.allTickers = state.tickers || [];
    this._callback = null;
    this._searchResults = [];
    this._debounce = null;
    this._build();
  }

  _build() {
    this.el.innerHTML = '';
    this.el.classList.add('pl-wrap');

    // Header
    var header = document.createElement('div');
    header.className = 'pl-header';
    header.innerHTML = '<span class="pl-title">Position Limits</span>';
    this.el.appendChild(header);

    // Search row
    var searchRow = document.createElement('div');
    searchRow.className = 'pl-search-row';
    this.searchInput = document.createElement('input');
    this.searchInput.type = 'text';
    this.searchInput.className = 'pl-search-input';
    this.searchInput.placeholder = 'Search ETF to add limit...';
    var self = this;
    this.searchInput.addEventListener('input', function() {
      self._onSearch();
    });
    this.searchInput.addEventListener('focus', function() {
      if (self.searchInput.value.length > 0) self._onSearch();
    });
    searchRow.appendChild(this.searchInput);

    // Search dropdown
    this.dropdown = document.createElement('div');
    this.dropdown.className = 'pl-dropdown pl-hidden';
    searchRow.appendChild(this.dropdown);
    this.el.appendChild(searchRow);

    // Close dropdown on outside click
    document.addEventListener('click', function(e) {
      if (!searchRow.contains(e.target)) {
        self.dropdown.classList.add('pl-hidden');
      }
    });

    // Constrained positions table
    this.tableWrap = document.createElement('div');
    this.tableWrap.className = 'pl-table-wrap';
    this.el.appendChild(this.tableWrap);

    this._renderTable();
  }

  _onSearch() {
    var query = this.searchInput.value.trim().toLowerCase();
    if (query.length < 1) {
      this.dropdown.classList.add('pl-hidden');
      return;
    }

    // Filter allTickers, exclude already-constrained
    var self = this;
    var results = this.allTickers.filter(function(t) {
      if (self.limits[t.ticker] !== undefined) return false;
      return t.ticker.toLowerCase().indexOf(query) >= 0 ||
        (t.name || '').toLowerCase().indexOf(query) >= 0 ||
        (t.region || '').toLowerCase().indexOf(query) >= 0;
    }).slice(0, 8);

    this.dropdown.innerHTML = '';
    if (results.length === 0) {
      this.dropdown.innerHTML = '<div class="pl-dropdown-empty">No matches</div>';
    } else {
      results.forEach(function(t) {
        var item = document.createElement('div');
        item.className = 'pl-dropdown-item';
        item.innerHTML = '<span class="pl-dd-ticker">' + t.ticker +
          '</span> <span class="pl-dd-name">' + (t.name || '') +
          '</span> <span class="pl-dd-region">' + (t.region || '') + '</span>';
        item.addEventListener('click', function(e) {
          e.stopPropagation();
          self.limits[t.ticker] = 0.10; // default 10%
          self.searchInput.value = '';
          self.dropdown.classList.add('pl-hidden');
          self._renderTable();
          self._submit();
        });
        self.dropdown.appendChild(item);
      });
    }
    this.dropdown.classList.remove('pl-hidden');
  }

  _renderTable() {
    this.tableWrap.innerHTML = '';
    var keys = Object.keys(this.limits);

    if (keys.length === 0) {
      var empty = document.createElement('div');
      empty.className = 'pl-empty';
      empty.textContent = 'No position limits set';
      this.tableWrap.appendChild(empty);
      return;
    }

    var table = document.createElement('table');
    table.className = 'pl-table';
    var thead = document.createElement('thead');
    thead.innerHTML = '<tr><th>Ticker</th><th>Name</th><th>Max %</th><th></th></tr>';
    table.appendChild(thead);

    var tbody = document.createElement('tbody');
    var self = this;

    keys.forEach(function(ticker) {
      var t = self._findTicker(ticker);
      var tr = document.createElement('tr');

      var tdTicker = document.createElement('td');
      tdTicker.className = 'pl-col-ticker';
      tdTicker.textContent = ticker;
      tr.appendChild(tdTicker);

      var tdName = document.createElement('td');
      tdName.className = 'pl-col-name';
      tdName.textContent = t ? t.name : '';
      tr.appendChild(tdName);

      var tdMax = document.createElement('td');
      tdMax.className = 'pl-col-max';
      var input = document.createElement('input');
      input.type = 'number';
      input.className = 'pl-max-input';
      input.min = 0;
      input.max = 100;
      input.step = 1;
      input.value = Math.round(self.limits[ticker] * 100);
      input.dataset.ticker = ticker;
      input.addEventListener('change', function() {
        var val = parseFloat(this.value);
        if (!isNaN(val) && val >= 0 && val <= 100) {
          self.limits[this.dataset.ticker] = val / 100;
          self._submit();
        }
      });
      tdMax.appendChild(input);
      tr.appendChild(tdMax);

      var tdRemove = document.createElement('td');
      tdRemove.className = 'pl-col-remove';
      var btn = document.createElement('button');
      btn.className = 'pl-remove-btn';
      btn.innerHTML = '&times;';
      btn.dataset.ticker = ticker;
      btn.addEventListener('click', function() {
        delete self.limits[this.dataset.ticker];
        self._renderTable();
        self._submit();
      });
      tdRemove.appendChild(btn);
      tr.appendChild(tdRemove);

      tbody.appendChild(tr);
    });

    table.appendChild(tbody);
    this.tableWrap.appendChild(table);
  }

  _findTicker(ticker) {
    for (var i = 0; i < this.allTickers.length; i++) {
      if (this.allTickers[i].ticker === ticker) return this.allTickers[i];
    }
    return null;
  }

  _submit() {
    // Send through the opt_ctrl handler using the ctrlId stored on the element
    var ctrlId = this.el.dataset.ctrlId;
    if (ctrlId && typeof Shiny !== 'undefined') {
      var result = [];
      for (var k in this.limits) {
        result.push({ticker: k, max: this.limits[k]});
      }
      Shiny.setInputValue(ctrlId, {
        param: 'ticker_limits',
        value: result.length > 0 ? result : null
      }, {priority: 'event'});
    }
    if (this._callback) this._callback();
  }

  getLimits() {
    return this.limits;
  }

  setTickers(tickers) {
    this.allTickers = tickers;
  }
}

// --- Shiny InputBinding ---
var positionLimitsBinding = new Shiny.InputBinding();
$.extend(positionLimitsBinding, {
  find: function(scope) {
    return $(scope).find('.pl-container');
  },
  getValue: function(el) {
    if (!el._posLimits) return null;
    var limits = el._posLimits.getLimits();
    var result = [];
    for (var k in limits) {
      result.push({ticker: k, max: limits[k]});
    }
    return result.length > 0 ? result : null;
  },
  setValue: function(el, value) {},
  subscribe: function(el, callback) {
    if (!el._posLimits) {
      var state = JSON.parse(el.dataset.state || '{}');
      el._posLimits = new PositionLimits(el, state);
    }
    el._posLimits._callback = callback;
  },
  unsubscribe: function(el) {
    if (el._posLimits) el._posLimits._callback = null;
  }
});
Shiny.inputBindings.register(positionLimitsBinding, 'blockr.portfolio.positionLimits');

$(function() {
  Shiny.addCustomMessageHandler('position-limits-tickers', function(msg) {
    var el = document.getElementById(msg.id);
    if (el && el._posLimits) {
      el._posLimits.setTickers(msg.tickers);
    }
  });
});
