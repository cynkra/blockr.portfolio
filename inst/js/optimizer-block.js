/**
 * OptimizerBlock — JS-first block following blockr.dplyr pattern.
 * Single state object flows bidirectionally between R and JS.
 *
 * State: { strategy, max_weight, max_positions, ticker_limits }
 */
(() => {
  'use strict';

  class OptimizerBlock {
    constructor(el) {
      this.el = el;
      this.strategy = 'mean_variance';
      this.max_weight = null;       // null = no limit
      this.max_positions = -1;      // -1 = auto, null = off, N = custom
      this.ticker_limits = {};      // { ticker: max_pct }
      this.allTickers = [];         // metadata from R
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      // Read initial state from data attribute
      const initState = this.el.dataset.state;
      if (initState) {
        try {
          const s = JSON.parse(initState);
          this.setState(s, true);
        } catch (e) {}
      }

      this._buildDOM();
    }

    // --- State management ---

    _compose() {
      return {
        strategy: this.strategy,
        max_weight: this.max_weight,
        max_positions: this.max_positions,
        ticker_limits: Object.keys(this.ticker_limits).length > 0
          ? this.ticker_limits : {}
      };
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _submit() {
      this._submitted = true;
      var state = this._compose();
      console.log('[OPT] _submit called, state:', JSON.stringify(state));
      console.log('[OPT] ticker_limits keys:', Object.keys(state.ticker_limits));
      console.log('[OPT] has callback:', !!this._callback);
      if (this._callback) this._callback(true);
    }

    getValue() {
      var state = this._compose();
      console.log('[OPT] getValue called, ticker_limits:', JSON.stringify(state.ticker_limits));
      return state;
    }

    setState(state, silent) {
      if (!state) return;
      if (state.strategy !== undefined) this.strategy = state.strategy;
      if (state.max_weight !== undefined) this.max_weight = state.max_weight;
      if (state.max_positions !== undefined) this.max_positions = state.max_positions;
      if (state.ticker_limits !== undefined) {
        // Ensure it's a plain object, not an array
        var tl = state.ticker_limits;
        if (Array.isArray(tl) || !tl) {
          this.ticker_limits = {};
        } else {
          this.ticker_limits = tl;
        }
      }
      if (this._built) this._syncUI();
      if (!silent) this._autoSubmit();
    }

    updateTickers(tickers) {
      this.allTickers = tickers || [];
    }

    // --- DOM building ---

    _buildDOM() {
      this.el.innerHTML = '';

      // Strategy section
      this._buildStrategy();

      // Position limits section
      this._buildPositionLimits();

      // Portfolio size section
      this._buildPortfolioSize();

      this._built = true;
      this._syncUI();
    }

    _buildStrategy() {
      const group = this._makeGroup('Strategy');
      const radios = document.createElement('div');
      radios.className = 'ob-radios';

      const strategies = [
        ['mean_variance', 'Mean-Variance'],
        ['min_vol', 'Min Volatility'],
        ['risk_parity', 'Risk Parity'],
        ['equal_weight', 'Equal Weight']
      ];
      strategies.forEach(([val, label]) => {
        const btn = document.createElement('button');
        btn.className = 'ob-radio';
        btn.dataset.param = 'strategy';
        btn.dataset.value = val;
        btn.textContent = label;
        btn.addEventListener('click', () => {
          this.strategy = val;
          radios.querySelectorAll('.ob-radio').forEach(b =>
            b.classList.toggle('is-active', b.dataset.value === val));
          this._autoSubmit();
        });
        radios.appendChild(btn);
      });
      group.appendChild(radios);
      this.el.appendChild(group);
      this._strategyRadios = radios;
    }

    _buildPositionLimits() {
      const group = this._makeGroup('Position Limits');

      // Default max row
      const maxRow = document.createElement('div');
      maxRow.className = 'ob-constraint-row';

      const maxLabel = document.createElement('label');
      maxLabel.className = 'ob-toggle-label';
      this._maxWeightCheckbox = document.createElement('input');
      this._maxWeightCheckbox.type = 'checkbox';
      this._maxWeightCheckbox.className = 'ob-checkbox';
      this._maxWeightCheckbox.addEventListener('change', () => {
        const on = this._maxWeightCheckbox.checked;
        this.max_weight = on ? 0.25 : null;
        this._maxWeightInput.disabled = !on;
        if (on) this._maxWeightInput.value = 25;
        this._autoSubmit();
      });
      maxLabel.appendChild(this._maxWeightCheckbox);
      maxLabel.appendChild(document.createTextNode(' Default max per ETF'));
      maxRow.appendChild(maxLabel);

      this._maxWeightInput = document.createElement('input');
      this._maxWeightInput.type = 'number';
      this._maxWeightInput.className = 'ob-numeric';
      this._maxWeightInput.min = 5;
      this._maxWeightInput.max = 100;
      this._maxWeightInput.step = 5;
      this._maxWeightInput.addEventListener('change', () => {
        const val = parseFloat(this._maxWeightInput.value);
        if (!isNaN(val) && val > 0 && val <= 100) {
          this.max_weight = val / 100;
          this._autoSubmit();
        }
      });
      maxRow.appendChild(this._maxWeightInput);

      const pct = document.createElement('span');
      pct.className = 'ob-unit';
      pct.textContent = '%';
      maxRow.appendChild(pct);

      group.appendChild(maxRow);

      // Individual limits: search + table
      const plHeader = document.createElement('div');
      plHeader.className = 'ob-pl-header';
      plHeader.innerHTML = '<span class="ob-sublabel">Individual overrides</span>';
      group.appendChild(plHeader);

      // Search input
      const searchRow = document.createElement('div');
      searchRow.className = 'ob-pl-search-row';
      this._plSearch = document.createElement('input');
      this._plSearch.type = 'text';
      this._plSearch.className = 'ob-pl-search';
      this._plSearch.placeholder = 'Search ETF to add limit...';
      this._plSearch.addEventListener('input', () => this._onPlSearch());
      this._plSearch.addEventListener('focus', () => {
        if (this._plSearch.value.length > 0) this._onPlSearch();
      });
      searchRow.appendChild(this._plSearch);

      this._plDropdown = document.createElement('div');
      this._plDropdown.className = 'ob-pl-dropdown ob-hidden';
      searchRow.appendChild(this._plDropdown);
      group.appendChild(searchRow);

      // Close dropdown on outside click
      document.addEventListener('click', (e) => {
        if (!searchRow.contains(e.target)) {
          this._plDropdown.classList.add('ob-hidden');
        }
      });

      // Constrained table
      this._plTable = document.createElement('div');
      this._plTable.className = 'ob-pl-table-wrap';
      group.appendChild(this._plTable);

      this.el.appendChild(group);
    }

    _buildPortfolioSize() {
      const group = this._makeGroup('Portfolio Size');
      const row = document.createElement('div');
      row.className = 'ob-constraint-row';

      const label = document.createElement('span');
      label.className = 'ob-constraint-label';
      label.textContent = 'Max ETFs';
      row.appendChild(label);

      const radios = document.createElement('div');
      radios.className = 'ob-radios ob-small-radios';
      const modes = [['auto', 'Auto'], ['custom', 'Custom'], ['off', 'Off']];
      modes.forEach(([val, lbl]) => {
        const btn = document.createElement('button');
        btn.className = 'ob-radio';
        btn.dataset.param = 'max_positions_mode';
        btn.dataset.value = val;
        btn.textContent = lbl;
        btn.addEventListener('click', () => {
          if (val === 'auto') this.max_positions = -1;
          else if (val === 'custom') this.max_positions = 10;
          else this.max_positions = null;
          this._maxPosInput.disabled = val !== 'custom';
          radios.querySelectorAll('.ob-radio').forEach(b =>
            b.classList.toggle('is-active', b.dataset.value === val));
          this._autoSubmit();
        });
        radios.appendChild(btn);
      });
      row.appendChild(radios);

      this._maxPosInput = document.createElement('input');
      this._maxPosInput.type = 'number';
      this._maxPosInput.className = 'ob-numeric';
      this._maxPosInput.min = 3;
      this._maxPosInput.max = 34;
      this._maxPosInput.step = 1;
      this._maxPosInput.addEventListener('change', () => {
        const val = parseInt(this._maxPosInput.value);
        if (!isNaN(val) && val >= 3) {
          this.max_positions = val;
          this._autoSubmit();
        }
      });
      row.appendChild(this._maxPosInput);

      group.appendChild(row);
      this.el.appendChild(group);
      this._maxPosRadios = radios;
    }

    // --- Sync UI from state ---

    _syncUI() {
      // Strategy
      if (this._strategyRadios) {
        this._strategyRadios.querySelectorAll('.ob-radio').forEach(b =>
          b.classList.toggle('is-active', b.dataset.value === this.strategy));
      }

      // Max weight
      if (this._maxWeightCheckbox) {
        const hasMax = this.max_weight !== null && !isNaN(this.max_weight);
        this._maxWeightCheckbox.checked = hasMax;
        this._maxWeightInput.disabled = !hasMax;
        this._maxWeightInput.value = hasMax ? Math.round(this.max_weight * 100) : 25;
      }

      // Max positions
      if (this._maxPosRadios) {
        const mode = this.max_positions === -1 ? 'auto'
          : this.max_positions === null ? 'off' : 'custom';
        this._maxPosRadios.querySelectorAll('.ob-radio').forEach(b =>
          b.classList.toggle('is-active', b.dataset.value === mode));
        this._maxPosInput.disabled = mode !== 'custom';
        this._maxPosInput.value = (mode === 'custom' && this.max_positions > 0)
          ? this.max_positions : 10;
      }

      // Position limits table
      this._renderPlTable();
    }

    // --- Position limits search ---

    _onPlSearch() {
      const query = this._plSearch.value.trim().toLowerCase();
      if (query.length < 1) {
        this._plDropdown.classList.add('ob-hidden');
        return;
      }

      const results = this.allTickers.filter(t => {
        if (this.ticker_limits[t.ticker] !== undefined) return false;
        return t.ticker.toLowerCase().indexOf(query) >= 0 ||
          (t.name || '').toLowerCase().indexOf(query) >= 0 ||
          (t.region || '').toLowerCase().indexOf(query) >= 0;
      }).slice(0, 8);

      this._plDropdown.innerHTML = '';
      if (results.length === 0) {
        this._plDropdown.innerHTML = '<div class="ob-pl-dd-empty">No matches</div>';
      } else {
        results.forEach(t => {
          const item = document.createElement('div');
          item.className = 'ob-pl-dd-item';
          item.innerHTML = '<span class="ob-pl-dd-ticker">' + t.ticker +
            '</span> <span class="ob-pl-dd-name">' + (t.name || '') + '</span>';
          item.addEventListener('click', (e) => {
            e.stopPropagation();
            console.log('[OPT] Adding limit for', t.ticker, '= 0.10');
            console.log('[OPT] ticker_limits type before:', typeof this.ticker_limits, Array.isArray(this.ticker_limits));
            this.ticker_limits[t.ticker] = 0.10;
            console.log('[OPT] ticker_limits after add:', JSON.stringify(this.ticker_limits));
            this._plSearch.value = '';
            this._plDropdown.classList.add('ob-hidden');
            this._renderPlTable();
            this._autoSubmit();
          });
          this._plDropdown.appendChild(item);
        });
      }
      this._plDropdown.classList.remove('ob-hidden');
    }

    _renderPlTable() {
      if (!this._plTable) return;
      this._plTable.innerHTML = '';
      const keys = Object.keys(this.ticker_limits);

      if (keys.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'ob-pl-empty';
        empty.textContent = 'No individual limits';
        this._plTable.appendChild(empty);
        return;
      }

      const table = document.createElement('table');
      table.className = 'ob-pl-table';
      const tbody = document.createElement('tbody');

      keys.forEach(ticker => {
        const t = this.allTickers.find(x => x.ticker === ticker);
        const tr = document.createElement('tr');

        const tdTicker = document.createElement('td');
        tdTicker.className = 'ob-pl-ticker';
        tdTicker.textContent = ticker;
        tr.appendChild(tdTicker);

        const tdName = document.createElement('td');
        tdName.className = 'ob-pl-name';
        tdName.textContent = t ? t.name : '';
        tr.appendChild(tdName);

        const tdMax = document.createElement('td');
        tdMax.className = 'ob-pl-max';
        const input = document.createElement('input');
        input.type = 'number';
        input.className = 'ob-pl-input';
        input.min = 0;
        input.max = 100;
        input.step = 1;
        input.value = Math.round(this.ticker_limits[ticker] * 100);
        input.addEventListener('change', () => {
          const val = parseFloat(input.value);
          console.log('[OPT] Limit input changed for', ticker, 'val:', val);
          if (!isNaN(val) && val >= 0 && val <= 100) {
            this.ticker_limits[ticker] = val / 100;
            console.log('[OPT] Updated ticker_limits:', JSON.stringify(this.ticker_limits));
            this._autoSubmit();
          }
        });
        tdMax.appendChild(input);
        tr.appendChild(tdMax);

        const tdRemove = document.createElement('td');
        tdRemove.className = 'ob-pl-remove';
        const btn = document.createElement('button');
        btn.className = 'ob-pl-remove-btn';
        btn.innerHTML = '&times;';
        btn.addEventListener('click', () => {
          delete this.ticker_limits[ticker];
          this._renderPlTable();
          this._autoSubmit();
        });
        tdRemove.appendChild(btn);
        tr.appendChild(tdRemove);

        tbody.appendChild(tr);
      });

      table.appendChild(tbody);
      this._plTable.appendChild(table);
    }

    // --- Helpers ---

    _makeGroup(label) {
      const group = document.createElement('div');
      group.className = 'ob-group';
      const lbl = document.createElement('div');
      lbl.className = 'ob-label';
      lbl.textContent = label;
      group.appendChild(lbl);
      return group;
    }
  }

  // --- Shiny InputBinding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.opt-container'),
    getId: (el) => el.id || null,

    getValue: (el) => el._block ? el._block.getValue() : null,

    setValue: (el, value) => {
      if (el._block) el._block.setState(value, true);
    },

    subscribe: (el, callback) => {
      if (!el._block) {
        el._block = new OptimizerBlock(el);
        if (el._pendingTickers) {
          el._block.updateTickers(el._pendingTickers);
          delete el._pendingTickers;
        }
        if (el._pendingState) {
          el._block.setState(el._pendingState, true);
          delete el._pendingState;
        }
      }
      el._block._callback = () => callback(true);
    },

    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },

    initialize: (el) => {
      // Also initialize here in case subscribe hasn't been called
      if (!el._block) {
        el._block = new OptimizerBlock(el);
        if (el._pendingTickers) {
          el._block.updateTickers(el._pendingTickers);
          delete el._pendingTickers;
        }
        if (el._pendingState) {
          el._block.setState(el._pendingState, true);
          delete el._pendingState;
        }
      }
    },

    receiveMessage: (el, data) => {
      if (data.state) el._block?.setState(data.state, true);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.portfolio.optimizer');

  // --- Custom message handlers ---

  Shiny.addCustomMessageHandler('optimizer-tickers', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.updateTickers(msg.tickers);
    } else if (el) {
      el._pendingTickers = msg.tickers;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) {
          el2._block.updateTickers(msg.tickers);
          clearInterval(t);
        } else if (el2) {
          el2._pendingTickers = msg.tickers;
          clearInterval(t);
        }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  Shiny.addCustomMessageHandler('optimizer-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setState(msg.state, true);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });

})();
