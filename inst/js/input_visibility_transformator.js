(function () {
  'use strict';
  var P = '__NS_PREFIX__', H = __HIDDEN_DEFAULTS__, C = '__CONTROLS_ID__', ok = false;

  // ---- findPanel ----
  function findPanel() {
    var s = document.getElementById(C);
    if (!s) return null;
    var tabPane = s.closest('.tab-pane.teal_module');
    if (!tabPane) tabPane = s.closest('.tab-pane') || document.body;
    var candidates = tabPane.querySelectorAll('aside,.sidebar,[id*=bslib-sidebar],.well');
    var best = null, bs = 0;
    for (var i = 0; i < candidates.length; i++) {
      var c = candidates[i];
      if (c.querySelector('.table_with_settings,.shiny-plot-output')) continue;
      var sc = c.querySelectorAll('.shiny-input-container,select,.btn').length;
      if (sc > bs) { bs = sc; best = c; }
    }
    return best;
  }

  // ---- findWrap ----
  function scoreChildren(el) {
    var c = 0;
    for (var i = 0; i < el.children.length; i++) {
      if (el.children[i].querySelector(
        'input,select,textarea,.selectize-control,.btn,' +
        '.shiny-input-container,.badge-wrapper,.data-extract,.draggableBuckets'
      )) c++;
    }
    return c;
  }
  function findWrap(root) {
    var best = root, bs = scoreChildren(root);
    function w(el, d) {
      if (d > 5) return;
      for (var i = 0; i < el.children.length; i++) {
        var ch = el.children[i], s = scoreChildren(ch);
        if (s > bs) { bs = s; best = ch; }
        w(ch, d + 1);
      }
    }
    w(root, 0);
    if (bs < 2) return null;
    return best;
  }

  // ---- discover helpers ----
  function lbl(el) {
    var t = el.querySelector(
      ':scope > .accordion-title,' +
      ':scope > .accordion-header .accordion-title,' +
      ':scope > .additional-settings .accordion-title,' +
      ':scope > .panel-heading .panel-title,' +
      ':scope > .card-header'
    );
    if (t) return t.textContent.trim();
    var l = el.querySelector(':scope > label, :scope > .control-label');
    if (l) return l.textContent.trim();
    var cb = el.querySelector(':scope > .checkbox > label > span');
    if (cb) return cb.textContent.trim();
    var de = el.querySelector(':scope > .data-extract > label');
    if (de) return de.textContent.trim();
    if (el.classList.contains('draggableBuckets')) {
      var ds = el.querySelector(':scope > span > span');
      if (ds) return ds.textContent.trim();
    }
    var db = el.querySelector(':scope > .draggableBuckets > span > span');
    if (db) return db.textContent.trim();
    var fl = el.querySelector('label:not(.checkbox label),.control-label');
    if (fl) return fl.textContent.trim();
    var b = el.querySelector(':scope > .btn, :scope > button');
    if (b) return b.textContent.trim();
    var x = el.textContent.trim();
    return (x.length > 0 && x.length < 80) ? x.split('\n')[0].trim() : '';
  }
  function panelLbl(el) {
    var t = el.querySelector('.accordion-title,.panel-title,.card-header,h4,h5');
    if (t) return t.textContent.trim();
    return lbl(el);
  }
  // Path of child indices from the discovery root down to `el`. Unique for
  // every element under the root, unlike an index among siblings alone.
  var discoveryRoot = null, issued = {}, idKeys = {};
  function keyEl(k) {
    return document.querySelector(
      '[data-vis-key="' + String(k).replace(/["\\]/g, '\\$&') + '"]'
    );
  }
  function domPath(el) {
    var parts = [];
    while (el && el !== discoveryRoot && el.parentElement) {
      parts.unshift(Array.prototype.indexOf.call(el.parentElement.children, el));
      el = el.parentElement;
    }
    return parts.join('_');
  }
  function addItem(out, seen, el, prefix, useLbl, parentKey) {
    // A key assigned in an earlier pass is reused as-is, so keys never change
    // between re-discovery passes. New elements get a key from their own id
    // or, failing that, from their full DOM path.
    var k = el.getAttribute('data-vis-key') || (el.id && idKeys[el.id]);
    if (!k) {
      // The suffix is sanitised the same way R builds checkbox ids, so keys
      // that are distinct here stay distinct as checkbox ids. A key is never
      // issued twice: a later element can land on a path that an earlier one
      // (live or since removed) held, and must not inherit its state.
      var base = P + (prefix + (el.id ? 'id_' + el.id : 'pos_' + domPath(el)))
        .replace(/[^A-Za-z0-9_]/g, '_');
      k = base;
      for (var n = 1; issued[k] || keyEl(k); n++) k = base + '_n' + n;
      issued[k] = true;
      // An element Shiny re-renders keeps its id, so it keeps its key.
      if (el.id) idKeys[el.id] = k;
    }
    if (seen[k]) return;
    seen[k] = true;
    el.setAttribute('data-vis-key', k);
    var lb = useLbl || lbl(el);
    if (!lb || lb === 'Encodings') return;
    var item = { key: k, label: (prefix ? '\u2514 ' : '') + lb.substring(0, 58) };
    if (parentKey) item.parentKey = parentKey;
    out.push(item);
  }

  // Only skip elements hidden by the module author (shinyjs-hide).
  // Do NOT skip elements hidden by this transformator (display:none via vis()).
  function isAuthorHidden(el) {
    return el.classList.contains('shinyjs-hide');
  }

  function collectInputs(container, out, seen, parentKey) {
    var children = container.children;
    for (var j = 0; j < children.length; j++) {
      var sc = children[j], stg = sc.tagName;
      if (stg === 'SCRIPT' || stg === 'STYLE' || stg === 'BR') continue;
      if (isAuthorHidden(sc)) continue;
      var hasInput = sc.querySelector(
        'input,select,textarea,.selectize-control,.shiny-input-container,.draggableBuckets'
      );
      if (!hasInput) continue;
      var directLabel = lbl(sc);
      if (!directLabel || directLabel.length > 70) {
        collectInputs(sc, out, seen, parentKey);
        continue;
      }
      addItem(out, seen, sc, 'sub_', null, parentKey);
    }
  }

  function discoverChildren(ch, out, seen) {
    for (var i = 0; i < ch.length; i++) {
      var c = ch[i], tg = c.tagName;
      if (tg === 'SCRIPT' || tg === 'STYLE' || tg === 'BR') continue;
      if (c.id === C) continue;
      if (c.classList.contains('accordion') || c.classList.contains('bslib-accordion')) {
        var items = c.querySelectorAll(':scope > .accordion-item');
        for (var k = 0; k < items.length; k++) {
          var item = items[k];
          var itemLbl = panelLbl(item);
          if (!itemLbl) continue;
          addItem(out, seen, item, '', itemLbl, null);
          var itemKey = item.getAttribute('data-vis-key');
          var itemBody = item.querySelector('.accordion-body');
          if (itemBody) collectInputs(itemBody, out, seen, itemKey);
        }
        continue;
      }
      if (isAuthorHidden(c)) continue;
      var hasI = c.querySelector(
        'input,select,textarea,.selectize-control,' +
        '.shiny-input-container,.badge-wrapper,.data-extract,.draggableBuckets'
      ) || c.classList.contains('draggableBuckets');
      var isP = c.classList.contains('panel') || c.classList.contains('card') ||
        c.querySelector('.panel-collapse,.accordion-collapse,.collapse') ||
        c.querySelector('.additional-settings');
      if (!hasI && !isP) continue;
      if (isP) {
        addItem(out, seen, c, '', panelLbl(c), null);
        var pKey = c.getAttribute('data-vis-key');
        var pBody = c.querySelector('.accordion-body,.panel-body,.card-body');
        if (pBody) collectInputs(pBody, out, seen, pKey);
      } else {
        addItem(out, seen, c, '', null, null);
      }
    }
  }

  function discover(enc) {
    var wr = findWrap(enc);
    if (!wr) return [];
    discoveryRoot = wr;
    var out = [], seen = {};
    discoverChildren(wr.children, out, seen);
    return out;
  }

  // ---- toggle ----
  // Wanted visibility per key, re-applied on every discovery pass so that an
  // element Shiny re-renders does not reappear while its box is unchecked.
  var desired = {};
  function vis(k, v) {
    desired[k] = v;
    var e = keyEl(k);
    if (e) e.style.display = v ? '' : 'none';
  }
  Shiny.addCustomMessageHandler(P + 'toggle_input', function (m) {
    vis(m.key, m.visible);
  });
  Shiny.addCustomMessageHandler(P + 'toggle_child_ui', function (m) {
    var cb = document.getElementById(m.cb_id);
    if (!cb) return;
    var wrap = cb.closest('.vis-child-item');
    if (wrap) wrap.style.display = m.visible ? '' : 'none';
  });

  // ---- init ----
  var lastKeyStr = '', defaultsApplied = {};
  function go() {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return false;
    var s = document.getElementById(C);
    if (!s) return false;
    var tp = s.closest('.tab-pane');
    if (tp && !tp.classList.contains('active')) return false;
    var enc = findPanel();
    if (!enc) return false;
    var inp = discover(enc);
    if (inp.length === 0) return false;

    for (var dk in desired) {
      if (desired.hasOwnProperty(dk)) {
        var de = keyEl(dk);
        if (de) de.style.display = desired[dk] ? '' : 'none';
      }
    }

    // Build a fingerprint of all keys and labels; only push to Shiny when it
    // changes.
    var keys = [];
    for (var i = 0; i < inp.length; i++) {
      keys.push(inp[i].key + '=' + inp[i].label + '<' + (inp[i].parentKey || ''));
    }
    var keyStr = keys.join('|');
    if (keyStr === lastKeyStr) return true; // stable — nothing to do
    lastKeyStr = keyStr;
    ok = true;

    Shiny.setInputValue(P + 'discovered_inputs', inp);
    // Apply hidden_by_default only the first time a key is seen, so a later
    // pass never re-hides an input the user has chosen to show.
    inp.forEach(function (x) {
      if (defaultsApplied[x.key]) return;
      defaultsApplied[x.key] = true;
      var lo = x.label.toLowerCase();
      for (var j = 0; j < H.length; j++) {
        if (lo.indexOf(H[j]) !== -1) { vis(x.key, false); break; }
      }
    });
    return true;
  }

  // Initial polling (fast, stops once discovered)
  var att = 0, tm = setInterval(function () {
    if (go() || ++att > 60) clearInterval(tm);
  }, 250);

  // Re-discover after tab switch
  $(document).on('shown.bs.tab', function () {
    if (ok) return;
    var r = 0;
    var rt = setInterval(function () {
      if (go() || ++r > 20) clearInterval(rt);
    }, 300);
  });

  // MutationObserver for late-rendered elements.
  // Observes the encoding panel but ignores mutations inside our own
  // controls div (which Shiny re-renders when toggle_ui updates).
  var obs = null, goTimer = null;
  function startObserver() {
    var s = document.getElementById(C);
    if (!s || obs) return;
    var target = s.closest('aside,.sidebar,[id*=bslib-sidebar],.well');
    if (!target) target = s.closest('.tab-pane') || document.body;
    obs = new MutationObserver(function (mutations) {
      // Ignore mutations that originate inside our controls div
      var dominated = document.getElementById(C);
      for (var m = 0; m < mutations.length; m++) {
        if (!dominated || !dominated.contains(mutations[m].target)) {
          // At least one mutation is outside our controls — re-discover
          if (goTimer) clearTimeout(goTimer);
          goTimer = setTimeout(function () { goTimer = null; go(); }, 500);
          return;
        }
      }
    });
    obs.observe(target, { childList: true, subtree: true });
  }
  var obsAtt = 0, obsTm = setInterval(function () {
    startObserver();
    if (obs || ++obsAtt > 60) clearInterval(obsTm);
  }, 250);

})();
