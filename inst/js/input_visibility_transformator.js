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
  function addItem(out, seen, el, prefix, useLbl, parentKey) {
    // Use the element's own id for a stable key; fall back to data-vis-key
    // if already assigned from a previous pass, ensuring re-discovery
    // produces identical keys regardless of sibling visibility.
    var id = el.id || el.getAttribute('data-vis-key') || null;
    if (!id) {
      // Generate a deterministic key from the element's position among
      // its siblings (counting ALL siblings, not just visible ones).
      var parent = el.parentNode;
      var idx = 0;
      if (parent) {
        for (var n = 0; n < parent.children.length; n++) {
          if (parent.children[n] === el) { idx = n; break; }
        }
      }
      id = 'pos_' + idx;
    }
    var k = P + prefix + id;
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
    var out = [], seen = {};
    discoverChildren(wr.children, out, seen);
    return out;
  }

  // ---- toggle ----
  function vis(k, v) {
    var e = document.querySelector('[data-vis-key="' + k + '"]');
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
  var lastKeyStr = '';
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

    // Build a fingerprint of all keys; only push to Shiny when it changes.
    var keys = [];
    for (var i = 0; i < inp.length; i++) keys.push(inp[i].key);
    var keyStr = keys.join('|');
    if (keyStr === lastKeyStr) return true; // stable — nothing to do
    lastKeyStr = keyStr;
    ok = true;

    Shiny.setInputValue(P + 'discovered_inputs', inp);
    inp.forEach(function (x) {
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
