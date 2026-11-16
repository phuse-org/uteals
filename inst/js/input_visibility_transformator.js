(function () {
  'use strict';
  var P = '__NS_PREFIX__', H = __HIDDEN_DEFAULTS__, C = '__CONTROLS_ID__', ok = false;

  // ---- findPanel ----
  function findPanel() {
    var s = document.getElementById(C);
    if (!s) return null;
    // Walk up to find the module's own tab-pane first
    var tabPane = s.closest('.tab-pane.teal_module');
    if (!tabPane) tabPane = s.closest('.tab-pane') || document.body;
    // Within this tab-pane, find the aside/sidebar with the most inputs
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
      if (el.children[i].querySelector('input,select,textarea,.selectize-control,.btn,.shiny-input-container,.badge-wrapper,.data-extract,.draggableBuckets')) c++;
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
    // Prefer the shallowest wrapper: if root itself scores well, use it
    var rootScore = scoreChildren(root);
    if (rootScore >= bs && rootScore >= 2) { best = root; bs = rootScore; }
    return bs >= 2 ? best : null;
  }

  // ---- discover ----
  function lbl(el) {
    // For panels/accordions: prioritize the title element
    var t = el.querySelector(':scope > .accordion-title, :scope > .accordion-header .accordion-title, :scope > .additional-settings .accordion-title, :scope > .panel-heading .panel-title, :scope > .card-header');
    if (t) return t.textContent.trim();
    // For regular inputs: use the label
    var l = el.querySelector(':scope > label, :scope > .control-label');
    if (l) return l.textContent.trim();
    // Checkbox labels are inside .checkbox > label > span
    var cb = el.querySelector(':scope > .checkbox > label > span');
    if (cb) return cb.textContent.trim();
    // data-extract labels
    var de = el.querySelector(':scope > .data-extract > label');
    if (de) return de.textContent.trim();
    // draggableBuckets (arm ref/comp): first span text
    if (el.classList.contains('draggableBuckets')) {
      var ds = el.querySelector(':scope > span > span');
      if (ds) return ds.textContent.trim();
    }
    var db = el.querySelector(':scope > .draggableBuckets > span > span');
    if (db) return db.textContent.trim();
    // Fallback: first label anywhere (but not too deep)
    var fl = el.querySelector('label:not(.checkbox label),.control-label');
    if (fl) return fl.textContent.trim();
    // Button text
    var b = el.querySelector(':scope > .btn, :scope > button');
    if (b) return b.textContent.trim();
    var x = el.textContent.trim();
    return (x.length > 0 && x.length < 80) ? x.split('\n')[0].trim() : '';
  }
  function panelLbl(el) {
    // Specifically for panel/accordion containers: find the title
    var t = el.querySelector('.accordion-title,.panel-title,.card-header,h4,h5');
    if (t) return t.textContent.trim();
    return lbl(el);
  }
  function addItem(out, seen, el, idx, prefix, useLbl, parentKey) {
    var k = P + prefix + (el.id || ('idx_' + idx));
    if (seen[k]) return;
    seen[k] = true;
    el.setAttribute('data-vis-key', k);
    var lb = useLbl || lbl(el);
    if (!lb || lb === 'Encodings') return;
    var item = { key: k, label: (prefix ? '\u2514 ' : '') + lb.substring(0, 58) };
    if (parentKey) item.parentKey = parentKey;
    out.push(item);
  }
  function isHiddenByShinyjs(el) {
    // Skip elements intentionally hidden by shinyjs or inline display:none
    if (el.classList.contains('shinyjs-hide')) return true;
    if (el.style.display === 'none') return true;
    return false;
  }
  function collectInputs(container, out, seen, parentIdx, parentKey) {
    // Recursively find all input-containing elements inside a panel body
    var children = container.children;
    var subIdx = 0;
    for (var j = 0; j < children.length; j++) {
      var sc = children[j], stg = sc.tagName;
      if (stg === 'SCRIPT' || stg === 'STYLE' || stg === 'BR') continue;
      if (isHiddenByShinyjs(sc)) continue;
      var hasInput = sc.querySelector('input,select,textarea,.selectize-control,.shiny-input-container,.draggableBuckets');
      if (!hasInput) continue;
      var directLabel = lbl(sc);
      if (!directLabel || directLabel.length > 70) {
        collectInputs(sc, out, seen, parentIdx + '_' + j, parentKey);
        continue;
      }
      addItem(out, seen, sc, parentIdx + '_' + subIdx, 'sub_', null, parentKey);
      subIdx++;
    }
  }
  function discoverChildren(ch, out, seen, offset) {
    for (var i = 0; i < ch.length; i++) {
      var c = ch[i], tg = c.tagName;
      if (tg === 'SCRIPT' || tg === 'STYLE' || tg === 'BR') continue;
      if (c.id === C) continue;
      // bslib accordion wrapper: recurse into its accordion-items
      if (c.classList.contains('accordion') || c.classList.contains('bslib-accordion')) {
        var items = c.querySelectorAll(':scope > .accordion-item');
        for (var k = 0; k < items.length; k++) {
          var item = items[k];
          var itemLbl = panelLbl(item);
          if (!itemLbl) continue;
          addItem(out, seen, item, offset + i + k, '', itemLbl, null);
          var itemKey = P + (item.id || ('idx_' + (offset + i + k)));
          var itemBody = item.querySelector('.accordion-body');
          if (itemBody) collectInputs(itemBody, out, seen, offset + i + k, itemKey);
        }
        continue;
      }
      if (isHiddenByShinyjs(c)) continue;
      var hasI = c.querySelector('input,select,textarea,.selectize-control,.shiny-input-container,.badge-wrapper,.data-extract,.draggableBuckets') || c.classList.contains('draggableBuckets');
      var isP = c.classList.contains('panel') || c.classList.contains('card') || c.querySelector('.panel-collapse,.accordion-collapse,.collapse') || c.querySelector('.additional-settings');
      if (!hasI && !isP) continue;
      if (isP) {
        addItem(out, seen, c, offset + i, '', panelLbl(c), null);
        var pKey = P + (c.id || ('idx_' + (offset + i)));
        var pBody = c.querySelector('.accordion-body,.panel-body,.card-body');
        if (pBody) collectInputs(pBody, out, seen, offset + i, pKey);
      } else {
        addItem(out, seen, c, offset + i, '', null, null);
      }
    }
  }
  function discover(enc) {
    var wr = findWrap(enc);
    if (!wr) return [];
    var out = [], seen = {};
    discoverChildren(wr.children, out, seen, 0);
    return out;
  }

  // ---- toggle ----
  function vis(k, v) { var e = document.querySelector('[data-vis-key="' + k + '"]'); if (e) e.style.display = v ? '' : 'none'; }
  Shiny.addCustomMessageHandler(P + 'toggle_input', function (m) { vis(m.key, m.visible); });
  // Handler to show/hide child checkbox wrappers in the toggle panel
  Shiny.addCustomMessageHandler(P + 'toggle_child_ui', function (m) {
    var cb = document.getElementById(m.cb_id);
    if (!cb) return;
    var wrap = cb.closest('.vis-child-item');
    if (wrap) wrap.style.display = m.visible ? '' : 'none';
  });

  // ---- init ----
  var known = {};
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
    // Check if there are any new items not in known
    var hasNew = false;
    inp.forEach(function (x) { if (!known[x.key]) { hasNew = true; known[x.key] = true; } });
    if (!hasNew && ok) return true;
    ok = true;
    // Send the full list (Shiny replaces the previous value)
    Shiny.setInputValue(P + 'discovered_inputs', inp);
    inp.forEach(function (x) {
      var lo = x.label.toLowerCase();
      for (var j = 0; j < H.length; j++) { if (lo.indexOf(H[j]) !== -1) { vis(x.key, false); break; } }
    });
    return true;
  }
  // Initial discovery
  var att = 0, tm = setInterval(function () { if (go() || ++att > 60) clearInterval(tm); }, 250);
  // Re-discover after tab switch
  $(document).on('shown.bs.tab', function () {
    if (ok) return;
    var r = 0;
    var rt = setInterval(function () { if (go() || ++r > 20) clearInterval(rt); }, 300);
  });
  // Re-discover after a delay to catch late-rendering elements (arm buckets, etc.)
  // Runs a few passes after initial discovery succeeds
  setTimeout(function () {
    var r2 = 0;
    var rt2 = setInterval(function () { go(); if (++r2 > 10) clearInterval(rt2); }, 1000);
  }, 2000);

})();
