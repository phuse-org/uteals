#' Watermark Decorator for UI
#'
#' @description `r lifecycle::badge("experimental")`
#' Creates a `teal_transform_module` that overlays a watermark on the table
#' or plot output container. The watermark stays visible while scrolling and
#' only appears when the output is rendered. All parameters are exposed as
#' interactive UI controls in the encoding panel.
#'
#' @param watermark_text (`character(1)`) initial watermark text. Defaults to `"DRAFT"`.
#' @param color (`character(1)`) initial hex color code (e.g. `"#C8C8C8"`). Defaults to `"#C8C8C8"`.
#' @param opacity (`numeric(1)`) initial opacity between 0 (transparent) and 1 (opaque). Defaults to `0.3`.
#' @param font_size (`numeric(1)`) initial font size in rem units. Defaults to `6`.
#' @param rotation (`numeric(1)`) initial rotation angle in degrees. Defaults to `-30`.
#' @param n_repeats (`integer(1)`) initial number of watermark repetitions
#'   tiled across the output container. Defaults to `1`.
#'
#' @return [`teal::teal_transform_module()`]
#'
#' @details The module creates a UI with controls for watermark text, color,
#' opacity, font size, rotation, and number of repetitions. The watermark is
#' rendered as a fixed-position overlay using JavaScript and updates
#' reactively when any control is changed via a custom Shiny message handler.
#' Note: the watermark is not applied on the exported tables/plots.
#'
#'
#' @importFrom teal teal_transform_module
#' @importFrom shiny NS tagList textInput sliderInput numericInput tags HTML moduleServer reactive req observe
#'
#' @examples
#' # Example 1: Table module
#' if (interactive()) {
#'   library(teal)
#'   library(teal.modules.clinical.junco)
#'
#'   data <- teal_data()
#'   data <- within(data, {
#'     ADSL <- pharmaverseadam::adsl
#'     ADSL$TRT01A <- as.factor(ADSL$TRT01A)
#'     ADAE <- pharmaverseadam::adae
#'   })
#'   teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
#'
#'   app <- init(
#'     data = data,
#'     modules = modules(
#'       tm_t_events_j(
#'         label = "AE Table with Watermark",
#'         dataname = "ADAE",
#'         treatment_var = choices_selected(c("TRT01A"), "TRT01A"),
#'         llt = choices_selected(choices = c("AETERM", "AEDECOD"), selected = "AEDECOD"),
#'         hlt = choices_selected(choices = c("AEBODSYS", "AESOC"), selected = "AEBODSYS"),
#'         decorators = list(
#'           table = watermark_ui_decorator("DRAFT", color = "#FF0000", opacity = 0.15, n_repeats = 4)
#'         )
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # Example 2: Plot module (tm_g_labs_j)
#' if (interactive()) {
#'   library(teal)
#'   library(teal.modules.clinical.junco)
#'
#'   ADSL <- pharmaverseadamjnj::adsl
#'   ADEG <- pharmaverseadamjnj::adeg
#'
#'   app <- init(
#'     data = cdisc_data(
#'       ADSL = ADSL,
#'       ADEG = ADEG,
#'       code = "
#'         ADSL <- pharmaverseadamjnj::adsl
#'         ADEG <- pharmaverseadamjnj::adeg
#'       "
#'     ),
#'     modules = modules(
#'       tm_g_labs_j(
#'         label = "Lab Plot with Watermark",
#'         dataname = "ADEG",
#'         treatment_var = choices_selected(
#'           choices = variable_choices("ADSL"),
#'           selected = "TRT01A"
#'         ),
#'         aval_var = choices_selected(
#'           choices = variable_choices("ADEG"),
#'           selected = "AVAL"
#'         ),
#'         decorators = list(
#'           plot = watermark_ui_decorator("DRAFT", color = "#0000FF", opacity = 0.2, n_repeats = 3)
#'         )
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
watermark_ui_decorator <- function(
  watermark_text = "DRAFT",
  color = "#C8C8C8",
  opacity = 0.3,
  font_size = 6,
  rotation = -30,
  n_repeats = 1L
) {
  checkmate::assert_string(watermark_text, min.chars = 1)
  checkmate::assert_string(color, pattern = "^#[0-9A-Fa-f]{6}$")
  checkmate::assert_number(opacity, lower = 0, upper = 1)
  checkmate::assert_number(font_size, lower = 0.5)
  checkmate::assert_number(rotation)
  checkmate::assert_int(n_repeats, lower = 1)

  watermark_js <- paste0(
    "(function() {",
    "  var uid = '%s';",
    "  var msgType = 'watermark-update-' + uid;",
    "  var overlays = [];",
    "  var scriptEl = document.currentScript;",
    "",
    "  function findOutputContainer() {",
    "    var root = scriptEl.closest(",
    "      '.bslib-sidebar-layout, .standard-layout-wrapper,",
    "       .teal_module, [class*=tab-pane], .row'",
    "    );",
    "    if (!root) return null;",
    "    var card = root.querySelector(",
    "      '[id$=-table-with-settings], [id$=-plot-with-settings]'",
    "    );",
    "    if (card) return card;",
    "    var outDiv = root.querySelector('.standard-layout-output .well');",
    "    if (outDiv) return outDiv;",
    "    return null;",
    "  }",
    "",
    "  function hasRenderedContent(el) {",
    "    if (!el) return false;",
    "    var tbl = el.querySelector('.shiny-text-output, table, pre');",
    "    if (tbl && tbl.textContent.trim().length > 0) return true;",
    "    var img = el.querySelector('img');",
    "    if (img && img.src) return true;",
    "    return false;",
    "  }",
    "",
    "  var current = {",
    "    text:'', color:'#C8C8C8', opacity:0.3,",
    "    fontSize:6, rotation:-30, nRepeats:1",
    "  };",
    "",
    "  function hexToRgba(hex, opacity) {",
    "    var r = parseInt(hex.slice(1,3), 16);",
    "    var g = parseInt(hex.slice(3,5), 16);",
    "    var b = parseInt(hex.slice(5,7), 16);",
    "    return 'rgba(' + r + ',' + g + ',' + b + ',' + opacity + ')';",
    "  }",
    "",
    "  function rebuild() {",
    "    overlays.forEach(function(o) { o.remove(); });",
    "    overlays = [];",
    "    for (var i = 0; i < current.nRepeats; i++) {",
    "      var el = document.createElement('div');",
    "      el.className = 'watermark-el-' + uid;",
    "      el.textContent = current.text;",
    "      el.style.cssText =",
    "        'position:fixed;pointer-events:none;z-index:9999;' +",
    "        'font-weight:bold;white-space:nowrap;user-select:none;display:none;' +",
    "        'font-size:' + current.fontSize + 'rem;' +",
    "        'color:' + hexToRgba(current.color, current.opacity) + ';' +",
    "        'transform:translate(-50%%,-50%%) rotate(' + current.rotation + 'deg);';",
    "      document.body.appendChild(el);",
    "      overlays.push(el);",
    "    }",
    "    reposition();",
    "  }",
    "",
    "  function reposition() {",
    "    var container = findOutputContainer();",
    "    if (!container || !hasRenderedContent(container)) {",
    "      overlays.forEach(function(o) { o.style.display = 'none'; });",
    "      return;",
    "    }",
    "    var rect = container.getBoundingClientRect();",
    "    if (rect.bottom <= 0 || rect.top >= window.innerHeight ||",
    "        rect.right <= 0 || rect.left >= window.innerWidth) {",
    "      overlays.forEach(function(o) { o.style.display = 'none'; });",
    "      return;",
    "    }",
    "    var vT = Math.max(rect.top, 0), vL = Math.max(rect.left, 0);",
    "    var vB = Math.min(rect.bottom, window.innerHeight);",
    "    var vR = Math.min(rect.right, window.innerWidth);",
    "    var n = current.nRepeats;",
    "    var cols = Math.ceil(Math.sqrt(n));",
    "    var rows = Math.ceil(n / cols);",
    "    var cW = (vR - vL) / cols, cH = (vB - vT) / rows;",
    "    for (var k = 0; k < n; k++) {",
    "      overlays[k].style.display = 'block';",
    "      overlays[k].style.left = (vL + cW * ((k %% cols) + 0.5)) + 'px';",
    "      overlays[k].style.top  = (vT + cH * (Math.floor(k / cols) + 0.5)) + 'px';",
    "    }",
    "  }",
    "",
    "  window.addEventListener('scroll', reposition, true);",
    "  window.addEventListener('resize', reposition);",
    "  var obs = new MutationObserver(reposition);",
    "  var obsTarget = scriptEl.closest(",
    "    '.bslib-sidebar-layout, .teal_module, [class*=tab-pane]'",
    "  ) || document.body;",
    "  obs.observe(obsTarget, { childList:true, subtree:true, characterData:true });",
    "",
    "  Shiny.addCustomMessageHandler(msgType, function(msg) {",
    "    current.text     = msg.text;",
    "    current.color    = msg.color;",
    "    current.opacity  = msg.opacity;",
    "    current.fontSize = msg.fontSize;",
    "    current.rotation = msg.rotation;",
    "    current.nRepeats = msg.nRepeats;",
    "    rebuild();",
    "  });",
    "})();"
  )

  color_binding_js <- paste0(
    "(function() {",
    "  var colorId = '%s';",
    "  var binding = new Shiny.InputBinding();",
    "  $.extend(binding, {",
    "    find: function(scope) {",
    "      return $(scope).find('#' + CSS.escape(colorId));",
    "    },",
    "    getValue: function(el) { return el.value; },",
    "    setValue: function(el, value) { el.value = value; },",
    "    subscribe: function(el, callback) {",
    "      $(el).on('input.wmColor change.wmColor', function() { callback(true); });",
    "    },",
    "    unsubscribe: function(el) { $(el).off('.wmColor'); }",
    "  });",
    "  Shiny.inputBindings.register(binding, 'watermark.colorBinding.' + colorId);",
    "})();"
  )

  teal::teal_transform_module(
    label = "Watermark",
    ui = function(id) {
      ns <- shiny::NS(id)
      uid <- gsub("[^a-zA-Z0-9]", "", ns("wm"))

      shiny::tagList(
        shiny::textInput(ns("watermark_text"), "Watermark text", value = watermark_text),
        shiny::tags$div(
          class = "form-group shiny-input-container",
          shiny::tags$label("Watermark color", `for` = ns("color")),
          shiny::tags$input(
            type = "color", id = ns("color"), name = ns("color"),
            value = color, class = "form-control",
            style = "width:100%;height:38px;padding:2px;cursor:pointer;"
          )
        ),
        shiny::tags$script(shiny::HTML(sprintf(color_binding_js, ns("color")))),
        shiny::sliderInput(ns("opacity"), "Opacity", min = 0, max = 1, value = opacity, step = 0.05),
        shiny::numericInput(ns("font_size"), "Font size (rem)", value = font_size, min = 0.5, step = 0.5),
        shiny::numericInput(ns("rotation"), "Rotation (degrees)", value = rotation, step = 5),
        shiny::numericInput(ns("n_repeats"), "Repetitions", value = n_repeats, min = 1, step = 1),
        shiny::tags$script(shiny::HTML(sprintf(watermark_js, uid)))
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        uid <- gsub("[^a-zA-Z0-9]", "", session$ns("wm"))
        msg_type <- paste0("watermark-update-", uid)

        shiny::observe({
          txt <- input$watermark_text
          col <- input$color
          opa <- input$opacity
          fs <- input$font_size
          rot <- input$rotation
          nr <- input$n_repeats
          shiny::req(nchar(txt) > 0, nchar(col) > 0, !is.null(opa), fs > 0, nr >= 1)
          session$sendCustomMessage(msg_type, list(
            text = txt, color = col, opacity = opa, fontSize = fs,
            rotation = rot, nRepeats = as.integer(nr)
          ))
        })

        shiny::reactive({
          shiny::req(data())
          data()
        })
      })
    }
  )
}
