#' Freeze table headers and first columns
#'
#' @description `r lifecycle::badge("experimental")`
#' Makes the header row and/or the first column of tables rendered with
#' [teal.widgets::table_with_settings_ui()] sticky, so that the headers and
#' row labels stay visible while scrolling. Include the returned tags once in the
#' app UI, e.g. in the `header` argument of [teal::init()].
#'
#' @param freeze_header (`logical(1)`) whether to freeze the header rows of the tables.
#' @param freeze_col (`logical(1)`) whether to freeze the first column of the tables.
#' @param max_height (`character(1)`) a CSS value for the maximum height of the table container.
#'   The table scrolls vertically inside the container when it is taller than that.
#' @param module_ids (`character` or `NULL`) if `NULL`, all tables in the app are frozen.
#'   Otherwise, only tables inside elements whose `id` contains any of the provided strings
#'   are frozen (e.g. the ids of `teal` modules).
#'
#' @return A [shiny::tagList()] with the CSS and the JavaScript freezing the tables.
#'
#' @details
#' `rtables` renders tables without a `<thead>` element, so the header rows cannot be selected
#' with CSS alone. The JavaScript watches the page for new tables and moves the leading rows containing `<th>`
#' cells into a `<thead>` element of every matching table. Once
#' <https://github.com/pharmaverse/rtables/issues/1123> is resolved, this can be simplified.
#'
#' @examples
#' library(teal.modules.clinical)
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- tmc_ex_adsl
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_summary(
#'       label = "Demographic Table",
#'       dataname = "ADSL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       add_total = TRUE,
#'       summarize_vars = choices_selected(
#'         c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
#'         c("SEX", "RACE")
#'       ),
#'       useNA = "ifany"
#'     )
#'   ),
#'   header = use_freeze_tables(freeze_header = TRUE, freeze_col = TRUE)
#' )
#'
#' if (interactive()) {
#'   shiny::shinyApp(app$ui, app$server)
#' }
#'
#' @export
use_freeze_tables <- function(freeze_header = TRUE, freeze_col = TRUE, max_height = "70vh", module_ids = NULL) {
  checkmate::assert_flag(freeze_header)
  checkmate::assert_flag(freeze_col)
  checkmate::assert_string(max_height)
  checkmate::assert_character(module_ids, min.len = 1, any.missing = FALSE, null.ok = TRUE)

  wrapper_classes <- c(
    "freeze-table-global",
    if (freeze_header) "freeze-header-active",
    if (freeze_col) "freeze-col-active"
  )

  selector <- if (is.null(module_ids)) {
    ".teal-widgets.table-content"
  } else {
    paste(sprintf("[id*=\"%s\"] .teal-widgets.table-content", module_ids), collapse = ", ")
  }

  shiny::tagList(
    shiny::tags$head(shiny::tags$style(sprintf(
      "/* uteals: freeze tables */
      .freeze-table-global .teal-widgets.table-content {
        overflow: auto;
        max-height: %s;
        position: relative;
      }
      /* collapsed borders make the scrolled body bleed through the sticky header in Chromium */
      .freeze-table-global .teal-widgets.table-content table {
        border-collapse: separate;
        border-spacing: 0;
      }
      .freeze-table-global.freeze-header-active .table-content table thead {
        position: sticky;
        top: 0;
        z-index: 2;
      }
      .freeze-table-global.freeze-header-active .table-content table thead th {
        background-color: var(--bs-body-bg, #fff);
        box-shadow: 0 2px 4px -1px rgba(0, 0, 0, 0.1);
      }
      .freeze-table-global.freeze-col-active .table-content table tr th:first-child,
      .freeze-table-global.freeze-col-active .table-content table tr td:first-child {
        position: sticky;
        left: 0;
        z-index: 1;
        background-color: var(--bs-body-bg, #fff);
        box-shadow: 2px 0 4px -1px rgba(0, 0, 0, 0.1);
      }
      .freeze-table-global.freeze-header-active.freeze-col-active .table-content table thead tr th:first-child {
        z-index: 3;
      }",
      max_height
    ))),
    shiny::tags$script(shiny::HTML(sprintf(
      "(function() {
        var SELECTOR = %s;
        var WRAPPER_CLASSES = %s;

        // rtables does not render <thead>, so move the leading header rows into one
        function restructure(table) {
          if (table.querySelector('thead')) return;
          var rows = Array.from(table.querySelectorAll('tr'));
          var thead = document.createElement('thead');
          var tbody = document.createElement('tbody');
          var inHeader = true;
          rows.forEach(function(tr) {
            if (inHeader && tr.querySelector('th')) {
              thead.appendChild(tr);
            } else {
              inHeader = false;
              tbody.appendChild(tr);
            }
          });
          if (thead.children.length > 0) {
            Array.from(table.querySelectorAll('tbody')).forEach(function(el) {
              if (el.children.length === 0) el.remove();
            });
            table.insertBefore(thead, table.firstChild);
            table.appendChild(tbody);
          }
        }

        function processAll() {
          document.querySelectorAll(SELECTOR).forEach(function(container) {
            var wrapper = container.closest('.card') || container.parentElement;
            if (wrapper) {
              WRAPPER_CLASSES.forEach(function(cls) {
                wrapper.classList.add(cls);
              });
            }
            container.querySelectorAll('table').forEach(restructure);
          });
        }

        function start() {
          processAll();
          new MutationObserver(function() {
            requestAnimationFrame(processAll);
          }).observe(document.body, { childList: true, subtree: true });
        }

        if (document.body) {
          start();
        } else {
          document.addEventListener('DOMContentLoaded', start);
        }
      })();",
      jsonlite::toJSON(selector, auto_unbox = TRUE),
      jsonlite::toJSON(wrapper_classes)
    )))
  )
}
