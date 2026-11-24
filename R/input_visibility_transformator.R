#' Input Visibility Transformator
#'
#' Creates a `teal_transform_module` that auto-discovers Shiny inputs in the
#' module's encoding panel and provides toggle controls to show or hide them.
#'
#' @param label (`character(1)`) Label shown in the accordion.
#' @param hidden_by_default (`character`) Vector of input label substrings
#'   (case-insensitive). Any discovered input whose label contains one of
#'   these strings will be hidden on first render.
#'
#' @return A `teal_transform_module` that passes data through unchanged.
#'
#' @examples
#' \dontrun{
#' teal.modules.clinical::tm_t_events(
#'   label = "AE Table",
#'   dataname = "ADAE",
#'   arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
#'   llt = teal.transform::choices_selected("AEDECOD", "AEDECOD"),
#'   hlt = teal.transform::choices_selected("AEBODSYS", "AEBODSYS"),
#'   transformators = list(
#'     input_visibility_transformator(
#'       hidden_by_default = c("Sort", "Prune")
#'     )
#'   )
#' )
#' }
#'
#' @export
input_visibility_transformator <- function(
    label = "Toggle Input Visibility",
    hidden_by_default = character(0)) {
  checkmate::assert_string(label)
  checkmate::assert_character(
    hidden_by_default,
    any.missing = FALSE,
    min.chars = 1L
  )

  teal::teal_transform_module(
    label = label,
    ui = function(id) {
      ns <- shiny::NS(id)
      hidden_json <- jsonlite::toJSON(
        tolower(hidden_by_default),
        auto_unbox = FALSE
      )
      ns_prefix <- sub("__x__$", "", ns("__x__"))

      js <- .input_visibility_js_template()
      js <- gsub("__NS_PREFIX__", ns_prefix, js, fixed = TRUE)
      js <- gsub("__HIDDEN_DEFAULTS__", hidden_json, js, fixed = TRUE)
      js <- gsub(
        "__CONTROLS_ID__",
        ns("visibility_controls"),
        js,
        fixed = TRUE
      )

      shiny::tagList(
        shiny::tags$div(
          id = ns("visibility_controls"),
          style = "padding: 5px 0;",
          shiny::uiOutput(ns("toggle_ui"))
        ),
        shiny::tags$script(shiny::HTML(js))
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        ns_prefix <- sub("__x__$", "", ns("__x__"))
        msg_type <- paste0(ns_prefix, "toggle_input")

        send_toggle <- function(key, visible) {
          session$sendCustomMessage(
            msg_type,
            list(key = key, visible = visible)
          )
        }

        discovered_inputs <- shiny::reactive({
          raw <- input$discovered_inputs
          shiny::req(raw)
          .normalise_discovered(raw)
        })

        output$toggle_ui <- shiny::renderUI({
          discovered <- discovered_inputs()
          hidden_lower <- tolower(hidden_by_default)
          parents <- child_keys_of()

          is_child <- vapply(
            discovered,
            function(inp) !is.null(inp$parentKey),
            logical(1)
          )

          ui_items <- lapply(seq_along(discovered), function(i) {
            inp <- discovered[[i]]
            cb_id <- .vis_cb_id(inp$key)
            default_visible <- .is_default_visible(
              inp$label,
              hidden_lower
            )
            cb <- shiny::checkboxInput(
              ns(cb_id),
              label = inp$label,
              value = default_visible
            )
            if (is_child[i]) {
              parent_visible <- .is_parent_visible(
                inp$parentKey,
                discovered,
                hidden_lower
              )
              shiny::tags$div(
                class = "vis-child-item",
                style = if (!parent_visible) {
                  "margin-left:15px;display:none;"
                } else {
                  "margin-left:15px;"
                },
                cb
              )
            } else {
              cb
            }
          })

          shiny::tagList(
            shiny::tags$div(
              style = paste0(
                "padding:5px 0;max-height:300px;overflow-y:auto;"
              ),
              shiny::tags$div(
                style = "margin-bottom:5px;",
                shiny::actionLink(ns("show_all"), "Show all"),
                " | ",
                shiny::actionLink(ns("hide_all"), "Hide all")
              ),
              ui_items
            )
          )
        })

        child_keys_of <- shiny::reactive({
          discovered <- discovered_inputs()
          parents <- list()
          for (inp in discovered) {
            if (is.null(inp$parentKey)) next
            pk <- inp$parentKey
            parents[[pk]] <- c(parents[[pk]], inp$key)
          }
          parents
        })

        registered_keys <- character(0)

        shiny::observeEvent(discovered_inputs(), {
          discovered <- discovered_inputs()
          parents <- child_keys_of()
          new_items <- Filter(
            function(inp) !inp$key %in% registered_keys,
            discovered
          )
          if (length(new_items) == 0L) return()
          registered_keys <<- c(
            registered_keys,
            vapply(new_items, `[[`, character(1), "key")
          )

          lapply(new_items, function(inp) {
            cb_id <- .vis_cb_id(inp$key)
            is_parent <- inp$key %in% names(parents)

            shiny::observeEvent(input[[cb_id]],
              {
                visible <- isTRUE(input[[cb_id]])
                send_toggle(inp$key, visible)

              if (is_parent) {
                for (ck in parents[[inp$key]]) {
                  child_cb <- .vis_cb_id(ck)
                  shiny::updateCheckboxInput(
                    session,
                    child_cb,
                    value = visible
                  )
                  session$sendCustomMessage(
                    paste0(ns_prefix, "toggle_child_ui"),
                    list(cb_id = ns(child_cb), visible = visible)
                  )
                  send_toggle(ck, visible)
                }
              },
              ignoreInit = TRUE
            )
          })
        })

        shiny::observeEvent(input$show_all, {
          lapply(discovered_inputs(), function(inp) {
            cb_id <- .vis_cb_id(inp$key)
            shiny::updateCheckboxInput(session, cb_id, value = TRUE)
            send_toggle(inp$key, TRUE)
          })
        })

        shiny::observeEvent(input$hide_all, {
          lapply(discovered_inputs(), function(inp) {
            cb_id <- .vis_cb_id(inp$key)
            shiny::updateCheckboxInput(session, cb_id, value = FALSE)
            send_toggle(inp$key, FALSE)
          })
        })

        shiny::reactive(data())
      })
    }
  )
}

# ---- internal helpers ----

#' @keywords internal
.input_visibility_js_template <- function() {
  js_path <- system.file(
    "js",
    "input_visibility_transformator.js",
    package = "uteals"
  )
  paste(readLines(js_path, warn = FALSE), collapse = "\n")
}

#' Sanitise a vis-key into a valid checkbox input ID
#' @keywords internal
.vis_cb_id <- function(key) {
  paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", key))
}

#' Check whether a label should be visible by default
#' @keywords internal
.is_default_visible <- function(label, hidden_lower) {
  !any(vapply(hidden_lower, function(h) {
    grepl(h, tolower(label), fixed = TRUE)
  }, logical(1)))
}

#' Check whether a child's parent is visible by default
#' @keywords internal
.is_parent_visible <- function(parent_key, discovered, hidden_lower) {
  parent <- Filter(
    function(x) identical(x$key, parent_key),
    discovered
  )
  if (length(parent) == 0L) return(TRUE)
  .is_default_visible(parent[[1L]]$label, hidden_lower)
}

#' Normalise the discovered_inputs payload from JS
#'
#' Shiny deserialises the JS array-of-objects in different ways depending
#' on shape.
#' @param raw Raw input value from Shiny.
#' @return A list of lists, each with `key`, `label`, and optionally
#'   `parentKey`.
#' @keywords internal
.normalise_discovered <- function(raw) {
  if (is.data.frame(raw)) {
    lapply(seq_len(nrow(raw)), function(i) {
      item <- list(key = raw$key[i], label = raw$label[i])
      if ("parentKey" %in% names(raw) &&
        !is.na(raw$parentKey[i]) &&
        nzchar(raw$parentKey[i])) {
        item$parentKey <- raw$parentKey[i]
      }
      item
    })
  } else if (is.list(raw) && length(raw) > 0L && is.list(raw[[1L]])) {
    lapply(raw, function(x) {
      item <- list(
        key = as.character(x$key),
        label = as.character(x$label)
      )
      if (!is.null(x$parentKey) && nzchar(x$parentKey)) {
        item$parentKey <- as.character(x$parentKey)
      }
      item
    })
  } else if (is.character(raw) && !is.null(names(raw))) {
    keys <- unname(raw[names(raw) == "key"])
    labels <- unname(raw[names(raw) == "label"])
    Map(function(k, l) list(key = k, label = l), keys, labels)
  } else {
    list()
  }
}
