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
#' tm_t_frequency_j(
#'   ...,
#'   transformators = list(
#'     input_visibility_transformator(
#'       hidden_by_default = c("Stratify", "Risk Difference")
#'     )
#'   )
#' )
#' }
#'
#' @keywords internal
input_visibility_transformator <- function(label = "Toggle Input Visibility",
                                           hidden_by_default = character(0)) {
  checkmate::assert_string(label)
  checkmate::assert_character(hidden_by_default, any.missing = FALSE)

  teal::teal_transform_module(
    label = label,
    ui = function(id) {
      ns <- shiny::NS(id)
      hidden_json <- jsonlite::toJSON(tolower(hidden_by_default), auto_unbox = FALSE)
      ns_prefix <- sub("__x__$", "", ns("__x__"))

      # Build JS by substitution
      js <- .input_visibility_js_template()
      js <- gsub("__NS_PREFIX__", ns_prefix, js, fixed = TRUE)
      js <- gsub("__HIDDEN_DEFAULTS__", hidden_json, js, fixed = TRUE)
      js <- gsub("__CONTROLS_ID__", ns("visibility_controls"), js, fixed = TRUE)

      message("[vis] UI ns_prefix=", ns_prefix)

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
        message("[vis] server ns_prefix=", ns_prefix)

        send_toggle <- function(key, visible) {
          session$sendCustomMessage(msg_type, list(key = key, visible = visible))
        }

        # Normalize discovered inputs from JS.
        # Shiny flattens [{key:"a",label:"b"}, ...] into a flat named
        # character vector: c(key="a", label="b", key="c", label="d", ...)
        discovered_inputs <- shiny::reactive({
          raw <- input$discovered_inputs
          shiny::req(raw)
          result <- if (is.data.frame(raw)) {
            lapply(seq_len(nrow(raw)), function(i) {
              item <- list(key = raw$key[i], label = raw$label[i])
              if (!is.null(raw$parentKey) && !is.na(raw$parentKey[i]) && nzchar(raw$parentKey[i]))
                item$parentKey <- raw$parentKey[i]
              item
            })
          } else if (is.list(raw) && length(raw) > 0 && is.list(raw[[1]])) {
            lapply(raw, function(x) {
              item <- list(key = as.character(x$key), label = as.character(x$label))
              if (!is.null(x$parentKey) && nzchar(x$parentKey)) item$parentKey <- as.character(x$parentKey)
              item
            })
          } else if (is.character(raw) && !is.null(names(raw))) {
            keys <- unname(raw[names(raw) == "key"])
            labels <- unname(raw[names(raw) == "label"])
            Map(function(k, l) list(key = k, label = l), keys, labels)
          } else {
            list()
          }
          message("[vis] discovered ", length(result), " inputs")
          for (r in result) message("  key=", r$key, " label=", r$label, if (!is.null(r$parentKey)) paste0(" parent=", r$parentKey) else "")
          result
        })

        output$toggle_ui <- shiny::renderUI({
          discovered <- discovered_inputs()
          hidden_lower <- tolower(hidden_by_default)

          # Build parent-child map: parent keys -> list of child keys
          # Children have "sub_" in their key; the parent prefix before "sub_" matches the parent key
          is_child <- vapply(discovered, function(inp) grepl("sub_", inp$key, fixed = TRUE), logical(1))

          ui_items <- lapply(seq_along(discovered), function(i) {
            inp <- discovered[[i]]
            cb_id <- paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", inp$key))
            default_visible <- !any(vapply(hidden_lower, function(h) {
              grepl(h, tolower(inp$label), fixed = TRUE)
            }, logical(1)))
            cb <- shiny::checkboxInput(ns(cb_id), label = inp$label, value = default_visible)
            if (is_child[i]) {
              # Wrap child checkboxes in a div we can show/hide as a group
              shiny::tags$div(
                class = "vis-child-item",
                style = paste0("margin-left:15px;", if (!default_visible) "display:none;" else ""),
                cb
              )
            } else {
              cb
            }
          })

          shiny::tagList(
            shiny::tags$div(
              style = "padding:5px 0;max-height:300px;overflow-y:auto;",
              shiny::tags$div(
                style = "margin-bottom:5px;",
                shiny::actionLink(ns("show_all"), "Show all"), " | ",
                shiny::actionLink(ns("hide_all"), "Hide all")
              ),
              ui_items
            )
          )
        })

        # Build parent -> children map directly from parentKey field sent by JS
        child_keys_of <- shiny::reactive({
          discovered <- discovered_inputs()
          parents <- list()
          for (inp in discovered) {
            if (is.null(inp$parentKey)) next
            parents[[inp$parentKey]] <- c(parents[[inp$parentKey]], inp$key)
          }
          parents
        })

        # Track which keys already have observers registered
        registered_keys <- character(0)

        # Per-checkbox observers — re-runs when discovered_inputs changes
        # to register observers for newly discovered items
        shiny::observeEvent(discovered_inputs(), {
          discovered <- discovered_inputs()
          parents <- child_keys_of()
          new_items <- Filter(function(inp) !inp$key %in% registered_keys, discovered)
          if (length(new_items) == 0) return()
          registered_keys <<- c(registered_keys, vapply(new_items, `[[`, character(1), "key"))

          lapply(new_items, function(inp) {
            cb_id <- paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", inp$key))
            is_parent <- inp$key %in% names(parents)

            shiny::observeEvent(input[[cb_id]], {
              visible <- isTRUE(input[[cb_id]])
              send_toggle(inp$key, visible)

              if (is_parent) {
                child_keys <- parents[[inp$key]]
                for (ck in child_keys) {
                  child_cb_id <- paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", ck))
                  if (visible) {
                    session$sendCustomMessage(
                      paste0(ns_prefix, "toggle_child_ui"),
                      list(cb_id = ns(child_cb_id), visible = TRUE)
                    )
                    child_visible <- isTRUE(input[[child_cb_id]])
                    send_toggle(ck, child_visible)
                  } else {
                    session$sendCustomMessage(
                      paste0(ns_prefix, "toggle_child_ui"),
                      list(cb_id = ns(child_cb_id), visible = FALSE)
                    )
                    send_toggle(ck, FALSE)
                  }
                }
              }
            }, ignoreInit = TRUE)
          })
        })

        shiny::observeEvent(input$show_all, {
          lapply(discovered_inputs(), function(inp) {
            cb_id <- paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", inp$key))
            shiny::updateCheckboxInput(session, cb_id, value = TRUE)
            send_toggle(inp$key, TRUE)
          })
        })

        shiny::observeEvent(input$hide_all, {
          lapply(discovered_inputs(), function(inp) {
            cb_id <- paste0("vis_", gsub("[^a-zA-Z0-9_]", "_", inp$key))
            shiny::updateCheckboxInput(session, cb_id, value = FALSE)
            send_toggle(inp$key, FALSE)
          })
        })

        shiny::reactive(data())
      })
    }
  )
}

# ---- JS template (uses __PLACEHOLDER__ tokens, not sprintf %s) ----

#' @keywords internal
.input_visibility_js_template <- function() {
  js_path <- system.file("js", "input_visibility_transformator.js", package = "uteals")
  paste(readLines(js_path, warn = FALSE), collapse = "\n")
}
