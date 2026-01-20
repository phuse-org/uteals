#' Combine levels of a variable into one level
#'
#' @description `r lifecycle::badge("experimental")`
#' @details
#' This transformator allows the user to select a column from the dataset
#' and combine values of this column into a single level. Only selected
#' levels are affected.
#'
#' The new combined level is called "Combined".
#'
#' Merging works only for `character` or `factor` columns.
#'
#' @param dataname (`character(1)`) the name of the dataset
#' which columns will be used for possible transformation.
#'
#' @import teal shiny shinyWidgets
#' @importFrom teal.code eval_code
#'
#' @return `teal::teal_transform_module`
#'
#' @examples
#'
#' app <- teal::init(
#'   data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
#'   modules = teal::modules(
#'     teal::example_module(
#'       transformators = list(merge_levels_transformator("IRIS"))
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shiny::shinyApp(app$ui, app$server)
#' }
#'
#' @export
merge_levels_transformator <- function(dataname) {
  teal::teal_transform_module(
    label = paste0("Merge level transformator"),
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$style(
          shiny::HTML(
            "
                    .centered-button {
                      display: flex;
                      justify-content: center;
                      align-items: center;
                      gap: 8px;
                    }
                  "
          )
        ),
        shiny::selectInput(
          inputId = ns("selected_columns"), label = "Selected column", choices = NULL, multiple = FALSE
        ),
        # namespaced container id so insert/remove UI works reliably
        shiny::div(id = ns("mapping_fluid_rows")),
        shiny::div(
          class = "centered-button",
          shiny::actionButton(ns("plus_button"), "Apply"),
          shiny::actionButton(ns("reset_button"), "Reset")
        )
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # reactive that returns levels for the currently selected column
        arm_levels <- shiny::eventReactive(input$selected_columns, {
          shiny::req(input$selected_columns)
          col_data <- data()[[dataname]][[input$selected_columns]]
          # levels() returns NULL for character vectors, convert to factor levels
          if (is.factor(col_data)) {
            levels(col_data)
          } else if (is.character(col_data)) {
            unique(col_data)
          } else {
            character(0)
          }
        })

        # Update the column names in the selectInput
        shiny::observe({
          trt_columns <- names(data()[[dataname]]) |> shiny::isolate()
          shiny::updateSelectInput(session, "selected_columns", choices = trt_columns)
        })

        # track mapping rows and store column association per row (so mappings persist
        # even if the user changes selected_columns)
        counter <- shiny::reactiveValues(ind = integer(0), prev_max = 0)
        id_names_map <- shiny::reactiveVal(list())

        # When selected column changes:
        # - if last existing mapping row is empty, just update its choices and store column
        # - otherwise create a new mapping row for the new column
        shiny::observeEvent(input$selected_columns, {
          shiny::req(input$selected_columns)

          if (length(counter$ind) > 0 && max(counter$ind) > 0) {
            last_idx <- max(counter$ind)
            prev_select_input_id <- paste0("col_levels_", last_idx)
            prev_text_input_id <- paste0("col_merged_name_", last_idx)

            # If the last mapping row hasn't been filled, update its choices and store column
            if (is.null(input[[prev_select_input_id]]) || input[[prev_text_input_id]] == "") {
              shiny::updateSelectInput(
                session,
                inputId = prev_select_input_id,
                choices = arm_levels()
              )
              current_rec <- id_names_map()
              current_rec[[as.character(last_idx)]] <- input$selected_columns
              id_names_map(current_rec)
              return()
            }
          }

          # create new mapping row id
          if (length(counter$ind) == 0) {
            counter$ind <- c(counter$prev_max + 1)
          } else {
            counter$ind <- c(counter$ind, max(counter$ind) + 1)
          }
          counter$prev_max <- max(counter$ind)
          # ensure no zero element
          counter$ind <- setdiff(counter$ind, 0)

          # store column name for this mapping row
          current_rec <- id_names_map()
          current_rec[[as.character(max(counter$ind))]] <- input$selected_columns
          id_names_map(current_rec)

          new_idx <- max(counter$ind)
          new_select_input_id <- paste0("col_levels_", new_idx)
          new_text_input_id <- paste0("col_merged_name_", new_idx)
          new_action_button_id <- paste0("close_button_", new_idx)

          shiny::insertUI(
            selector = paste0("#", ns("mapping_fluid_rows")),
            ui = shiny::div(
              id = ns(paste0("div_row_", new_idx)),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns(new_select_input_id),
                    label = paste0("Merged variable values: ", input$selected_columns),
                    choices = arm_levels(),
                    multiple = TRUE
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    inputId = ns(new_text_input_id), label = "Merged variable name", placeholder = "Combined"
                  )
                ),
                shiny::column(
                  width = 2,
                  br(),
                  br(),
                  shiny::actionButton(
                    inputId = session$ns(new_action_button_id),
                    label = "X",
                    class = "btn btn-primary",
                    style = "color: white;background: #ed6e6e;font-size: 10px;border: antiquewhite;
                    height: 28px;width: 28px;padding: 2px;"
                  )
                )
              )
            ),
            immediate = TRUE,
            multiple = TRUE
          )
        })

        # trigger to notify data_update that mapping rows changed (remove)
        trigger <- shiny::reactiveVal(0)

        # observe remove ("X") buttons for each mapping row
        shiny::observe({
          lapply(counter$ind, function(i) {
            close_id <- paste0("close_button_", i)
            shiny::observeEvent(input[[close_id]],
              {
                shiny::removeUI(selector = paste0("#", ns(paste0("div_row_", i))), immediate = TRUE, multiple = FALSE)
                counter$ind <- setdiff(counter$ind, i)
                # drop mapping from id_names_map as well
                current_rec <- id_names_map()
                current_rec[[as.character(i)]] <- NULL
                id_names_map(current_rec)
                trigger(trigger() + 1)
              },
              ignoreInit = TRUE
            )
          })
        })

        # reset button configuration
        reset_button_flag <- shiny::reactiveVal(FALSE)
        shiny::observeEvent(input$reset_button, {
          # remove all mapping rows
          shiny::removeUI(selector = paste0("#", ns("mapping_fluid_rows"), " div"), multiple = TRUE, immediate = TRUE)

          # prepare a fresh row id
          counter$ind <- (counter$prev_max + 1)
          counter$prev_max <- counter$ind

          current_rec <- id_names_map()
          current_rec[[as.character(max(counter$ind))]] <- input$selected_columns
          id_names_map(current_rec)

          new_select_input_id <- paste0("col_levels_", max(counter$ind))
          new_text_input_id <- paste0("col_merged_name_", max(counter$ind))
          new_action_button_id <- paste0("close_button_", max(counter$ind))

          shiny::insertUI(
            selector = paste0("#", ns("mapping_fluid_rows")),
            ui = shiny::div(
              id = ns(paste0("div_row_", max(counter$ind))),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns(new_select_input_id),
                    label = paste0("Merged variable values: ", input$selected_columns),
                    choices = arm_levels(),
                    multiple = TRUE
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    inputId = ns(new_text_input_id),
                    label = "Merged variable name",
                    placeholder = "Combined"
                  )
                ),
                shiny::column(
                  width = 2,
                  br(),
                  br(),
                  shiny::actionButton(
                    inputId = session$ns(new_action_button_id),
                    label = "X",
                    class = "btn btn-primary",
                    style = "color: white;background: #ed6e6e;font-size: 10px;border: antiquewhite;
                    height: 28px;width: 28px;padding: 2px;"
                  )
                )
              )
            ),
            immediate = TRUE,
            multiple = TRUE
          )

          reset_button_flag(TRUE)
        })

        # generate code dynamically based on Apply/Reset/row removal
        data_update <- shiny::eventReactive(list(input$plus_button, input$reset_button, trigger()), {
          # nothing to do if apply hasn't been clicked
          if (input$plus_button == 0) {
            return("")
          }

          # if reset was just clicked, do nothing (reset consumed)
          if (reset_button_flag()) {
            reset_button_flag(FALSE)
            return("")
          }

          replace_code <- ""
          # iterate over currently active mapping rows
          for (i in counter$ind) {
            # collect inputs for this mapping row
            sel_id <- paste0("col_levels_", i)
            txt_id <- paste0("col_merged_name_", i)
            col_levels <- input[[sel_id]]
            col_merged_name <- input[[txt_id]]
            # use stored column name for this mapping row
            col_name <- id_names_map()[[as.character(i)]]

            if (is.null(col_name) || col_name == "") next

            if ((length(col_levels) > 1) && (!is.null(col_merged_name)) && (col_merged_name != "")) {
              # build a chunk of R code as string that converts to character, replaces selected
              # values by new merged name, and converts back to factor
              replace_code <- paste0(
                replace_code,
                dataname,
                "[['",
                col_name,
                "']] <- replace(",
                dataname,
                "[['",
                col_name,
                "']], ",
                dataname,
                "[['",
                col_name,
                "']] %in% c('",
                paste(col_levels, collapse = "', '"),
                "'), '",
                col_merged_name,
                "'); "
              )
            }
          }

          if (replace_code != "") {
            # wrap with conversions to and from character/factor
            # use the last used col_name in the wrapper (safe because replace_code contains
            # explicit replacements for each mapping row)
            # but pick a column for wrapper — choose the last non-null mapping col_name if any
            wrapper_col <- NULL
            names_map <- id_names_map()
            if (length(names_map) > 0) {
              # get any non-null column name from names_map (prefer last)
              nm_vals <- unlist(names_map, use.names = FALSE)
              if (length(nm_vals) > 0) {
                wrapper_col <- nm_vals[length(nm_vals)]
              }
            }
            if (is.null(wrapper_col) || wrapper_col == "") {
              # fallback: no valid wrapper column found -> just return replace_code as-is
              replace_code
            } else {
              sprintf(
                "%s[['%s']] <- as.character(%s[['%s']]); %s%s[['%s']] <- as.factor(%s[['%s']]); ",
                dataname,
                wrapper_col,
                dataname,
                wrapper_col,
                replace_code,
                dataname,
                wrapper_col,
                dataname,
                wrapper_col
              )
            }
          } else {
            ""
          }
        })

        # Execute the generated code when Apply is clicked (or other triggers)
        shiny::reactive({
          teal.code::eval_code(data(), data_update())
        })
      })
    }
  )
}
