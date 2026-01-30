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
#'       transformators = list(uteals::merge_levels_transformator("IRIS"))
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
    label = paste0("Merge Column Levels - ", dataname),
    ui = function(id) {
      ns <- NS(id)

      tagList(
        tags$style(
          HTML(
            "
                    .centered-button {
                      display: flex;
                      justify-content: center;
                      align-items: center;
                      gap: 10px; /* Adjust spacing between buttons */
                    }
                  "
          )
        ),
        div(
          style = "float: right; margin-right: 10px;",
          tags$span(
            tags$i(
              class = "fa fa-info-circle info-icon",
              id = "infoIcon",
              title = "Select the column and merge the multiple levels to new level."
            )
          )
        ),
        shinyWidgets::pickerInput(
          inputId = ns("selected_columns"), label = "Selected column", choices = NULL, multiple = FALSE
        ),
        div(id = ns("mapping_fluid_rows")),
        div(
          class = "centered-button",
          actionButton(ns("plus_button"), label = "Apply"),
          actionButton(ns("reset_button"), label = "Reset")
        )
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        arm_levels <- eventReactive(input$selected_columns, {
          levels(data()[[dataname]][[input$selected_columns]])
        })

        # Update the column names in the selectInput
        observe({
          trt_columns <- names(data()[[dataname]])
          shinyWidgets::updatePickerInput(
            session,
            label = ,
            inputId = "selected_columns",
            choices = trt_columns,
            choicesOpt = list(subtext = formatters::var_labels(data()[[dataname]]))
          )
        })

        counter <- reactiveValues(ind = c(0), prev_max = 0)
        id_names_map <- reactiveVal(list())

        observeEvent(list(input$selected_columns), {
          req(input$selected_columns)

          if (max(counter$ind) > 0) {
            prev_select_inputid <- paste0("col_levels_", max(counter$ind))
            prev_text_inputid <- paste0("col_merged_name_", max(counter$ind))

            if (is.null(input[[prev_select_inputid]]) | (input[[prev_text_inputid]] == "")) {
              updateSelectInput(
                session,
                label = paste0("Merged levels :", input$selected_columns),
                inputId = prev_select_inputid,
                choices = arm_levels()
              )
              ## Store the Column label in reactive val
              current_rec <- id_names_map()
              current_rec[[max(counter$ind)]] <- input$selected_columns
              id_names_map(current_rec)

              return()
            }
          }

          if (length(counter$ind) == 0) { # case of one of close button was clicked
            counter$ind <- c(counter$prev_max + 1)
          } else {
            counter$ind <- c(counter$ind, max(counter$ind) + 1)
          }

          counter$prev_max <- max(counter$ind)
          counter$ind <- setdiff(counter$ind, 0)

          ## Store the Column label in reactive val
          current_rec <- id_names_map()
          current_rec[[max(counter$ind)]] <- input$selected_columns
          id_names_map(current_rec)

          new_select_inputid <- paste0("col_levels_", max(counter$ind))
          new_text_inputid <- paste0("col_merged_name_", max(counter$ind))
          new_action_buttonid <- paste0("close_button_", max(counter$ind))

          insertUI(
            selector = paste0("#", ns("mapping_fluid_rows")),
            ui = div(
              id = ns(paste0("div_row_", max(counter$ind))),
              fluidRow(
                column(
                  width = 5,
                  selectInput(
                    inputId = ns(new_select_inputid),
                    label = paste0("Merged levels :", input$selected_columns),
                    choices = arm_levels(),
                    multiple = TRUE
                  )
                ),
                column(
                  width = 4,
                  textInput(inputId = ns(new_text_inputid),
                            label = "Merged level name",
                            placeholder = "Enter new level")
                ),
                column(
                  width = 2,
                  br(),
                  br(),
                  actionButton(
                    inputId = session$ns(new_action_buttonid),
                    label = "X",
                    class = "btn btn-primary",
                    style = paste0(
                      "color: white;background: #ed6e6e;font-size:",
                      " 10px;border: antiquewhite;height:",
                      " 19px;width: 17px;padding: 2px;"
                    )
                  )
                )
              )
            ),
            immediate = TRUE,
            multiple = TRUE
          )
        })

        trigger <- reactiveVal(0)

        observe({
          lapply(counter$ind, function(i) {
            observeEvent(input[[paste0("close_button_", i)]], {
              removeUI(paste0("#", ns(paste0("div_row_", i))), TRUE, TRUE)

              counter$ind <- setdiff(counter$ind, i)
              trigger(trigger() + 1)
            })
          })
        })

        reset_button_flag <- reactiveVal(FALSE)

        # reset button configuration
        observeEvent(input$reset_button, {
          removeUI(selector = paste0("#", ns("mapping_fluid_rows"), " div"), multiple = TRUE, immediate = TRUE)

          counter$ind <- (counter$prev_max + 1)
          counter$prev_max <- counter$ind
          new_select_inputid <- paste0("col_levels_", max(counter$ind))
          new_text_inputid <- paste0("col_merged_name_", max(counter$ind))
          new_action_buttonid <- paste0("close_button_", max(counter$ind))

          current_rec <- id_names_map()
          current_rec[[max(counter$ind)]] <- input$selected_columns
          id_names_map(current_rec)

          insertUI(
            selector = paste0("#", ns("mapping_fluid_rows")),
            ui = div(
              id = ns(paste0("div_row_", max(counter$ind))),
              fluidRow(
                column(
                  width = 5,
                  selectInput(
                    inputId = ns(new_select_inputid),
                    label = paste0("Merged levels :", input$selected_columns),
                    choices = arm_levels(),
                    multiple = TRUE
                  )
                ),
                column(
                  width = 4,
                  textInput(inputId = ns(new_text_inputid),
                            label = "Merged level name",
                            placeholder = "Enter new level")
                ),
                column(
                  width = 2,
                  br(),
                  br(),
                  actionButton(
                    inputId = session$ns(new_action_buttonid),
                    label = "X",
                    class = "btn btn-primary",
                    style = paste0("color: white;background: #ed6e6e;font-size:",
                                   " 10px;border: antiquewhite;height: ",
                                   "19px;width: 17px;padding: 2px;")
                  )
                )
              )
            ),
            immediate = TRUE,
            multiple = TRUE
          )

          reset_button_flag(TRUE)
        })

        # generate code dynamically based on the input change & number of items from mapping:
        data_update <- eventReactive(list(input$plus_button, input$reset_button, trigger()), {
          final <- list()
          data_list <- list()

          if (input$plus_button == 0) {
            return(data_list)
          }

          if (reset_button_flag()) {
            reset_button_flag(FALSE)
            return(data_list)
          }

          for (i in counter$ind) {
            col_levels <- input[[paste0("col_levels_", i)]]
            col_merged_name <- input[[paste0("col_merged_name_", i)]]
            col_name <- id_names_map()[[i]]

            if ((length(col_levels) > 1) & (col_merged_name != "")) {
              data_list <- teal.modules.clinical.junco::add_expr(data_list, {
                substitute(
                  expr = {
                    dataname[[col_name]] <- as.character(dataname[[col_name]])
                    dataname[[col_name]] <- ifelse(
                      dataname[[col_name]] %in% col_levels, col_merged_name, dataname[[col_name]]
                    )
                    dataname[[col_name]] <- as.factor(dataname[[col_name]])
                  },
                  env = list(
                    dataname = as.name(dataname),
                    col_name = col_name,
                    col_levels = col_levels,
                    col_merged_name = col_merged_name
                  )
                )
              })
            }
          }
          final$data <- teal.modules.clinical.junco::bracket_expr(data_list)
          final
        })

        # Merge the selected levels into one
        reactive({
          teal.code::eval_code(data(), as.expression(data_update()))
        })
      })
    }
  )
}
