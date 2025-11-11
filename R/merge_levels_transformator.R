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
                    }
                  "
          )
        ),
        shiny::selectInput(
          inputId = ns("selected_columns"), label = "Selected column", choices = NULL, multiple = FALSE
        ),
        shiny::div(id = "mapping_fluid_rows"),
        shiny::div(class = "centered-button", shinyWidgets::circleButton(ns("plus_button"), icon = shiny::icon("plus")))
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        arm_levels <- shiny::eventReactive(input$selected_columns, {
          levels(data()[[dataname]][[input$selected_columns]])
        })

        # Update the column names in the selectInput
        shiny::observe({
          trt_columns <- names(data()[[dataname]]) |> shiny::isolate()
          shiny::updateSelectInput(session, "selected_columns", choices = trt_columns)
        })

        counter <- shiny::reactiveVal(0)

        shiny::observeEvent(input$selected_columns, {
          # reset a counter
          counter <- shiny::reactiveVal(0)
        })

        shiny::observeEvent(list(input$plus_button, input$selected_columns), {
          shiny::req(input$selected_columns)

          counter(counter() + 1)

          new_select_input_id <- paste0("col_levels_", counter())
          new_text_input_id <- paste0("col_merged_name_", counter())

          shiny::insertUI(
            selector = "#mapping_fluid_rows",
            ui = shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = ns(new_select_input_id),
                  label = "Merged variable values",
                  choices = arm_levels(),
                  multiple = TRUE
                )
              ),
              shiny::column(
                width = 5,
                shiny::textInput(
                  inputId = ns(new_text_input_id), label = "Merged variable name", placeholder = "Combined"
                )
              )
            )
          )
        })

        # generate code dynamically based on the input change & number of items from mapping:
        data_update <- shiny::reactive({
          if (input$plus_button == 0) {
            return("")
          }

          replace_code <- ""
          for (i in 1:counter()) {
            col_levels <- input[[paste0("col_levels_", i)]]
            col_merged_name <- input[[paste0("col_merged_name_", i)]]
            col_name <- input$selected_columns

            if ((length(col_levels) > 1) & (col_merged_name != "")) {
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
            sprintf(
              "%s[[%s]] <- as.character(%s[[%s]]); %s%s[[%s]] <- as.factor(%s[[%s]]); ",
              dataname,
              col_name,
              dataname,
              col_name,
              replace_code,
              dataname,
              col_name,
              dataname,
              col_name
            )
          } else {
            ""
          }
        })

        # Merge the selected levels into one
        shiny::reactive({
          teal.code::eval_code(data(), data_update())
        })
      })
    }
  )
}
