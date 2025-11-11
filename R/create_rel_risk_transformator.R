#' Create Relative risk column
#'
#' @description `r lifecycle::badge("experimental")`
#' @details
#' This transformator allows the user to select a column and control group from the dataset
#' and create a relative risk column.
#'
#' @param dataname (`character(1)`) the name of the dataset
#' which columns will be used for possible transformation.
#' And dataname should be passed in quotes ex: "ADSL".
#' @param column_name (`character(1)`) field or variable from the dataset.
#' @param control_group (`character(1)`) one of the existing level from the selected `column_name`.
#' @param label_name (`character(1)`) label for the new field or variable.
#'
#' @import teal shiny
#'
#' @return `teal::teal_transform_module`
#'
#' @examples
#'
#' app <- teal::init(
#'   data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
#'   modules = teal::modules(
#'     teal::example_module(
#'       transformators = list(create_rel_risk_transformator("IRIS"))
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shiny::shinyApp(app$ui, app$server)
#' }
#'
#' @export
create_rel_risk_transformator <- function(dataname, column_name, control_group, label_name) {
  teal::teal_transform_module(
    label = paste0("Relative Risk Transformator"),
    ui = function(id) {
      ns <- NS(id)
      tagList(
        selectInput(inputId = ns("selected_columns"), label = "Selected Variable", choices = NULL, multiple = FALSE),
        selectInput(inputId = ns("sel_cntrl_grp"), label = "Selected Control group", choices = NULL, multiple = FALSE),
        textInput(inputId = ns("col_label"), label = "Enter the Label", value = label_name)
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Update the column names in the selectInput
        observe({
          df <- data()[[dataname]]
          df_cols <- names(df[sapply(df, function(x) (is.factor(x) | is.character(x)))])
          updateSelectInput(session, "selected_columns", choices = df_cols, selected = column_name)
        })

        observe({
          df <- data()[[dataname]]
          levels <- levels(df[[input$selected_columns]])
          updateSelectInput(session, inputId = "sel_cntrl_grp", choices = levels, selected = control_group)
        })

        # Merge the selected levels into one
        reactive({
          if (nchar(input$col_label) > 0) {
            within(
              data(),
              {
                df[["rrisk_header"]] <- "Risk Difference (%) (95% CI)"
                df[[col_label]] <- paste(df[[col_name]], "vs", cntrl_grp)
                ## converting the column to factor
                df[[col_label]] <- as.factor(df[[col_label]])
              },
              arm_col = as.name(input$selected_columns),
              col_name = input$selected_columns,
              cntrl_grp = input$sel_cntrl_grp,
              col_label = input$col_label,
              df = as.name(dataname)
            )
          }
        })
      })
    }
  )
}
