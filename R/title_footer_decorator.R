#' Title and Footer Decorator
#'
#' @description `r lifecycle::badge("experimental")`
#' A function to create a UI component for selecting a title and footer
#' for tables or plots. It reads title information from a specified Excel
#' file and allows users to choose a title from the provided options.
#' It also provides user with flexibility to customize the title and footer
#' according to their specific needs.
#' @param output_name (`character(1)`) a name for the output object (e.g., a plot or table).
#' @param titles_file (`character(1)`) the path to an Excel file containing title and footer
#' information. The function expects the titles to be in the first sheet
#' named `Sheet1`.
#' @param choices (`character`) an array of titles and footers, which are available for
#' selection. Default `NULL`, indicates all titles and footers are available.
#' @param selected (`character(1)`) the selected title or footer. Default `NULL`,
#' indicates no title or footer is selected.
#'
#' @return [`teal::teal_transform_module()`]
#'
#' @details The module creates a UI with a dropdown for selecting a title.
#' Once a title is selected, it updates the output by either adding titles to
#' a table or modifying a plot's title and caption accordingly.
#' Additionally, it includes a checkbox that user can check to enable
#' customization, allowing them to enter their own values for the title and
#' footer in the designated input fields.
#'
#' @seealso For the exact Excel workbook layout expected by this function, see the package vignette:
#' `vignette("title-footer-decorator-excel-structure", package = "uteals")`
#'
#' @import openxlsx ggplotify ggplot2 patchwork teal shiny
#' @importFrom grDevices graphics.off
#'
#' @examples
#' library(openxlsx)
#' library(teal.modules.general)
#'
#' example_excel <- data.frame(
#'   `TABLE ID` = c(
#'     "DO_NOT_DELETE",
#'     "TSFAE01A", "TSFAE01A", "TSFAE01A",
#'     "TSFAE01B", "TSFAE01B"
#'   ),
#'   IDENTIFIER = c(
#'     "DO_NOT_DELETE",
#'     "TITLE", "FOOTNOTE1", "FOOTNOTE2",
#'     "TITLE", "FOOTNOTE1"
#'   ),
#'   TEXT = c(
#'     "DO_NOT_DELETE",
#'     "Adverse Events Summary A", "Source: Clinical Study Report", "Confidential",
#'     "Adverse Events Summary B", "Draft Version"
#'   ),
#'   stringsAsFactors = FALSE,
#'   check.names = FALSE
#' )
#'
#' temp_titles <- tempfile(fileext = ".xlsx")
#' write.xlsx(example_excel, temp_titles, sheetName = "Sheet1", asTable = TRUE)
#' plot_module <- tm_g_scatterplot(
#'   label = "Scatter Plot",
#'   x = data_extract_spec(
#'     dataname = "IRIS",
#'     select = select_spec(
#'       choices = variable_choices("IRIS", c("Sepal.Length", "Sepal.Width")), selected = "Sepal.Length"
#'     )
#'   ),
#'   y = data_extract_spec(
#'     dataname = "IRIS",
#'     select = select_spec(
#'       choices = variable_choices("IRIS", c("Petal.Length", "Petal.Width")), selected = "Petal.Length"
#'     )
#'   ),
#'   decorators = list(
#'     plot = title_footer_decorator("plot", temp_titles, choices = c("TSFAE01A", "TSFAE01B"), selected = NULL)
#'   )
#' )
#'
#' # Initialize the teal app
#' app <- init(
#'   data = teal_data(IRIS = iris),
#'   modules = list(plot_module)
#' )
#'
#' # Run the app
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
title_footer_decorator <- function(output_name, titles_file, choices = NULL, selected = NULL) {
  checkmate::assert_string(output_name)
  checkmate::assert_string(titles_file)
  checkmate::assert_character(choices, null.ok = TRUE)
  checkmate::assert_string(selected, null.ok = TRUE)

  titles <- openxlsx::read.xlsx(titles_file, "Sheet1")
  titles$TABLE.ID[1] <- "blank"

  choices <- `if`(is.null(choices), unique(titles$TABLE.ID), intersect(choices, titles$TABLE.ID))
  checkmate::assert(
    checkmate::check_null(selected),
    \() `if`(selected %in% choices, TRUE, "selected must be one of the choices")
  )


  teal::teal_transform_module(
    label = "Title and footer decorator",
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div(
          selectInput(ns("selectTitle"), label = "Select Title", choices = choices, selected = selected),
          checkboxInput(ns("customize"), label = "Customize Title and Footer", value = FALSE),
          uiOutput(ns("customInputs"))
        )
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Render UI for custom title and footer when checkbox is checked
        output$customInputs <- renderUI({
          if (input$customize) {
            tagList(
              textInput(session$ns("customTitle"), label = "Custom Title", value = ""),
              textInput(session$ns("customFooter"), label = "Custom Footer", value = "")
            )
          }
        })
        reactive({
          req(data())

          res <- within(
            data(),
            {
              grDevices::graphics.off()
              titles_footers <- list()
              titles_footers$title <- ""
              titles_footers$main_footer <- ""
            },
            titles_file = titles_file,
            output_type = output_name,
            output_name = as.name(output_name),
            selectTitle = input$selectTitle,
            customize = input$customize,
            customTitle = input$customTitle,
            customFooter = input$customFooter
          )
          # Determine the title and footer
          if (input$customize) {
            res <- within(
              res,
              {
                titles_footers$title <- if (!is.null(customTitle)) customTitle else "default"
                titles_footers$main_footer <- if (!is.null(customFooter)) customFooter else "default"
              },
              titles_file = titles_file,
              output_type = output_name,
              output_name = as.name(output_name),
              selectTitle = input$selectTitle,
              customize = input$customize,
              customTitle = input$customTitle,
              customFooter = input$customFooter
            )
          } else if (input$selectTitle != "blank") {
            res <- within(
              res,
              {
                titles_footers <- junco::get_titles_from_file(selectTitle, file = titles_file)
              },
              titles_file = titles_file,
              output_type = output_name,
              output_name = as.name(output_name),
              selectTitle = input$selectTitle,
              customize = input$customize,
              customTitle = input$customTitle,
              customFooter = input$customFooter
            )
          }

          if (output_name == "table") {
            res <- within(
              res,
              {
                output_name <- junco::set_titles(output_name, titles_footers)
              },
              titles_file = titles_file,
              output_type = output_name,
              output_name = as.name(output_name),
              selectTitle = input$selectTitle,
              customize = input$customize,
              customTitle = input$customTitle,
              customFooter = input$customFooter
            )
          } else {
            res <- within(
              res,
              {
                if (length(titles_footers$main_footer) > 1) {
                  titles_footers$main_footer <- paste(titles_footers$main_footer, collapse = "\n")
                }

                if ((inherits(output_name, "ggplot")) && (inherits(output_name, "patchwork"))) {
                  output_name <- output_name +
                    patchwork::plot_annotation(title = titles_footers$title, caption = titles_footers$main_footer)
                } else if ((inherits(output_name, "ggplot")) && !(inherits(output_name, "patchwork"))) {
                  output_name <- output_name +
                    ggplot2::labs(title = titles_footers$title, caption = titles_footers$main_footer) +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
                } else if ((inherits(output_name, "trellis")) || (inherits(output_name, "grob"))) {
                  output_name <- ggplotify::as.ggplot(output_name) +
                    ggplot2::labs(title = titles_footers$title, caption = titles_footers$main_footer) +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
                } else {
                  output_name <- output_name
                }
              },
              titles_file = titles_file,
              output_type = output_name,
              output_name = as.name(output_name),
              selectTitle = input$selectTitle,
              customize = input$customize,
              customTitle = input$customTitle,
              customFooter = input$customFooter
            )
          }
        })
      })
    }
  )
}
