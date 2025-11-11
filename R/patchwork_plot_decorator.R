#' Patchwork Decorator
#'
#' @description `r lifecycle::badge("experimental")`
#' Decorator function to add plot title and footnote to patchwork plots
#' @param output_name (`character(1)`) A name for the output plot object.
#' @param label_text (`character(1)`) A customized label text for the decorator.
#' @return (`teal.data::qenv`) Returns a modified plot object with the transformation applied.
#'
#' @details The module creates a UI with text controls for plot title and footnote.
#' The entered title and footnote text are applied to the patchwork plots.
#'
#' @import teal shiny patchwork
#'
#' @export
patchwork_plot_decorator <- function(output_name, label_text = "decorator") {
  teal::teal_transform_module(
    label = label_text,
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::textInput(inputId = ns("plot_title"), label = "Specify plot title", value = ""),
        shiny::textInput(inputId = ns("plot_footnote"), label = "Specify plot_footnote", value = ""),
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive({
          shiny::req(data())

          data1 <- data()

          if (input$plot_title != "") {
            data1 <- within(
              data(),
              {
                output_name <- output_name + patchwork::plot_annotation(title = plot_title)
              },
              output_name = as.name(output_name),
              plot_title = input$plot_title
            )
          }

          data2 <- data1

          if (input$plot_footnote != "") {
            data2 <- within(
              data1,
              {
                output_name <- output_name + patchwork::plot_annotation(caption = plot_footnote)
              },
              output_name = as.name(output_name),
              plot_footnote = input$plot_footnote
            )
          }

          within(
            data2,
            {
              output_name
            },
            output_name = as.name(output_name)
          )
        })
      })
    }
  )
}
