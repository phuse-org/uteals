#' Watermark Decorator
#'
#' @description `r lifecycle::badge("experimental")`
#' A function to create a UI component for selecting watermark text
#' for tables or plots. 
#' @param output_name (`character(1)`) a name for the output object (e.g., a plot or table).
#' @param watermark_text (`character(1)`) text to display for the watermark
#' @param font_size (`character(1)`) font size for the watermark text
#'
#' @return [`teal::teal_transform_module()`]
#'
#' @details The module creates a UI with textInput for specifying watermark text and 
#' font size.
#' the entered watermark text is displayed with a default gridify layout.
#'
#' @import cowplot gridify rtables.officer
#' @importFrom grDevices graphics.off
#'
#' @export
watermark_decorator <- function(output_name, watermark_text = "", font_size = 90) {
  checkmate::assert_string(output_name)
  checkmate::assert_string(watermark_text)
  
  teal::teal_transform_module(
    label = "Watermark decorator",
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div(
          textInput(ns("txtWatermark"), label = "Enter Text", value = watermark_text),
          numericInput(ns("numFontsize"), label = "Enter Font size", value = font_size)
        )
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          req(data())
          
          res <- data()
          # Determine the title and footer
          res <- within(
            res,
            {
              watermark_text <- txtWatermark
              gridify_layout <- gridify::gridifyLayout(
                nrow = 3L,
                ncol = 1L,
                heights = grid::unit(c(0.05, 0.9, 0.05), "npc"),
                widths = grid::unit(1, "npc"),
                margin = grid::unit(c(t = 0.1, r = 0.1, b = 0.1, l = 0.1), units = "cm"),
                global_gpar = grid::gpar(),
                background = grid::get.gpar()$fill,
                adjust_height = FALSE,
                object = gridifyObject(row = 2, col = 1),
                cells = gridifyCells(
                  title = gridifyCell(row = 1, col = 1),
                  footer = gridifyCell(row = 3, col = 1),
                  watermark = gridifyCell(row = 1:3, col = 1, rot = 45, gpar = grid::gpar(fontsize = numFontsize, alpha = 0.3))
                )
              )
            },
            output_type = output_name,
            output_name = as.name(output_name),
            numFontsize = input$numFontsize,
            txtWatermark = input$txtWatermark
          )
          
          if (output_name == "table") {
            res <- within(
              res,
              {
                output_name <- gridify::gridify(
                  object = rtables.officer::tt_to_flextable(output_name, theme = NULL),
                  layout = gridify_layout
                ) %>%
                  set_cell("watermark", watermark_text)

              },
              output_name = as.name(output_name)
            )
          } else {
            res <- within(
              res,
              {
                output_name <- gridify::gridify(
                  object = cowplot::as_grob(output_name),
                  layout = gridify_layout
                ) %>%
                  set_cell("watermark", watermark_text)
              },
              output_name = as.name(output_name)
            )
          }
        })
      })
    }
  )
}
