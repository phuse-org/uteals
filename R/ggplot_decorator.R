#' ggplot Decorator
#'
#' @description `r lifecycle::badge("experimental")`
#' Decorator function to update various settings for ggplot plot objects
#' @param output_name A name for the output plot object.
#' @param label_text Customized label text for the decorator
#' @param render_ui Vector of ggplot_options from the following list:
#' "title" - Title of the plot,
#' "footnote" - Footnote of the plot,
#' "y_breaks" - Value of breaks(numeric) for y-axis. Note: y_limits_max and y_limits_min
#' should also be provided,
#' "y_limits_max" - Value of y-axis maximum limit(numeric). Note: y_limits_max and y_breaks
#' should also be provided,
#' "y_limits_min" - Value of y-axis minimum limit(numeric). Note: y_breaks and y_limits_min
#' should also be provided,
#' "x_breaks"- Value of breaks for continuous x-axis(numeric). Note: should be comma separated,
#' "x_labels_discrete" - Values of labels for discrete x-axis. Note: should be comma separated,
#' "x_labels_cont" - Values of labels for continuous x-axis. Note: should be comma separated,
#' "y_labels_discrete" - Values of labels for discrete y-axis. Note: should be comma separated,
#' "y_labels_cont" - Values of labels for continuous y-axis. Note: should be comma separated,
#' "font_size_geom_text" - Font size of geom_text. Note: numeric value should be provided,
#' "font_size_plot_title"- Font size of plot title text. Note: numeric value should be provided,
#' "font_size_axis_title"- Font size of axis title text. Note: numeric value should be provided,
#' "font_size_axis_text"- Font size of axis labels text. Note: numeric value should be provided
#' @param plot_options Named list with the list of values for the ggplot options.
#' The app developer can specify the required list of options while calling the decorator.
#'
#' @return Returns a modified plot object with the transformation applied.
#'
#' @details The module creates a UI with text controls for specifying the list
#' of ggplot options given in the plot_options param value.
#' The entered ggplot options are applied to ggplot plot object.
#'
#' @import teal shiny ggplot2
#'
#' @examples
#' library(teal)
#' library(teal.modules.general)
#' app <- teal::init(
#'   data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
#'   modules = teal::modules(
#'     teal.modules.general::tm_g_scatterplot(
#'       x = teal.transform::data_extract_spec(
#'         dataname = "IRIS",
#'         select = teal.transform::select_spec(choices = teal.transform::variable_choices(iris))
#'       ),
#'       y = teal.transform::data_extract_spec(
#'         dataname = "IRIS",
#'         select = teal.transform::select_spec(choices = teal.transform::variable_choices(iris))
#'       ),
#'       decorators = list(
#'         plot = ggplot_decorator(
#'           output_name = "plot", render_ui = c("title", "footnote", "font_size_axis_title")
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
ggplot_decorator <- function(output_name,
                             label_text = "decorator",
                             render_ui = c(),
                             plot_options = list(
                               "title" = "",
                               "footnote" = "",
                               "y_breaks" = "",
                               "y_limits_max" = "",
                               "y_limits_min" = "",
                               "x_breaks" = "",
                               "x_labels_discrete" = "",
                               "x_labels_cont" = "",
                               "y_labels_discrete" = "",
                               "y_labels_cont" = "",
                               "font_size_geom_text" = "",
                               "font_size_plot_title" = "",
                               "font_size_axis_title" = "",
                               "font_size_axis_text" = ""
                             )) {
  supported_settings <- list(
    "title" = list(label = "Specify plot title", ns = "ggplot_title"),
    "footnote" = list(label = "Specify plot footnote", ns = "ggplot_footnote"),
    "y_breaks" = list(label = "Specify break intervals for Y-axis", ns = "y_breaks"),
    "y_limits_max" = list(label = "Specify max limit for Y-axis", ns = "y_limits_max"),
    "y_limits_min" = list(label = "Specify min limit for Y-axis", ns = "y_limits_min"),
    "x_breaks" = list(label = "Specify breaks for X-axis (separated by comma)", ns = "x_breaks"),
    "x_labels_discrete" = list(
      label = "Specify labels for X-axis - discrete (separated by comma)", ns = "x_labels_discrete"
    ),
    "x_labels_cont" = list(label = "Specify labels for X-axis - continuous (separated by comma)", ns = "x_labels_cont"),
    "y_labels_discrete" = list(
      label = "Specify labels for Y-axis - discrete (separated by comma)", ns = "y_labels_discrete"
    ),
    "y_labels_cont" = list(label = "Specify labels for Y-axis - continuous (separated by comma)", ns = "y_labels_cont"),
    "font_size_geom_text" = list(label = "Specify font size for geom text", ns = "font_size_geom_text"),
    "font_size_plot_title" = list(label = "Specify font size for plot title", ns = "font_size_plot_title"),
    "font_size_axis_title" = list(label = "Specify font size for axis title", ns = "font_size_axis_title"),
    "font_size_axis_text" = list(label = "Specify font size for axis text", ns = "font_size_axis_text")
  )
  to_render <- match.arg(render_ui, names(supported_settings), several.ok = TRUE)

  teal::teal_transform_module(
    label = label_text,
    ui = function(id) {
      ns <- shiny::NS(id)

      input_tag_list <- shiny::tagList()

      for (supported_setting in names(supported_settings)) {
        if (supported_setting %in% to_render) {
          input_tag_list <- shiny::tagAppendChild(
            input_tag_list,
            shiny::textInput(
              inputId = ns(supported_settings[[supported_setting]]$ns),
              label = supported_settings[[supported_setting]]$label,
              value = plot_options[[supported_setting]]
            )
          )
        }
      }

      input_tag_list
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        shiny::reactive({
          shiny::req(data())

          data1 <- data()

          if ("title" %in% to_render) {
            if (!is.null(input$ggplot_title) && input$ggplot_title != "") {
              data1 <- within(
                data(),
                {
                  output_name <- output_name +
                    ggplot2::labs(title = plot_title) +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
                },
                output_name = as.name(output_name),
                plot_title = input$ggplot_title
              )
            }
          }

          data2 <- data1

          if ("footnote" %in% to_render) {
            if (!is.null(input$ggplot_footnote) && input$ggplot_footnote != "") {
              data2 <- within(
                data1,
                {
                  output_name <- output_name + ggplot2::labs(caption = plot_footnote)
                },
                output_name = as.name(output_name),
                plot_footnote = input$ggplot_footnote
              )
            }
          }

          data3 <- data2

          if (("y_limits_min" %in% to_render) && ("y_limits_max" %in% to_render) && ("y_breaks" %in% to_render)) {
            if (
              (!is.null(input$y_limits_min) && input$y_limits_min != "") &&
                (!is.null(input$y_limits_max) && input$y_limits_max != "") &&
                (!is.null(input$y_breaks) && input$y_breaks != "")
            ) {
              data3 <- within(
                data2,
                {
                  y_breaks_val <- seq(as.numeric(y_limits_min), as.numeric(y_limits_max), by = as.numeric(y_breaks))

                  output_name <- output_name +
                    ggplot2::scale_y_continuous(
                      breaks = y_breaks_val, limits = c(as.numeric(y_limits_min), as.numeric(y_limits_max))
                    )
                },
                output_name = as.name(output_name),
                y_breaks = input$y_breaks,
                y_limits_max = input$y_limits_max,
                y_limits_min = input$y_limits_min
              )
            }
          }

          data4 <- data3

          if ("y_labels_cont" %in% to_render) {
            if (!is.null(input$y_labels_cont) && input$y_labels_cont != "") {
              data4 <- within(
                data3,
                {
                  output_name <- output_name +
                    ggplot2::scale_y_continuous(
                      breaks = layer_scales(output_name)$y$break_positions(),
                      labels = trimws(str_split(y_labels_cont, ",")[[1]])
                    )
                },
                output_name = as.name(output_name),
                y_labels_cont = input$y_labels_cont
              )
            }
          }

          data5 <- data4

          if ("x_breaks" %in% to_render) {
            if (!is.null(input$x_breaks) && input$x_breaks != "") {
              data5 <- within(
                data4,
                {
                  output_name <- output_name +
                    ggplot2::scale_x_continuous(breaks = trimws(str_split(x_breaks, ",")[[1]]))
                },
                output_name = as.name(output_name),
                x_breaks = input$x_breaks
              )
            }
          }

          data6 <- data5
          if ("x_labels_cont" %in% to_render) {
            if (!is.null(input$x_labels_cont) && input$x_labels_cont != "") {
              data6 <- within(
                data5,
                {
                  output_name <- output_name +
                    ggplot2::scale_x_continuous(
                      breaks = layer_scales(output_name)$x$break_positions(),
                      labels = trimws(str_split(x_labels_cont, ",")[[1]])
                    )
                },
                output_name = as.name(output_name),
                x_labels_cont = input$x_labels_cont
              )
            }
          }

          data7 <- data6

          if ("x_labels_discrete" %in% to_render) {
            if (!is.null(input$x_labels_discrete) && input$x_labels_discrete != "") {
              data7 <- within(
                data6,
                {
                  x_labels_discrete <- trimws(str_split(x_labels_discrete, ",")[[1]])
                  output_name <- output_name + ggplot2::scale_x_discrete(labels = x_labels_discrete)
                },
                output_name = as.name(output_name),
                x_labels_discrete = input$x_labels_discrete
              )
            }
          }

          data8 <- data7

          if ("y_labels_discrete" %in% to_render) {
            if (!is.null(input$y_labels_discrete) && input$y_labels_discrete != "") {
              data8 <- within(
                data7,
                {
                  y_labels_discrete <- trimws(str_split(y_labels_discrete, ",")[[1]])
                  output_name <- output_name + ggplot2::scale_y_discrete(labels = y_labels_discrete)
                },
                output_name = as.name(output_name),
                y_labels_discrete = input$y_labels_discrete
              )
            }
          }

          data9 <- data8

          if ("font_size_geom_text" %in% to_render) {
            if (!is.null(input$font_size_geom_text) && input$font_size_geom_text != "") {
              data9 <- within(
                data8,
                {
                  geom_layers <- sapply(output_name$layers, function(x) class(x$geom)[1])

                  if (length(geom_layers) > 0) {
                    for (i in 1:length(geom_layers)) {
                      if ("GeomText" %in% geom_layers[i]) {
                        output_name$layers[[i]] <- ggplot2::geom_text(size = as.numeric(font_size_geom_text))
                      }
                    }
                  }
                },
                output_name = as.name(output_name),
                font_size_geom_text = input$font_size_geom_text
              )
            }
          }

          data10 <- data9

          if ("font_size_plot_title" %in% to_render) {
            if (!is.null(input$font_size_plot_title) && input$font_size_plot_title != "") {
              data10 <- within(
                data9,
                {
                  output_name <- output_name +
                    ggplot2::theme(plot.title = element_text(size = as.numeric(font_size_plot_title)))
                },
                output_name = as.name(output_name),
                font_size_plot_title = input$font_size_plot_title
              )
            }
          }

          data11 <- data10
          if ("font_size_axis_title" %in% to_render) {
            if (!is.null(input$font_size_axis_title) && input$font_size_axis_title != "") {
              data11 <- within(
                data10,
                {
                  output_name <- output_name +
                    ggplot2::theme(axis.title = element_text(size = as.numeric(font_size_axis_title)))
                },
                output_name = as.name(output_name),
                font_size_axis_title = input$font_size_axis_title
              )
            }
          }

          data12 <- data11
          if ("font_size_axis_text" %in% to_render) {
            if (!is.null(input$font_size_axis_text) && input$font_size_axis_text != "") {
              data12 <- within(
                data11,
                {
                  output_name <- output_name +
                    ggplot2::theme(axis.text = element_text(size = as.numeric(font_size_axis_text)))
                },
                output_name = as.name(output_name),
                font_size_axis_text = input$font_size_axis_text
              )
            }
          }

          within(
            data12,
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
