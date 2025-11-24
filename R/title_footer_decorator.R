# Helper function to replace junco::get_titles_from_file
get_titles_from_file <- function(
  id, file = .find_titles_file(input_path), input_path = ".",
  title_df = .read_titles_file(file)
) {
  title_df <- title_df[title_df[["TABLE ID"]] == id, , drop = FALSE]
  message(paste0("Static titles file/data.frame used: "))
  msg <- NULL
  if (nrow(title_df) == 0) {
    msg <- paste0(
      paste("Warning: Table ID", id, "not found in Title file."),
      "\n", "A dummy title will be generated to be able to get rtf file produced.",
      "\n", "Ensure the titles file gets updated to include the table identifier"
    )
    title <- "Table ID not found in titles file, dummy title for rtf generation purpose"
    main_footer <- "Ensure the titles file gets updated to include the table identifier"
  } else {
    title <- title_df[title_df$IDENTIFIER == "TITLE", ]$TEXT
    if (length(title) != 1) {
      msg <- "Warning: Title file should contain exactly one title record per Table ID"
    } else {
      message(file)
    }
    main_footer <- title_df[grep("^FOOT", title_df$IDENTIFIER), ]$TEXT
    if (length(main_footer) == 0) {
      main_footer <- character()
    }
  }
  if (!is.null(msg)) {
    warning(msg)
  }
  title_foot <- list(
    title = title, subtitles = NULL, main_footer = main_footer,
    prov_footer = NULL
  )
  return(title_foot)
}

# Helper function to replace junco::set_titles
set_titles <- function(obj, titles) {
  main_title(obj) <- titles$title
  if (!is.null(titles$subtitles)) {
    subtitles(obj) <- titles$subtitles
  }
  main_footer(obj) <- titles$main_footer
  if (!is.null(titles$prov_footer)) {
    prov_footer(obj) <- titles$prov_footer
  }
  return(obj)
}

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
#' named "Sheet1".
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
#' @import openxlsx ggplotify ggplot2 patchwork teal shiny
#' @importFrom grDevices graphics.off
#'
#' @export
title_footer_decorator <- function(output_name, titles_file) {
  checkmate::assert_string(output_name)
  checkmate::assert_string(titles_file)

  titles <- openxlsx::read.xlsx(titles_file, "Sheet1")
  titles$TABLE.ID[1] <- "blank"
  teal::teal_transform_module(
    label = "Title and footer decorator",
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div(
          selectInput(ns("selectTitle"), label = "Select Title", choices = unique(titles$TABLE.ID)),
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
                titles_footers <- get_titles_from_file(selectTitle, file = titles_file)
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
                output_name <- set_titles(output_name, titles_footers)
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
                    ggplot2::labs(title = titles_footers$title, 100, caption = titles_footers$main_footer) +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
                } else if ((inherits(output_name, "trellis")) || (inherits(output_name, "grob"))) {
                  output_name <- ggplotify::as.ggplot(output_name) +
                    ggplot2::labs(title = titles_footers$title, 100, caption = titles_footers$main_footer) +
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
