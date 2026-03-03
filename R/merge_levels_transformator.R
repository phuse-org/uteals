# TransformationManager R6 Class for managing the transformation objects
# @noRd
TransformationManager <- R6::R6Class( # nolint: object_name_linter.
  "TransformationManager",
  public = list(
    counter = 0,
    active_ids = NULL,
    predefined = NULL,
    initialized = FALSE,
    initialize = function(predefined = list()) {
      self$active_ids <- reactiveVal(integer(0))
      self$predefined <- predefined
    },
    add_id = function() {
      self$counter <- self$counter + 1
      self$active_ids(c(self$active_ids(), self$counter))
      self$counter
    },
    remove_id = function(id) {
      self$active_ids(setdiff(self$active_ids(), id))
    },
    reset = function() {
      self$active_ids(integer(0))
      self$counter <- 0
      self$initialized <- FALSE
    }
  )
)

#' UI design of the transformator
#'
#' @param id (`character(1)`) the id of the module.
#' @noRd
merge_level_transformer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$div(id = ns("transformation_container")),
    hr(),
    actionButton(ns("add"), "Add", class = "btn-primary"),
    actionButton(ns("apply"), "Apply", class = "btn-success"),
    actionButton(ns("reset_all"), "Reset", class = "btn-danger")
  )
}

# Server part of transformator
merge_level_transformer_srv <- function(id, data, manager, dataname) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    trigger <- reactiveVal(0)

    create_levels_output <- function(idx, var_sel, lev_sel) {
      force(idx)
      force(var_sel)
      force(lev_sel)

      renderUI({
        req(input[[paste0("col_name_", idx)]])
        col_data <- data()[[dataname]][[input[[paste0("col_name_", idx)]]]]
        choices <- if (is.factor(col_data)) levels(col_data) else unique(col_data)
        selectInput(
          ns(paste0("levs_", idx)), "Levels to Update",
          choices = choices, multiple = TRUE, selected = lev_sel
        )
      })
    }

    add_container_row <- function(idx, var_sel = NULL, lev_sel = NULL, new_name = "") {
      row_id <- ns(paste0("row_", idx))
      body_id <- ns(paste0("body_", idx))
      toggle_btn_id <- ns(paste0("toggle_", idx))
      del_btn_id <- ns(paste0("del_", idx))

      insertUI(
        selector = paste0("#", ns("transformation_container")),
        ui = tags$div(
          id = row_id,
          class = "well well-sm",
          style = "border-left: 5px solid #008080; margin-bottom: 10px;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            tags$strong(paste("Transformation", idx)),
            div(
              actionButton(toggle_btn_id, "", icon = icon("minus"), class = "btn-xs"),
              actionButton(del_btn_id, "", icon = icon("times"), class = "btn-danger btn-xs")
            )
          ),
          div(
            id = body_id,
            style = "margin-top: 10px;",
            selectInput(
              ns(paste0("col_name_", idx)), "Variable",
              choices = names(data()[[dataname]]), selected = var_sel
            ),
            uiOutput(ns(paste0("col_levels_ui_", idx))),
            textInput(ns(paste0("new_label_", idx)), "New Level Name", value = new_name)
          ),
          ## Javascript for toggle effect
          tags$script(sprintf("
                        $('#%s').on('click',function(){
                        $('#%s').slideToggle();
                        $(this).find('i').toggleClass('fa-minus fa-plus');
                        });
                      ", toggle_btn_id, body_id))
        )
      )

      output[[paste0("col_levels_ui_", idx)]] <- create_levels_output(idx, var_sel, lev_sel)

      observeEvent(input[[paste0("del_", idx)]], {
        removeUI(selector = paste0("#", row_id))
        manager$remove_id(idx)
        trigger(trigger() + 1)
      })
    }

    observeEvent(input$add, {
      add_container_row(manager$add_id())
    })

    observe({
      req(data())
      if (!manager$initialized && length(manager$predefined) > 0) {
        for (item in manager$predefined) {
          idx <- manager$add_id()
          add_container_row(idx, var_sel = item[[1]], lev_sel = item[[2]], new_name = item[[3]])
        }
        manager$initialized <- TRUE
        trigger(trigger() + 1)
      }
    })

    observeEvent(input$reset_all, {
      for (idx in manager$active_ids()) {
        removeUI(selector = paste0("#", ns(paste0("row_", idx))))
      }
      manager$reset()
      trigger(trigger() + 1)
    })

    observeEvent(input$apply, {
      trigger(trigger() + 1)
    })

    # generate code dynamically based on the input change & number of items from mapping:
    data_update <- eventReactive(trigger(), {
      final <- list()
      data_list <- list()
      ids <- manager$active_ids()

      for (idx in ids) {
        col_name <- input[[paste0("col_name_", idx)]]
        col_levels <- input[[paste0("levs_", idx)]]
        col_merged_name <- input[[paste0("new_label_", idx)]]

        if (!is.null(col_name) && length(col_levels) > 0 && col_merged_name != "") {
          data_list <- teal.modules.clinical::add_expr(data_list, {
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

      final$data <- teal.modules.clinical::bracket_expr(data_list)
      final
    })


    # Merge the selected levels into one
    reactive({
      teal.code::eval_code(data(), as.expression(data_update()))
    })
  })
}

#' Combine levels of a variable into one level
#'
#' @description `r lifecycle::badge("experimental")`
#' @details
#' This transformator allows the user to select a column from the dataset
#' and merge levels of this column into a new level. Only selected
#' levels are affected.
#'
#' @param dataname (`character(1)`) the name of the dataset
#' which columns will be used for possible transformation.
#' @param predefined (`list`) the list which has variable name, levels and new label
#'
#' @importFrom teal teal_transform_module
#' @importFrom shiny NS tagList actionButton hr div tags uiOutput renderUI req
#' @importFrom shiny selectInput textInput moduleServer reactiveVal observeEvent observe eventReactive reactive
#' @importFrom shinyWidgets pickerInput
#' @importFrom teal.code eval_code
#' @importFrom teal.modules.clinical add_expr bracket_expr
#'
#' @return `teal::teal_transform_module`
#'
#' @examples
#'
#' app <- teal::init(
#'   data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
#'   modules = teal::modules(
#'     teal::example_module(
#'       transformators = list(merge_levels_transformator(
#'         dataname = "IRIS",
#'         predefined = list(
#'           list("Species", "setosa", "SETOSA_WITHIN_FIX"),
#'           list("Petal.Width", c(0.2, 0.3, 0.5), 12)
#'         )
#'       ))
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shiny::shinyApp(app$ui, app$server)
#' }
#'
#' @export
merge_levels_transformator <- function(dataname, predefined = list()) {
  teal_transform_module(
    label = paste("Merge Levels for :", dataname),
    ui = merge_level_transformer_ui,
    server = function(id, data) {
      manager <- TransformationManager$new(predefined = predefined)
      merge_level_transformer_srv(id, data, manager, dataname)
    }
  )
}
