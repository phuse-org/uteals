#' Apply Logical AND/OR filter transformations
#'
#' @description `r lifecycle::badge("experimental")`
#' This transformator provides users with flexible, dynamic filtering capabilities for datasets.
#'
#'   Each instance of the transformator operates on a **single dataset**.
#'   Users can define multiple filter blocks, where:
#'   - Conditions within each block are combined with **logical AND**.
#'   - Multiple blocks are combined with **logical OR**.
#'
#'   This allows creating complex filter expressions like:
#'
#'   \code{(Condition1 AND Condition2) OR (Condition3 AND Condition4)}
#'
#'   To apply filtering across **multiple datasets**, users can instantiate multiple
#'   instances of this module, each configured for a different dataset.
#'   Each module call is independent and manages filters for its specific dataset.
#'
#' - **Supported Data Types & Expressions:**
#'   - Supports filtering on \code{character}, \code{factor}, and \code{numeric} columns.
#'   - Conditions can use operators: \code{==}, \code{!=}, \code{<}, \code{>}, \code{<=}, \code{>=}.
#'   - Conditions are specified as simple expressions, e.g.,
#'     \code{columnA == 'value'}
#'     \code{columnB != 5}
#'     \code{columnC >= 10}
#'   - Each block's conditions are combined with **AND**.
#'   - Multiple blocks are combined with **OR**.
#'
#' - **Features:**
#'   - **Add Multiple OR Blocks:** Dynamically add new blocks for alternative conditions.
#'   - **Add Conditions:** Within each block, add multiple conditions, with duplicate prevention.
#'   - **Preview Filter Expression:** Generate and display the current combined filter expression.
#'   - **Remove Conditions:** Remove individual conditions within a block.
#'   - **Expression Generation:** The resulting expression can be directly used
#'     with \code{dplyr::filter()} or similar functions.
#'
#' - **Usage Pattern:**
#'   - Call the module multiple times with different dataset names to filter multiple datasets independently.
#'   - Each call manages its own filter state and expression.
#'   - Users can build complex filters per dataset and apply or combine them as needed.
#'
#' @param dataname (`character(1)`) Name of the dataset to filter. Pass a single dataset name as a string.
#'
#' @import teal shiny shinyWidgets dplyr shinyBS rlang
#' @importFrom methods new
#' @importFrom shinyjs toggle show hidden hide
#' @importFrom rlang parse_expr
#'
#' @return `teal::teal_transform_module`
#'
#' @examples
#'
#' app <- teal::init(
#'   data = teal.data::teal_data(IRIS = iris),
#'   modules = teal::modules(teal::example_module(
#'     transformators = list(or_filtering_transformator("IRIS"))
#'   ))
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
or_filtering_transformator <- function(dataname) {
  teal::teal_transform_module(
    label = "Logical Filtering Transformator",
    ui = function(id) {
      ns <- shiny::NS(id)
      tagList(
        shinyjs::useShinyjs(),
        shiny::tags$div(
          style = "font-size: initial;display: inline-flex; align-items: center;",
          shiny::textOutput(ns("dataset")),
          shiny::tags$span(
            style = "margin-left: 8px;",
            shiny::tags$i(
              class = "fa fa-info-circle info-icon",
              id = "infoIcon",
              title = "Support expressions like this: (COND1 [&& COND2]) [|| (COND3 [&& COND4])]*"
            )
          )
        ),
        shiny::actionButton(
          ns("preview"),
          "Preview Filtering Expression",
          class = "btn btn-primary",
          style = "width: -webkit-fill-available;margin: 5px;",
          title = "Click to preview the current filtering expression"
        ),
        shinyBS::bsModal(
          "filterPreview",
          "Filtering Expression Preview",
          "preview",
          size = "large",
          shiny::htmlOutput("filter_preview")
        ),
        shiny::div(
          shiny::actionButton(
            ns("add_alternative"),
            "Add Alternative",
            class = "btn btn-primary",
            style = "margin-top: 5px;",
            title = "Add another alternative OR block"
          )
        )
      )
    },
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- shiny::NS(id)

        view_model <- filtering_transformator_model$new(data, dataname)

        output$dataset <- shiny::renderText({
          paste0(dataname, " - Additional Filters")
        })

        update_condition_ui <- function(current_block_id) {
          output[[paste0("condition_ui_", current_block_id)]] <- renderUI({
            conds <- view_model$block_conditions[[as.character(current_block_id)]]
            if (is.null(conds) || length(conds) == 0) {
              shinyjs::hide(paste0("condition_ui_container", current_block_id))
              return()
            }

            # Create a list of condition UI elements
            condition_ui_list <- lapply(seq_along(conds), function(i) {
              cond_text <- conds[[i]] # get the condition text

              div(
                style = "display:flex; align-items:center; margin-bottom:4px;",
                span(cond_text, style = "margin-right:10px;"),
                actionButton(
                  session$ns(paste0("remove_", current_block_id, "_", i)),
                  "X",
                  class = "btn btn-primary",
                  style = "
                      color: white;background: #ed6e6e;font-size: 10px;border: antiquewhite;
                      height: 19px;width: 17px;padding: 2px;
                    ",
                  title = "Remove Condition"
                )
              )
            })

            do.call(tagList, condition_ui_list)
          })
          shinyjs::show(paste0("condition_ui_container", current_block_id))
        }

        # Observe to add a new block = handler of the "Add alternative" button
        shiny::observeEvent(input$add_alternative, {
          view_model$add_alternative()

          blocks <- view_model$block_objects()
          shiny::insertUI(
            selector = paste0("#", session$ns("add_alternative")),
            where = "beforeBegin",
            ui = shiny::tags$div(
              id = paste0("alt_block_", view_model$alt_id()),
              style = "background-color: #ededed;margin-top: 7px;padding: 10px;",
              shiny::div(
                style = "display:flex;",
                shiny::div(
                  class = "col-3",
                  shiny::actionButton(
                    session$ns(paste0("toggle_alt_", view_model$alt_id())),
                    "+",
                    style = "font-size:18px; width:28px; height:28px;background: white;padding: 1px;",
                    title = "Add conditions"
                  )
                ),
                # Container for condition display
                shinyjs::hidden(
                  shiny::div(
                    class = "col-9",
                    id = session$ns(paste0("condition_ui_container", view_model$alt_id())),
                    shiny::uiOutput(session$ns(paste0("condition_ui_", view_model$alt_id()))),
                    style = "margin-left: 5%;"
                  )
                )
              ),
              shiny::div(
                id = session$ns(paste0("alt_dropdown_", view_model$alt_id())),
                style = "margin-top:10px;",
                shinyWidgets::pickerInput(
                  inputId = session$ns(paste0("column_selector_", view_model$alt_id())),
                  label = "Select Column",
                  choices = view_model$choices_for_columns(),
                  choicesOpt = list(subtext = teal.data::col_labels(data()[[dataname]]))
                ),
                shiny::selectInput(
                  session$ns(paste0("operator_selector_", view_model$alt_id())),
                  "Select Condition",
                  choices = c("==", "!=", "<", ">")
                ),
                shiny::selectInput(
                  session$ns(paste0("value_input_", view_model$alt_id())), "Select Value",
                  choices = NULL
                ),
                shiny::actionButton(
                  session$ns(paste0("add_condition_", view_model$alt_id())),
                  "Add",
                  class = "btn btn-primary",
                  title = "Add condition"
                )
              )
            )
          )

          # Handler for the Add button for the new block
          observeEvent(input[[paste0("add_condition_", view_model$alt_id())]], {
            blocks <- view_model$block_objects()
            current_block <- blocks[[paste0("block_", view_model$alt_id())]]
            variable <- input[[paste0("column_selector_", view_model$alt_id())]]
            operator <- input[[paste0("operator_selector_", view_model$alt_id())]]
            value <- input[[paste0("value_input_", view_model$alt_id())]]

            cond_str <- paste0(variable, " ", operator, " ", value)

            # Check for duplicates in current block
            existing_conds <- if (is.null(view_model$block_conditions[[as.character(view_model$alt_id())]])) {
              list()
            } else {
              view_model$block_conditions[[as.character(view_model$alt_id())]]
            }

            if (view_model$is_duplicate_condition(cond_str, existing_conds)) {
              shiny::showNotification("This condition already exists in the block.", type = "warning")
              return() # Exit without adding
            }

            # Add condition to the block object
            current_block <- add_condition(current_block, variable, operator, value)
            blocks[[paste0("block_", view_model$alt_id())]] <- current_block
            view_model$block_objects(blocks)

            view_model$block_conditions[[as.character(view_model$alt_id())]] <- c(
              view_model$block_conditions[[as.character(view_model$alt_id())]], list(cond_str)
            )

            # Update the UI
            update_condition_ui(view_model$alt_id())
          })

          prev_button_states <- reactiveValues()
          observe({
            # Find all remove button IDs
            all_ids <- grep("^remove_", names(input), value = TRUE)

            # Check each button for a new click
            clicked_id <- NULL
            for (id in all_ids) {
              # Initialize previous state if not present
              if (is.null(prev_button_states[[id]])) {
                prev_button_states[[id]] <- 0
              }

              current_state <- input[[id]]
              prev_state <- prev_button_states[[id]]

              # Detect if button was clicked
              if (!is.null(current_state) && current_state > prev_state) {
                clicked_id <- id
                prev_button_states[[id]] <- current_state
                break # Only handle one click at a time
              }
            }

            if (is.null(clicked_id)) {
              return()
            }
            parts <- strsplit(clicked_id, "_")[[1]]
            current_block_id <- parts[2]
            cond_index <- as.numeric(parts[3])

            # Condition removal
            shiny::req(view_model$block_conditions[[as.character(current_block_id)]])
            view_model$block_conditions[[as.character(current_block_id)]][[cond_index]] <- NULL

            # Remove from block object
            blocks <- view_model$block_objects()
            current_block <- blocks[[paste0("block_", current_block_id)]]
            current_block@conditions <- current_block@conditions[-cond_index]
            blocks[[paste0("block_", current_block_id)]] <- current_block
            view_model$block_objects(blocks)

            # Re-render the UI
            update_condition_ui(current_block_id)
          })
        })

        # Observe toggle button for new blocks
        shiny::observeEvent(eventExpr = view_model$alt_id(), handlerExpr = {
          current_id <- view_model$alt_id()
          shiny::observeEvent(input[[paste0("toggle_alt_", current_id)]], {
            shinyjs::toggle(paste0("alt_dropdown_", current_id))
          })
        })

        # Update choices for values for the new block
        shiny::observeEvent(eventExpr = view_model$alt_id(), handlerExpr = {
          current_id <- view_model$alt_id()
          shiny::observeEvent(input[[paste0("column_selector_", current_id)]], {
            shiny::req(input[[paste0("column_selector_", current_id)]])
            selected_col <- input[[paste0("column_selector_", current_id)]]
            col_data <- data()[[dataname]][[selected_col]]

            if (is.numeric(col_data)) {
              shiny::updateSelectInput(session, paste0("value_input_", current_id), choices = c(unique(col_data)))
              shiny::updateSelectInput(
                session, paste0("operator_selector_", current_id),
                choices = c("==", "!=", "<=", ">=")
              )
            } else if (is.character(col_data) || is.factor(col_data)) {
              shiny::updateSelectInput(
                session, paste0("value_input_", current_id),
                choices = c(levels(factor(col_data)))
              )
              shiny::updateSelectInput(session, paste0("operator_selector_", current_id), choices = c("==", "!="))
            } else {
              shiny::updateSelectInput(session, paste0("value_input_", current_id), choices = NULL)
              shiny::updateSelectInput(session, paste0("operator_selector_", current_id), choices = NULL)
            }
          })
        })

        shiny::observeEvent(input$preview, {
          # Get the filter expression
          filter_expr <- view_model$final_filter_expr()

          shiny::showModal(
            shiny::modalDialog(
              title = "Filtering Expression Preview",
              if (is.null(filter_expr) || filter_expr == "" || filter_expr == "()") {
                "No conditions added yet"
              } else {
                expression <- gsub("\\(\\)", "", filter_expr)
                expression <- gsub("\\s*\\|\\s*$", "", expression)
                shiny::tags$div(
                  style = "max-height: 600px; overflow-y: auto; padding: 10px;", shiny::tags$pre(expression)
                )
              },
              size = "l",
              easyClose = TRUE
            )
          )
        })

        # Return the filtered data reactively
        shiny::reactive({
          within(
            data(),
            {
              if (filters != "" & filters != "()") {
                df <- df |> dplyr::filter(!!rlang::parse_expr(filters))
              }
              df
            },
            filters = view_model$final_exp(),
            df = as.name(dataname)
          )
        })
      })
    }
  )
}
