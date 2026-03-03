#' View model for [or_filtering_transformator()].
#'
#' @import R6
#' @keywords internal
filtering_transformator_model <- R6::R6Class("filtering_transformator_model",
  public = list(
    #' @field block_objects Block objects.
    block_objects = NULL,
    #' @field r_vals Reactive values.
    r_vals = NULL,
    #' @field alt_id The id of the last alternative.
    alt_id = NULL,
    #' @field block_conditions Conditions as strings.
    block_conditions = NULL,
    #' @field choices_for_columns Choices for the columns.
    choices_for_columns = NULL,
    #' @field final_filter_expr Final filtering expression.
    final_filter_expr = NULL,
    #' @field final_exp Final expression for `teal.data`.
    final_exp = NULL,
    #' Initializes the object
    #' @param data the reactive data object from `teal`.
    #' @param dataname `character(1)` the name of the dataset.
    #' @keywords internal
    initialize = function(data, dataname) {
      self$block_objects <- shiny::reactiveVal(list())
      self$r_vals <- shiny::reactiveValues(filter_conditions = list())
      self$alt_id <- shiny::reactiveVal(0)
      self$block_conditions <- shiny::reactiveValues()
      self$choices_for_columns <- shiny::reactive({
        shiny::req(data())
        names(data()[[dataname]])
      })
      self$final_filter_expr <- reactive({
        blocks <- self$block_objects()
        exprs <- Filter(\(block) length(block@conditions) > 0, self$block_objects()) |>
          (\(blocks) Map(\(block) get_str_expression(block, dataname, data), blocks))()
        paste(paste(exprs, collapse = " | "))
      })
      self$final_exp <- shiny::reactive({
        filter_expr <- self$final_filter_expr()
        expression <- gsub("\\(\\)", "", filter_expr)
        expr <- paste(gsub("\\s*\\|\\s*$", "", expression))
        paste(gsub("^\\s*[|&]+\\s*", "", expr))
      })
    },
    #' Adds an alternative
    #' @return invisibly self.
    #' @keywords internal
    add_alternative = function() {
      self$alt_id(self$alt_id() + 1)
      new_block <- methods::new("BlockConditions", conditions = list())
      blocks <- self$block_objects()
      blocks[[paste0("block_", self$alt_id())]] <- new_block
      self$block_objects(blocks)
      invisible(self)
    },
    #' Checks for duplicate conditions
    #' @param cond_str `character(1)` added condition.
    #' @param block_cond_list `character(1)` list of all conditions.
    #' @return `logical(1)` whether the added condition is a duplicate.
    #' @keywords internal
    is_duplicate_condition = function(cond_str, block_cond_list) {
      cond_str %in% block_cond_list
    }
  ),
  private = list()
)
