#' Class `BlockConditions`
#'
#' This class represents a collection of conditions used for filtering datasets.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{conditions}}{A list of conditions,
#'      where each condition is a list containing variable, operator, and value.}
#' }
#' @keywords internal
setClass("BlockConditions", slots = list(conditions = "list"))

#' Add a condition to a `BlockConditions` object
#'
#' @param object A `BlockConditions` object.
#' @param variable A character string specifying the variable/column name.
#' @param operator A character string specifying the operator (e.g., "==", "!=", "<", ">", "<=", ">=").
#' @param value The value to compare against.
#'
#' @return An updated `BlockConditions` object with the new condition added.
#' @method add_condition BlockConditions
#' @keywords internal
setGeneric("add_condition", function(object, variable, operator, value) standardGeneric("add_condition"))

#' Add conditions
#' @keywords internal
setMethod("add_condition", "BlockConditions", function(object, variable, operator, value) {
  object@conditions <- c(object@conditions, list(list(variable = variable, operator = operator, value = value)))
    object
})

#' Get condition expression
#'
#' @param object A `BlockConditions` object.
#' @param dataname `character(1)` The name of the dataset to filter.
#' @param data The reactive `data` object from `teal`.
#' @return `character(1)` The condition expression.
#' @method get_str_expressions BlockConditions
#' @keywords internal
setGeneric("get_str_expression", function(object, dataname, data) standardGeneric("get_str_expression"))

#' Get condition expression
#' @keywords internal
setMethod("get_str_expression", "BlockConditions", function(object, dataname, data) {
  conds <- lapply(object@conditions, function(cond) {
    var <- cond$variable
    val <- if (is.numeric(data()[[dataname]][[cond$variable]])) {
      cond$value
    } else if (isTRUE(cond$operator == "%in%" || cond$operator == "!%in%")) {
      quoted_vals <- paste0(sprintf("'%s'", cond$value), collapse = ", ")
      sprintf("c(%s)", quoted_vals)
    } else {
      sprintf("'%s'", cond$value)
    }
    if (cond$operator == "!%in%") {
      sprintf("!(%s %%in%% %s)", var, val)
    } else {
      sprintf("%s %s %s", var, cond$operator, val)
    }
  })
  sprintf("(%s)", paste(conds, collapse = " & "))
})
