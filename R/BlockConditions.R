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
#' @method addCondition BlockConditions
#' @keywords internal
setGeneric("addCondition", function(object, variable, operator, value) standardGeneric("addCondition"))

#' Add conditions
#' @keywords internal
setMethod("addCondition", "BlockConditions", function(object, variable, operator, value) {
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
    } else {
      paste0("'", cond$value, "'")
    }
    paste0(var, " ", cond$operator, " ", val)
  })
  paste0("(", paste(conds, collapse = " & "), ")")
})
