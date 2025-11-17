#' Extract Non-Parent Module Labels to `YAML`
#'
#' @description `r lifecycle::badge("experimental")`
#' Extracts module labels from a teal modules object, filters out parent modules
#' (grouping containers), and generates a `YAML` file with the functional modules.
#'
#' @param mods (`teal_module` or `teal_modules`) a teal modules object
#'   containing the module structure.
#' @param filepath (`character(1)`) character string specifying the output `YAML` file path.
#'
#' @return Character vector of non-parent module labels
#'
#' @import yaml
#' @examples
#' # Extract modules from mods object to YAML file
#' mods <- teal::modules(
#'   teal::example_module("mod1"),
#'   teal::example_module("mod2")
#' )
#' labels <- extract_modules_to_yaml(mods, "panel_str_modules.yml")
#'
#' @export
extract_modules_to_yaml <- function(mods, filepath) {
  # Recursively extract module labels, excluding parent containers
  extract_labels <- function(mod_obj) {
    labels <- character(0)

    if (inherits(mod_obj, "teal_module")) {
      # This is a leaf module - extract its label
      return(mod_obj$label)
    } else if (inherits(mod_obj, "teal_modules")) {
      # This is a container - recurse into children
      for (child in mod_obj$children) {
        labels <- c(labels, extract_labels(child))
      }
    }

    labels
  }

  # Extract all non-parent module labels
  non_parent_labels <- extract_labels(mods)

  # Create panel_str structure
  panel_str <- list(
    list(access_panel = "ADMIN"),
    list(
      access_panel = "Modules",
      access_units = lapply(non_parent_labels, function(label) list(unit = label))
    )
  )

  # Write to YAML file
  writeLines(yaml::as.yaml(list(panel_str = panel_str)), filepath)

  cat("Generated", filepath, "with", length(non_parent_labels), "non-parent module labels\n")
  non_parent_labels
}

#' Filter Teal Modules by Label
#'
#' @description `r lifecycle::badge("experimental")`
#' Recursively filters a teal modules object to keep only modules whose labels
#' match the specified labels. Removes modules that don't match and empty
#' parent containers.
#'
#' @param x (`teal_module` or `teal_modules`) the object to filter.
#' @param label (`character(1)`) character vector of module labels to keep.
#'
#' @return Filtered `teal_modules` or `teal_module` object, or `NULL` if none matches.
#'
#' @import checkmate
#' @examples
#' # Keep only specific modules by label
#' mods <- teal::modules(
#'   teal::example_module("mod1"),
#'   teal::example_module("mod2")
#' )
#' filtered_mods <- keep_by_label(mods, c("Data Table", "Disposition"))
#'
#' @export
keep_by_label <- function(x, label) {
  checkmate::assert_multi_class(x, c("teal_modules", "teal_module"))
  if (inherits(x, "teal_modules")) {
    x$children <- Filter(
      length,
      lapply(x$children, keep_by_label, label = label)
    )
    if (length(x$children) == 0) {
      return(NULL)
    }
    return(x)
  }
  if (x$label %in% label) {
    return(x)
  }
  NULL
}

#' Remove Teal Modules by Label
#'
#' @description `r lifecycle::badge("experimental")`
#' Recursively removes modules from a teal modules structure that match the specified labels.
#'
#' @param x (`teal_module` or `teal_modules`) The object to filter.
#' @param label (`character(1)`) character vector of module labels to remove.
#'
#' @return The filtered teal modules object with matching modules removed, or `NULL`
#'   if all modules are removed.
#'
#' @import checkmate
#' @examples
#' mods <- teal::modules(
#'   teal::example_module("mod1"),
#'   teal::example_module("mod2")
#' )
#' # Remove a single module
#' filtered_mods <- remove_by_label(mods, "Deaths")
#'
#' # Remove multiple modules
#' filtered_mods <- remove_by_label(mods, c("Deaths", "Lab Summary Table"))
#'
#' @export
remove_by_label <- function(x, label) {
  checkmate::assert_multi_class(x, c("teal_modules", "teal_module"))

  # Check if label exists and matches
  if (!is.null(x$label) && length(x$label) > 0 && x$label %in% label) {
    return(NULL)
  }

  if (inherits(x, "teal_modules")) {
    x$children <- Filter(
      function(child) !is.null(child),
      lapply(x$children, remove_by_label, label = label)
    )
    if (length(x$children) == 0) {
      return(NULL)
    }
  }

  x
}
