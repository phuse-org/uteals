#' Change X-axis scaling decorator for [`teal.modules.clinical::tm_g_forest_rsp`].
#'
#' @description `r lifecycle::badge("experimental")`
#' A function to create a UI component for selecting a transform function
#' for the forest plot x axis.
#'
#' @return [`teal::teal_transform_module`] Returns a modified plot object with the transformation applied.
#'
#' @details The module creates a UI with a radio control for selecting the transform function.
#' The selected transformation function is applied to the forest plot to update the
#' plot's axis and annotations accordingly.
#'
#' @import teal shiny
#'
#' @examples
#' library(teal.modules.clinical)
#' data <- teal.data::teal_data()
#' data <- within(data, {
#'   ADSL <- teal.modules.clinical::tmc_ex_adsl
#'   ADRS <- teal.modules.clinical::tmc_ex_adrs |>
#'     dplyr::mutate(AVALC = tern::d_onco_rsp_label(AVALC) |>
#'       formatters::with_label("Character Result/Finding")) |>
#'     dplyr::filter(PARAMCD != "OVRINV" | AVISIT == "FOLLOW UP")
#' })
#' teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
#'
#' arm_ref_comp <- list(
#'   ARM = list(
#'     ref = "B: Placebo",
#'     comp = c("A: Drug X", "C: Combination")
#'   ),
#'   ARMCD = list(
#'     ref = "ARM B",
#'     comp = c("ARM A", "ARM C")
#'   )
#' )
#'
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     teal.modules.clinical::tm_g_forest_rsp(
#'       label = "Forest Response",
#'       dataname = "ADRS",
#'       arm_var = choices_selected(
#'         variable_choices(ADSL, c("ARM", "ARMCD")),
#'         "ARMCD"
#'       ),
#'       arm_ref_comp = arm_ref_comp,
#'       paramcd = choices_selected(
#'         value_choices(ADRS, "PARAMCD", "PARAM"),
#'         "INVET"
#'       ),
#'       subgroup_var = choices_selected(
#'         variable_choices(ADSL, names(ADSL)),
#'         c("BMRKR2", "SEX")
#'       ),
#'       strata_var = choices_selected(
#'         variable_choices(ADSL, c("STRATA1", "STRATA2")),
#'         "STRATA2"
#'       ),
#'       plot_height = c(600L, 200L, 2000L),
#'       decorators = list(
#'         plot = forestplot_x_decorator()
#'       ),
#'       default_responses = list(
#'         BESRSPI = list(
#'           rsp = c("Stable Disease (SD)", "Not Evaluable (NE)"),
#'           levels = c(
#'             "Complete Response (CR)", "Partial Response (PR)", "Stable Disease (SD)",
#'             "Progressive Disease (PD)", "Not Evaluable (NE)"
#'           )
#'         ),
#'         INVET = list(
#'           rsp = c("Complete Response (CR)", "Partial Response (PR)"),
#'           levels = c(
#'             "Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)",
#'             "Progressive Disease (PD)", "Stable Disease (SD)"
#'           )
#'         ),
#'         OVRINV = list(
#'           rsp = c("Progressive Disease (PD)", "Stable Disease (SD)"),
#'           levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Not Evaluable (NE)")
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
forestplot_x_decorator <- function() {
  teal::teal_transform_module(
    label = "Plot X transform decorator",
    ui = function(id) {
      ns <- NS(id)
      div(
        radioButtons(
          ns("transform_fn"),
          label = "Select transformation function",
          choices = c("ln" = "log", "log 10" = "log10", "linear" = "linear")
        )
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          req(data())
          within(
            data(),
            expr = {
              f <- g_forest_with_transform(tbl = result, transform_x = transform_fn, as_list = TRUE)
              table <- f[["table"]]
              plot <- f[["plot"]]
              plot_list <- list(table = table, plot = plot)
              plot_list
            },
            transform_fn = ifelse(input$transform_fn == "linear", NA, as.name(input$transform_fn))
          )
        })
      })
    }
  )
}

#' Create a forest plot from an `rtable` with x transform settings
#'
#' @description `r lifecycle::badge("experimental")`
#' Given a [rtables::rtable()] object with at least one column with a single value and one column with 2
#' values, converts table to a [ggplot2::ggplot()] object and generates an accompanying forest plot. The
#' table and forest plot are printed side-by-side.
#'
#'
#' @param tbl (`VTableTree`)\cr `rtables` table with at least one column with a single value and one column with 2
#'   values.
#' @param col_x (`integer(1)` or `NULL`)\cr column index with estimator. By default tries to get this from
#'   `tbl` attribute `col_x`, otherwise needs to be manually specified. If `NULL`, points will be excluded
#'   from forest plot.
#' @param col_ci (`integer(1)` or `NULL`)\cr column index with confidence intervals. By default tries to get this from
#'   `tbl` attribute `col_ci`, otherwise needs to be manually specified. If `NULL`, lines will be excluded
#'   from forest plot.
#' @param forest_header (`character(2)`)\cr text displayed to the left and right of `vline`, respectively.
#'   If `vline = NULL` then `forest_header` is not printed. By default tries to get this from `tbl` attribute
#'   `forest_header`. If `NULL`, defaults will be extracted from the table if possible, and set to
#'   `"Comparison\nBetter"` and `"Treatment\nBetter"` if not.
#' @param transform_x (`character`)\cr function for x-values transformation
#' @param width_columns (`numeric`)\cr a vector of column widths. Each element's position in
#'   `colwidths` corresponds to the column of `tbl` in the same position. If `NULL`, column widths are calculated
#'   according to maximum number of characters per column.
#' @param rel_width_forest (`proportion`)\cr proportion of total width to allocate to the forest plot. Relative
#'   width of table is then `1 - rel_width_forest`. If `as_list = TRUE`, this parameter is ignored.
#' @param font_size (`numeric(1)`)\cr font size.
#' @param col_symbol_size (`numeric` or `NULL`)\cr column index from `tbl` containing data to be used
#'   to determine relative size for estimator plot symbol. Typically, the symbol size is proportional
#'   to the sample size used to calculate the estimator. If `NULL`, the same symbol size is used for all subgroups.
#'   By default tries to get this from `tbl` attribute `col_symbol_size`, otherwise needs to be manually specified.
#' @param col (`character`)\cr color(s).
#' @param vline (`numeric`)\cr position of the vertical reference line on the plot. like 0 and 1 in forest plot.
#' @param xlim (`numeric`)\cr range of x-axis limits. Example: `c(0.1, 10)`
#' @param x_at (`numeric`)\cr specifies the tick marks on the axis.
#'   The value of `union(xlim, vline)` or Example: `c(0.1,1,10)`.
#' @param lbl_col_padding (`numeric`)\cr padding between label and columns value. Default is `0`.
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to control styling of the plot.
#' @param as_list (`flag`)\cr whether the two `ggplot` objects should be returned as a list. If `TRUE`, a named list
#'   with two elements, `table` and `plot`, will be returned. If `FALSE` (default) the table and forest plot are
#'   printed side-by-side via [cowplot::plot_grid()].
#'
#' @return `ggplot` forest plot and table.
#' @import checkmate tern ggplot2 rtables
#' @importFrom cowplot plot_grid
#'
#' @export
g_forest_with_transform <- function(tbl,
                                    col_x = attr(tbl, "col_x"),
                                    col_ci = attr(tbl, "col_ci"),
                                    vline = 1,
                                    forest_header = attr(tbl, "forest_header"),
                                    xlim = c(0.1, 10),
                                    transform_x = NA,
                                    x_at = c(0.1, 1, 10),
                                    width_columns = NULL,
                                    lbl_col_padding = 0,
                                    rel_width_forest = 0.25,
                                    font_size = 12,
                                    col_symbol_size = attr(tbl, "col_symbol_size"),
                                    col = getOption("ggplot2.discrete.colour")[1],
                                    ggtheme = NULL,
                                    as_list = FALSE) {
  checkmate::assert_class(tbl, "VTableTree")
  checkmate::assert_number(col_x, lower = 0, upper = ncol(tbl), null.ok = TRUE)
  checkmate::assert_number(col_ci, lower = 0, upper = ncol(tbl), null.ok = TRUE)
  checkmate::assert_number(col_symbol_size, lower = 0, upper = ncol(tbl), null.ok = TRUE)
  checkmate::assert_number(font_size, lower = 0)
  checkmate::assert_character(col, null.ok = TRUE)
  checkmate::assert_true(is.null(col) | length(col) == 1 | length(col) == nrow(tbl))

  # Extract info from table
  mat <- formatters::matrix_form(tbl, indent_rownames = TRUE)
  mat_strings <- formatters::mf_strings(mat)
  nlines_hdr <- formatters::mf_nlheader(mat)
  nrows_body <- nrow(mat_strings) - nlines_hdr
  tbl_stats <- mat_strings[nlines_hdr, -1]

  # Generate and modify table as ggplot object
  gg_table <- tern::rtable2gg(tbl, fontsize = font_size, colwidths = width_columns, lbl_col_padding = lbl_col_padding) +
    ggplot2::theme(plot.margin = margin(0, 0, 0, 0.025, "npc"))
  gg_table$scales$scales[[1]]$expand <- c(0.01, 0.01)
  gg_table$scales$scales[[2]]$limits[2] <- nrow(mat_strings) + 1
  if (nlines_hdr == 2) {
    gg_table$scales$scales[[2]]$expand <- c(0, 0)
    arms <- unique(mat_strings[1, ][nzchar(trimws(mat_strings[1, ]))])
  } else {
    arms <- NULL
  }

  tbl_df <- rtables::as_result_df(tbl)
  dat_cols <- seq(which(names(tbl_df) == "node_class") + 1, ncol(tbl_df))
  tbl_df <- tbl_df[, c(which(names(tbl_df) == "row_num"), dat_cols)]
  names(tbl_df) <- c("row_num", tbl_stats)

  # Check table data columns
  if (!is.null(col_ci)) {
    ci_col <- col_ci + 1
  } else {
    tbl_df[["empty_ci"]] <- rep(list(c(NA_real_, NA_real_)), nrow(tbl_df))
    ci_col <- which(names(tbl_df) == "empty_ci")
  }
  if (length(tbl_df[, ci_col][[1]]) != 2) stop("CI column must have two elements (lower and upper limits).")

  if (!is.null(col_x)) {
    x_col <- col_x + 1
  } else {
    tbl_df[["empty_x"]] <- NA_real_
    x_col <- which(names(tbl_df) == "empty_x")
  }
  if (!is.null(col_symbol_size)) {
    sym_size <- unlist(tbl_df[, col_symbol_size + 1])
  } else {
    sym_size <- rep(1, nrow(tbl_df))
  }

  tbl_df[, c("ci_lwr", "ci_upr")] <- t(sapply(tbl_df[, ci_col], unlist))
  x <- unlist(tbl_df[, x_col])
  lwr <- unlist(tbl_df[["ci_lwr"]])
  upr <- unlist(tbl_df[["ci_upr"]])
  row_num <- nrow(mat_strings) - tbl_df[["row_num"]] - as.numeric(nlines_hdr == 2)

  if (is.null(col)) col <- "#343cff"
  if (length(col) == 1) col <- rep(col, nrow(tbl_df))

  # Apply transformation
  if (!is.na(transform_x)) {
    x <- sapply(x, transform_x)
    lwr <- sapply(lwr, transform_x)
    upr <- sapply(upr, transform_x)
  }

  xlim <- c(round(min(x), 0), round(max(x), 0))
  vline <- round(mean(x), 1)
  x_at <- union(xlim, vline)
  x_labels <- x_at

  # Set up plot area
  gg_plt <- ggplot2::ggplot(data = tbl_df) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA_character_),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(),
      axis.text = ggplot2::element_text(size = font_size),
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0.1, 0.05, 0, "npc")
    ) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = x_at, labels = x_labels, expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, nrow(mat_strings) + 1), breaks = NULL, expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off")

  if (is.null(ggtheme)) {
    gg_plt <- gg_plt +
      ggplot2::annotate("rect", xmin = xlim[1], xmax = xlim[2], ymin = 0, ymax = nrows_body + 0.5, fill = "grey92")
  }

  if (!is.null(vline)) {
    # Set default forest header
    if (is.null(forest_header)) {
      forest_header <- c(
        paste(if (length(arms) == 2) arms[1] else "Comparison", "Better", sep = "\n"),
        paste(if (length(arms) == 2) arms[2] else "Treatment", "Better", sep = "\n")
      )
    }

    mid_pts <- c(mean(c(xlim[1], vline)), mean(c(vline, xlim[2])))
    gg_plt <- gg_plt +
      ggplot2::annotate("segment", x = vline, xend = vline, y = 0, yend = nrows_body + 0.5) +
      ggplot2::annotate(
        "text",
        x = mid_pts[1],
        y = nrows_body + 1.25,
        label = forest_header[1],
        size = font_size / .pt,
        lineheight = 0.9
      ) +
      ggplot2::annotate(
        "text",
        x = mid_pts[2],
        y = nrows_body + 1.25,
        label = forest_header[2],
        size = font_size / .pt,
        lineheight = 0.9
      )
  }

  # Add points to plot
  if (any(!is.na(x))) {
    x[x < xlim[1] | x > xlim[2]] <- NA
    gg_plt <- gg_plt + geom_point(x = x, y = row_num, color = col, aes(size = sym_size), na.rm = TRUE)
  }

  for (i in seq_len(nrow(tbl_df))) {
    # Determine which arrow(s) to add to CI lines
    which_arrow <- c(lwr[i] < xlim[1], upr[i] > xlim[2])
    which_arrow <- dplyr::case_when(
      all(which_arrow) ~ "both", which_arrow[1] ~ "first", which_arrow[2] ~ "last", TRUE ~ NA_character_
    )

    # Add CI lines
    gg_plt <- gg_plt +
      if (!is.na(which_arrow)) {
        ggplot2::annotate(
          "segment",
          x = if (!which_arrow %in% c("first", "both")) lwr[i] else xlim[1],
          xend = if (!which_arrow %in% c("last", "both")) upr[i] else xlim[2],
          y = row_num[i],
          yend = row_num[i],
          color = if (length(col) == 1) col else col[i],
          arrow = arrow(length = unit(0.05, "npc"), ends = which_arrow),
          na.rm = TRUE
        )
      } else {
        ggplot2::annotate(
          "segment",
          x = lwr[i],
          xend = upr[i],
          y = row_num[i],
          yend = row_num[i],
          color = if (length(col) == 1) col else col[i],
          na.rm = TRUE
        )
      }
  }

  # Apply custom ggtheme to plot
  if (!is.null(ggtheme)) gg_plt <- gg_plt + ggtheme

  if (as_list) {
    list(table = gg_table, plot = gg_plt)
  } else {
    cowplot::plot_grid(
      gg_table, gg_plt,
      align = "h", axis = "tblr", rel_widths = c(1 - rel_width_forest, rel_width_forest)
    )
  }
}
