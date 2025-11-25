# Create a forest plot from an `rtable` with x transform settings

**\[experimental\]** Given a
[`rtables::rtable()`](https://insightsengineering.github.io/rtables/latest-release/reference/rtable.html)
object with at least one column with a single value and one column with
2 values, converts table to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object and generates an accompanying forest plot. The table and forest
plot are printed side-by-side.

## Usage

``` r
g_forest_with_transform(
  tbl,
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
  as_list = FALSE
)
```

## Arguments

- tbl:

  (`VTableTree`)  
  `rtables` table with at least one column with a single value and one
  column with 2 values.

- col_x:

  (`integer(1)` or `NULL`)  
  column index with estimator. By default tries to get this from `tbl`
  attribute `col_x`, otherwise needs to be manually specified. If
  `NULL`, points will be excluded from forest plot.

- col_ci:

  (`integer(1)` or `NULL`)  
  column index with confidence intervals. By default tries to get this
  from `tbl` attribute `col_ci`, otherwise needs to be manually
  specified. If `NULL`, lines will be excluded from forest plot.

- vline:

  (`numeric`)  
  position of the vertical reference line on the plot. like 0 and 1 in
  forest plot.

- forest_header:

  (`character(2)`)  
  text displayed to the left and right of `vline`, respectively. If
  `vline = NULL` then `forest_header` is not printed. By default tries
  to get this from `tbl` attribute `forest_header`. If `NULL`, defaults
  will be extracted from the table if possible, and set to
  `"Comparison\nBetter"` and `"Treatment\nBetter"` if not.

- xlim:

  (`numeric`)  
  range of x-axis limits. Example: `c(0.1, 10)`

- transform_x:

  (`character`)  
  function for x-values transformation

- x_at:

  (`numeric`)  
  specifies the tick marks on the axis. The value of
  `union(xlim, vline)` or Example: `c(0.1,1,10)`.

- width_columns:

  (`numeric`)  
  a vector of column widths. Each element's position in `colwidths`
  corresponds to the column of `tbl` in the same position. If `NULL`,
  column widths are calculated according to maximum number of characters
  per column.

- lbl_col_padding:

  (`numeric`)  
  padding between label and columns value. Default is `0`.

- rel_width_forest:

  (`proportion`)  
  proportion of total width to allocate to the forest plot. Relative
  width of table is then `1 - rel_width_forest`. If `as_list = TRUE`,
  this parameter is ignored.

- font_size:

  (`numeric(1)`)  
  font size.

- col_symbol_size:

  (`numeric` or `NULL`)  
  column index from `tbl` containing data to be used to determine
  relative size for estimator plot symbol. Typically, the symbol size is
  proportional to the sample size used to calculate the estimator. If
  `NULL`, the same symbol size is used for all subgroups. By default
  tries to get this from `tbl` attribute `col_symbol_size`, otherwise
  needs to be manually specified.

- col:

  (`character`)  
  color(s).

- ggtheme:

  (`theme`)  
  a graphical theme as provided by `ggplot2` to control styling of the
  plot.

- as_list:

  (`flag`)  
  whether the two `ggplot` objects should be returned as a list. If
  `TRUE`, a named list with two elements, `table` and `plot`, will be
  returned. If `FALSE` (default) the table and forest plot are printed
  side-by-side via
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

## Value

`ggplot` forest plot and table.
