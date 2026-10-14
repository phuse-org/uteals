# Freeze table headers and first columns

**\[experimental\]** Makes the header row and/or the first column of
tables rendered with
[`teal.widgets::table_with_settings_ui()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/table_with_settings.html)
sticky, so that the headers and row labels stay visible while scrolling.
Include the returned tags once in the app UI, e.g. in the `header`
argument of
[`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).

## Usage

``` r
use_freeze_tables(
  freeze_header = TRUE,
  freeze_col = TRUE,
  max_height = "70vh",
  module_ids = NULL
)
```

## Arguments

- freeze_header:

  (`logical(1)`) whether to freeze the header rows of the tables.

- freeze_col:

  (`logical(1)`) whether to freeze the first column of the tables.

- max_height:

  (`character(1)`) a CSS value for the maximum height of the table
  container. The table scrolls vertically inside the container when it
  is taller than that.

- module_ids:

  (`character` or `NULL`) if `NULL`, all tables in the app are frozen.
  Otherwise, only tables inside elements whose `id` contains any of the
  provided strings are frozen (e.g. the ids of `teal` modules).

## Value

A
[`shiny::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
with the CSS and the JavaScript freezing the tables.

## Details

`rtables` renders tables without a `<thead>` element, so the header rows
cannot be selected with CSS alone. The JavaScript watches the page for
new tables and moves the leading rows containing `<th>` cells into a
`<thead>` element of every matching table. Once
<https://github.com/pharmaverse/rtables/issues/1123> is resolved, this
can be simplified.

## Examples

``` r
library(teal.modules.clinical)
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      add_total = TRUE,
      summarize_vars = choices_selected(
        c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
        c("SEX", "RACE")
      ),
      useNA = "ifany"
    )
  ),
  header = use_freeze_tables(freeze_header = TRUE, freeze_col = TRUE)
)
#> Initializing tm_t_summary
#> Warning: `summarize_vars` was deprecated in teal.modules.clinical 0.13.0.
#> ℹ Pass `teal.picks::variables()` (or a full `teal.picks::picks()` chain).
#>   Support for legacy `teal.transform::choices_selected()`, `filter_spec`, and
#>   `select_spec` is deprecated.
#> ℹ The deprecated feature was likely used in the teal.modules.clinical package.
#>   Please report the issue at
#>   <https://github.com/insightsengineering/teal.modules.clinical/issues>.
#> Warning: The `header` argument of `init()` is deprecated as of teal 0.16.0.
#> ℹ Use `modify_header()` on the teal app object instead. See ?modify_header for
#>   examples and more details.

if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}
```
