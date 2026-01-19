# Combine levels of a variable into one level

**\[experimental\]**

## Usage

``` r
merge_levels_transformator(dataname)
```

## Arguments

- dataname:

  (`character(1)`) the name of the dataset which columns will be used
  for possible transformation.

## Value

[`teal::teal_transform_module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Details

This transformator allows the user to select a column from the dataset
and combine values of this column into a single level. Only selected
levels are affected.

The new combined level is called "Combined".

Merging works only for `character` or `factor` columns.

## Examples

``` r

app <- teal::init(
  data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
  modules = teal::modules(
    teal::example_module(
      transformators = list(merge_levels_transformator("IRIS"))
    )
  )
)
if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}
```
