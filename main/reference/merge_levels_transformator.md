# Combine levels of a variable into one level

**\[experimental\]**

## Usage

``` r
merge_levels_transformator(dataname, predefined = list())
```

## Arguments

- dataname:

  (`character(1)`) the name of the dataset which columns will be used
  for possible transformation.

- predefined:

  (`list`) the list which has variable name, levels and new label

## Value

[`teal::teal_transform_module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Details

This transformator allows the user to select a column from the dataset
and merge levels of this column into a new level. Only selected levels
are affected.

## Examples

``` r

app <- teal::init(
  data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
  modules = teal::modules(
    teal::example_module(
      transformators = list(merge_levels_transformator(
        dataname = "IRIS",
        predefined = list(
          list("Species", "setosa", "SETOSA_WITHIN_FIX"),
          list("Petal.Width", c(0.2, 0.3, 0.5), 12)
        )
      ))
    )
  )
)
if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}
```
