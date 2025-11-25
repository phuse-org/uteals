# Create Relative risk column

**\[experimental\]**

## Usage

``` r
create_rel_risk_transformator(dataname, column_name, control_group, label_name)
```

## Arguments

- dataname:

  (`character(1)`) the name of the dataset which columns will be used
  for possible transformation. `dataname` should be passed in quotes ex:
  `"ADSL"`.

- column_name:

  (`character(1)`) field or variable from the dataset.

- control_group:

  (`character(1)`) one of the existing level from the selected
  `column_name`.

- label_name:

  (`character(1)`) label for the new field or variable.

## Value

[`teal::teal_transform_module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Details

This transformator allows the user to select a column and control group
from the dataset and create a relative risk column.

## Examples

``` r
app <- teal::init(
  data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
  modules = teal::modules(
    teal::example_module(
      transformators = list(create_rel_risk_transformator("IRIS"))
    )
  )
)
if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}
```
