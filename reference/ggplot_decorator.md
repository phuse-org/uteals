# ggplot Decorator

**\[experimental\]** Decorator function to update various settings for
ggplot plot objects

## Usage

``` r
ggplot_decorator(
  output_name,
  label_text = "decorator",
  render_ui = c(),
  plot_options = list(title = "", footnote = "", y_breaks = "", y_limits_max = "",
    y_limits_min = "", x_breaks = "", x_labels_discrete = "", x_labels_cont = "",
    y_labels_discrete = "", y_labels_cont = "", font_size_geom_text = "",
    font_size_plot_title = "", font_size_axis_title = "", font_size_axis_text = "")
)
```

## Arguments

- output_name:

  A name for the output plot object.

- label_text:

  Customized label text for the decorator

- render_ui:

  Vector of ggplot_options from the following list: "title" - Title of
  the plot, "footnote" - Footnote of the plot, "y_breaks" - Value of
  breaks(numeric) for y-axis. Note: y_limits_max and y_limits_min should
  also be provided, "y_limits_max" - Value of y-axis maximum
  limit(numeric). Note: y_limits_max and y_breaks should also be
  provided, "y_limits_min" - Value of y-axis minimum limit(numeric).
  Note: y_breaks and y_limits_min should also be provided, "x_breaks"-
  Value of breaks for continuous x-axis(numeric). Note: should be comma
  separated, "x_labels_discrete" - Values of labels for discrete x-axis.
  Note: should be comma separated, "x_labels_cont" - Values of labels
  for continuous x-axis. Note: should be comma separated,
  "y_labels_discrete" - Values of labels for discrete y-axis. Note:
  should be comma separated, "y_labels_cont" - Values of labels for
  continuous y-axis. Note: should be comma separated,
  "font_size_geom_text" - Font size of geom_text. Note: numeric value
  should be provided, "font_size_plot_title"- Font size of plot title
  text. Note: numeric value should be provided, "font_size_axis_title"-
  Font size of axis title text. Note: numeric value should be provided,
  "font_size_axis_text"- Font size of axis labels text. Note: numeric
  value should be provided

- plot_options:

  Named list with the list of values for the ggplot options. The app
  developer can specify the required list of options while calling the
  decorator.

## Value

Returns a modified plot object with the transformation applied.

## Details

The module creates a UI with text controls for specifying the list of
ggplot options given in the plot_options param value. The entered ggplot
options are applied to ggplot plot object.

## Examples

``` r
library(teal)
#> Loading required package: shiny
#> Loading required package: teal.data
#> Loading required package: teal.code
#> Loading required package: teal.slice
#> 
#> You are using teal version 1.0.0
#> 
#> Attaching package: ‘teal’
#> The following objects are masked from ‘package:teal.slice’:
#> 
#>     as.teal_slices, teal_slices
library(teal.modules.general)
#> Loading required package: ggplot2
#> Loading required package: teal.transform
app <- teal::init(
  data = teal.data::teal_data(IRIS = iris, code = "IRIS <- iris"),
  modules = teal::modules(
    teal.modules.general::tm_g_scatterplot(
      x = teal.transform::data_extract_spec(
        dataname = "IRIS",
        select = teal.transform::select_spec(choices = teal.transform::variable_choices(iris))
      ),
      y = teal.transform::data_extract_spec(
        dataname = "IRIS",
        select = teal.transform::select_spec(choices = teal.transform::variable_choices(iris))
      ),
      decorators = list(
        plot = ggplot_decorator(
          output_name = "plot", render_ui = c("title", "footnote", "font_size_axis_title")
        )
      )
    )
  )
)
#> Initializing tm_g_scatterplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
