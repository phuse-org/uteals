# Title and Footer Decorator

**\[experimental\]** A function to create a UI component for selecting a
title and footer for tables or plots. It reads title information from a
specified Excel file and allows users to choose a title from the
provided options. It also provides user with flexibility to customize
the title and footer according to their specific needs.

## Usage

``` r
title_footer_decorator(
  output_name,
  titles_file,
  choices = NULL,
  selected = NULL
)
```

## Arguments

- output_name:

  (`character(1)`) a name for the output object (e.g., a plot or table).

- titles_file:

  (`character(1)`) the path to an Excel file containing title and footer
  information. The function expects the titles to be in the first sheet
  named `Sheet1`.

- choices:

  (`character`) an array of titles and footers, which are available for
  selection. Default `NULL`, indicates all titles and footers are
  available.

- selected:

  (`character(1)`) the selected title or footer. Default `NULL`,
  indicates no title or footer is selected.

## Value

[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Details

The module creates a UI with a dropdown for selecting a title. Once a
title is selected, it updates the output by either adding titles to a
table or modifying a plot's title and caption accordingly. Additionally,
it includes a checkbox that user can check to enable customization,
allowing them to enter their own values for the title and footer in the
designated input fields.

## See also

For the exact Excel workbook layout expected by this function, see the
package vignette:
[`vignette("title-footer-decorator-excel-structure", package = "uteals")`](https://github.com/phuse-org/uteals/articles/title-footer-decorator-excel-structure.md)

## Examples

``` r
library(openxlsx)
library(teal.modules.general)
#> Loading required package: ggplot2

example_excel <- data.frame(
  `TABLE ID` = c(
    "DO_NOT_DELETE",
    "TSFAE01A", "TSFAE01A", "TSFAE01A",
    "TSFAE01B", "TSFAE01B"
  ),
  IDENTIFIER = c(
    "DO_NOT_DELETE",
    "TITLE", "FOOTNOTE1", "FOOTNOTE2",
    "TITLE", "FOOTNOTE1"
  ),
  TEXT = c(
    "DO_NOT_DELETE",
    "Adverse Events Summary A", "Source: Clinical Study Report", "Confidential",
    "Adverse Events Summary B", "Draft Version"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

temp_titles <- tempfile(fileext = ".xlsx")
write.xlsx(example_excel, temp_titles, sheetName = "Sheet1", asTable = TRUE)
plot_module <- tm_g_scatterplot(
  label = "Scatter Plot",
  x = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      choices = variable_choices(
        "IRIS", c("Sepal.Length", "Sepal.Width")
      ), selected = "Sepal.Length"
    )
  ),
  y = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      choices = variable_choices(
        "IRIS", c("Petal.Length", "Petal.Width")
      ), selected = "Petal.Length"
    )
  ),
  decorators = list(
    plot = title_footer_decorator(
      "plot", temp_titles,
      choices = c("TSFAE01A", "TSFAE01B"), selected = NULL
    )
  )
)
#> Initializing tm_g_scatterplot

# Initialize the teal app
app <- init(
  data = teal_data(IRIS = iris),
  modules = list(plot_module)
)

# Run the app
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
