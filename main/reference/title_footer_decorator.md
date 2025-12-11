# Title and Footer Decorator

**\[experimental\]** A function to create a UI component for selecting a
title and footer for tables or plots. It reads title information from a
specified Excel file and allows users to choose a title from the
provided options. It also provides user with flexibility to customize
the title and footer according to their specific needs.

## Usage

``` r
title_footer_decorator(output_name, titles_file)
```

## Arguments

- output_name:

  (`character(1)`) a name for the output object (e.g., a plot or table).

- titles_file:

  (`character(1)`) the path to an Excel file containing title and footer
  information. The function expects the titles to be in the first sheet
  named `Sheet1`.

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
