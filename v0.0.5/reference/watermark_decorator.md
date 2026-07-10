# Watermark Decorator

**\[experimental\]** A function to create a UI component for selecting
watermark text for plots. Note: Currently tables are not supported

## Usage

``` r
watermark_decorator(output_name, watermark_text = "", font_size = 90)
```

## Arguments

- output_name:

  (`character(1)`) a name for the output object (e.g., a plot or table).

- watermark_text:

  (`character(1)`) text to display for the watermark.

- font_size:

  (`character(1)`) font size for the watermark text.

## Value

[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)

## Details

The module creates a UI with `textInput` for specifying watermark text
and font size. the entered watermark text is displayed with a default
`gridify` layout.
