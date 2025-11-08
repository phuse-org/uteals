# Patchwork Decorator

**\[experimental\]** Decorator function to add plot title and footnote
to patchwork plots

## Usage

``` r
patchwork_plot_decorator(output_name, label_text = "decorator")
```

## Arguments

- output_name:

  (`character(1)`) A name for the output plot object.

- label_text:

  (`character(1)`) A customised label text for the decorator.

## Value

(`teal.data::qenv`) Returns a modified plot object with the
transformation applied.

## Details

The module creates a UI with text controls for plot title and footnote.
The entered title and footnote text are applied to the patchwork plots.
