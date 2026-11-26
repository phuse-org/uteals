# Convert the discovered_inputs payload into a list

Shiny converts the array of objects sent by the browser in different
ways depending on its shape.

## Usage

``` r
.normalise_discovered(raw)
```

## Arguments

- raw:

  Raw input value from Shiny.

## Value

A list of lists, each with `key`, `label`, and optionally `parentKey`.
