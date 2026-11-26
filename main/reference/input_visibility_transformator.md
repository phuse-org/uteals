# Input Visibility Transformator

Creates a `teal_transform_module` that auto-discovers Shiny inputs in
the module's encoding panel and provides toggle controls to show or hide
them.

## Usage

``` r
input_visibility_transformator(
  label = "Toggle Input Visibility",
  hidden_by_default = character(0)
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the accordion.

- hidden_by_default:

  (`character`) Vector of input label substrings (case-insensitive). Any
  discovered input whose label contains one of these strings will be
  hidden on first render.

## Value

A `teal_transform_module` that passes data through unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
teal.modules.clinical::tm_t_events(
  label = "AE Table",
  dataname = "ADAE",
  arm_var = teal.transform::choices_selected(c("ARM", "ARMCD"), "ARM"),
  llt = teal.transform::choices_selected("AEDECOD", "AEDECOD"),
  hlt = teal.transform::choices_selected("AEBODSYS", "AEBODSYS"),
  transformators = list(
    input_visibility_transformator(
      hidden_by_default = c("Sort", "Prune")
    )
  )
)
} # }
```
