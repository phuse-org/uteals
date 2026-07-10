# Filter Teal Modules by Label

**\[experimental\]** Recursively filters a teal modules object to keep
only modules whose labels match the specified labels. Removes modules
that don't match and empty parent containers.

## Usage

``` r
keep_by_label(x, label)
```

## Arguments

- x:

  (`teal_module` or `teal_modules`) the object to filter.

- label:

  (`character(1)`) character vector of module labels to keep.

## Value

Filtered `teal_modules` or `teal_module` object, or `NULL` if none
matches.

## Examples

``` r
# Keep only specific modules by label
mods <- teal::modules(
  teal::example_module("mod1"),
  teal::example_module("mod2")
)
filtered_mods <- keep_by_label(mods, c("Data Table", "Disposition"))
```
