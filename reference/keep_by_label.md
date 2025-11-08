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

  A teal_modules or teal_module object to filter

- label:

  Character vector of module labels to keep

## Value

Filtered teal_modules or teal_module object, or NULL if no matches

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep only specific modules by label
filtered_mods <- keep_by_label(mods, c("Data Table", "Disposition"))
} # }
```
