# Remove Teal Modules by Label

**\[experimental\]** Recursively removes modules from a teal modules
structure that match the specified label(s).

## Usage

``` r
remove_by_label(x, label)
```

## Arguments

- x:

  A `teal_modules` or `teal_module` object to filter

- label:

  Character vector of module labels to remove

## Value

The filtered teal modules object with matching modules removed, or NULL
if all modules are removed

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove a single module
filtered_mods <- remove_by_label(mods, "Deaths")

# Remove multiple modules
filtered_mods <- remove_by_label(mods, c("Deaths", "Lab Summary Table"))
} # }
```
