# Remove Teal Modules by Label

**\[experimental\]** Recursively removes modules from a teal modules
structure that match the specified labels.

## Usage

``` r
remove_by_label(x, label)
```

## Arguments

- x:

  (`teal_module` or `teal_modules`) The object to filter.

- label:

  (`character(1)`) character vector of module labels to remove.

## Value

The filtered teal modules object with matching modules removed, or
`NULL` if all modules are removed.

## Examples

``` r
mods <- teal::modules(
  teal::example_module("mod1"),
  teal::example_module("mod2")
)
# Remove a single module
filtered_mods <- remove_by_label(mods, "Deaths")

# Remove multiple modules
filtered_mods <- remove_by_label(mods, c("Deaths", "Lab Summary Table"))
```
