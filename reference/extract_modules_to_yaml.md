# Extract Non-Parent Module Labels to YAML

**\[experimental\]** Extracts module labels from a teal modules object,
filters out parent modules (grouping containers), and generates a YAML
file with the functional modules.

## Usage

``` r
extract_modules_to_yaml(mods, filepath)
```

## Arguments

- mods:

  A teal modules object containing the module structure

- filepath:

  Character string specifying the output YAML file path

## Value

Character vector of non-parent module labels

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract modules from mods object to YAML file
labels <- extract_modules_to_yaml(mods, "panel_str_modules.yml")
} # }
```
