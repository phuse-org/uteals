
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/phuse-org/uteals/)

[Please Enter our Hex-Design Contest](https://github.com/phuse-org/uteals/blob/hex_contest/HEXAGON_DESIGN_CONTEST.md)

# uteals

An R package that provides common decorators, transformators, and utility functions to enhance the [teal package](https://cran.r-project.org/package=teal) ecosystem.

## Installation

```r
# Install from GitHub
devtools::install_github("phuse-org/uteals")
```

## Development Status

This package is currently under development. Please see references and vignettes for latest available features.

## Current Features

### Decorators
- **`ggplot_decorator()`** - Enhances teal modules with ggplot2 functionality
- **`patchwork_plot_decorator()`** - Combines multiple plots using patchwork

### Transformators  
- **`merge_levels_transformator()`** - Merges factor levels in datasets
- **`or_filtering_transformator()`** - Implements OR-based filtering logic

### Access Management
- **`extract_modules_to_yaml()`** - Extracts module structure to YAML configuration
- **`keep_by_label()`** - Filters modules by label patterns
- **`remove_by_label()`** - Removes modules by label patterns

## Vignettes

- [Module Access Management with rAccess](articles/module-access-management.html)

## About

The `uteals` package is being developed by the **Teal Enhancements for Industry Adoption** PHUSE working group. For more information about this initiative, visit: https://advance.hub.phuse.global/wiki/spaces/WEL/pages/30441473/Teal+Enhancements+for+Cross-Industry+Adoption

## Contributing

We encourage the open source community to join this effort and contribute to the development of uteals.