# Module Access Management with \`rAccess\`

This vignette demonstrates how to implement module-level access control
in `teal` applications using the `rAccess` package integration provided
by uteals.

## Overview

The `rAccess` package enables role-based access control for `teal`
modules, allowing you to:

- Restrict module access based on user permissions
- Implement admin-only functionality
- Dynamically filter modules at runtime
- Provide graceful handling for unauthorized access

Users will only see and interact with modules they are authorized to
use.

## Library Calls

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`uteals`](https://github.com/phuse-org/uteals)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`rAccess`](https://johnsonandjohnson.github.io/rAccess/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`shiny`](https://shiny.posit.co/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`bslib`](https://rstudio.github.io/bslib/)`)`

## Quick Start

Here’s a minimal example to get started:

\
`# 1. Create modules`\
`mods`` ``<-`` `[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`  `[`tm_data_table`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_data_table.html)`(``"Data Table"``)``,`\
`  `[`tm_variable_browser`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_variable_browser.html)`(``"Variable Browser"``)`\
`)`\
\
`# 2. Extract module structure`\
[`extract_modules_to_yaml`](https://phuse-org.github.io/uteals/reference/extract_modules_to_yaml.md)`(``mods``, ``"panel_str.yml"``)`\
\
`# 3. Filter modules by user permissions`\
`filtered_mods`` ``<-`` `[`keep_by_label`](https://phuse-org.github.io/uteals/reference/keep_by_label.md)`(``mods``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Data Table"``)``)`

## Configuration

### Step 1: Generate Module Structure

Use the
[`extract_modules_to_yaml()`](https://phuse-org.github.io/uteals/reference/extract_modules_to_yaml.md)
function to automatically generate the module structure:

\
`# Define your teal modules`\
`mods`` ``<-`` `[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`  `[`tm_data_table`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_data_table.html)`(``"Data Table"``)``,`\
`  `[`tm_variable_browser`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_variable_browser.html)`(``"Variable Browser"``)``,`\
`  `[`tm_g_scatterplot`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_g_scatterplot.html)`(``"Scatterplot"``, x ``=`` ``...``, y ``=`` ``...``)``,`\
`  `[`tm_outliers`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_outliers.html)`(``"Summary Statistics"``, outlier_var ``=`` ``...``)`\
`)`\
\
`# Generate YAML structure`\
[`extract_modules_to_yaml`](https://phuse-org.github.io/uteals/reference/extract_modules_to_yaml.md)`(``mods``, ``"panel_str.yml"``)`

This creates:

``` yaml
# panel_str.yml
panel_str:
- access_panel: ADMIN
- access_panel: Modules
  access_units:
  - unit: Data Table
  - unit: Variable Browser
  - unit: Scatterplot
  - unit: Summary Statistics
```

### Step 2: Create `rAccess` Configuration

Create your main configuration file (`rAccess_config.yml`):

``` yaml
# rAccess_config.yml
module: rAccess
parameters:
  app_name: TealAccessDemo
  board_type: local
  access_mode: default
  unit_display: 'dropdown'
  user_df: !expr tibble::tribble(
    ~userid, ~username,
    "user1", "Regular User",
    "user2", "Limited User")
  use_rconnect_users: FALSE
  verbose: FALSE
  secure_mode: FALSE

# Copy the panel_str section from generated file
panel_str:
- access_panel: ADMIN
- access_panel: Modules
  access_units:
  - unit: Data Table
  - unit: Variable Browser
  - unit: Scatterplot
  - unit: Summary Statistics
```

## Implementation

### Pattern 1: Basic Module Filtering

\
`# Filter modules based on user access`\
`user_modules`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Data Table"``, ``"Variable Browser"``)`\
`filtered_modules`` ``<-`` `[`keep_by_label`](https://phuse-org.github.io/uteals/reference/keep_by_label.md)`(``all_modules``, ``user_modules``)`\
\
`# Remove specific modules`\
`restricted_modules`` ``<-`` `[`remove_by_label`](https://phuse-org.github.io/uteals/reference/remove_by_label.md)`(``all_modules``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Admin Panel"``)``)`

### Pattern 2: Dynamic Access Control

\
`server`` ``<-`` ``function``(``input``, ``output``, ``session``)`` ``{`\
`  ``# Get user identity`\
`  ``user_id`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``, `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"USER"``)``, ``session``$``user``)`\
\
`  ``# Initialize rAccess`\
`  ``iam`` ``<-`` `[`rAccess`](https://johnsonandjohnson.github.io/rAccess/reference/rAccess.html)`$``new``(``user ``=`` ``user_id``, config ``=`` ``"rAccess_config.yml"``)`\
\
`  ``# Get user permissions`\
`  ``user_access`` ``<-`` ``iam``$``get_user_accesslist``(``)`\
`  ``allowed_modules`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``user_access``, use.names ``=`` ``FALSE``)`\
\
`  ``# Filter modules`\
`  ``filtered_modules`` ``<-`` `[`keep_by_label`](https://phuse-org.github.io/uteals/reference/keep_by_label.md)`(``all_modules``, ``allowed_modules``)`\
\
`  ``# Handle edge cases`\
`  ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``filtered_modules``)``)`` ``{`\
`    ``filtered_modules`` ``<-`` ``create_no_access_module``(``)`\
`  ``}`\
`}`

## Complete Application Example

### Configuration

``` yaml
# rAccess_config.yml
module: rAccess
parameters:
  app_name: TealAccessDemo
  board_type: local
  access_mode: default
  unit_display: 'dropdown'
  user_df: !expr tibble::tribble(
    ~userid, ~username,
    "user1", "Regular User",
    "user2", "Limited User")
  use_rconnect_users: FALSE
  verbose: FALSE
  secure_mode: FALSE
```

\
`### Data Setup`\
`# Prepare sample data`\
`data`` ``<-`` ``teal.data``::`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(`\
`  IRIS ``=`` ``iris``,`\
`  MTCARS ``=`` ``mtcars``,`\
`  code ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"IRIS <- iris"``, ``"MTCARS <- mtcars"``)`\
`)`\
\
`### Module Definition`\
`# Define all available modules`\
`all_modules`` ``<-`` `[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`  `[`tm_data_table`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_data_table.html)`(``"Data Table"``)``,`\
`  `[`tm_variable_browser`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_variable_browser.html)`(``"Variable Browser"``)``,`\
`  `[`tm_g_scatterplot`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_g_scatterplot.html)`(`\
`    label ``=`` ``"Scatterplot"``,`\
`    x ``=`` `[`data_extract_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html)`(`\
`      dataname ``=`` ``"IRIS"``,`\
`      select ``=`` `[`select_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/select_spec.html)`(``choices ``=`` `[`variable_choices`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)`(``iris``)``)`\
`    ``)``,`\
`    y ``=`` `[`data_extract_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html)`(`\
`      dataname ``=`` ``"IRIS"``,`\
`      select ``=`` `[`select_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/select_spec.html)`(``choices ``=`` `[`variable_choices`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)`(``iris``)``)`\
`    ``)`\
`  ``)``,`\
`  `[`tm_outliers`](https://insightsengineering.github.io/teal.modules.general/latest-tag/reference/tm_outliers.html)`(`\
`    label ``=`` ``"Summary Statistics"``,`\
`    outlier_var ``=`` `[`data_extract_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/data_extract_spec.html)`(`\
`      dataname ``=`` ``"MTCARS"``,`\
`      select ``=`` `[`select_spec`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/select_spec.html)`(``choices ``=`` `[`variable_choices`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/variable_choices.html)`(``mtcars``, `[`c`](https://rdrr.io/r/base/c.html)`(``"mpg"``, ``"hp"``, ``"wt"``)``)``)`\
`    ``)`\
`  ``)`\
`)`\
\
`### UI Implementation`\
`ui`` ``<-`` ``bslib``::`[`page_navbar`](https://rstudio.github.io/bslib/reference/page_navbar.html)`(`\
`  title ``=`` ``"Teal Access Demo"``,`\
\
`  ``# Main application tab`\
`  ``bslib``::`[`nav_panel`](https://rstudio.github.io/bslib/reference/nav-items.html)`(`\
`    ``"Analytics Modules"``,`\
`    `[`uiOutput`](https://rdrr.io/pkg/shiny/man/htmlOutput.html)`(``"teal_ui"``)`\
`  ``)``,`\
\
`  ``# Admin panel (conditionally visible)`\
`  ``bslib``::`[`nav_panel`](https://rstudio.github.io/bslib/reference/nav-items.html)`(`\
`    ``"Access Control"``,`\
`    ``rAccess``::`[`module_iam_ui`](https://johnsonandjohnson.github.io/rAccess/reference/module_iam_ui.html)`(``"access_control"``)``,`\
`    ``tags``$``div``(`\
`      class ``=`` ``"alert alert-info"``,`\
`      ``tags``$``h5``(``"Administrator Panel"``)``,`\
`      ``tags``$``p``(``"Manage user access and permissions here."``)`\
`    ``)`\
`  ``)`\
`)`\
\
`### Server Implementation`\
`server`` ``<-`` ``function``(``input``, ``output``, ``session``)`` ``{`\
`  ``# User authentication`\
`  ``user_id`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``, ``"user1"``, ``session``$``user``)`\
\
`  ``# Initialize access control`\
`  ``iam`` ``<-`` `[`rAccess`](https://johnsonandjohnson.github.io/rAccess/reference/rAccess.html)`$``new``(``user ``=`` ``user_id``, config ``=`` ``"rAccess_config.yml"``)`\
\
`  ``# Admin module server (conditional)`\
`  ``if`` ``(``iam``$``no_admin``(``)`` ``||`` ``iam``$``is_admin``(``)``)`` ``{`\
`    ``rAccess``::`[`module_iam_server`](https://johnsonandjohnson.github.io/rAccess/reference/module_iam_server.html)`(``"access_control"``, ``iam``)`\
`  ``}`\
\
`  ``# Filter modules based on permissions`\
`  ``user_access_list`` ``<-`` ``iam``$``get_user_accesslist``(``)`\
`  ``users_modules`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``user_access_list``, use.names ``=`` ``FALSE``)`\
`  ``filtered_modules`` ``<-`` `[`keep_by_label`](https://phuse-org.github.io/uteals/reference/keep_by_label.md)`(``all_modules``, ``users_modules``)`\
\
`  ``# Handle no access`\
`  ``if`` ``(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``filtered_modules``)``)`` ``{`\
`    ``filtered_modules`` ``<-`` `[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`      `[`module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`        label ``=`` ``"No Access"``,`\
`        ui ``=`` ``function``(``id``)`` ``tags``$``div``(`\
`          class ``=`` ``"alert alert-warning"``,`\
`          `[`h4`](https://rstudio.github.io/htmltools/reference/builder.html)`(``"Access Restricted"``)``,`\
`          `[`p`](https://rstudio.github.io/htmltools/reference/builder.html)`(``"You do not have permission to access any modules."``)``,`\
`          `[`p`](https://rstudio.github.io/htmltools/reference/builder.html)`(``"Please contact your administrator."``)`\
`        ``)`\
`      ``)`\
`    ``)`\
`  ``}`\
\
`  ``# Render teal UI`\
`  ``output``$``teal_ui`` ``<-`` `[`renderUI`](https://rdrr.io/pkg/shiny/man/renderUI.html)`(``{`\
`    `[`tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)`(`\
`      `[`ui_teal`](https://insightsengineering.github.io/teal/latest-tag/reference/module_teal.html)`(``"teal_app"``, ``filtered_modules``)``,`\
`      `[`ui_session_info`](https://insightsengineering.github.io/teal/latest-tag/reference/module_session_info.html)`(``"session_info"``)`\
`    ``)`\
`  ``}``)`\
\
`  ``# Teal server`\
`  `[`srv_teal`](https://insightsengineering.github.io/teal/latest-tag/reference/module_teal.html)`(``"teal_app"``, data ``=`` ``data``, modules ``=`` ``filtered_modules``)`\
`  `[`srv_session_info`](https://insightsengineering.github.io/teal/latest-tag/reference/module_session_info.html)`(``"session_info"``)`\
`}`\
\
`### Application Launch`\
`# Create and run the application`\
`app`` ``<-`` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui``, ``server``)`\
\
`# For development`\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  `[`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)`(``app``)`\
`}`

### Running the Example

The app will show different modules based on the user: - `admin`: Access
to admin panel - `other users`: Access based on `rAccess` permissions -
Unauthorized users: “No Access” message

**Note**: Until you define and add at least one admin, the admin tab
will be accessible by everyone. Once there is at least one admin
defined, access control becomes enforced.

### Access-boards Storage Setup

In this example, with `board_type: local` configured in
`rAccess_config.yml`, access lists are stored locally in the `./data/`
directory within your application. Alternative storage options include
`AWS S3` or `Posit Connect` pins - refer to the `rAccess` documentation
for configuration details.

### Debugging Tips

``` r
# Ensure the rAccess configuration (`rAccess_config.yml` in the example) is
accessible by your app.

# Ensure module labels match exactly between configuration and code

# Enable verbose logging
iam <- rAccess$new(user = user_id, config = "config.yml", verbose = TRUE)

# Check user access list
print(iam$get_user_accesslist())

# Verify module labels
print(sapply(all_modules$children, function(x) x$label))
```

## Summary

The uteals package provides essential utilities for implementing access
control in teal applications:

- [`extract_modules_to_yaml()`](https://phuse-org.github.io/uteals/reference/extract_modules_to_yaml.md):
  Automates configuration generation
- [`keep_by_label()`](https://phuse-org.github.io/uteals/reference/keep_by_label.md):
  Filters modules by user permissions
- [`remove_by_label()`](https://phuse-org.github.io/uteals/reference/remove_by_label.md):
  Excludes restricted modules

This integration enables secure, role-based access control while
maintaining the flexibility and power of the teal framework.

## Further Reading

- [`rAccess`
  Documentation](https://johnsonandjohnson.github.io/rAccess/)
- [`Teal` as `Shiny`
  Module](https://insightsengineering.github.io/teal/latest-tag/articles/teal-as-a-shiny-module.html)
