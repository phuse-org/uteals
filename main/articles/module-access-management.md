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

``` r
library(uteals)
library(teal)
library(teal.modules.general)
library(rAccess)
library(shiny)
library(bslib)
```

## Quick Start

Here’s a minimal example to get started:

``` r
# 1. Create modules
mods <- modules(
  tm_data_table("Data Table"),
  tm_variable_browser("Variable Browser")
)

# 2. Extract module structure
extract_modules_to_yaml(mods, "panel_str.yml")

# 3. Filter modules by user permissions
filtered_mods <- keep_by_label(mods, c("Data Table"))
```

## Configuration

### Step 1: Generate Module Structure

Use the
[`extract_modules_to_yaml()`](https://github.com/phuse-org/uteals/reference/extract_modules_to_yaml.md)
function to automatically generate the module structure:

``` r
# Define your teal modules
mods <- modules(
  tm_data_table("Data Table"),
  tm_variable_browser("Variable Browser"),
  tm_g_scatterplot("Scatterplot", x = ..., y = ...),
  tm_outliers("Summary Statistics", outlier_var = ...)
)

# Generate YAML structure
extract_modules_to_yaml(mods, "panel_str.yml")
```

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

``` r
# Filter modules based on user access
user_modules <- c("Data Table", "Variable Browser")
filtered_modules <- keep_by_label(all_modules, user_modules)

# Remove specific modules
restricted_modules <- remove_by_label(all_modules, c("Admin Panel"))
```

### Pattern 2: Dynamic Access Control

``` r
server <- function(input, output, session) {
  # Get user identity
  user_id <- ifelse(interactive(), Sys.getenv("USER"), session$user)

  # Initialize rAccess
  iam <- rAccess$new(user = user_id, config = "rAccess_config.yml")

  # Get user permissions
  user_access <- iam$get_user_accesslist()
  allowed_modules <- unlist(user_access, use.names = FALSE)

  # Filter modules
  filtered_modules <- keep_by_label(all_modules, allowed_modules)

  # Handle edge cases
  if (is.null(filtered_modules)) {
    filtered_modules <- create_no_access_module()
  }
}
```

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

``` r
### Data Setup
# Prepare sample data
data <- teal.data::teal_data(
  IRIS = iris,
  MTCARS = mtcars,
  code = c("IRIS <- iris", "MTCARS <- mtcars")
)

### Module Definition
# Define all available modules
all_modules <- modules(
  tm_data_table("Data Table"),
  tm_variable_browser("Variable Browser"),
  tm_g_scatterplot(
    label = "Scatterplot",
    x = data_extract_spec(
      dataname = "IRIS",
      select = select_spec(choices = variable_choices(iris))
    ),
    y = data_extract_spec(
      dataname = "IRIS",
      select = select_spec(choices = variable_choices(iris))
    )
  ),
  tm_outliers(
    label = "Summary Statistics",
    outlier_var = data_extract_spec(
      dataname = "MTCARS",
      select = select_spec(choices = variable_choices(mtcars, c("mpg", "hp", "wt")))
    )
  )
)

### UI Implementation
ui <- bslib::page_navbar(
  title = "Teal Access Demo",

  # Main application tab
  bslib::nav_panel(
    "Analytics Modules",
    uiOutput("teal_ui")
  ),

  # Admin panel (conditionally visible)
  bslib::nav_panel(
    "Access Control",
    rAccess::module_iam_ui("access_control"),
    tags$div(
      class = "alert alert-info",
      tags$h5("Administrator Panel"),
      tags$p("Manage user access and permissions here.")
    )
  )
)

### Server Implementation
server <- function(input, output, session) {
  # User authentication
  user_id <- ifelse(interactive(), "user1", session$user)

  # Initialize access control
  iam <- rAccess$new(user = user_id, config = "rAccess_config.yml")

  # Admin module server (conditional)
  if (iam$no_admin() || iam$is_admin()) {
    rAccess::module_iam_server("access_control", iam)
  }

  # Filter modules based on permissions
  user_access_list <- iam$get_user_accesslist()
  users_modules <- unlist(user_access_list, use.names = FALSE)
  filtered_modules <- keep_by_label(all_modules, users_modules)

  # Handle no access
  if (is.null(filtered_modules)) {
    filtered_modules <- modules(
      module(
        label = "No Access",
        ui = function(id) tags$div(
          class = "alert alert-warning",
          h4("Access Restricted"),
          p("You do not have permission to access any modules."),
          p("Please contact your administrator.")
        )
      )
    )
  }

  # Render teal UI
  output$teal_ui <- renderUI({
    tagList(
      ui_teal("teal_app", filtered_modules),
      ui_session_info("session_info")
    )
  })

  # Teal server
  srv_teal("teal_app", data = data, modules = filtered_modules)
  srv_session_info("session_info")
}

### Application Launch
# Create and run the application
app <- shinyApp(ui, server)

# For development
if (interactive()) {
  runApp(app)
}
```

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

- [`extract_modules_to_yaml()`](https://github.com/phuse-org/uteals/reference/extract_modules_to_yaml.md):
  Automates configuration generation
- [`keep_by_label()`](https://github.com/phuse-org/uteals/reference/keep_by_label.md):
  Filters modules by user permissions
- [`remove_by_label()`](https://github.com/phuse-org/uteals/reference/remove_by_label.md):
  Excludes restricted modules

This integration enables secure, role-based access control while
maintaining the flexibility and power of the teal framework.

## Further Reading

- [`rAccess`
  Documentation](https://johnsonandjohnson.github.io/rAccess/)
- [`Teal` as `Shiny`
  Module](https://insightsengineering.github.io/teal/latest-tag/articles/teal-as-a-shiny-module.html)
