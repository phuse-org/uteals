# Collaborative Report Management with \`tm_report_manager\`

This vignette demonstrates how to add collaborative report management to
a `teal` application using the `tm_report_manager` module from `uteals`.

## Overview

The `tm_report_manager` module extends `teal.reporter` with persistent,
multi-user report management. It allows users to:

- Save and load reports across sessions
- Collaborate on shared reports with file-based locking
- Auto-save report changes
- Rebuild reports with updated data
- Merge multiple reports into one

## Library Calls

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`uteals`](https://github.com/phuse-org/uteals)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal`](https://insightsengineering.github.io/teal/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.data`](https://insightsengineering.github.io/teal.data/)`)`

## Quick Start

Add
[`tm_report_manager()`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md)
to your
[`teal::modules()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
call:

\
`app`` ``<-`` ``teal``::`[`init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)`(`\
`  data ``=`` ``teal.data``::`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(``IRIS ``=`` ``iris``)``,`\
`  modules ``=`` ``teal``::`[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`    ``teal``::`[`example_module`](https://insightsengineering.github.io/teal/latest-tag/reference/example_module.html)`(``)``,`\
`    `[`tm_report_manager`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md)`(``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`\
`}`

Reports are stored in a `reports/` folder relative to the working
directory by default.

## Configuration

[`tm_report_manager()`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md)
accepts two arguments:

| Argument | Default | Description |
|----|----|----|
| `reports_path` | `"reports"` | Absolute or relative path where reports are stored |
| `auto_save` | `TRUE` | Whether to auto-save the active report on every card change |

\
[`tm_report_manager`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md)`(`\
`  reports_path ``=`` ``"/shared/network/drive/reports"``,`\
`  auto_save ``=`` ``TRUE`\
`)`

Use an absolute path pointing to a shared network location to enable
multi-user collaboration.

## How It Works

### Creating a Report

When a user adds the first card to the `teal.reporter` previewer, a
modal dialog prompts them to name the new report. The report is then
saved to `reports_path` and locked for that user.

A report directory contains:

    reports/
    └── My Report/
        ├── Report.json       # Report cards (teal.reporter format)
        ├── code.rds          # R code for each card (used for rebuild)
        ├── .lockfile.rds     # Username of the locking user
        ├── .creator.rds      # Username of the report creator
        └── .rebuild_time.rds # Timestamp of last rebuild

### Locking

Each report can be locked by one user at a time. The lock prevents other
users from saving or deleting the report. The report table shows the
lock status:

- **Active** (green): The report is currently loaded by you with full
  access.
- **Locked** (yellow): The report is locked by another user — you can
  view it in read-only mode.
- **Unlocked** (grey): No user holds the lock — you can view it in
  read-only mode or delete it.

Locks are automatically released when the Shiny session ends.

### Auto-Save

With `auto_save = TRUE` (the default), any change to the active report’s
cards triggers an automatic save. A progress indicator is shown during
the save.

If the active report is in read-only mode, a warning notification is
shown instead of saving.

### Loading a Report

Click the load/eye icon next to a report in the table:

- **Your locked report**: loaded with full edit access.
- **Another user’s locked report**: loaded in read-only mode.
- **Unlocked report**: loaded in read-only mode.

### Rebuilding a Report

The rebuild action re-evaluates the R code stored for each card against
the current data environment. This is useful when the underlying data
has changed and you want to refresh the report output without manually
re-adding cards.

Click the refresh icon next to any report to trigger a rebuild.

### Merging Reports

Click **Merge reports** (only enabled when you have an active report) to
combine cards from two or more reports into a single new report. Only
reports that have saved content (a `Report.json` file) appear as merge
candidates.

## Complete Example

\
`app`` ``<-`` ``teal``::`[`init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)`(`\
`  data ``=`` ``teal.data``::`[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html)`(`\
`    ADSL ``=`` ``teal.data``::`[`rADSL`](https://insightsengineering.github.io/teal.data/latest-tag/reference/random_cdisc_data.html)`,`\
`    code ``=`` ``"ADSL <- teal.data::rADSL"`\
`  ``)``,`\
`  modules ``=`` ``teal``::`[`modules`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)`(`\
`    ``teal``::`[`example_module`](https://insightsengineering.github.io/teal/latest-tag/reference/example_module.html)`(``label ``=`` ``"Summary"``)``,`\
`    `[`tm_report_manager`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md)`(`\
`      reports_path ``=`` ``"reports"``,`\
`      auto_save ``=`` ``TRUE`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`\
`}`

## Permissions Summary

The table below summarises which actions are available depending on the
report state:

| Action | Active report | Your locked report | Others’ locked report | Unlocked report |
|----|----|----|----|----|
| Edit title | Yes | Yes | No | No |
| Delete | No | Yes | No | Yes |
| Rebuild | Yes | Yes | Yes | Yes |
| Load / View | Already active | Full access | Read-only | Read-only |
| Release lock | Yes | Yes | No | N/A |
| Merge | Yes\* | Yes\* | Yes\* | Yes\* |

\* Merge requires reports to have saved content (cards).

## Further Reading

- [`teal.reporter`
  Documentation](https://insightsengineering.github.io/teal.reporter/)
- [`teal` Documentation](https://insightsengineering.github.io/teal/)
