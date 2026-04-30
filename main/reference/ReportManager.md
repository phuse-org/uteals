# `ReportManager` Object

This can be used to manage state of report, load and save them into file
It will create
[`shiny::reactiveVal`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
variables and load report list from `report_path`

## Public fields

- `reports_path`:

  (`character(1)`)\
  An absolute path to a folder where reports are stored

- `current_report_title`:

  (`character(1)`)\
  `reactiveVal` field that stores current report title

- `available_reports`:

  ([`list()`](https://rdrr.io/r/base/list.html))\
  `reactiveVal` field that stores a list of available reports

- `my_locked_report`:

  (`character(1)`)\
  `reactiveVal` field that stores currently locked report title

- `read_only_mode`:

  (`logical(1)`)\
  `reactiveVal` field that indicates if current report is in read-only
  mode

- `session`:

  Placeholder for Shiny session object Initialize `ReportManager`

## Methods

### Public methods

- [`ReportManager$new()`](#method-ReportManager-new)

- [`ReportManager$list_reports()`](#method-ReportManager-list_reports)

- [`ReportManager$set_current_report_title()`](#method-ReportManager-set_current_report_title)

- [`ReportManager$get_abs_report_path()`](#method-ReportManager-get_abs_report_path)

- [`ReportManager$save_report_files()`](#method-ReportManager-save_report_files)

- [`ReportManager$load_report()`](#method-ReportManager-load_report)

- [`ReportManager$merge_reports()`](#method-ReportManager-merge_reports)

- [`ReportManager$reset()`](#method-ReportManager-reset)

- [`ReportManager$delete_report()`](#method-ReportManager-delete_report)

- [`ReportManager$is_locked_by_other()`](#method-ReportManager-is_locked_by_other)

- [`ReportManager$rename_report()`](#method-ReportManager-rename_report)

- [`ReportManager$is_new_report()`](#method-ReportManager-is_new_report)

- [`ReportManager$create_new_report()`](#method-ReportManager-create_new_report)

- [`ReportManager$auto_save_observer()`](#method-ReportManager-auto_save_observer)

- [`ReportManager$unlock_report_public()`](#method-ReportManager-unlock_report_public)

- [`ReportManager$release_lock()`](#method-ReportManager-release_lock)

- [`ReportManager$rebuild_report()`](#method-ReportManager-rebuild_report)

- [`ReportManager$clone()`](#method-ReportManager-clone)

------------------------------------------------------------------------

### Method `new()`

This can be used to manage state of report, load and save them into file
It will create
[`shiny::reactiveVal`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
variables and load report list from `reports_path`

#### Usage

    ReportManager$new(reports_path, session)

#### Arguments

- `reports_path`:

  (character) An absolute path to where reports are stored

- `session`:

  Session object passed from `moduleServer`

#### Returns

`ReportManager` object List available reports from objects'
`reports_path`

------------------------------------------------------------------------

### Method `list_reports()`

#### Usage

    ReportManager$list_reports()

#### Returns

`data.frame` with fields `reports`, `created_by`, `locked_by`, and
`last_rebuild` Method for storing loaded report title inside object.
This assigns a value in `current_report_title` field

------------------------------------------------------------------------

### Method `set_current_report_title()`

#### Usage

    ReportManager$set_current_report_title(title_or_index)

#### Arguments

- `title_or_index`:

  (character/numeric) Title of report or an index from objects'
  `available_reports` Get absolute report path

------------------------------------------------------------------------

### Method `get_abs_report_path()`

#### Usage

    ReportManager$get_abs_report_path(title)

#### Arguments

- `title`:

  (character) A title of report to get path of

#### Returns

(character) A path of desired report Save report with provided title
This utilizes methods from
[`teal.reporter::Reporter`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/Reporter.html)
to store files required to load and save reports If a report with
specific title exists, it will overwrite it.

------------------------------------------------------------------------

### Method `save_report_files()`

#### Usage

    ReportManager$save_report_files(report_title, reporter, first = FALSE)

#### Arguments

- `report_title`:

  (`character`) A title for the saved report.

- `reporter`:

  (`Reporter`) A reporter object passed from
  [`teal::init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- `first`:

  (`logical`) Whether this is the first time saving the report.

  Load report to report previewer This utilizes methods from
  [`teal.reporter::Reporter`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/Reporter.html)
  to load reports

------------------------------------------------------------------------

### Method `load_report()`

#### Usage

    ReportManager$load_report(selected_title, reporter, skip_lock = FALSE)

#### Arguments

- `selected_title`:

  (`integer` or `character`) An index or a title of a report to load

- `reporter`:

  (`Reporter`) A reporter object passed from
  [`teal::init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- `skip_lock`:

  (`boolean`) Whether to skip locking a report or not Merge reports
  together

------------------------------------------------------------------------

### Method `merge_reports()`

#### Usage

    ReportManager$merge_reports(x_title, y_title, reporter)

#### Arguments

- `x_title`:

  Base report title

- `y_title`:

  Title of report to merge into x

- `reporter`:

  A `TealReporter` object Reset report manager

------------------------------------------------------------------------

### Method `reset()`

#### Usage

    ReportManager$reset()

#### Returns

the return of `self$list_reports()` Delete report

------------------------------------------------------------------------

### Method `delete_report()`

#### Usage

    ReportManager$delete_report(selected_title)

#### Arguments

- `selected_title`:

  (integer or character) An index or a title of a report to load Checks
  if report is locked by another user

------------------------------------------------------------------------

### Method `is_locked_by_other()`

#### Usage

    ReportManager$is_locked_by_other(
      report_title,
      verbose = TRUE,
      message_type = "error"
    )

#### Arguments

- `report_title`:

  (character) A title for the saved report.

- `verbose`:

  (boolean) Whether to show notification on locked report or not

- `message_type`:

  (character) `shiny::showNotification's` type argument

#### Returns

logical(1) TRUE if locked by another user, FALSE if unlocked or locked
by me Rename report

------------------------------------------------------------------------

### Method `rename_report()`

#### Usage

    ReportManager$rename_report(old_title, new_title, reporter)

#### Arguments

- `old_title`:

  (character) Current report title

- `new_title`:

  (character) New report title

- `reporter`:

  (Reporter) A reporter object Check if this would be a new report

------------------------------------------------------------------------

### Method `is_new_report()`

#### Usage

    ReportManager$is_new_report(reporter)

#### Arguments

- `reporter`:

  (Reporter) A reporter object

#### Returns

logical indicating if this is a new report Create new report with title

------------------------------------------------------------------------

### Method `create_new_report()`

#### Usage

    ReportManager$create_new_report(report_title, reporter)

#### Arguments

- `report_title`:

  (character) Title for the new report

- `reporter`:

  (Reporter) A reporter object Add `observeEvent` to invoke auto save on
  reporter cards change.

------------------------------------------------------------------------

### Method `auto_save_observer()`

#### Usage

    ReportManager$auto_save_observer(reporter)

#### Arguments

- `reporter`:

  (Reporter) A reporter object passed from
  [`teal::init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
  Unlock a locked report

------------------------------------------------------------------------

### Method `unlock_report_public()`

#### Usage

    ReportManager$unlock_report_public(report_title)

#### Arguments

- `report_title`:

  (character) Title of the report to unlock Release lock on current
  report (keep report loaded but make it read-only)

------------------------------------------------------------------------

### Method `release_lock()`

#### Usage

    ReportManager$release_lock(report_title = NULL)

#### Arguments

- `report_title`:

  (character) Title of the report to release lock from Re-build reports

------------------------------------------------------------------------

### Method `rebuild_report()`

Rebuild reports to include data that has changed. This will replace
loaded report cards with new cards, that were rebuilt from a code used
to generate these cards.

#### Usage

    ReportManager$rebuild_report(report_title, rp)

#### Arguments

- `report_title`:

  (character) A title for the saved report.

- `rp`:

  (Reporter) A reporter object passed from
  [`teal::init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
  Get current user name Get metadata file path Lock report so that it
  can't be overwritten by another user Unlock report Save creator
  information Save code from each card as `code.rds` in the report
  directory. Register `onSessionEnded` to unlock report when session is
  closed

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReportManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
