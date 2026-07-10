# The server part of [`tm_report_manager()`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md).

The server part of
[`tm_report_manager()`](https://phuse-org.github.io/uteals/reference/tm_report_manager.md).

## Usage

``` r
report_manager_server(id, reports_path = "reports", auto_save = TRUE, reporter)
```

## Arguments

- id:

  (`character`) the id of the module.

- reports_path:

  character. Absolute path where reports should be stored.

- auto_save:

  logical. Whether to save active report whenever there are any changes
  made.

- reporter:

  the object that holds the report. Provided by `teal`.
