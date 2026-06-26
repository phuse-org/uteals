# Helpers ---------------------------------------------------------------

make_rm <- function(reports_path) {
  shiny::isolate(ReportManager$new(reports_path = reports_path, session = NULL))
}

make_report_on_disk <- function(reports_path, report_title, df = NULL) {
  report_dir <- file.path(reports_path, report_title)
  dir.create(report_dir, recursive = TRUE)

  card_file <- "card_001.rds"
  saveRDS(if (is.null(df)) list("no_table") else list(df), file.path(report_dir, card_file))

  jsonlite::write_json(
    list(
      name = "teal Reporter", version = "1", id = report_title,
      cards = stats::setNames(
        list(list(name = "card_001", path = card_file)),
        "teal_card"
      ),
      metadata = list()
    ),
    file.path(report_dir, "Report.json"),
    auto_unbox = TRUE
  )
}

# initialize ------------------------------------------------------------

testthat::test_that("initialize sets reports_path field", {
  tmp <- withr::local_tempdir()
  rm <- make_rm(tmp)

  testthat::expect_equal(rm$reports_path, tmp)
})

# list_reports ----------------------------------------------------------

testthat::test_that("list_reports returns one row per report directory", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  dir.create(file.path(tmp, "report_b"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$list_reports())

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$reports, c("report_a", "report_b"))
})

testthat::test_that("list_reports reads created_by from .creator.rds", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_x"))
  saveRDS("alice", file.path(tmp, "report_x", ".creator.rds"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$list_reports())

  testthat::expect_equal(result$created_by, "alice")
})

testthat::test_that("list_reports reads locked_by from .lockfile.rds", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "locked_report"))
  saveRDS("bob", file.path(tmp, "locked_report", ".lockfile.rds"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$list_reports())

  testthat::expect_equal(result$locked_by, "bob")
})

testthat::test_that("list_reports returns Never for last_rebuild when .rebuild_time.rds absent", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_no_rebuild"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$list_reports())

  testthat::expect_equal(result$last_rebuild, "Never")
})

# get_abs_report_path ---------------------------------------------------

testthat::test_that("get_abs_report_path concatenates reports_path and title", {
  tmp <- withr::local_tempdir()
  rm <- make_rm(tmp)

  testthat::expect_equal(rm$get_abs_report_path("my_report"), file.path(tmp, "my_report"))
})

# set_current_report_title ----------------------------------------------

testthat::test_that("set_current_report_title sets title from character", {
  tmp <- withr::local_tempdir()
  rm <- make_rm(tmp)

  shiny::isolate(rm$set_current_report_title("my_report"))

  testthat::expect_equal(shiny::isolate(rm$current_report_title()), "my_report")
})

# is_locked_by_other ----------------------------------------------------

testthat::test_that("is_locked_by_other returns FALSE when no lockfile exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$is_locked_by_other("report_a", verbose = FALSE))

  testthat::expect_false(result)
})

testthat::test_that("is_locked_by_other returns TRUE when lockfile exists and report is not mine", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  saveRDS("other_user", file.path(tmp, "report_a", ".lockfile.rds"))
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$is_locked_by_other("report_a", verbose = FALSE))

  testthat::expect_true(result)
})

testthat::test_that("is_locked_by_other returns FALSE when report is locked by me", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  saveRDS(ifelse(interactive(), Sys.getenv("USER"), self$session$user),
   file.path(tmp, "report_a", ".lockfile.rds"))
  rm <- make_rm(tmp)
  rm$my_locked_report <- "report_a"

  result <- shiny::isolate(rm$is_locked_by_other("report_a", verbose = FALSE))

  testthat::expect_false(result)
})

# delete_report ---------------------------------------------------------

testthat::test_that("delete_report removes the report directory", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "to_delete"))
  rm <- make_rm(tmp)

  shiny::isolate(rm$delete_report("to_delete"))

  testthat::expect_false(dir.exists(file.path(tmp, "to_delete")))
})

testthat::test_that("delete_report does not remove a report locked by another user", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "locked"))
  saveRDS("other_user", file.path(tmp, "locked", ".lockfile.rds"))
  rm <- make_rm(tmp)

  shiny::isolate(rm$delete_report("locked"))

  testthat::expect_true(dir.exists(file.path(tmp, "locked")))
})

# rename_report ---------------------------------------------------------

testthat::test_that("rename_report renames the directory on disk", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "old_name"))
  rm <- make_rm(tmp)

  shiny::isolate(rm$rename_report("old_name", "new_name", reporter = list(set_id = function(...) NULL, to_jsondir = function(...) NULL)))

  testthat::expect_false(dir.exists(file.path(tmp, "old_name")))
  testthat::expect_true(dir.exists(file.path(tmp, "new_name")))
})

testthat::test_that("rename_report errors when new name already exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  dir.create(file.path(tmp, "report_b"))
  rm <- make_rm(tmp)

  testthat::expect_error(
    shiny::isolate(rm$rename_report("report_a", "report_b", reporter = list()))
  )
})

# reset -----------------------------------------------------------------

testthat::test_that("reset sets current_report_title to NULL", {
  tmp <- withr::local_tempdir()
  rm <- make_rm(tmp)
  shiny::isolate(rm$current_report_title("some_report"))

  shiny::isolate(rm$reset())

  testthat::expect_null(shiny::isolate(rm$current_report_title()))
})

testthat::test_that("reset sets read_only_mode to FALSE", {
  tmp <- withr::local_tempdir()
  rm <- make_rm(tmp)
  shiny::isolate(rm$read_only_mode(TRUE))

  shiny::isolate(rm$reset())

  testthat::expect_false(shiny::isolate(rm$read_only_mode()))
})

# release_lock ----------------------------------------------------------

testthat::test_that("release_lock removes the lockfile", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  saveRDS("me", file.path(tmp, "report_a", ".lockfile.rds"))
  rm <- make_rm(tmp)
  rm$my_locked_report <- "report_a"

  shiny::isolate(rm$release_lock("report_a"))

  testthat::expect_false(file.exists(file.path(tmp, "report_a", ".lockfile.rds")))
})

testthat::test_that("release_lock sets my_locked_report to NULL", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "report_a"))
  saveRDS("me", file.path(tmp, "report_a", ".lockfile.rds"))
  rm <- make_rm(tmp)
  rm$my_locked_report <- "report_a"

  shiny::isolate(rm$release_lock("report_a"))

  testthat::expect_null(rm$my_locked_report)
})

# export_tables_to_csv --------------------------------------------------

testthat::test_that("export_tables_to_csv returns character(0) when report has no tables", {
  tmp <- withr::local_tempdir()
  make_report_on_disk(tmp, "no_tables")
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$export_tables_to_csv("no_tables", file.path(tmp, "out")))

  testthat::expect_equal(result, character(0))
})

testthat::test_that("export_tables_to_csv writes a CSV and returns its path", {
  tmp <- withr::local_tempdir()
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  make_report_on_disk(tmp, "report_with_table", df)
  rm <- make_rm(tmp)
  out_dir <- file.path(tmp, "out")

  result <- shiny::isolate(rm$export_tables_to_csv("report_with_table", out_dir))

  testthat::expect_length(result, 1L)
  testthat::expect_true(file.exists(result[[1L]]))
  written <- utils::read.csv(result[[1L]], header = FALSE, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(written), 3L)
})

testthat::test_that("export_tables_to_csv creates output_dir when it does not exist", {
  tmp <- withr::local_tempdir()
  make_report_on_disk(tmp, "report_dir_test", data.frame(x = 1:2))
  rm <- make_rm(tmp)
  out_dir <- file.path(tmp, "new_subdir", "csv_out")

  shiny::isolate(rm$export_tables_to_csv("report_dir_test", out_dir))

  testthat::expect_true(dir.exists(out_dir))
})

testthat::test_that("export_tables_to_csv writes one CSV per card when multiple cards contain tables", {
  tmp <- withr::local_tempdir()
  report_dir <- file.path(tmp, "multi_card")
  dir.create(report_dir)
  saveRDS(list(data.frame(a = 1:2)), file.path(report_dir, "card_1.rds"))
  saveRDS(list(data.frame(b = 3:4)), file.path(report_dir, "card_2.rds"))
  jsonlite::write_json(
    list(
      name = "teal Reporter", version = "1", id = "multi_card",
      cards = list(
        teal_card   = list(name = "card_001", path = "card_1.rds"),
        teal_card.1 = list(name = "card_002", path = "card_2.rds")
      ),
      metadata = list()
    ),
    file.path(report_dir, "Report.json"),
    auto_unbox = TRUE
  )
  rm <- make_rm(tmp)

  result <- shiny::isolate(rm$export_tables_to_csv("multi_card", file.path(tmp, "out_multi")))

  testthat::expect_length(result, 2L)
  testthat::expect_true(all(file.exists(result)))
})
