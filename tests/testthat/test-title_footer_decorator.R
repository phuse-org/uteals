titles_file <- testthat::test_path("dps_titles_08042026.xlsx")

testthat::test_that("title_footer_decorator works when choices and selected are NULL", {
  result <- title_footer_decorator("plot", titles_file, choices = NULL, selected = NULL)
  testthat::expect_s3_class(result, "teal_data_module")
})

testthat::test_that("title_footer_decorator works when selected is in choices", {
  result <- title_footer_decorator("plot", titles_file, choices = c("TSFLAB01", "TSFLAB01b"), selected = "TSFLAB01")
  testthat::expect_s3_class(result, "teal_data_module")
})

testthat::test_that("title_footer_decorator errors when selected is not in choices", {
  testthat::expect_error(
    title_footer_decorator("plot", titles_file, choices = c("TSFLAB01", "TSFLAB01b"), selected = "TBL99")
  )
})
