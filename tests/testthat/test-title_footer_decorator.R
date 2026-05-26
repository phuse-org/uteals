test_that("title_footer_decorator works when choices and selected are NULL", {
  mock_titles <- data.frame(TABLE.ID = c("TBL01", "TBL02"), IDENTIFIER = "TITLE", TEXT = "Some title")
  mockery::stub(title_footer_decorator, "openxlsx::read.xlsx", mock_titles)

  result <- title_footer_decorator("plot", "fake.xlsx", choices = NULL, selected = NULL)
  expect_s3_class(result, "teal_data_module")
})

test_that("title_footer_decorator works when selected is in choices", {
  mock_titles <- data.frame(TABLE.ID = c("TBL01", "TBL02"), IDENTIFIER = "TITLE", TEXT = "Some title")
  mockery::stub(title_footer_decorator, "openxlsx::read.xlsx", mock_titles)

  result <- title_footer_decorator("plot", "fake.xlsx", choices = c("TBL01", "TBL02"), selected = "TBL01")
  expect_s3_class(result, "teal_data_module")
})

test_that("title_footer_decorator errors when selected is not in choices", {
  mock_titles <- data.frame(TABLE.ID = c("TBL01", "TBL02"), IDENTIFIER = "TITLE", TEXT = "Some title")
  mockery::stub(title_footer_decorator, "openxlsx::read.xlsx", mock_titles)

  expect_error(
    title_footer_decorator("plot", "fake.xlsx", choices = c("TBL01", "TBL02"), selected = "TBL99")
  )
})
