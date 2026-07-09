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

testthat::test_that("server logic handles empty selection correctly for plots", {
  decorator <- title_footer_decorator("plot", titles_file, choices = c("TSFLAB01", "TSFLAB01b"), selected = "")

  mock_data <- shiny::reactive({
    q <- teal.code::qenv()
    teal.code::eval_code(q, "plot <- ggplot2::ggplot()")
  })

  shiny::testServer(decorator$server, args = list(data = mock_data), {
    session$setInputs(customize = FALSE, selectTitle = "")
    res_reactive <- session$getReturned()
    res <- withr::with_options(list(device = function() pdf(file = NULL)), {
      res_reactive()
    })

    testthat::expect_equal(res[["plot"]]$labels$title, "")
    testthat::expect_equal(res[["plot"]]$labels$caption, "")
  })
})

testthat::test_that("server logic handles empty selection correctly for tables", {
  decorator <- title_footer_decorator("table", titles_file, choices = c("TSFLAB01", "TSFLAB01b"), selected = "")

  mock_data <- shiny::reactive({
    q <- teal.code::qenv()
    teal.code::eval_code(q, "table <- rtables::rtable(header = 'a', rtables::rrow('1', 1))")
  })

  shiny::testServer(decorator$server, args = list(data = mock_data), {
    session$setInputs(customize = FALSE, selectTitle = "")
    res_reactive <- session$getReturned()
    res <- withr::with_options(list(device = function() pdf(file = NULL)), {
      res_reactive()
    })

    testthat::expect_equal(formatters::main_title(res[["table"]]), "")
    testthat::expect_equal(formatters::main_footer(res[["table"]]), "")
  })
})
