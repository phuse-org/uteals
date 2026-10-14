testthat::test_that("use_freeze_tables returns a tagList with style and script", {
  result <- use_freeze_tables()
  testthat::expect_s3_class(result, "shiny.tag.list")
  html <- paste(as.character(result[[1]]$children), as.character(result[[2]]))
  testthat::expect_match(html, "max-height: 70vh;", fixed = TRUE)
  testthat::expect_match(html, "var SELECTOR = \".teal-widgets.table-content\";", fixed = TRUE)
  testthat::expect_match(
    html,
    "var WRAPPER_CLASSES = [\"freeze-table-global\",\"freeze-header-active\",\"freeze-col-active\"];",
    fixed = TRUE
  )
})

testthat::test_that("use_freeze_tables omits the classes of disabled features", {
  result <- use_freeze_tables(freeze_header = FALSE, freeze_col = FALSE, max_height = "500px")
  html <- paste(as.character(result[[1]]$children), as.character(result[[2]]))
  testthat::expect_match(html, "max-height: 500px;", fixed = TRUE)
  testthat::expect_match(html, "var WRAPPER_CLASSES = [\"freeze-table-global\"];", fixed = TRUE)
})

testthat::test_that("use_freeze_tables limits the selector to the provided module ids", {
  html <- as.character(use_freeze_tables(module_ids = c("mod_a", "mod_b")))
  testthat::expect_match(
    html,
    paste0(
      "var SELECTOR = \"[id*=\\\"mod_a\\\"] .teal-widgets.table-content, ",
      "[id*=\\\"mod_b\\\"] .teal-widgets.table-content\";"
    ),
    fixed = TRUE
  )
})

testthat::test_that("use_freeze_tables validates its arguments", {
  testthat::expect_error(use_freeze_tables(freeze_header = "yes"))
  testthat::expect_error(use_freeze_tables(freeze_col = NA))
  testthat::expect_error(use_freeze_tables(max_height = 100))
  testthat::expect_error(use_freeze_tables(module_ids = character(0)))
})
