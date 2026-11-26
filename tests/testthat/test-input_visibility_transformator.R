# ---- .normalise_discovered ------------------------------------------------

testthat::test_that(".normalise_discovered handles data.frame input", {
  raw <- data.frame(
    key = c("k1", "k2"),
    label = c("Label 1", "Label 2"),
    parentKey = c(NA_character_, "k1"),
    stringsAsFactors = FALSE
  )
  result <- uteals:::.normalise_discovered(raw)

  testthat::expect_length(result, 2L)
  testthat::expect_equal(result[[1]]$key, "k1")
  testthat::expect_equal(result[[1]]$label, "Label 1")
  testthat::expect_null(result[[1]]$parentKey)
  testthat::expect_equal(result[[2]]$parentKey, "k1")
})

testthat::test_that(".normalise_discovered handles data.frame without parentKey column", {
  raw <- data.frame(
    key = c("a", "b"),
    label = c("A", "B"),
    stringsAsFactors = FALSE
  )
  result <- uteals:::.normalise_discovered(raw)

  testthat::expect_length(result, 2L)
  testthat::expect_null(result[[1]]$parentKey)
  testthat::expect_null(result[[2]]$parentKey)
})

testthat::test_that(".normalise_discovered handles list-of-lists input", {
  raw <- list(
    list(key = "k1", label = "L1"),
    list(key = "k2", label = "L2", parentKey = "k1")
  )
  result <- uteals:::.normalise_discovered(raw)

  testthat::expect_length(result, 2L)
  testthat::expect_null(result[[1]]$parentKey)
  testthat::expect_equal(result[[2]]$parentKey, "k1")
})

testthat::test_that(".normalise_discovered handles named character vector", {
  raw <- c(key = "k1", label = "L1", key = "k2", label = "L2")
  result <- uteals:::.normalise_discovered(raw)

  testthat::expect_length(result, 2L)
  testthat::expect_equal(result[[1]]$key, "k1")
  testthat::expect_equal(result[[2]]$label, "L2")
})

testthat::test_that(".normalise_discovered returns empty list for unrecognised input", {
  testthat::expect_equal(uteals:::.normalise_discovered(42), list())
  testthat::expect_equal(uteals:::.normalise_discovered(NULL), list())
})

# ---- .is_default_visible -------------------------------------------------

testthat::test_that(".is_default_visible returns TRUE when no match", {
  testthat::expect_true(
    uteals:::.is_default_visible("Arm Variable", c("risk", "stratify"))
  )
})

testthat::test_that(".is_default_visible returns FALSE on substring match", {
  testthat::expect_false(
    uteals:::.is_default_visible("Risk Difference", c("risk", "stratify"))
  )
})

testthat::test_that(".is_default_visible is case-insensitive", {
  testthat::expect_false(
    uteals:::.is_default_visible("RISK DIFFERENCE", c("risk"))
  )
})

testthat::test_that(".is_default_visible returns TRUE for empty hidden list", {
  testthat::expect_true(
    uteals:::.is_default_visible("Anything", character(0))
  )
})

# ---- .vis_cb_id -----------------------------------------------------------

testthat::test_that(".vis_cb_id sanitises special characters", {
  testthat::expect_equal(
    uteals:::.vis_cb_id("ns-prefix-key_1"),
    "vis_ns_prefix_key_1"
  )
})

# ---- .is_parent_visible --------------------------------------------------

testthat::test_that(".is_parent_visible returns TRUE when parent not found", {
  discovered <- list(list(key = "a", label = "A"))
  testthat::expect_true(
    uteals:::.is_parent_visible("nonexistent", discovered, character(0))
  )
})

testthat::test_that(".is_parent_visible returns FALSE when parent matches hidden", {
  discovered <- list(
    list(key = "p1", label = "Additional table settings"),
    list(key = "c1", label = "Display NA counts", parentKey = "p1")
  )
  testthat::expect_false(
    uteals:::.is_parent_visible("p1", discovered, c("additional"))
  )
})

# ---- input validation -----------------------------------------------------

testthat::test_that("hidden_by_default rejects empty strings", {
  testthat::expect_error(
    input_visibility_transformator(hidden_by_default = c("valid", "")),
    "at least 1 characters"
  )
})

testthat::test_that("hidden_by_default rejects NA values", {
  testthat::expect_error(
    input_visibility_transformator(hidden_by_default = c("valid", NA)),
    "missing"
  )
})

# ---- testServer: discovered_inputs normalisation --------------------------

testthat::test_that("server normalises list-of-lists input", {
  transform <- input_visibility_transformator()
  # Extract the server function from the transform module
  srv <- transform$server

  shiny::testServer(srv, args = list(data = shiny::reactive(NULL)), {
    session$setInputs(
      discovered_inputs = list(
        list(key = "k1", label = "Arm Variable"),
        list(key = "k2", label = "Sort Criteria", parentKey = "k1")
      )
    )
    result <- discovered_inputs()
    testthat::expect_length(result, 2L)
    testthat::expect_equal(result[[1]]$key, "k1")
    testthat::expect_equal(result[[2]]$parentKey, "k1")
  })
})

testthat::test_that("server builds parent-child map from parentKey", {
  transform <- input_visibility_transformator()
  srv <- transform$server

  shiny::testServer(srv, args = list(data = shiny::reactive(NULL)), {
    session$setInputs(
      discovered_inputs = list(
        list(key = "parent1", label = "Settings"),
        list(key = "child1", label = "Option A", parentKey = "parent1"),
        list(key = "child2", label = "Option B", parentKey = "parent1"),
        list(key = "standalone", label = "Other")
      )
    )
    parents <- child_keys_of()
    testthat::expect_equal(parents[["parent1"]], c("child1", "child2"))
    testthat::expect_null(parents[["standalone"]])
  })
})

# ---- testServer: state across re-renders -----------------------------------

testthat::test_that("re-rendering the toggle list keeps current checkbox values", {
  srv <- input_visibility_transformator()$server

  shiny::testServer(srv, args = list(data = shiny::reactive(NULL)), {
    session$setInputs(discovered_inputs = list(list(key = "k1", label = "A")))
    session$setInputs(vis_k1 = FALSE)
    # A late-rendered input triggers a new discovery payload and a re-render.
    session$setInputs(
      discovered_inputs = list(
        list(key = "k1", label = "A"),
        list(key = "k2", label = "B")
      )
    )
    html <- as.character(output$toggle_ui$html)
    testthat::expect_false(grepl('vis_k1"[^>]*checked', html))
    testthat::expect_true(grepl('vis_k2"[^>]*checked', html))
  })
})

testthat::test_that("child rows follow the parent's current value on re-render", {
  srv <- input_visibility_transformator()$server

  shiny::testServer(srv, args = list(data = shiny::reactive(NULL)), {
    session$setInputs(
      discovered_inputs = list(
        list(key = "p", label = "Settings"),
        list(key = "c", label = "Option", parentKey = "p")
      )
    )
    testthat::expect_false(
      grepl("display:none", as.character(output$toggle_ui$html), fixed = TRUE)
    )
    session$setInputs(vis_p = FALSE)
    session$setInputs(
      discovered_inputs = list(
        list(key = "p", label = "Settings"),
        list(key = "c", label = "Option", parentKey = "p"),
        list(key = "late", label = "Late")
      )
    )
    testthat::expect_true(
      grepl("display:none", as.character(output$toggle_ui$html), fixed = TRUE)
    )
  })
})

testthat::test_that("hidden_by_default inputs start unchecked", {
  srv <- input_visibility_transformator(hidden_by_default = "sort")$server

  shiny::testServer(srv, args = list(data = shiny::reactive(NULL)), {
    session$setInputs(
      discovered_inputs = list(
        list(key = "a", label = "Arm"),
        list(key = "s", label = "Sort Criteria")
      )
    )
    html <- as.character(output$toggle_ui$html)
    testthat::expect_true(grepl('vis_a"[^>]*checked', html))
    testthat::expect_false(grepl('vis_s"[^>]*checked', html))
  })
})
