test_that("make_tabset_div works correctly", {
  expect_equal(make_tabset_div(FALSE, "default"), "::: {.panel-tabset}")
  expect_equal(make_tabset_div(TRUE, "default"), "::: {.panel-tabset} .nav-pills")
  expect_equal(make_tabset_div(TRUE, "fill"), "::: {.panel-tabset} .nav-pills .nav-fill")
  expect_equal(make_tabset_div(FALSE, "justified"), "::: {.panel-tabset} .nav-justified")
})

# Mock data for testing
mock_data <- data.frame(
  tab1 = c("A", "B"),
  tab2 = c("X", "Y"),
  output1 = c("Result1", "Result2"),
  output2 = I(list(1:2, c("nested1", "nested2"))),
  stringsAsFactors = FALSE
)

tabset_master <- get_tabset_master(mock_data, c("tab1", "tab2"))
heading_levels <- rep_len(NA_integer_, ncol(tabset_master) / 2)
len_tab <- length(heading_levels)
layout <- "::: layout"
tabset_div <- "::: {.panel-tabset}"

# Test for print_tabset_start
test_that("print_tabset_start prints tabset div correctly", {
  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, heading_levels, 1, tabset_div)
    ),
    c("::: {.panel-tabset}", "")
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, heading_levels, 2, tabset_div)
    ),
    c("::: {.panel-tabset}", "")
  )

  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, c(1, 2), 1, tabset_div)
    ),
    character()
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, c(1, 2), 2, tabset_div)
    ),
    character()
  )

  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, c(NA, 2), 1, tabset_div)
    ),
    c("::: {.panel-tabset}", "")
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(mock_data, 1, tabset_master, c(NA, 2), 2, tabset_div)
    ),
    character()
  )
})

# Test for print_nested_tabsets
test_that("print_nested_tabsets prints nested tabsets correctly", {
  expect_equal(
    capture.output(
      print_nested_tabsets(
        data = mock_data,
        i = 1,
        tabset_names = c("tab1", "tab2"),
        tabset_master = tabset_master,
        heading_levels = heading_levels,
        len_tab = len_tab,
        tabset_div = tabset_div
      )
    ),
    c("# A", "", "::: {.panel-tabset}", "")
  )

  expect_equal(
    capture.output(
      print_nested_tabsets(
        data = mock_data,
        i = 2,
        tabset_names = c("tab1", "tab2"),
        tabset_master = tabset_master,
        heading_levels = heading_levels,
        len_tab = len_tab,
        tabset_div = tabset_div
      )
    ),
    c("# B", "", "::: {.panel-tabset}", "")
  )


  # Returns character() if there is no nest
  tabset_master <- get_tabset_master(mock_data, "tab1")
  heading_levels <- rep_len(NA_integer_, ncol(tabset_master) / 2)
  len_tab <- length(heading_levels)
  layout <- "::: layout"
  tabset_div <- "::: {.panel-tabset}"

  expect_equal(
    capture.output(
      print_nested_tabsets(
        data = mock_data,
        i = 2,
        tabset_names = c("tab1", "tab2"),
        tabset_master = tabset_master,
        heading_levels = heading_levels,
        len_tab = len_tab,
        tabset_div = tabset_div
      )
    ),
    character()
  )
})

# Test for print_outputs
test_that("print_outputs prints output correctly", {
  expect_equal(
    utils::capture.output(
      print_outputs(
        mock_data,
        1,
        c("tab1", "tab2"),
        heading_levels,
        2,
        c("output1"),
        layout
      )
    ),
    c("## X", "", "::: layout", "", "Result1", "", ":::", "")
  )

  expect_equal(
    utils::capture.output(
      print_outputs(
        mock_data,
        1,
        c("tab1", "tab2"),
        heading_levels,
        2,
        c("output1", "output2"),
        layout
      )
    ),
    c("## X", "", "::: layout", "", "Result1", "", ":::", "")
  )
})

# Test for print_tabset_end
test_that("print_tabset_end prints tabset end markers", {
  expect_output(print_tabset_end(1, tabset_master, 2, heading_levels), ":::")
})

# Test for print_row_tabsets
test_that("print_row_tabsets calls the required functions in order", {
  expect_output(print_row_tabsets(mock_data, 1, c("tab1", "tab2"), tabset_master, heading_levels, 2, c("output"), layout, "::: tabset"), "::: tabset")
})
