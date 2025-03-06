test_that("make_tabset_div works correctly", {
  expect_equal(
    make_tabset_div(FALSE, "default"),
    "::: {.panel-tabset}"
  )
  expect_equal(
    make_tabset_div(TRUE, "default"),
    "::: {.panel-tabset .nav-pills}"
  )
  expect_equal(
    make_tabset_div(TRUE, "fill"),
    "::: {.panel-tabset .nav-pills .nav-fill}"
  )
  expect_equal(
    make_tabset_div(FALSE, "justified"),
    "::: {.panel-tabset .nav-justified}"
  )
})

# Mock data for testingB
mock_data <- data.frame(
  tab1 = c("A", "B"),
  tab2 = c("X", "Y"),
  output1 = c("Result1", "Result2"),
  output2 = I(list(1:2, list("nested1", "nested2"))),
  stringsAsFactors = FALSE
)

tabset_names <- c("tab1", "tab2")
len_tab <- length(tabset_names)
tabset_master <- get_tabset_master(mock_data, tabset_names)
heading_levels <- rep_len(NA_integer_, len_tab)
output_names <- c("output1", "output2")
layout <- "::: {layout-ncol=2}"
tabset_div <- "::: {.panel-tabset}"

# Test for print_tabset_start
test_that("print_tabset_start prints tabset div correctly", {
  expect_equal(
    utils::capture.output(
      print_tabset_start(heading_levels, 1, tabset_master, tabset_div)
    ),
    c("::: {.panel-tabset}", "")
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(heading_levels, 2, tabset_master, tabset_div)
    ),
    character()
  )

  expect_equal(
    utils::capture.output(
      print_tabset_start(1:2, 1, tabset_master, tabset_div)
    ),
    character()
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(1:2, 2, tabset_master, tabset_div)
    ),
    character()
  )

  expect_equal(
    utils::capture.output(
      print_tabset_start(c(NA, 2), 1, tabset_master, tabset_div)
    ),
    c("::: {.panel-tabset}", "")
  )
  expect_equal(
    utils::capture.output(
      print_tabset_start(c(NA, 2), 2, tabset_master, tabset_div)
    ),
    character()
  )
})

# Test for print_nested_tabsets
test_that("print_nested_tabsets prints nested tabsets correctly", {
  expect_equal(
    utils::capture.output(
      print_nested_tabsets(
        data = mock_data,
        heading_levels = heading_levels,
        i = 1,
        tabset_names = tabset_names,
        len_tab = len_tab,
        tabset_master = tabset_master,
        tabset_div = tabset_div
      )
    ),
    c("# A", "", "::: {.panel-tabset}", "")
  )

  expect_equal(
    utils::capture.output(
      print_nested_tabsets(
        data = mock_data,
        heading_levels = heading_levels,
        i = 2,
        tabset_names = tabset_names,
        len_tab = len_tab,
        tabset_master = tabset_master,
        tabset_div = tabset_div
      )
    ),
    c("# B", "", "::: {.panel-tabset}", "")
  )

  # Returns character() if there is no nest
  tabset_names <- "tab1"
  len_tab <- length(tabset_names)
  tabset_master <- get_tabset_master(mock_data, tabset_names)
  heading_levels <- rep_len(NA_integer_, len_tab)

  expect_equal(
    utils::capture.output(
      print_nested_tabsets(
        data = mock_data,
        i = 1,
        tabset_names = tabset_names,
        tabset_master = tabset_master,
        heading_levels = heading_levels,
        len_tab = len_tab,
        tabset_div = tabset_div
      )
    ),
    character()
  )
  expect_equal(
    utils::capture.output(
      print_nested_tabsets(
        data = mock_data,
        i = 2,
        tabset_names = tabset_names,
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
        data = mock_data,
        heading_levels = heading_levels,
        layout = layout,
        i = 1,
        tabset_names = tabset_names,
        len_tab = len_tab,
        output_names = output_names
      )
    ),
    c(
      "## X",
      "",
      "::: {layout-ncol=2}",
      "",
      "Result1",
      "",
      "[1] 1 2",
      "",
      "",
      ":::",
      ""
    )
  )

  expect_equal(
    utils::capture.output(
      print_outputs(
        data = mock_data,
        heading_levels = heading_levels,
        layout = layout,
        i = 2,
        tabset_names = tabset_names,
        len_tab = len_tab,
        output_names = output_names
      )
    ),
    c(
      "## Y",
      "",
      "::: {layout-ncol=2}",
      "",
      "Result2",
      "",
      "[[1]]",
      "[1] \"nested1\"",
      "",
      "[[2]]",
      "[1] \"nested2\"",
      "",
      "",
      "",
      ":::",
      ""
    )
  )
})

# Test for print_tabset_end
test_that("print_tabset_end prints tabset end markers", {
  expect_equal(
    utils::capture.output(
      print_tabset_end(
        heading_levels = heading_levels,
        i = 1,
        len_tab = len_tab,
        tabset_master = tabset_master
      )
    ),
    c(":::", "")
  )
  expect_equal(
    utils::capture.output(
      print_tabset_end(
        heading_levels = heading_levels,
        i = 2,
        len_tab = len_tab,
        tabset_master = tabset_master
      )
    ),
    c(":::", "", ":::", "")
  )
})

# Test for print_row_tabsets
test_that("print_row_tabsets calls the required functions in order", {
  expect_equal(
    utils::capture.output(
      print_row_tabsets(
        data = mock_data,
        heading_levels = heading_levels,
        layout = layout,
        i = 1,
        tabset_names = tabset_names,
        len_tab = len_tab,
        output_names = output_names,
        tabset_master = tabset_master,
        tabset_div = tabset_div
      )
    ),
    c(
      "::: {.panel-tabset}",
      "",
      "# A",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "::: {layout-ncol=2}",
      "",
      "Result1",
      "",
      "[1] 1 2",
      "",
      "",
      ":::",
      "",
      ":::",
      ""
    )
  )

  expect_equal(
    utils::capture.output(
      print_row_tabsets(
        data = mock_data,
        heading_levels = heading_levels,
        layout = layout,
        i = 2,
        tabset_names = tabset_names,
        len_tab = len_tab,
        output_names = output_names,
        tabset_master = tabset_master,
        tabset_div = tabset_div
      )
    ),
    c(
      "# B",
      "",
      "::: {.panel-tabset}",
      "",
      "## Y",
      "",
      "::: {layout-ncol=2}",
      "",
      "Result2",
      "",
      "[[1]]",
      "[1] \"nested1\"",
      "",
      "[[2]]",
      "[1] \"nested2\"",
      "",
      "",
      "",
      ":::",
      "",
      ":::",
      "",
      ":::",
      ""
    )
  )
})

test_that("Date and POSIXt are converted to character", {
  df1 <- data.frame(
    tab = as.POSIXct(1, tz = "UTC"),
    out = as.Date(1)
  )

  expect_equal(
    utils::capture.output(
      render_tabset(df1, tab, out)
    ),
    c(
      "::: {.panel-tabset}",
      "",
      "# 1970-01-01 00:00:01",
      "",
      "1970-01-02",
      "",
      ":::",
      ""
    )
  )
})
