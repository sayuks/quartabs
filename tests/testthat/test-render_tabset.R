df_sample <- data.frame(
  group1 = factor(c(rep("A", 3), rep("B", 3))),
  group2 = rep(c("X", "Y", "Z"), 2),
  var1 = 1:6,
  var2 = 7:12,
  var3 = factor(letters[1:6])
)

test_that("render_tabset(), basic", {
  expected <-
    c(
      "::: {.panel-tabset}",
      "",
      "# A",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      ":::",
      "",
      "# B",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      ":::",
      "",
      ":::",
      ""
    )

  res <- utils::capture.output(
    render_tabset(df_sample, c(group1, group2), c(var1, var2, var3))
  )

  expect_equal(
    res,
    expected
  )

  # tibble input works
  expect_equal(
    utils::capture.output(
      render_tabset(
        tibble::as_tibble(df_sample),
        c(group1, group2),
        c(var1, var2, var3)
      )
    ),
    res
  )
})

test_that("render_tabset(), layout argument", {
  expected <-
    c(
      "::: {.panel-tabset}",
      "",
      "# A",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      ":::",
      "",
      "## Y",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      ":::",
      "",
      "## Z",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      ":::",
      "",
      ":::",
      "",
      "# B",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      ":::",
      "",
      "## Y",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      ":::",
      "",
      "## Z",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      ":::",
      "",
      ":::",
      "",
      ":::",
      ""
    )

  expect_equal(
    utils::capture.output(
      render_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        layout = '::: {layout="[2, 3, 5]"}'
      )
    ),
    expected
  )
})

test_that("render_tabset(), heading_levels argument", {
  expected <-
    c(
      "# A",
      "",
      "## X",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      "# B",
      "",
      "## X",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      ""
    )

  expect_equal(
    utils::capture.output(
      render_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        heading_levels = c(1, 2)
      )
    ),
    expected
  )
})

test_that("render_tabset(), heading_levels argument with NA", {
  expected <-
    c(
      "# A",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      "## Y",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      "## Z",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      ":::",
      "",
      "# B",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      "## Y",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      "## Z",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      ":::",
      ""
    )

  expect_equal(
    utils::capture.output(
      render_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        heading_levels = c(1, NA)
      )
    ),
    expected
  )
})

test_that("render_tabset(), both layout and heading_levels arguments", {
  expected <-
    c(
      "# A",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "1",
      "",
      "7",
      "",
      "a",
      "",
      ":::",
      "",
      "## Y",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "2",
      "",
      "8",
      "",
      "b",
      "",
      ":::",
      "",
      "## Z",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "3",
      "",
      "9",
      "",
      "c",
      "",
      ":::",
      "",
      ":::",
      "",
      "# B",
      "",
      "::: {.panel-tabset}",
      "",
      "## X",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "4",
      "",
      "10",
      "",
      "d",
      "",
      ":::",
      "",
      "## Y",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "5",
      "",
      "11",
      "",
      "e",
      "",
      ":::",
      "",
      "## Z",
      "",
      "::: {layout=\"[2, 3, 5]\"}",
      "",
      "6",
      "",
      "12",
      "",
      "f",
      "",
      ":::",
      "",
      ":::",
      ""
    )

  expect_equal(
    utils::capture.output(
      render_tabset(
        df_sample,
        c(group1, group2),
        c(var1, var2, var3),
        layout = '::: {layout="[2, 3, 5]"}',
        heading_levels = c(1, NA)
      )
    ),
    expected
  )
})

test_that("render_tabset(), tab order follows factor levels (#5)", {
  df <- data.frame(
    parent = c("P2", "P2", "P1", "P1"),
    child = c("Z", "X", "Z", "X"),
    value = 1:4
  )

  headings <- function(x) grep("^#", x, value = TRUE)

  # character columns are sorted in ascending order
  expect_equal(
    headings(utils::capture.output(render_tabset(df, c(parent, child), value))),
    c("# P1", "## X", "## Z", "# P2", "## X", "## Z")
  )

  # factor columns follow their level order (here, first appearance)
  df$parent <- factor(df$parent, levels = unique(df$parent))
  df$child <- factor(df$child, levels = unique(df$child))
  expect_equal(
    headings(utils::capture.output(render_tabset(df, c(parent, child), value))),
    c("# P2", "## Z", "## X", "# P1", "## Z", "## X")
  )
})

test_that("render_tabset(), active marks the tab given by a value (#4)", {
  df <- data.frame(
    group = c("A", "B", "C"),
    value = 1:3
  )

  expect_equal(
    utils::capture.output(
      render_tabset(df, group, value, active = list(group = "B"))
    ),
    c(
      "::: {.panel-tabset}",
      "",
      "# A",
      "",
      "1",
      "",
      "# B {.active}",
      "",
      "2",
      "",
      "# C",
      "",
      "3",
      "",
      ":::",
      ""
    )
  )
})

test_that("render_tabset(), active function is applied per parent tab (#4)", {
  df <- data.frame(
    parent = c("P1", "P1", "P1", "P2", "P2"),
    year = c(8, 9, 10, 8, 9),
    value = 1:5
  )

  # numbers are compared as numbers, not as strings ("9" > "10")
  expect_equal(
    tab_headings(df, c(parent, year), value, active = list(year = max)),
    c(
      "# P1", "## 8", "## 9", "## 10 {.active}",
      "# P2", "## 8", "## 9 {.active}"
    )
  )

  # both levels at once
  expect_equal(
    tab_headings(
      df,
      c(parent, year),
      value,
      active = list(parent = "P2", year = min)
    ),
    c(
      "# P1", "## 8 {.active}", "## 9", "## 10",
      "# P2 {.active}", "## 8 {.active}", "## 9"
    )
  )
})

test_that("render_tabset(), active function sees factor levels (#4)", {
  df <- data.frame(
    group = factor(c("b", "c", "a"), levels = c("c", "a", "b")),
    value = 1:3
  )

  expect_equal(
    tab_headings(
      df,
      group,
      value,
      active = list(group = function(x) x[length(x)])
    ),
    c("# c", "# a", "# b {.active}")
  )
})

test_that("render_tabset(), active value missing under some parents (#4)", {
  df <- data.frame(
    parent = c("P1", "P1", "P2"),
    child = c("X", "Y", "X"),
    value = 1:3
  )

  expect_equal(
    tab_headings(df, c(parent, child), value, active = list(child = "Y")),
    c("# P1", "## X", "## Y {.active}", "# P2", "## X")
  )

  expect_error(
    render_tabset(df, c(parent, child), value, active = list(child = "Z")),
    "was not found in the column `child`"
  )
})

test_that("render_tabset(), active function returning NA marks nothing (#4)", {
  df <- data.frame(
    parent = c("P1", "P1", "P2"),
    child = c(1, 2, NA),
    value = 1:3
  )

  expect_equal(
    tab_headings(df, c(parent, child), value, active = list(child = max)),
    c("# P1", "## 1", "## 2 {.active}", "# P2", "## NA")
  )

  run <- function(fn) {
    render_tabset(df, c(parent, child), value, active = list(child = fn))
  }
  expect_error(
    run(function(x) 99),
    "must return one of the values it was given"
  )
  expect_error(run(function(x) x), "must return a single value")
  expect_error(run(function(x) list(2)), "must return a single value")
})

test_that("render_tabset(), active matches Date columns (#4)", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-02-01")),
    value = 1:2
  )

  expected <- c("# 2024-01-01", "# 2024-02-01 {.active}")

  expect_equal(
    tab_headings(df, date, value, active = list(date = as.Date("2024-02-01"))),
    expected
  )
  expect_equal(
    tab_headings(df, date, value, active = list(date = "2024-02-01")),
    expected
  )
  expect_equal(
    tab_headings(df, date, value, active = list(date = max)),
    expected
  )
})

test_that("render_tabset(), active character value matches tab labels (#4)", {
  df <- data.frame(
    time = as.POSIXct(c("2024-01-02 10:00:00", "2024-01-03 10:00:00")),
    num = c(9, 10),
    value = 1:2
  )

  expect_equal(
    tab_headings(df, time, value, active = list(time = "2024-01-03 10:00:00")),
    c("# 2024-01-02 10:00:00", "# 2024-01-03 10:00:00 {.active}")
  )
  expect_equal(
    tab_headings(df, num, value, active = list(num = "10")),
    c("# 9", "# 10 {.active}")
  )
  expect_equal(
    tab_headings(df, num, value, active = list(num = 10)),
    c("# 9", "# 10 {.active}")
  )
})

test_that("render_tabset(), active validation (#4)", {
  df <- data.frame(
    parent = c("P1", "P2"),
    child = c("X", "Y"),
    value = 1:2
  )

  run <- function(active, heading_levels = NULL) {
    render_tabset(
      df,
      c(parent, child),
      value,
      heading_levels = heading_levels,
      active = active
    )
  }

  # an empty list leaves the output unchanged
  expect_equal(
    utils::capture.output(run(list())),
    utils::capture.output(render_tabset(df, c(parent, child), value))
  )

  not_list <- "`active` must be `NULL` or a list."
  not_named <- "All elements of `active` must be named."
  bad_element <- "must be a function or a single non-missing value"

  expect_error(run("P1"), not_list)
  expect_error(run(df), not_list)
  expect_error(run(list("P1")), not_named)
  expect_error(run(list(parent = "P1", "X")), not_named)
  expect_error(
    run(list(parent = "P1", parent = "P2")),
    "The names of `active` must be unique."
  )
  expect_error(
    run(list(value = 1)),
    "is not a column specified in `tabset_vars`"
  )
  expect_error(run(list(child = NA)), bad_element)
  expect_error(run(list(child = c("X", "Y"))), bad_element)
  expect_error(run(list(child = list("X"))), bad_element)
  expect_error(
    run(list(parent = "P1"), heading_levels = c(2, NA)),
    "is displayed as headings by `heading_levels`"
  )

  # a column that stays a tabset under heading_levels can be active
  expect_equal(
    tab_headings(
      df,
      c(parent, child),
      value,
      heading_levels = c(2, NA),
      active = list(child = "Y")
    ),
    c("## P1", "## X", "## P2", "## Y {.active}")
  )
})
