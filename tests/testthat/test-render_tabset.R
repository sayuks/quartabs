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
