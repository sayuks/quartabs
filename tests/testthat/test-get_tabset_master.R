# Test data
data <- data.frame(
  group1 = c("a", "a", "a", "b", "b", NA, NA),
  group2 = c(1, 1, NA, 2, NA, 3, NA),
  group3 = NA_integer_,
  group4 = "x",
  value = 1:7
)

data <- rbind(data, data, data)

data <- data[order(data$group1, data$group2, data$group3, data$group4), ]

test_that("return a expected data frame columns correctly", {
  output <- get_tabset_master(data, c("group1", "group2", "group3", "group4"))

  expected_output <- data.frame(
    tabset1_start = rep(c(TRUE, FALSE), c(1L, 20L)),
    tabset1_end = rep(c(FALSE, TRUE), c(20L, 1L)),
    tabset2_start = rep(rep(c(TRUE, FALSE), 3), c(1L, 8L, 1L, 5L, 1L, 5L)),
    tabset2_end = rep(rep(c(FALSE, TRUE), 3), c(8L, 1L, 5L, 1L, 5L, 1L)),
    tabset3_start = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
      FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE
    ),
    tabset3_end = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE
    ),
    tabset4_start = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
      FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE
    ),
    tabset4_end = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE
    )
  )

  expect_equal(output, expected_output)
})

test_that("prep_data handles single grouping variable correctly", {
  output <- get_tabset_master(data, "group1")
  expected_output <- data.frame(
    tabset1_start = rep(c(TRUE, FALSE), c(1L, 20L)),
    tabset1_end = rep(c(FALSE, TRUE), c(20L, 1L))
  )
  expect_equal(output, expected_output)
})

test_that("error if a group column contains a reserved word.", {
  pf <- "__####_THIS_IS_A_PLACE_HOLDER_FOR_MISSING_VALUE_####__"
  data <- data.frame(
    group1 = c(pf, "a"),
    group2 = c(NA, pf),
    value = NA
  )
  expect_error(
    get_tabset_master(data, c("group1", "group2")),
    paste0(
      "If there are missing values in the column `group2` ",
      "that is specified in `tabset_vars`, ",
      "it must not contain the string ",
      sprintf("\"%s\"\\. ", pf),
      "It is an internal reserved word used ",
      "in the temporary handling of missing values\\."
    )
  )
})
