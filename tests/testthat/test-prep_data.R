data <- data.frame(
  output = c(10, 20, 30, 40),
  g2 = as.Date(c(NA, 1, NA, 2)),
  g1 = c(NA, NA, "A", "B"),
  g3 = as.POSIXct(c(NA, 1, 2, NA), tz = "UTC"),
  other = 1:4,
  g5 = c(NA, 2, 3, 1),
  g4 = factor(c(NA, "x", "z", "y"))
)

test_that("select, order, converts factor, Date and POSIXt to characters", {
  result <- prep_data(
    data,
    tabset_names = c("g1", "g2", "g3", "g4", "g5"),
    output_names = "output"
  )

  expect_equal(
    result,
    data.frame(
      g1 = c("A", "B", NA, NA),
      g2 = c(NA, "1970-01-03", "1970-01-02", NA),
      g3 = c("1970-01-01 00:00:02", NA, "1970-01-01 00:00:01", NA),
      g4 = c("z", "y", "x", NA),
      g5 = c(3, 1, 2, NA),
      output = c(30, 40, 20, 10),
      row.names = c(3L, 4L, 2L, 1L)
    )
  )
})

test_that("prep_data returns error when columns are missing", {
  expect_error(
    prep_data(
      data,
      tabset_names = "non_existent",
      output_names = "output"
    ),
    "undefined columns selected"
  )
})
