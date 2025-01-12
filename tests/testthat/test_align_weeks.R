library(testthat)

test_that("Bad days of the week error", {
  expect_error(align_weeks(
    # Jan 1, 2020 is a Wednesday
    ISOdate(2020, 1, 1),
    target_season = 2020,
    week_day = 6
  ))
})
