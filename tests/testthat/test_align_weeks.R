library(testthat)

test_that("Bad days of the week error", {
  expect_error(align_weeks(
    # Jan 1, 2020 is a Wednesday
    ISOdate(2020, 1, 1),
    target_season = 2020,
    week_day = 6
  ))
})

test_that("Weeks in the same season do not move", {
  # Aug 1, 2020 and Feb 1, 2021 are both in the 2020/2021 season
  x <- as.Date(c(ISOdate(2020, 8, 1), ISOdate(2021, 2, 1)))
  expect_equal(
    align_weeks(x, target_season = 2020, week_day = 6),
    x
  )
})
