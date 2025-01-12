library(testthat)

test_that("Bad days of the week error", {
  expect_error(
    align_weeks(
      # Jan 1, 2020 is a Wednesday
      as.Date(ISOdate(2020, 1, 1)),
      target_season = 2020,
      week_day = 6
    ),
    regexp = "specified week day"
  )
})

test_that("Weeks in the same season do not move", {
  # Aug 1, 2020 and Feb 6, 2021 are both in the 2020/2021 season
  # and are both Saturdays
  x <- as.Date(c(ISOdate(2020, 8, 1), ISOdate(2021, 2, 6)))
  expect_equal(
    align_weeks(x, target_season = 2020),
    x
  )
})

test_that("Weeks in adjacent season move by 52 weeks", {
  x <- as.Date(ISOdate(2020, 8, 1))
  y <- align_weeks(x, target_season = 2021)
  expect_equal((y - x) / lubridate::dweeks(1), 52)
})

test_that("Dates in the same season get moved by the same number of weeks", {
  x <- as.Date(ISOdate(2020, 8, 1)) + lubridate::dweeks(0:35)
  y <- align_weeks(x, target_season = 2000)
  expect_true(length(unique((y - x) / lubridate::dweeks(1))) == 1)
})
