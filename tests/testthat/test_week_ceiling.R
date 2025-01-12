library(testthat)

test_that("Week ceiling does not change on boundary", {
  # Jan 4, 2020 is a Saturday
  x <- as.Date(ISOdate(2020, 1, 4))
  stopifnot(lubridate::wday(x, week_start = 1) == 6)
  expect_equal(week_ceiling(x, week_day = 6), x)
})

test_that("Week ceiling rounds up", {
  # Jan 5, 2020 is a Sunday; should go up to following Saturday
  x <- as.Date(ISOdate(2020, 1, 5))
  stopifnot(lubridate::wday(x, week_start = 1) == 7)
  expect_equal(week_ceiling(x, week_day = 6), as.Date(ISOdate(2020, 1, 11)))
})

test_that("Can take vector of dates", {
  expect_equal(
    week_ceiling(as.Date(ISOdate(2020, 1, 5:7)), week_day = 6),
    as.Date(ISOdate(2020, 1, c(11, 11, 11)))
  )
})

test_that("Week ceiling fails on non-Date", {
  expect_error(week_ceiling(ISOdate(2020, 1, 1)), "must be a Date object")
})
