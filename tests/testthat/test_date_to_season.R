library(testthat)

test_that("After season start gives that year", {
  x <- ISOdate(2020, 8, 1) # Aug 1, 2020
  # if season starts July 1, then Aug 1 is in 2020/2021 season
  expect_equal(date_to_season(x, start_month = 7, start_day = 1), 2020)
})

test_that("Before season start gives the next year", {
  x <- ISOdate(2021, 2, 1) # Feb 1, 2021
  # if season starts July 1, then Feb 1 is in 2020/2021 season
  expect_equal(date_to_season(x, start_month = 7, start_day = 1), 2020)
})

test_that("First date of season gives that calendar year", {
  expect_equal(date_to_season(ISOdate(2020, 7, 1), start_month = 7, start_day = 1), 2020)
})

test_that("Vectorized date to season", {
  expect_equal(date_to_season(ISOdate(2020, c(6, 7, 8), 1), start_month = 7, start_day = 1), c(2019, 2020, 2020))
})
