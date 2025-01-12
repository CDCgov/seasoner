library(testthat)

test_that("After season start gives year = season", {
  expect_equal(
    ref_date_in_season(
      season = 2020,
      season_start_month = 7,
      season_start_day = 1,
      ref_month = 8,
      ref_day = 1
    ),
    as.Date(ISOdate(2020, 8, 1))
  )
})

test_that("Before season start gives year = season + 1", {
  expect_equal(
    ref_date_in_season(
      season = 2020,
      season_start_month = 7,
      season_start_day = 1,
      ref_month = 2,
      ref_day = 1
    ),
    as.Date(ISOdate(2021, 2, 1))
  )
})

test_that("Vectorized works", {
  expect_equal(
    ref_date_in_season(
      season = c(2020, 2021, 2022),
      season_start_month = 7,
      season_start_day = 1,
      ref_month = 2,
      ref_day = 1
    ),
    as.Date(ISOdate(c(2021, 2022, 2023), 2, 1))
  )
})
