library(testthat)

test_that("All length-1 works", {
  expect_true(validate_lengths(NA, x = 1, y = "foo"))
})

test_that("All but 1 are length-1 works", {
  expect_true(validate_lengths(x = 1, y = c("foo", "bar"), z = NA))
})

test_that("Multiple lengths fail", {
  expect_error(validate_length(x = 1, y = c("foo", "bar"), z = c(NA, NA, NA)))
})

test_that("Two lengths, but wrongly mixed, fail", {
  expect_error(validate_length(x = 1, y = c("foo", "bar"), z = c(NA, NA)))
})
