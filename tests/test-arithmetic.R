# Ensure testthat is available; in packages, add to Suggests in DESCRIPTION.
# For non-package repos, your CI installs testthat explicitly.

source(file.path("..", "arithmetic.R"))

test_that("add, subtract, multiply work on scalars", {
  expect_equal(add(1, 2), 3)
  expect_equal(subtract(5, 3), 2)
  expect_equal(multiply(2, 4), 8)
})

test_that("divide works and rejects division by zero", {
  expect_equal(divide(10, 2), 5)
  expect_error(divide(1, 0), "Division by zero")
})

test_that("power and modulo behave correctly", {
  expect_equal(power(2, 3), 8)
  expect_equal(modulo(10, 3), 1)
  expect_error(modulo(1, 0), "Modulo by zero")
})

test_that("vectorization with equal lengths", {
  expect_equal(add(c(1, 2, 3), c(4, 5, 6)), c(5, 7, 9))
  expect_equal(subtract(c(5, 5, 5), c(1, 2, 3)), c(4, 3, 2))
  expect_equal(multiply(c(2, 2, 2), c(3, 4, 5)), c(6, 8, 10))
})

test_that("safe recycling when one side is length 1", {
  expect_equal(add(10, c(1, 2, 3)), c(11, 12, 13))
  expect_equal(multiply(c(2, 3, 4), 10), c(20, 30, 40))
})

test_that("mismatched lengths error out", {
  expect_error(add(1:3, 1:2), "Lengths must match")
})

test_that("NA handling is preserved (no unexpected errors)", {
  expect_equal(add(c(1, NA, 3), 1), c(2, NA, 4))
  expect_equal(divide(c(10, NA), 2), c(5, NA))
  # division/modulo by zero with NA present should still error due to zero
  expect_error(divide(c(1, NA), c(0, 2)), "Division by zero")
  expect_error(modulo(c(1, NA), c(0, 2)), "Modulo by zero")
})

test_that("type checks provide helpful errors", {
  expect_error(add("a", 1), "`a` must be numeric")
  expect_error(add(1, "b"), "`b` must be numeric")
})

test_that("floating-point comparisons allow tolerance", {
  expect_equal(divide(1, 3), 1/3, tolerance = 1e-12)
})

