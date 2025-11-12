# tests/testthat/test-specifyDecimal.R

test_that("specifyDecimal works with basic numeric inputs", {
  expect_equal(specifyDecimal(3.14159, 2), "3.14")
  expect_equal(specifyDecimal(3.14159, 3), "3.142")
  expect_equal(specifyDecimal(3.14159, 0), "3")
  expect_equal(specifyDecimal(3.14159, 1), "3.1")
})

test_that("specifyDecimal handles integers correctly", {
  expect_equal(specifyDecimal(5, 2), "5.00")
  expect_equal(specifyDecimal(10, 1), "10.0")
  expect_equal(specifyDecimal(0, 3), "0.000")
})

test_that("specifyDecimal handles negative numbers", {
  expect_equal(specifyDecimal(-3.14159, 2), "-3.14")
  expect_equal(specifyDecimal(-5, 1), "-5.0")
  expect_equal(specifyDecimal(-0.123, 3), "-0.123")
})

test_that("specifyDecimal handles zero decimal places", {
  expect_equal(specifyDecimal(3.14159, 0), "3")
  expect_equal(specifyDecimal(3.7, 0), "4") # Should round up
  expect_equal(specifyDecimal(3.4, 0), "3") # Should round down
})

test_that("specifyDecimal handles numeric vectors", {
  x <- c(1.234, 2.567, 3.891)
  expected <- c("1.23", "2.57", "3.89")
  expect_equal(specifyDecimal(x, 2), expected)

  # Test with different values
  x2 <- c(10, 20.5, 30.789)
  expected2 <- c("10.0", "20.5", "30.8")
  expect_equal(specifyDecimal(x2, 1), expected2)
})

test_that("specifyDecimal handles rounding correctly", {
  # Test rounding up
  expect_equal(specifyDecimal(1.235, 2), "1.24") # Round up from 5
  expect_equal(specifyDecimal(1.236, 2), "1.24") # Round up from 6

  # Test rounding down
  expect_equal(specifyDecimal(1.234, 2), "1.23") # Round down from 4
  expect_equal(specifyDecimal(1.231, 2), "1.23") # Round down from 1
})

test_that("specifyDecimal handles edge cases with very small numbers", {
  expect_equal(specifyDecimal(0.001, 2), "0.00")
  expect_equal(specifyDecimal(0.001, 3), "0.001")
  expect_equal(specifyDecimal(0.0001, 3), "0.000")
})

test_that("specifyDecimal handles very large numbers", {
  expect_equal(specifyDecimal(1234567.89, 2), "1234567.89")
  expect_equal(specifyDecimal(1e6, 1), "1000000.0") # Now this would work
  expect_equal(specifyDecimal(1e7, 0), "10000000")
})

test_that("specifyDecimal trims whitespace", {
  # This tests the trimws() functionality
  # format() can sometimes add leading spaces for alignment
  result <- specifyDecimal(c(1, 10, 100), 1)
  expect_true(all(!grepl("^\\s", result))) # No leading whitespace
  expect_true(all(!grepl("\\s$", result))) # No trailing whitespace
})

test_that("specifyDecimal returns character vector", {
  result <- specifyDecimal(3.14, 2)
  expect_type(result, "character")
  expect_length(result, 1)

  # Test with vector input
  result_vector <- specifyDecimal(c(1.1, 2.2, 3.3), 1)
  expect_type(result_vector, "character")
  expect_length(result_vector, 3)
})

test_that("specifyDecimal handles special numeric values", {
  # Test Inf
  expect_equal(specifyDecimal(Inf, 2), "Inf")
  expect_equal(specifyDecimal(-Inf, 2), "-Inf")

  # Test NaN
  expect_equal(specifyDecimal(NaN, 2), "NaN")
})

test_that("specifyDecimal handles NA values", {
  expect_equal(specifyDecimal(NA, 2), "NA")
  expect_equal(specifyDecimal(c(1.23, NA, 4.56), 2), c("1.23", "NA", "4.56"))
})

test_that("specifyDecimal preserves vector length", {
  x <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  result <- specifyDecimal(x, 1)
  expect_length(result, length(x))
})

test_that("specifyDecimal examples from documentation work", {
  expect_equal(specifyDecimal(pi, 1), "3.1")
  expect_equal(specifyDecimal(pi, 2), "3.14")
  expect_equal(specifyDecimal(pi, 3), "3.142")
})

test_that("specifyDecimal handles zero correctly", {
  expect_equal(specifyDecimal(0, 0), "0")
  expect_equal(specifyDecimal(0, 2), "0.00")
  expect_equal(specifyDecimal(0.0, 3), "0.000")
})

test_that("specifyDecimal error handling for invalid inputs", {
  # Test non-numeric input
  expect_error(specifyDecimal("3.14", 2))
  expect_error(specifyDecimal(c("1", "2"), 1))

  # Test invalid digits parameter
  expect_error(specifyDecimal(3.14, "2"))
  expect_error(specifyDecimal(3.14, c(1, 2)))
})

test_that("specifyDecimal rejects negative digits parameter", {
  expect_error(
    specifyDecimal(1234.56, -1),
    "digits must be a single non-negative numeric value"
  )
  expect_error(
    specifyDecimal(1234.56, -2),
    "digits must be a single non-negative numeric value"
  )
})

test_that("specifyDecimal consistency with different input types", {
  # Test that different ways of representing the same number give same result
  expect_equal(specifyDecimal(3, 2), specifyDecimal(3.0, 2))
  expect_equal(specifyDecimal(3L, 2), specifyDecimal(3.0, 2))
})

test_that("specifyDecimal handles very high precision", {
  # Test with many decimal places
  x <- 1 / 3 # 0.333333...
  result <- specifyDecimal(x, 10)
  expect_equal(nchar(result), 12) # "0." + 10 digits
  expect_match(result, "^0\\.3{10}$")
})

test_that("specifyDecimal handles mixed positive and negative numbers", {
  x <- c(-2.456, 0, 1.234, -0.789)
  expected <- c("-2.46", "0.00", "1.23", "-0.79")
  expect_equal(specifyDecimal(x, 2), expected)
})

test_that("specifyDecimal maintains order in vectors", {
  x <- c(3.1, 1.4, 5.9, 2.6)
  result <- specifyDecimal(x, 1)
  expected <- c("3.1", "1.4", "5.9", "2.6")
  expect_equal(result, expected)
})
