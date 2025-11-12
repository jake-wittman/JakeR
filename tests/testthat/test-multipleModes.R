test_that("multipleModes handles single mode correctly", {
  # Single clear mode
  expect_equal(multipleModes(c(1, 2, 2, 3, 4)), 2)
  expect_equal(multipleModes(c("a", "b", "b", "c")), "b")
})

test_that("multipleModes averages multiple modes", {
  # Two modes: 2 and 4 both appear twice
  expect_equal(multipleModes(c(1, 2, 2, 3, 4, 4, 5)), 3)

  # Three modes: 1, 3, 5 all appear twice
  expect_equal(multipleModes(c(1, 1, 2, 3, 3, 4, 5, 5)), 3)

  # Two modes with decimals
  expect_equal(multipleModes(c(1.5, 1.5, 2.5, 2.5, 3)), 2.0)
})

test_that("multipleModes handles all equal values", {
  # All values the same (technically all are modes)
  result <- multipleModes(c(5, 5, 5, 5))
  expect_equal(result, 5)
})

test_that("multipleModes handles all unique values", {
  # All values unique (all are modes with frequency 1)
  # Should return NA
  result <- multipleModes(c(1, 2, 3, 4, 5))
  expect_equal(result, NA_real_) # Average of 1,2,3,4,5
})

test_that("multipleModes handles NA values correctly", {
  # NA values should be ignored due to na.rm = TRUE
  expect_equal(multipleModes(c(1, 2, 2, 3, NA, NA)), 2)

  # Multiple modes with NAs
  expect_equal(multipleModes(c(1, 1, 2, 2, 3, NA)), 1.5)
})

test_that("multipleModes handles vectors with only NAs", {
  # All NA should return NA
  result <- multipleModes(c(NA, NA, NA))
  expect_true(is.na(result))
})

test_that("multipleModes handles empty vectors", {
  # Empty vector behavior
  result <- multipleModes(numeric(0))
  expect_true(length(result) == 0 || is.na(result))
})

test_that("multipleModes handles negative numbers", {
  # Single mode with negatives
  expect_equal(multipleModes(c(-5, -3, -3, -1)), -3)

  # Multiple modes with negatives
  expect_equal(multipleModes(c(-2, -2, 0, 0, 2)), -1)
})

test_that("multipleModes returns correct type", {
  # Numeric input returns numeric
  expect_type(multipleModes(c(1, 2, 2, 3)), "double")

  # Character input returns character
  expect_type(multipleModes(c("a", "b", "b")), "character")
})

test_that("multipleModes handles large datasets", {
  # Large vector with clear mode
  large_vec <- c(rep(1, 100), rep(2, 200), rep(3, 100))
  expect_equal(multipleModes(large_vec), 2)

  # Large vector with two modes
  large_vec2 <- c(rep(1, 100), rep(2, 100), 3, 4, 5)
  expect_equal(multipleModes(large_vec2), 1.5)
})
