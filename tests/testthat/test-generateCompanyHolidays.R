library(testthat)
library(timeDate)

# Source the function (adjust path as needed)
# source("path/to/your/function.R")

test_that("generateCompanyHolidays returns correct number of holidays for single year", {
  result <- generateCompanyHolidays(2024)
  # 3 fixed + 4 floating = 7 holidays per year
  expect_equal(length(result), 7)
})

test_that("generateCompanyHolidays returns correct number of holidays for multiple years", {
  result <- generateCompanyHolidays(c(2024, 2025))
  expect_equal(length(result), 14)
  
  result <- generateCompanyHolidays(2020:2023)
  expect_equal(length(result), 28)
})

test_that("generateCompanyHolidays includes all fixed holidays", {
  result <- generateCompanyHolidays(2024)
  
  expect_true("2024-01-01" %in% result) # New Year's Day
  expect_true("2024-07-04" %in% result) # Independence Day
  expect_true("2024-12-25" %in% result) # Christmas
})

test_that("generateCompanyHolidays includes all floating holidays", {
  result <- generateCompanyHolidays(2024)
  
  # Check that floating holidays are present (exact dates vary by year)
  expect_true(any(grepl("2024-02-", result))) # Presidents Day in February
  expect_true(any(grepl("2024-05-", result))) # Memorial Day in May
  expect_true(any(grepl("2024-09-", result))) # Labor Day in September
  expect_true(any(grepl("2024-11-", result))) # Thanksgiving in November
})

test_that("generateCompanyHolidays returns dates in chronological order", {
  result <- generateCompanyHolidays(c(2024, 2023, 2025))
  
  # Check that result is sorted
  expect_equal(result, sort(result))
  
  # First holiday should be from earliest year
  expect_true(grepl("^2023", result[1]))
  
  # Last holiday should be from latest year
  expect_true(grepl("^2025", result[length(result)]))
})

test_that("generateCompanyHolidays returns character vector", {
  result <- generateCompanyHolidays(2024)
  expect_type(result, "character")
})

test_that("generateCompanyHolidays dates are in correct format", {
  result <- generateCompanyHolidays(2024)
  
  # Check YYYY-MM-DD format
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", result)))
})

test_that("generateCompanyHolidays handles single year as numeric", {
  result <- generateCompanyHolidays(2024)
  expect_equal(length(result), 7)
  expect_true(all(grepl("^2024", result)))
})

test_that("generateCompanyHolidays handles empty input", {
  result <- generateCompanyHolidays(c())
  expect_equal(length(result), 0)
  expect_type(result, "character")
})

test_that("generateCompanyHolidays produces correct specific dates for known years", {
  result <- generateCompanyHolidays(2024)
  
  # Known 2024 holidays
  expect_true("2024-01-01" %in% result) # New Year's Day
  expect_true("2024-02-19" %in% result) # Presidents Day
  expect_true("2024-05-27" %in% result) # Memorial Day
  expect_true("2024-07-04" %in% result) # Independence Day
  expect_true("2024-09-02" %in% result) # Labor Day
  expect_true("2024-11-28" %in% result) # Thanksgiving
  expect_true("2024-12-25" %in% result) # Christmas
})

test_that("generateCompanyHolidays maintains consistency across multiple calls", {
  result1 <- generateCompanyHolidays(2024)
  result2 <- generateCompanyHolidays(2024)
  
  expect_identical(result1, result2)
})

test_that("generateCompanyHolidays handles historical and future years", {
  # Historical year
  result_past <- generateCompanyHolidays(2000)
  expect_equal(length(result_past), 7)
  
  # Future year
  result_future <- generateCompanyHolidays(2030)
  expect_equal(length(result_future), 7)
})

test_that("generateCompanyHolidays has no duplicate dates", {
  result <- generateCompanyHolidays(c(2024, 2025))
  expect_equal(length(result), length(unique(result)))
})