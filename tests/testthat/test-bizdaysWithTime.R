library(testthat)
library(bizdays)

# Setup test calendar
holidays_2025 <- as.Date(c(
  "2025-01-01", # New Year's Day
  "2025-01-20", # MLK Day
  "2025-12-25" # Christmas
))

create.calendar(
  name = "test_calendar",
  holidays = holidays_2025,
  weekdays = c("saturday", "sunday")
)

# Test 1: Basic functionality - business days only
test_that("calculates business days between weekday dates", {
  # Monday to Wednesday (2 business days)
  start <- as.POSIXct("2025-01-06 00:00:00") # Monday
  end <- as.POSIXct("2025-01-08 00:00:00") # Wednesday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 2.0)
})

# Test 2: Fractional business days
test_that("calculates fractional business days with time component", {
  # Monday 9 AM to Monday 3 PM (0.25 days = 6 hours)
  start <- as.POSIXct("2025-01-06 09:00:00")
  end <- as.POSIXct("2025-01-06 15:00:00")

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 0.25, tolerance = 0.001)
})

# Test 3: Weekend handling
test_that("excludes weekends from calculation", {
  # Friday 2:30 PM to Monday 10:15 AM (should be ~1.82 business days)
  # Friday partial: 11.5 hours (2:30 PM to midnight) = 0.479 days
  # Weekend: 0 days
  # Monday partial: 10.25 hours (midnight to 10:15 AM) = 0.427 days
  # Total calendar time: 2.82 days (67.75 hours)
  # Business days in span: 1 (just Monday, Friday doesn't count as full day crossed)
  # Ratio: 1/3 = 0.333
  # Result: 2.82 * 0.333 = ~0.94

  start <- as.POSIXct("2025-01-03 14:30:00") # Friday
  end <- as.POSIXct("2025-01-06 10:15:00") # Monday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_true(result > 0 && result < 3) # Should be less than calendar days
  expect_true(result < 2) # Should be less than 2 business days
})

# Test 4: Holiday handling
test_that("excludes holidays from calculation", {
  # Friday to Tuesday when Monday is a holiday (MLK Day)
  # Should only count Tuesday as business day
  start <- as.POSIXct("2025-01-17 00:00:00") # Friday
  end <- as.POSIXct("2025-01-21 00:00:00") # Tuesday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  # 1 business day (just Tuesday) out of 4 calendar days = ratio of 0.25
  expect_equal(result, 1.0)
})

# Test 5: Same day - business day
test_that("handles same business day correctly", {
  # Monday 9 AM to Monday 5 PM (8 hours = 1/3 day)
  start <- as.POSIXct("2025-01-06 09:00:00")
  end <- as.POSIXct("2025-01-06 17:00:00")

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 8 / 24, tolerance = 0.001)
})

# Test 6: Same day - weekend
test_that("returns zero for same weekend day", {
  # Saturday 9 AM to Saturday 5 PM
  start <- as.POSIXct("2025-01-04 09:00:00") # Saturday
  end <- as.POSIXct("2025-01-04 17:00:00")

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 0)
})

# Test 7: Same day - holiday
test_that("returns zero for same holiday", {
  # New Year's Day 9 AM to 5 PM
  start <- as.POSIXct("2025-01-01 09:00:00")
  end <- as.POSIXct("2025-01-01 17:00:00")

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 0)
})

# Test 8: Entire period is non-business days
test_that("returns zero when entire period is weekends/holidays", {
  # Saturday to Sunday
  start <- as.POSIXct("2025-01-04 09:00:00") # Saturday
  end <- as.POSIXct("2025-01-05 17:00:00") # Sunday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 0)
})

# Test 9: Vectorization - multiple date pairs
test_that("handles vectorized input correctly", {
  starts <- as.POSIXct(c(
    "2025-01-06 00:00:00", # Monday to Wednesday
    "2025-01-06 09:00:00", # Monday 9 AM to 3 PM same day
    "2025-01-04 09:00:00" # Saturday (weekend)
  ))

  ends <- as.POSIXct(c(
    "2025-01-08 00:00:00", # Wednesday
    "2025-01-06 15:00:00", # Monday 3 PM
    "2025-01-04 17:00:00" # Saturday
  ))

  result <- bizdaysWithTime(starts, ends, cal = "test_calendar")

  expect_length(result, 3)
  expect_equal(result[1], 2.0)
  expect_equal(result[2], 0.25, tolerance = 0.001)
  expect_equal(result[3], 0)
})

# Test 10: Negative time span
test_that("handles negative time spans (end before start)", {
  start <- as.POSIXct("2025-01-08 00:00:00")
  end <- as.POSIXct("2025-01-06 00:00:00")

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_true(result < 0)
  expect_equal(result, -2.0)
})

# Test 11: Date objects (no time component)
test_that("handles Date objects as input", {
  start <- as.Date("2025-01-06") # Monday
  end <- as.Date("2025-01-08") # Wednesday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")
  expect_equal(result, 2.0)
})

# Test 12: Long time span with multiple weekends and holidays
test_that("handles long time spans correctly", {
  # Full month including holidays and weekends
  start <- as.POSIXct("2025-01-01 00:00:00") # New Year's (Wed, holiday)
  end <- as.POSIXct("2025-01-31 00:00:00") # Friday

  result <- bizdaysWithTime(start, end, cal = "test_calendar")

  # Should be significantly less than 30 days
  expect_true(result < 30)
  expect_true(result > 0)

  # Verify it's reasonable (roughly 21 business days in January 2025)
  expect_true(result >= 20 && result <= 23)
})

# Test 13: Edge case - midnight crossings
test_that("handles midnight boundary correctly", {
  # 11 PM to 1 AM next day (business days)
  start <- as.POSIXct("2025-01-06 23:00:00") # Monday 11 PM
  end <- as.POSIXct("2025-01-07 01:00:00") # Tuesday 1 AM

  result <- bizdaysWithTime(start, end, cal = "test_calendar")

  # 2 hours over 1 calendar day, 1 business day
  expect_equal(result, 2 / 24, tolerance = 0.001)
})

# Test 14: NA handling
test_that("handles NA values appropriately", {
  starts <- as.POSIXct(c("2025-01-06 00:00:00", NA, "2025-01-08 00:00:00"))
  ends <- as.POSIXct(c("2025-01-08 00:00:00", "2025-01-10 00:00:00", NA))

  result <- bizdaysWithTime(starts, ends, cal = "test_calendar")

  expect_length(result, 3)
  expect_equal(result[1], 2.0)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

# Test 15: Performance test (optional, for large datasets)
test_that("performs efficiently with large vectors", {
  n <- 10000
  starts <- as.POSIXct("2025-01-06 00:00:00") + (0:(n - 1)) * 3600
  ends <- starts + 86400 # Add 1 day to each

  expect_silent({
    result <- bizdaysWithTime(starts, ends, cal = "test_calendar")
  })

  expect_length(result, n)
  expect_true(all(is.numeric(result)))
})
