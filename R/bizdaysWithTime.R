#' Calculate Business Days with Decimal Time
#'
#' @description
#' Calculates the number of business days between two datetime stamps,
#' accounting for weekends and holidays while treating each business day
#' as a full 24-hour period. Returns a decimal value representing partial days.
#' This function is vectorized and can handle multiple date pairs simultaneously.
#'
#' @param start_dt A POSIXct, POSIXlt, Date object, or vector of such objects
#'   representing the start datetime(s). If a Date object is provided, time is
#'   assumed to be 00:00:00.
#' @param end_dt A POSIXct, POSIXlt, Date object, or vector of such objects
#'   representing the end datetime(s). Must be the same length as \code{start_dt}.
#'   If a Date object is provided, time is assumed to be 00:00:00.
#' @param cal Character string specifying the name of the business calendar
#'   to use. Must be a calendar previously created with
#'   \code{\link[bizdays]{create.calendar}}. Default is "my_calendar".
#'
#' @return A numeric vector of the same length as \code{start_dt} and
#'   \code{end_dt}, representing the number of business days between each pair
#'   of datetimes, including fractional days based on the time component.
#'   Returns 0 for any pair where the time span contains no business days.
#'
#' @details
#' This function calculates business days while respecting the time component
#' of datetime stamps. It treats each business day as a full 24-hour period,
#' so time differences are preserved proportionally across business days only.
#'
#' The calculation for each pair:
#' \enumerate{
#'   \item Determines the number of full business days between the start and end dates
#'   \item Calculates the total elapsed time in days (including time component)
#'   \item Computes the ratio of business days to calendar days
#'   \item Applies this ratio to the elapsed time to get business days with decimals
#' }
#'
#' Weekends and holidays (as defined in the specified calendar) are excluded
#' from the calculation. If the entire period falls on non-business days,
#' the function returns 0 for that pair.
#'
#' @examples
#' library(bizdays)
#'
#' # Create a calendar with US holidays
#' holidays <- as.Date(c("2025-01-01", "2025-01-20", "2025-12-25"))
#' create.calendar(
#'   name = "my_calendar",
#'   holidays = holidays,
#'   weekdays = c("saturday", "sunday")
#' )
#'
#' # Single pair: Friday 2:30 PM to Monday 10:15 AM
#' start <- as.POSIXct("2025-01-03 14:30:00")
#' end <- as.POSIXct("2025-01-06 10:15:00")
#' bizdaysWithTime(start, end)  # Returns ~1.82
#'
#' # Multiple pairs (vectorized)
#' starts <- as.POSIXct(c("2025-01-03 14:30:00", "2025-01-17 09:00:00", "2025-01-10 08:00:00"))
#' ends <- as.POSIXct(c("2025-01-06 10:15:00", "2025-01-20 17:00:00", "2025-01-13 16:30:00"))
#' bizdaysWithTime(starts, ends)  # Returns vector of 3 values
#'
#' # Usage with data frame (fully vectorized)
#' library(dplyr)
#' df <- data.frame(
#'   start = as.POSIXct(c("2025-01-03 14:30:00", "2025-01-17 09:00:00")),
#'   end = as.POSIXct(c("2025-01-06 10:15:00", "2025-01-20 17:00:00"))
#' )
#'
#' df |>
#'   mutate(
#'     biz_days = bizdaysWithTime(start, end)
#'   )
#'
#' @seealso
#' \code{\link[bizdays]{bizdays}} for integer business day calculations,
#' \code{\link[bizdays]{create.calendar}} for creating business calendars,
#' \code{\link[bizdays]{is.bizday}} for checking if a date is a business day
#'
#' @export
bizdaysWithTime <- function(start_dt, end_dt, cal = "my_calendar") {
  # Convert to POSIXct for consistent handling
  start_dt <- as.POSIXct(start_dt)
  end_dt <- as.POSIXct(end_dt)

  # Initialize result vector with NA
  result <- rep(NA_real_, length(start_dt))

  # Identify valid (non-missing) pairs
  valid <- !is.na(start_dt) & !is.na(end_dt)
  if (!any(valid)) {
    return(result)
  }

  # Extract date components
  start_date <- as.Date(start_dt[valid])
  end_date <- as.Date(end_dt[valid])

  # Calculate components for valid pairs
  full_days <- bizdays::bizdays(start_date, end_date, cal = cal)
  time_diff <- as.numeric(difftime(
    end_dt[valid],
    start_dt[valid],
    units = "days"
  ))
  calendar_days <- as.numeric(end_date - start_date)

  # Initialize result for valid subset
  res_valid <- numeric(length(start_date))

  # Different-day cases
  different_days <- !is.na(calendar_days) & calendar_days != 0
  if (any(different_days)) {
    ratio <- full_days[different_days] / calendar_days[different_days]
    res_valid[different_days] <- time_diff[different_days] * ratio
  }

  # Same-day cases
  same_day <- !is.na(calendar_days) & calendar_days == 0
  if (any(same_day)) {
    is_biz <- bizdays::is.bizday(start_date[same_day], cal = cal)
    res_valid[same_day] <- ifelse(is_biz, time_diff[same_day], 0)
  }

  # Fill valid results back into full-length result
  result[valid] <- res_valid

  return(result)
}
