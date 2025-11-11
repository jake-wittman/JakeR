#' Generate Company Holidays for Multiple Years
#'
#' @description
#' Generates a sorted list of US company holidays for one or more years.
#' Includes both fixed holidays (New Year's Day, Independence Day, Christmas)
#' and floating holidays that vary by year (Presidents Day, Memorial Day,
#' Labor Day, Thanksgiving).
#'
#' @param years A numeric vector of years for which to generate holidays.
#'   Can be a single year or multiple years.
#'
#' @return A character vector of holiday dates in "YYYY-MM-DD" format,
#'   sorted chronologically. Returns an empty character vector if `years`
#'   is empty.
#'
#' @details
#' The function generates 7 holidays per year:
#'
#' **Fixed holidays:**
#' \itemize{
#'   \item New Year's Day (January 1)
#'   \item Independence Day (July 4)
#'   \item Christmas Day (December 25)
#' }
#'
#' **Floating holidays:**
#' \itemize{
#'   \item Presidents Day (3rd Monday in February)
#'   \item Memorial Day (Last Monday in May)
#'   \item Labor Day (1st Monday in September)
#'   \item Thanksgiving Day (4th Thursday in November)
#' }
#'
#' Floating holiday dates are calculated using the \code{timeDate} package.
#'
#' @importFrom timeDate USPresidentsDay USMemorialDay USLaborDay USThanksgivingDay
#'
#' @examples
#' # Generate holidays for a single year
#' generateCompanyHolidays(2024)
#'
#' # Generate holidays for multiple years
#' generateCompanyHolidays(c(2024, 2025))
#'
#' # Generate holidays for a range of years
#' generateCompanyHolidays(2020:2025)
#'
#' # Empty input returns empty character vector
#' generateCompanyHolidays(c())
#'
#' @seealso
#' \code{\link[timeDate]{USPresidentsDay}},
#' \code{\link[timeDate]{USMemorialDay}},
#' \code{\link[timeDate]{USLaborDay}},
#' \code{\link[timeDate]{USThanksgivingDay}}
#'
#' @export
generateCompanyHolidays <- function(years) {
  # Check if timeDate package is available
  if (!requireNamespace("timeDate", quietly = TRUE)) {
    stop(
      "Package 'timeDate' is required but not installed. Please install it with: install.packages('timeDate')"
    )
  }

  holidays <- character(0)
  for (year in years) {
    # Fixed holidays
    holidays <- c(
      holidays,
      paste0(year, "-01-01"), # New Year's Day
      paste0(year, "-07-04"), # Independence Day
      paste0(year, "-12-25") # Christmas
    )
    # Floating holidays (using timeDate package)
    holidays <- c(
      holidays,
      as.character(timeDate::USPresidentsDay(year)), # 3rd Monday in February
      as.character(timeDate::USMemorialDay(year)), # Last Monday in May
      as.character(timeDate::USLaborDay(year)), # 1st Monday in September
      as.character(timeDate::USThanksgivingDay(year)) # 4th Thursday in November
    )
  }
  return(sort(holidays))
}
