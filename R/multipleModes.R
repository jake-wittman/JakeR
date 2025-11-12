#' Handle Multiple Modes by Averaging
#'
#' Calculates the mode of a vector. If multiple modes exist (i.e., multiple
#' values appear with the same highest frequency), returns the average of all
#' modes. If a single mode exists, returns that mode value.
#'
#' @param .x A numeric or character vector for which to find the mode(s).
#'
#' @return A single numeric value representing either the mode (if one exists)
#'   or the average of multiple modes. Returns NA if the input is empty or
#'   contains only NA values.
#'
#' @details
#' This function wraps \code{\link[DescTools]{Mode}} with logic to handle
#' the case where multiple values tie for the highest frequency. In such cases,
#' rather than returning all modes, it returns their mean. NA values are
#' automatically removed before calculation.
#'
#' @examples
#' # Single mode
#' multipleModes(c(1, 2, 2, 3, 4))  # Returns 2
#'
#' # Multiple modes - returns average
#' multipleModes(c(1, 1, 2, 2, 3))  # Returns 1.5 (average of 1 and 2)
#'
#' # All values unique - returns average of all
#' multipleModes(c(1, 2, 3, 4, 5))  # Returns 3
#'
#' # With NA values
#' multipleModes(c(1, 2, 2, NA, NA))  # Returns 2
#'
#' @seealso \code{\link[DescTools]{Mode}}
#'
#' @export
multipleModes <- function(.x) {
  # If multiple modes exist, take the average of the modes
  modes <- DescTools::Mode(.x, na.rm = TRUE)
  if (length(modes) > 1) {
    mean(modes)
  } else {
    modes[1] # Strip the attribute of frequency so it just returns the value
  }
}
