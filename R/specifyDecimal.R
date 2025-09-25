#' Specify number of decimal places to show
#'
#' @param x A numeric vector
#' @param digits The number of decimals to display
#'
#' @return A character vector
#' @export
#'
#' @examples
#' specifyDecimal(pi, 1)
#' specifyDecimal(pi, 2)
#' specifyDecimal(pi, 3)
specifyDecimal <- function(x, digits) {
  # Add input validation - allow NA values
  if (!is.numeric(x) && !all(is.na(x))) {
    stop("x must be numeric")
  }
  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop("digits must be a single non-negative numeric value")
  }

  trimws(format(round(x, digits), nsmall = digits, scientific = FALSE))
}
