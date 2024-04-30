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
specifyDecimal <-
  function(x, digits) {
    trimws(format(round(x, digits), nsmall = digits))
  }
