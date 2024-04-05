#' Inverse Value Matching
#'
#' Complement of \code{%in%}. Returns elements of \code{x} that are not in \code{y}.
#'
#' @param x vector or NULL: values to be matched
#' @param y vector or NULL: values to be matched against
#'
#' @usage x \%nin\% y
#' @return Logical vector, indicating if value x was not found in y
#' @export


#' @examples
#' 1 %nin% 2:6
#' 1 %nin% 1:5

`%nin%` <- function(x, y) {
  return( !(x %in% y))
}
