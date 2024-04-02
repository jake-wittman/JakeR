#' Not Value Matching
#'
#' Check if the left hand side value is present in the right hand side. Return FALSE if it is, TRUE if not. Negation of %in%.
#'
#' @param x vector or NULL: values to be matched
#' @param table vector or NULL: values to be matched against
#'
#' @return Logical vector, indicating if value x was not found in table.
#' @export
#'
#' @examples
#' 1 %!in% 2:6
#' 1 %!in% 1:5

`%!in%` <- Negate(`%in%`)
