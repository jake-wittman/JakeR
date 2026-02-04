#' Coalesce Join
#'
#' Perform a join operation and coalesce overlapping columns, keeping values
#' from either the left or right table based on preference.
#'
#' @param x A data frame (left table in the join).
#' @param y A data frame (right table in the join).
#' @param by A character vector of variables to join by. If NULL, will use all
#'   common variables. See `?dplyr::join` for details.
#' @param keep Character string specifying which table's values to keep when
#'   both tables have non-NA values for overlapping columns. Options are
#'   "left" (default) to keep values from x, or "right" to keep values from y.
#' @param suffix Character vector of length 2 specifying suffixes to use for
#'   overlapping column names. Default is c(".x", ".y").
#' @param join Character string specifying the type of join to perform. Options
#'   are "full_join" (default), "left_join", "right_join", or "inner_join".
#'
#' @return A data frame resulting from the specified join operation with
#'   overlapping columns coalesced according to the `keep` parameter. Temporary
#'   suffix columns are removed from the output.
#'
#' @details
#' This function extends standard dplyr join operations by automatically
#' coalescing overlapping columns based on the `keep` argument. When both
#' tables contain non-NA values for the same column, the value from the
#' specified table (left or right) will be retained.
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(id = 1:3, name = c("A", NA, "C"))
#' df2 <- data.frame(id = 2:4, name = c("B", "C", "D"))
#'
#' # Keep left values when both exist
#' coalesce_join(df1, df2, by = "id", keep = "left")
#'
#' # Keep right values when both exist
#' coalesce_join(df1, df2, by = "id", keep = "right", join = "left_join")
#' }
#'
#' @export
coalesce_join <- function(
  x,
  y,
  by = NULL,
  keep = c("left", "right"), # "left" means keep value from left table if values exist in both tables.
  suffix = c(".x", ".y"), # Same as the suffix argument in dplyr joins.
  join = c("full_join", "left_join", "right_join", "inner_join") # Choose a join type from the list. The default is full_join.
) {
  keep = match.arg(keep)
  join = match.arg(join)
  join = match.fun(join) # Confirm the join argument is in the list and match the string to the function
  # Depends on the keep argument, overwrite the duplicate value
  # If keep = "left", the value from the left table will be kept, vice versa.
  if (keep == "left") {
    suffix_ = suffix
  } else {
    suffix_ = rev(suffix)
  }
  join(x, y, by = by, suffix = suffix) |>
    mutate(
      across(
        # Apply the coalesce function to all overlapped columns
        ends_with(suffix_[1]), # Select columns ended with .x if keep = "left"; or .y if keep = "right"
        ~ coalesce(
          .,
          get(str_replace(cur_column(), suffix_[1], suffix_[2])) # Replace .x in var.x with .y to generate var.y, if keep = "left"; or vice versa.
        ),
        .names = "{str_remove(.col, suffix_[1])}" # Remove the suffix from the combined columns
      ),
      .keep = "unused"
    ) # Remove the temporary columns ended with suffix
}
