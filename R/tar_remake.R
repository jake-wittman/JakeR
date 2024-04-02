#' Make and load a target
#'
#' Make and load a target from a targets workflow
#'
#' @param names The name(s) of the target to be remade. Accepts tidyselect helpers.
#'
#' @return If the target is successfully made, loads the target into the workspace.
#' @export
#'
#' @examples
#' \dontrun{
#' tar_remake(starts_with('model'))
#' }
#'  #example stolen from the targets documentation
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#'   tar_script({
#'     list(
#'       tar_target(y1, 1 + 1),
#'       tar_target(y2, 1 + 1),
#'       tar_target(z, y1 + y2)
#'     )
#'   }, ask = FALSE)
#'   tar_remake(starts_with("y")) # Only processes y1 and y2.
#'}
#'}
tar_remake <- function(names) {
  targets::tar_make( {{ names }}, reporter = 'timestamp_positives' )
  targets::tar_load( {{ names }}, envir = .GlobalEnv )
}
