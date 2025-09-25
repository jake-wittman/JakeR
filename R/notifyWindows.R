#' Send Windows notification message
#'
#' Sends a notification message using Windows' built-in `msg` command.
#' This function displays a pop-up message to all users on the local machine.
#' Useful for alerting when long-running R code has completed.
#'
#' @param message Character string. The message to display in the notification.
#'   Default is "R code has finished running!".
#'
#' @return Invisible NULL. The function is called for its side effect of
#'   displaying a notification and printing a status message to the console.
#'
#' @details
#' This function uses the Windows `msg` command to send notifications. The `msg`
#' command may not be available on all Windows systems (particularly Home editions)
#' or may be disabled for security reasons. If the command fails, a message is
#' printed to the console instead.
#'
#' The notification is sent asynchronously (wait = FALSE), so the function
#' returns immediately without waiting for the notification to be dismissed.
#'
#' @note
#' This function only works on Windows systems where the `msg` command is
#' available and enabled. On other operating systems or if the command fails,
#' only a console message will be displayed.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default message
#' notifyWindows()
#'
#' # Custom notification message
#' notifyWindows("Data analysis completed successfully!")
#'
#' # Use at the end of long-running processes
#' long_analysis <- function() {
#'   # Your analysis code here...
#'   Sys.sleep(5)  # Simulate long process
#'   notifyWindows("Analysis finished!")
#' }
#' long_analysis()
#'
#' # Notify about specific results
#' notifyWindows("Model training complete - Accuracy: 94.2%")
#' }
#'
#' @seealso
#' \code{\link{system}} for running system commands
#'
#' @export
notifyWindows <- function(message = "R code has finished running!") {
  # Build the msg command
  notification_cmd <- sprintf('msg * "%s"', message)
  # Execute the command
  result <- system(
    notification_cmd,
    wait = FALSE,
    show.output.on.console = FALSE
  )
  # Print confirmation to console
  if (result == 0) {
    cat("Notification sent:", message, "\n")
  } else {
    cat("Notification failed - msg command may not be available\n")
  }
}
