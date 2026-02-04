#' Send Teams Notification via Power Automate Webhook
#'
#' Sends an adaptive card notification to Microsoft Teams using a Power Automate
#' webhook. Optionally supports mentioning users.
#'
#' @param title Character string. The title/header of the notification.
#' @param message Character string. The main message body of the notification.
#'   Default is "An alert has been triggered."
#' @param webhook_url Character string. The Power Automate webhook URL to send
#'   the notification to.
#' @param mention_upn Character string or NULL. The User Principal Name (UPN)
#'   of a user to mention in the notification (e.g., "user@domain.com").
#'   Default is NULL (no mention).
#' @param color Character string. The color theme for the title. Options include
#'   "Attention" (red), "Good" (green), "Warning" (yellow), "Accent" (blue),
#'   or "Default". Default is "Attention".
#' @param facts Named list or NULL. Optional key-value pairs to display as facts
#'   in the card. Default is NULL.
#' @param include_timestamp Logical. Whether to include a timestamp in the facts.
#'   Default is TRUE.
#'
#' @return Logical. Returns TRUE if notification sent successfully, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' # Basic notification
#' sendTeamsNotification(
#'   title = "Pipeline Failed",
#'   message = "The data processing pipeline encountered an error.",
#'   webhook_url = "https://prod-xx.logic.azure.com:443/workflows/..."
#' )
#'
#' # Notification with user mention
#' sendTeamsNotification(
#'   title = "Action Required",
#'   message = "Please review the following items.",
#'   webhook_url = "https://prod-xx.logic.azure.com:443/workflows/...",
#'   mention_upn = "user@company.com"
#' )
#'
#' # Notification with custom facts
#' sendTeamsNotification(
#'   title = "Report Complete",
#'   message = "Monthly report has been generated.",
#'   webhook_url = "https://prod-xx.logic.azure.com:443/workflows/...",
#'   color = "Good",
#'   facts = list("Records Processed" = "1,234", "Duration" = "5 minutes")
#' )
#' }
#'
#' @export
sendTeamsNotification <- function(title, 
                                   message = "An alert has been triggered.",
                                   webhook_url, 
                                   mention_upn = NULL,
                                   color = "Attention",
                                   facts = NULL,
                                   include_timestamp = TRUE) {
  
  # Validate inputs
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required but not installed. Install it with: install.packages('httr')")
  }
  
  if (missing(title) || is.null(title) || title == "") {
    stop("'title' must be provided and cannot be empty.")
  }
  
  if (missing(webhook_url) || is.null(webhook_url) || webhook_url == "") {
    stop("'webhook_url' must be provided and cannot be empty.")
  }
  
  if (!color %in% c("Attention", "Good", "Warning", "Accent", "Default")) {
    warning("Invalid color specified. Using 'Attention' as default.")
    color <- "Attention"
  }
  
  tryCatch(
    {
      # Create the basic card body
      card_body <- list(
        list(
          type = "TextBlock",
          text = title,
          weight = "Bolder",
          size = "Large",
          color = color
        ),
        list(
          type = "TextBlock",
          text = message,
          wrap = TRUE
        )
      )
      
      # Add facts if provided or include timestamp
      fact_list <- list()
      
      if (include_timestamp) {
        fact_list <- c(fact_list, list(list(title = "Time:", value = as.character(Sys.time()))))
      }
      
      if (!is.null(facts) && length(facts) > 0) {
        for (i in seq_along(facts)) {
          fact_list <- c(fact_list, list(list(
            title = paste0(names(facts)[i], ":"),
            value = as.character(facts[[i]])
          )))
        }
      }
      
      # Only add FactSet if there are facts to display
      if (length(fact_list) > 0) {
        card_body <- c(
          card_body,
          list(list(
            type = "FactSet",
            facts = fact_list
          ))
        )
      }
      
      # Prepare msteams object for mentions if provided
      msteams <- NULL
      if (!is.null(mention_upn) && mention_upn != "") {
        # Add mention text to card body
        card_body <- c(
          card_body,
          list(
            list(
              type = "TextBlock",
              text = paste0("<at>", mention_upn, "</at>"),
              wrap = TRUE
            )
          )
        )
        
        # Add entities for the mention
        msteams <- list(
          entities = list(
            list(
              type = "mention",
              text = paste0("<at>", mention_upn, "</at>"),
              mentioned = list(
                id = mention_upn,
                name = mention_upn
              )
            )
          )
        )
      }
      
      # Create adaptive card
      content <- list(
        type = "AdaptiveCard",
        version = "1.4",
        body = card_body
      )
      
      # Add msteams property if there are mentions
      if (!is.null(msteams)) {
        content$msteams <- msteams
      }
      
      body <- list(
        attachments = list(
          list(
            contentType = "application/vnd.microsoft.card.adaptive",
            content = content
          )
        )
      )
      
      # Send the request
      response <- httr::POST(
        url = webhook_url,
        body = body,
        encode = "json"
      )
      
      if (httr::status_code(response) %in% c(200, 202)) {
        message("Teams notification sent successfully.")
        return(invisible(TRUE))
      } else {
        warning(sprintf("Failed to send Teams notification. Status: %d", httr::status_code(response)))
        return(invisible(FALSE))
      }
    },
    error = function(e) {
      warning(sprintf("Error sending Teams notification: %s", conditionMessage(e)))
      return(invisible(FALSE))
    }
  )
}