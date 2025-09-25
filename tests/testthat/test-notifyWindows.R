# tests/testthat/test-notifyWindows.R

library(testthat)

test_that("notifyWindows works with default parameters", {
  # Use a variable to capture the command instead of function attributes
  captured_command <- NULL
  captured_wait <- NULL
  captured_show <- NULL

  mock_system <- function(command, wait = TRUE, show.output.on.console = TRUE) {
    captured_command <<- command
    captured_wait <<- wait
    captured_show <<- show.output.on.console
    return(0)
  }

  with_mocked_bindings(
    system = mock_system,
    .package = "base",
    {
      output <- capture.output({
        result <- notifyWindows()
      })

      # Check that system was called with correct command
      expected_cmd <- 'msg * "R code has finished running!"'
      expect_equal(captured_command, expected_cmd)
      expect_equal(captured_wait, FALSE)
      expect_equal(captured_show, FALSE)

      # Check console output
      expect_match(
        paste(output, collapse = " "),
        "Notification sent.*R code has finished running!"
      )
    }
  )
})

test_that("notifyWindows works with custom message", {
  custom_message <- "Analysis completed successfully!"
  captured_command <- NULL

  mock_system <- function(command, wait = TRUE, show.output.on.console = TRUE) {
    captured_command <<- command
    return(0)
  }

  with_mocked_bindings(
    system = mock_system,
    .package = "base",
    {
      output <- capture.output({
        notifyWindows(custom_message)
      })

      expected_cmd <- sprintf('msg * "%s"', custom_message)
      expect_equal(captured_command, expected_cmd)
      expect_match(
        paste(output, collapse = " "),
        "Notification sent.*Analysis completed successfully!"
      )
    }
  )
})
