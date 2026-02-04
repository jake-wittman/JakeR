test_that("sendTeamsNotification constructs correct body structure", {
  captured_body <- NULL

  # Mock POST to capture the body being sent
  mock_post <- function(url, body, encode) {
    captured_body <<- body # Capture the body in parent environment
    structure(
      list(status_code = 200),
      class = "response"
    )
  }

  with_mocked_bindings(
    POST = mock_post,
    {
      sendTeamsNotification(
        title = "Test Title",
        message = "Test Message",
        webhook_url = "https://test.webhook.com/test123"
      )
    },
    .package = "httr"
  )

  # Now verify the structure of captured_body
  expect_equal(
    captured_body$attachments[[1]]$contentType,
    "application/vnd.microsoft.card.adaptive"
  )
  expect_equal(
    captured_body$attachments[[1]]$content$type,
    "AdaptiveCard"
  )
  expect_equal(
    captured_body$attachments[[1]]$content$version,
    "1.4"
  )

  # Verify body structure
  body_elements <- captured_body$attachments[[1]]$content$body
  expect_equal(body_elements[[1]]$type, "TextBlock")
  expect_equal(body_elements[[1]]$text, "Test Title")
  expect_equal(body_elements[[1]]$color, "Attention")
  expect_equal(body_elements[[2]]$text, "Test Message")
})

test_that("sendTeamsNotification includes custom facts correctly", {
  captured_body <- NULL

  mock_post <- function(url, body, encode) {
    captured_body <<- body
    structure(list(status_code = 200), class = "response")
  }

  test_facts <- list("Status" = "Complete", "Count" = "100")

  with_mocked_bindings(
    POST = mock_post,
    {
      sendTeamsNotification(
        title = "Test",
        message = "Test message",
        webhook_url = "https://test.com",
        facts = test_facts
      )
    },
    .package = "httr"
  )

  # Find the FactSet in the body
  body_elements <- captured_body$attachments[[1]]$content$body
  fact_set <- body_elements[[length(body_elements)]] # FactSet should be last

  expect_equal(fact_set$type, "FactSet")

  # Should have timestamp + 2 custom facts = 3 total
  expect_equal(length(fact_set$facts), 3)

  # Check custom facts are present
  fact_titles <- sapply(fact_set$facts, function(f) f$title)
  expect_true("Status:" %in% fact_titles)
  expect_true("Count:" %in% fact_titles)
})

test_that("sendTeamsNotification constructs mention correctly", {
  captured_body <- NULL

  mock_post <- function(url, body, encode) {
    captured_body <<- body
    structure(list(status_code = 200), class = "response")
  }

  with_mocked_bindings(
    POST = mock_post,
    {
      sendTeamsNotification(
        title = "Test",
        message = "Test",
        webhook_url = "https://test.com",
        mention_upn = "user@example.com"
      )
    },
    .package = "httr"
  )

  content <- captured_body$attachments[[1]]$content

  # Check msteams property exists
  expect_false(is.null(content$msteams))
  expect_equal(content$msteams$entities[[1]]$type, "mention")
  expect_equal(content$msteams$entities[[1]]$mentioned$id, "user@example.com")

  # Check mention text is in body
  body_texts <- sapply(content$body, function(b) b$text)
  expect_true(any(grepl("<at>user@example.com</at>", body_texts)))
})

test_that("sendTeamsNotification respects include_timestamp parameter", {
  captured_with_time <- NULL
  captured_without_time <- NULL

  mock_post <- function(url, body, encode) {
    if (is.null(captured_with_time)) {
      captured_with_time <<- body
    } else {
      captured_without_time <<- body
    }
    structure(list(status_code = 200), class = "response")
  }

  with_mocked_bindings(
    POST = mock_post,
    {
      # With timestamp
      sendTeamsNotification(
        title = "Test",
        webhook_url = "https://test.com",
        include_timestamp = TRUE
      )

      # Without timestamp
      sendTeamsNotification(
        title = "Test",
        webhook_url = "https://test.com",
        include_timestamp = FALSE
      )
    },
    .package = "httr"
  )

  # With timestamp should have FactSet
  body_with <- captured_with_time$attachments[[1]]$content$body
  has_factset_with <- any(sapply(body_with, function(b) b$type == "FactSet"))
  expect_true(has_factset_with)

  # Without timestamp should not have FactSet (assuming no other facts)
  body_without <- captured_without_time$attachments[[1]]$content$body
  has_factset_without <- any(sapply(body_without, function(b) {
    b$type == "FactSet"
  }))
  expect_false(has_factset_without)
})

test_that("sendTeamsNotification applies correct color", {
  captured_body <- NULL

  mock_post <- function(url, body, encode) {
    captured_body <<- body
    structure(list(status_code = 200), class = "response")
  }

  with_mocked_bindings(
    POST = mock_post,
    {
      sendTeamsNotification(
        title = "Success",
        webhook_url = "https://test.com",
        color = "Good"
      )
    },
    .package = "httr"
  )

  title_block <- captured_body$attachments[[1]]$content$body[[1]]
  expect_equal(title_block$color, "Good")
})
