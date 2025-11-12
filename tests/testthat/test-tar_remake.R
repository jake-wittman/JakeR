test_that("tar_remake makes and loads a single target", {
  # Create a temporary targets project
  targets::tar_dir({
    targets::tar_script(
      {
        list(
          targets::tar_target(test_target, 42)
        )
      },
      ask = FALSE
    )

    # Run tar_remake
    tar_remake(test_target, notify = FALSE)

    # Check that target exists in global environment
    expect_true(exists("test_target", envir = .GlobalEnv))
    expect_equal(get("test_target", envir = .GlobalEnv), 42)

    # Clean up
    rm(test_target, envir = .GlobalEnv)
  })
})

test_that("tar_remake handles multiple targets with tidyselect", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(
          targets::tar_target(y1, 1 + 1),
          targets::tar_target(y2, 2 + 2),
          targets::tar_target(z, 10)
        )
      },
      ask = FALSE
    )

    # Run tar_remake with tidyselect
    tar_remake(starts_with("y"), notify = FALSE)

    # Check that both y targets exist
    expect_true(exists("y1", envir = .GlobalEnv))
    expect_true(exists("y2", envir = .GlobalEnv))
    expect_equal(get("y1", envir = .GlobalEnv), 2)
    expect_equal(get("y2", envir = .GlobalEnv), 4)

    # z should not be loaded
    expect_false(exists("z", envir = .GlobalEnv))

    # Clean up
    rm(y1, y2, envir = .GlobalEnv)
  })
})

test_that("tar_remake passes reporter argument correctly", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(targets::tar_target(simple, 1))
      },
      ask = FALSE
    )

    # Should not error with different reporter settings
    expect_no_error(tar_remake(simple, notify = FALSE, reporter = "silent"))
    expect_true(exists("simple", envir = .GlobalEnv))

    rm(simple, envir = .GlobalEnv)
  })
})

test_that("tar_remake works with all targets if used with everything()", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(
          targets::tar_target(a, 1),
          targets::tar_target(b, 2)
        )
      },
      ask = FALSE
    )

    # Load all targets by not specifying names
    tar_remake(everything(), notify = FALSE)

    expect_true(exists("a", envir = .GlobalEnv))
    expect_true(exists("b", envir = .GlobalEnv))

    rm(a, b, envir = .GlobalEnv)
  })
})

test_that("tar_remake handles dependencies correctly", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(
          targets::tar_target(upstream, 5),
          targets::tar_target(downstream, upstream * 2)
        )
      },
      ask = FALSE
    )

    # Make downstream (should also make upstream)
    tar_remake(downstream, notify = FALSE)

    expect_true(exists("downstream", envir = .GlobalEnv))
    expect_equal(get("downstream", envir = .GlobalEnv), 10)

    rm(downstream, envir = .GlobalEnv)
  })
})

test_that("tar_remake notify parameter is boolean", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(targets::tar_target(x, 1))
      },
      ask = FALSE
    )

    # notify = FALSE should work
    expect_no_error(tar_remake(x, notify = FALSE))

    # notify = TRUE should work (but won't actually notify in tests)
    # We can't easily test the notification itself without mocking
    expect_no_error(tar_remake(x, notify = TRUE))

    rm(x, envir = .GlobalEnv)
  })
})


test_that("tar_remake with non-existent target errors appropriately", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(targets::tar_target(exists, 1))
      },
      ask = FALSE
    )

    # Trying to make a target that doesn't exist should error
    expect_error(tar_remake(nonexistent, notify = FALSE))
  })
})

test_that("tar_remake loads to GlobalEnv not local environment", {
  targets::tar_dir({
    targets::tar_script(
      {
        list(targets::tar_target(env_test, "global"))
      },
      ask = FALSE
    )

    # Create a function that calls tar_remake
    test_function <- function() {
      tar_remake(env_test, notify = FALSE)
      # Check it's not in local environment
      return(exists("env_test", envir = environment(), inherits = FALSE))
    }

    local_exists <- test_function()

    # Should NOT exist in local environment
    expect_false(local_exists)

    # Should exist in global environment
    expect_true(exists("env_test", envir = .GlobalEnv))

    rm(env_test, envir = .GlobalEnv)
  })
})
