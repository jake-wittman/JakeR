# Test coalesce_join function
test_that("coalesce_join performs full_join with left keep", {
  df1 <- data.frame(id = 1:3, name = c("Alice", NA, "Charlie"), value = c(10, 20, 30))
  df2 <- data.frame(id = 2:4, name = c("Bob", "Charlie", "David"), value = c(25, 35, 40))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left", join = "full_join")
  
  expect_equal(nrow(result), 4)
  expect_equal(result$name[result$id == 1], "Alice")
  expect_equal(result$name[result$id == 2], "Bob")  # NA in left, so takes right
  expect_equal(result$name[result$id == 3], "Charlie")  # Both exist, keeps left
  expect_equal(result$value[result$id == 3], 30)  # Both exist, keeps left
})

test_that("coalesce_join performs full_join with right keep", {
  df1 <- data.frame(id = 1:3, name = c("Alice", NA, "Charlie"), value = c(10, 20, 30))
  df2 <- data.frame(id = 2:4, name = c("Bob", "Charlie_Right", "David"), value = c(25, 35, 40))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "right", join = "full_join")
  
  expect_equal(nrow(result), 4)
  expect_equal(result$name[result$id == 1], "Alice")  # Only in left
  expect_equal(result$name[result$id == 2], "Bob")  # NA in left, so takes right
  expect_equal(result$name[result$id == 3], "Charlie_Right")  # Both exist, keeps right
  expect_equal(result$value[result$id == 3], 35)  # Both exist, keeps right
})

test_that("coalesce_join performs left_join correctly", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 2:4, name = c("Bob_Right", "Charlie_Right", "David"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left", join = "left_join")
  
  expect_equal(nrow(result), 3)
  expect_equal(result$id, 1:3)
  expect_equal(result$name[result$id == 2], "Bob")  # Keeps left value
})

test_that("coalesce_join performs right_join correctly", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 2:4, name = c("Bob_Right", "Charlie_Right", "David"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "right", join = "right_join")
  
  expect_equal(nrow(result), 3)
  expect_equal(result$id, 2:4)
  expect_equal(result$name[result$id == 4], "David")
})

test_that("coalesce_join performs inner_join correctly", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 2:4, name = c("Bob_Right", "Charlie_Right", "David"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left", join = "inner_join")
  
  expect_equal(nrow(result), 2)
  expect_equal(result$id, 2:3)
  expect_equal(result$name, c("Bob", "Charlie"))  # Keeps left values
})

test_that("coalesce_join handles all NA in one table", {
  df1 <- data.frame(id = 1:3, name = c(NA, NA, NA))
  df2 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left")
  
  expect_equal(result$name, c("Alice", "Bob", "Charlie"))  # Takes from right when left is NA
})

test_that("coalesce_join handles all NA in right table", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 1:3, name = c(NA, NA, NA))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "right")
  
  expect_equal(result$name, c("Alice", "Bob", "Charlie"))  # Takes from left when right is NA
})

test_that("coalesce_join works with custom suffix", {
  df1 <- data.frame(id = 1:2, name = c("Alice", "Bob"))
  df2 <- data.frame(id = 1:2, name = c("Alice2", "Bob2"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left", suffix = c("_1", "_2"))
  
  expect_equal(result$name, c("Alice", "Bob"))
  expect_false("name_1" %in% names(result))
  expect_false("name_2" %in% names(result))
})

test_that("coalesce_join works with multiple join columns", {
  df1 <- data.frame(id = c(1, 1, 2), type = c("A", "B", "A"), value = c(10, 20, 30))
  df2 <- data.frame(id = c(1, 1, 2), type = c("A", "B", "A"), value = c(15, 25, 35))
  
  result <- coalesce_join(df1, df2, by = c("id", "type"), keep = "left")
  
  expect_equal(nrow(result), 3)
  expect_equal(result$value, c(10, 20, 30))  # Keeps left values
})

test_that("coalesce_join handles no overlapping columns", {
  df1 <- data.frame(id = 1:3, col_a = c("A", "B", "C"))
  df2 <- data.frame(id = 1:3, col_b = c("X", "Y", "Z"))
  
  result <- coalesce_join(df1, df2, by = "id", keep = "left")
  
  expect_equal(ncol(result), 3)
  expect_true(all(c("id", "col_a", "col_b") %in% names(result)))
})

test_that("coalesce_join throws error for invalid join type", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 1:3, name = c("Alice2", "Bob2", "Charlie2"))
  
  expect_error(
    coalesce_join(df1, df2, by = "id", join = "invalid_join"),
    "'arg' should be one of"
  )
})

test_that("coalesce_join throws error for invalid keep argument", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = 1:3, name = c("Alice2", "Bob2", "Charlie2"))
  
  expect_error(
    coalesce_join(df1, df2, by = "id", keep = "invalid"),
    "'arg' should be one of"
  )
})

