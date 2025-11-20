# splitNames testing -----------------------------------------------------

test_that("splitNames handles basic two-part names", {
  df <- data.frame(Name = c("JOHN SMITH", "MARY JONES"))
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("", ""))
  expect_equal(result$last_name, c("SMITH", "JONES"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles three-part names with middle names", {
  df <- data.frame(Name = c("JOHN ALLEN SMITH", "MARY ELIZABETH JONES"))
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("ALLEN", "ELIZABETH"))
  expect_equal(result$last_name, c("SMITH", "JONES"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles comma-separated format (LAST, FIRST)", {
  df <- data.frame(Name = c("SMITH, JOHN", "JONES, MARY"))
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("", ""))
  expect_equal(result$last_name, c("SMITH", "JONES"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles comma-separated format with middle names", {
  df <- data.frame(Name = c("SMITH, JOHN ALLEN", "JONES, MARY ELIZABETH"))
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("ALLEN", "ELIZABETH"))
  expect_equal(result$last_name, c("SMITH", "JONES"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles suffixes in standard format", {
  df <- data.frame(
    Name = c(
      "JOHN SMITH JR",
      "MARY JONES SR",
      "ROBERT DOE III",
      "JAMES BROWN II"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY", "ROBERT", "JAMES"))
  expect_equal(result$middle_name, c("", "", "", ""))
  expect_equal(result$last_name, c("SMITH", "JONES", "DOE", "BROWN"))
  expect_equal(result$suffix, c("JR", "SR", "III", "II"))
})

test_that("splitNames handles suffixes with middle names", {
  df <- data.frame(
    Name = c(
      "JOHN ALLEN SMITH JR",
      "MARY ELIZABETH JONES SR"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("ALLEN", "ELIZABETH"))
  expect_equal(result$last_name, c("SMITH", "JONES"))
  expect_equal(result$suffix, c("JR", "SR"))
})

test_that("splitNames handles suffixes in comma-separated format", {
  df <- data.frame(
    Name = c(
      "SMITH, JOHN JR",
      "JONES, MARY SR",
      "DOE, ROBERT III"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY", "ROBERT"))
  expect_equal(result$middle_name, c("", "", ""))
  expect_equal(result$last_name, c("SMITH", "JONES", "DOE"))
  expect_equal(result$suffix, c("JR", "SR", "III"))
})

test_that("splitNames handles suffix before comma (LAST SUFFIX, FIRST)", {
  df <- data.frame(
    Name = c(
      "SMITH JR, JOHN",
      "JONES SR, MARY ELIZABETH",
      "DOE III, ROBERT ALLEN"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY", "ROBERT"))
  expect_equal(result$middle_name, c("", "ELIZABETH", "ALLEN"))
  expect_equal(result$last_name, c("SMITH", "JONES", "DOE"))
  expect_equal(result$suffix, c("JR", "SR", "III"))
})

test_that("splitNames handles parenthetical suffixes", {
  df <- data.frame(
    Name = c(
      "JOHN SMITH (JR)",
      "SMITH (JR), JOHN",
      "SMITH (JR), JOHN ALLEN"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "JOHN", "JOHN"))
  expect_equal(result$middle_name, c("", "", "ALLEN"))
  expect_equal(result$last_name, c("SMITH", "SMITH", "SMITH"))
  expect_equal(result$suffix, c("JR", "JR", "JR"))
})

test_that("splitNames handles name prefixes (Van, Von, Mc, etc.)", {
  df <- data.frame(
    Name = c(
      "JOHN VAN HOLLEN",
      "MARY VON TRAPP",
      "ROBERT MC DONALD",
      "SARAH DE LA CRUZ"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY", "ROBERT", "SARAH"))
  expect_equal(result$middle_name, c("", "", "", ""))
  expect_equal(
    result$last_name,
    c("VAN HOLLEN", "VON TRAPP", "MC DONALD", "DE LA CRUZ")
  )
  expect_equal(result$suffix, c("", "", "", ""))
})

test_that("splitNames handles prefixes with middle names", {
  df <- data.frame(
    Name = c(
      "JOHN ALLEN VAN HOLLEN",
      "MARY ELIZABETH VON TRAPP"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("ALLEN", "ELIZABETH"))
  expect_equal(result$last_name, c("VAN HOLLEN", "VON TRAPP"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles prefixes in comma-separated format", {
  df <- data.frame(
    Name = c(
      "VAN HOLLEN, JOHN",
      "VON TRAPP, MARY ELIZABETH"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$middle_name, c("", "ELIZABETH"))
  expect_equal(result$last_name, c("VAN HOLLEN", "VON TRAPP"))
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles prefixes with suffixes", {
  df <- data.frame(
    Name = c(
      "JOHN VAN HOLLEN JR",
      "VAN HOLLEN JR, JOHN",
      "MARY ELIZABETH VON TRAPP SR"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "JOHN", "MARY"))
  expect_equal(result$middle_name, c("", "", "ELIZABETH"))
  expect_equal(result$last_name, c("VAN HOLLEN", "VAN HOLLEN", "VON TRAPP"))
  expect_equal(result$suffix, c("JR", "JR", "SR"))
})

test_that("splitNames handles mixed case names", {
  df <- data.frame(
    Name = c(
      "John Smith",
      "Mary Elizabeth Jones",
      "Robert Van Hollen Jr"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("John", "Mary", "Robert"))
  expect_equal(result$middle_name, c("", "Elizabeth", ""))
  expect_equal(result$last_name, c("Smith", "Jones", "Van Hollen"))
  expect_equal(result$suffix, c("", "", "Jr"))
})

test_that("splitNames handles extra whitespace", {
  df <- data.frame(
    Name = c(
      "  JOHN   SMITH  ",
      "MARY  ELIZABETH   JONES",
      "  SMITH  ,  JOHN  ALLEN  "
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "MARY", "JOHN"))
  expect_equal(result$middle_name, c("", "ELIZABETH", "ALLEN"))
  expect_equal(result$last_name, c("SMITH", "JONES", "SMITH"))
  expect_equal(result$suffix, c("", "", ""))
})

test_that("splitNames handles Roman numeral suffixes", {
  df <- data.frame(
    Name = c(
      "JOHN SMITH I",
      "MARY JONES II",
      "ROBERT DOE III",
      "JAMES BROWN IV",
      "WILLIAM DAVIS V"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$suffix, c("I", "II", "III", "IV", "V"))
})

test_that("splitNames handles complex multi-part last names", {
  df <- data.frame(
    Name = c(
      "MARIA DE LA CRUZ",
      "JOHN ST JAMES",
      "PATRICK O'BRIEN"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("MARIA", "JOHN", "PATRICK"))
  expect_equal(result$last_name, c("DE LA CRUZ", "ST JAMES", "O'BRIEN"))
})

test_that("splitNames handles custom column name parameter", {
  df <- data.frame(FullName = c("JOHN SMITH", "MARY JONES"))
  result <- splitNames(df, name_column = "FullName")

  expect_equal(result$first_name, c("JOHN", "MARY"))
  expect_equal(result$last_name, c("SMITH", "JONES"))
})

test_that("splitNames preserves other columns in dataframe", {
  df <- data.frame(
    ID = c(1, 2),
    Name = c("JOHN SMITH", "MARY JONES"),
    Age = c(30, 25)
  )
  result <- splitNames(df, name_column = 'Name')

  expect_true("ID" %in% names(result))
  expect_true("Age" %in% names(result))
  expect_equal(result$ID, c(1, 2))
  expect_equal(result$Age, c(30, 25))
})

test_that("splitNames handles edge case with suffix in second position", {
  df <- data.frame(Name = c("JOHN JR A SMITH"))
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, "JOHN")
  expect_equal(result$suffix, "JR")
  # The exact middle and last name handling for this edge case may vary
  # based on your specific requirements
})

test_that("splitNames handles all Mac/Mc variations", {
  df <- data.frame(
    Name = c(
      "JOHN MC DONALD",
      "MARY MAC DONALD"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$last_name, c("MC DONALD", "MAC DONALD"))
})

test_that("splitNames handles case insensitive suffixes", {
  df <- data.frame(
    Name = c(
      "JOHN SMITH jr",
      "MARY JONES Sr",
      "ROBERT DOE iii"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$suffix, c("jr", "Sr", "iii"))
})

test_that("splitNames handles names with multiple middle names", {
  df <- data.frame(
    Name = c(
      "JOHN ALLEN MICHAEL SMITH",
      "SMITH, JOHN ALLEN MICHAEL"
    )
  )
  result <- splitNames(df, name_column = 'Name')

  expect_equal(result$first_name, c("JOHN", "JOHN"))
  expect_equal(result$middle_name, c("ALLEN", "ALLEN MICHAEL"))
  expect_equal(result$last_name, c("MICHAEL SMITH", "SMITH"))
})

test_that("splitNames returns empty strings instead of NA", {
  df <- data.frame(Name = c("JOHN SMITH", "MARY"))
  result <- splitNames(df, name_column = 'Name')

  # Check that there are no NA values in the output columns
  expect_false(any(is.na(result$first_name)))
  expect_false(any(is.na(result$middle_name)))
  expect_false(any(is.na(result$last_name)))
  expect_false(any(is.na(result$suffix)))

  # Check that empty values are truly empty strings
  expect_equal(result$middle_name[1], "")
  expect_equal(result$suffix, c("", ""))
})

test_that("splitNames handles empty or single-name inputs gracefully", {
  df <- data.frame(Name = c("JOHN", ""))
  result <- splitNames(df, name_column = 'Name')

  # Single name should at least have first_name populated
  expect_equal(result$first_name[1], "JOHN")

  # Empty name handling (should not error)
  expect_true(nrow(result) == 2)
})

# removeNicknames testing ------------------------------------------------

test_that("removeNicknames removes single nicknames in quotes", {
  df <- data.frame(Name = "SMITH WILSON, JOHNATHAN \"JOHN\"")
  result <- removeNicknames(df)

  expect_equal(result$Name, "SMITH WILSON, JOHNATHAN")
})

test_that("removeNicknames removes nicknames at the beginning, middle, or end", {
  df <- data.frame(
    Name = c(
      "\"JOHNNY\" SMITH, JOHN",
      "SMITH, \"JOHNNY\" JOHN",
      "SMITH, JOHN \"JOHNNY\""
    )
  )

  result <- removeNicknames(df)

  expect_equal(
    result$Name,
    c(
      "SMITH, JOHN",
      "SMITH, JOHN",
      "SMITH, JOHN"
    )
  )
})

test_that("removeNicknames handles multiple nicknames in one string", {
  df <- data.frame(
    Name = "SMITH, JOHN \"JOHNNY\" \"THE BOSS\""
  )
  result <- removeNicknames(df)

  expect_equal(result$Name, "SMITH, JOHN")
})

test_that("removeNicknames preserves names without nicknames", {
  df <- data.frame(Name = "NORMAL NAME WITHOUT NICKNAME")
  result <- removeNicknames(df)

  expect_equal(result$Name, "NORMAL NAME WITHOUT NICKNAME")
})

test_that("removeNicknames collapses whitespace correctly", {
  df <- data.frame(
    Name = "SMITH   JOHN   \"JOHNNY\"   JR"
  )
  result <- removeNicknames(df)

  expect_equal(result$Name, "SMITH JOHN JR")
})

test_that("removeNicknames works with custom name_column", {
  df <- data.frame(FullName = "DOE, JANE \"JANIE\"")
  result <- removeNicknames(df, name_column = "FullName")

  expect_equal(result$FullName, "DOE, JANE")
})

test_that("removeNicknames handles empty strings and NA gracefully", {
  df <- data.frame(Name = c("", NA, "\"TEST\""))
  result <- removeNicknames(df)

  expect_equal(result$Name[1], "")
  expect_true(is.na(result$Name[2]))
  expect_equal(result$Name[3], "")
})

test_that("removeNicknames is vectorized (each row processed independently)", {
  df <- data.frame(
    Name = c(
      "SMITH, JOHN \"JOHNNY\"",
      "JONES, MIKE \"M\""
    )
  )

  result <- removeNicknames(df)

  expect_equal(
    result$Name,
    c(
      "SMITH, JOHN",
      "JONES, MIKE"
    )
  )
})

# Remove compound separators testing -------------------------------------

test_that("removeCompoundSeparators removes hyphens and apostrophes", {
  df <- data.frame(
    last_name = c("SMITH-JONES", "O'CONNOR", "MARTINEZ-RODRIGUEZ")
  )

  result <- removeCompoundSeparators(df)

  expect_equal(
    result$last_name,
    c("SMITH JONES", "O CONNOR", "MARTINEZ RODRIGUEZ")
  )
})

test_that("removeCompoundSeparators leaves names with no separators unchanged", {
  df <- data.frame(last_name = c("GARCIA", "LEE", "PATEL"))

  result <- removeCompoundSeparators(df)

  expect_equal(result$last_name, df$last_name)
})

test_that("removeCompoundSeparators handles multiple separators in one name", {
  df <- data.frame(last_name = c("DE-LA-CRUZ-O'NEILL"))

  result <- removeCompoundSeparators(df)

  expect_equal(result$last_name, "DE LA CRUZ O NEILL")
})

test_that("removeCompoundSeparators works with custom last_name_column", {
  df <- data.frame(
    surname = c("VAN-DER-BERG", "O'MALLEY")
  )

  result <- removeCompoundSeparators(df, last_name_column = "surname")

  expect_equal(
    result$surname,
    c("VAN DER BERG", "O MALLEY")
  )
})

test_that("removeCompoundSeparators preserves other columns", {
  df <- data.frame(
    first = c("JOHN", "MARY"),
    last_name = c("SMITH-JONES", "O'CONNOR"),
    suffix = c("", "JR"),
    stringsAsFactors = FALSE
  )

  result <- removeCompoundSeparators(df)

  expect_equal(result$first, df$first)
  expect_equal(result$suffix, df$suffix)
})

# create matching keys test ----------------------------------------------

test_that("createMatchingKeys generates correct keys for simple names", {
  df <- data.frame(
    last_name = "SMITH",
    first_name = "JOHN",
    date_of_birth = "1980-01-01",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(df)

  expect_equal(result$match_key_full, "SMITH|1980-01-01")
  expect_equal(result$match_key_first_word, "SMITH|1980-01-01")
  expect_equal(result$match_key_last_word, "SMITH|1980-01-01")
  expect_equal(result$match_key_first_name, "JOHN|1980-01-01")
  expect_equal(result$match_key_first_plus_first_last, "JOHN|SMITH|1980-01-01")
  expect_equal(result$match_key_first_plus_last_last, "JOHN|SMITH|1980-01-01")

  # Individual word key list
  expect_equal(result$individual_keys[[1]], "SMITH|1980-01-01")
})

test_that("createMatchingKeys handles compound last names correctly", {
  df <- data.frame(
    last_name = "SMITH WEST",
    first_name = "JANE",
    date_of_birth = "1990-05-10",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(df)

  # First and last word
  expect_equal(result$last_name_first_word, "SMITH")
  expect_equal(result$last_name_last_word, "WEST")

  # Matching keys
  expect_equal(result$match_key_full, "SMITH WEST|1990-05-10")
  expect_equal(result$match_key_first_word, "SMITH|1990-05-10")
  expect_equal(result$match_key_last_word, "WEST|1990-05-10")
  expect_equal(result$match_key_first_name, "JANE|1990-05-10")
  expect_equal(result$match_key_first_plus_first_last, "JANE|SMITH|1990-05-10")
  expect_equal(result$match_key_first_plus_last_last, "JANE|WEST|1990-05-10")

  # Individual keys must include both words
  expect_setequal(
    result$individual_keys[[1]],
    c("SMITH|1990-05-10", "WEST|1990-05-10")
  )
})

test_that("createMatchingKeys preserves other columns", {
  df <- data.frame(
    id = 1,
    last_name = "SMITH",
    first_name = "JOHN",
    date_of_birth = "2000-12-12",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(df)

  expect_equal(result$id, 1)
})

test_that("createMatchingKeys works with custom column names", {
  df <- data.frame(
    ln = "MARTINEZ RODRIGUEZ",
    fn = "MARIA",
    dob = "1975-03-22",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(
    df,
    last_name_col = "ln",
    first_name_col = "fn",
    dob_col = "dob"
  )

  # Check first and last extracted words
  expect_equal(result$last_name_first_word, "MARTINEZ")
  expect_equal(result$last_name_last_word, "RODRIGUEZ")

  # Individual keys list
  expect_setequal(
    result$individual_keys[[1]],
    c("MARTINEZ|1975-03-22", "RODRIGUEZ|1975-03-22")
  )
})

test_that("createMatchingKeys handles extra whitespace correctly", {
  df <- data.frame(
    last_name = "  VAN   DER   BERG  ",
    first_name = "  JOHN   ",
    date_of_birth = "1988-09-09",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(df)

  expect_equal(result$last_name_clean, "VAN DER BERG")
  expect_equal(result$first_name_clean, "JOHN")

  expect_equal(result$last_name_first_word, "VAN")
  expect_equal(result$last_name_last_word, "BERG")

  expect_setequal(
    result$individual_keys[[1]],
    c(
      "VAN|1988-09-09",
      "DER|1988-09-09",
      "BERG|1988-09-09"
    )
  )
})

test_that("createMatchingKeys lists unique individual keys when duplicates appear", {
  df <- data.frame(
    last_name = "LEE LEE",
    first_name = "ALAN",
    date_of_birth = "2001-01-01",
    stringsAsFactors = FALSE
  )

  result <- createMatchingKeys(df)

  # Should list only one unique LEE key
  expect_equal(result$individual_keys[[1]], "LEE|2001-01-01")
})


# multipass matching tests -----------------------------------------------

### PASS 1: exact full-name matching -----------------------------------------

test_that("multiPassMatching identifies exact full-name matches", {
  df1 <- data.frame(
    last_name = "SMITH",
    first_name = "JOHN",
    date_of_birth = "1990-01-01"
  )

  df2 <- data.frame(
    last_name = "SMITH",
    first_name = "JOHN",
    date_of_birth = "1990-01-01"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "exact_full_name")
})


### PASS 2: Compound → Single (first word matches) ----------------------------

test_that("multiPassMatching handles compound-to-single first-word matches", {
  df1 <- data.frame(
    last_name = "SMITH JOHNSON",
    first_name = "SARA",
    date_of_birth = "1985-04-04"
  )

  df2 <- data.frame(
    last_name = "SMITH",
    first_name = "SARA",
    date_of_birth = "1985-04-04"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "compound_to_single_first")
})


### PASS 3: Compound → Single (last word matches) -----------------------------

test_that("multiPassMatching handles compound-to-single last-word matches", {
  df1 <- data.frame(
    last_name = "SMITH JOHNSON",
    first_name = "SARA",
    date_of_birth = "1985-04-04"
  )

  df2 <- data.frame(
    last_name = "JOHNSON",
    first_name = "SARA",
    date_of_birth = "1985-04-04"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "compound_to_single_last")
})


### PASS 4: Single → Compound (single matches first word of compound) ---------

test_that("multiPassMatching handles single-to-compound first-word matches", {
  df1 <- data.frame(
    last_name = "SMITH",
    first_name = "JACOB",
    date_of_birth = "1970-02-02"
  )

  df2 <- data.frame(
    last_name = "SMITH ANDERSON",
    first_name = "JACOB",
    date_of_birth = "1970-02-02"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "single_to_compound_first")
})


### PASS 5: Single → Compound (single matches last word of compound) ----------

test_that("multiPassMatching handles single-to-compound last-word matches", {
  df1 <- data.frame(
    last_name = "ANDERSON",
    first_name = "JACOB",
    date_of_birth = "1970-02-02"
  )

  df2 <- data.frame(
    last_name = "SMITH ANDERSON",
    first_name = "JACOB",
    date_of_birth = "1970-02-02"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "single_to_compound_last")
})


### PASS 6: First name + DOB only (maiden-name match) -------------------------

test_that("multiPassMatching matches on first name + DOB when last names differ", {
  df1 <- data.frame(
    last_name = "DOE",
    first_name = "JANE",
    date_of_birth = "1995-07-07"
  )

  df2 <- data.frame(
    last_name = "JOHNSON", # new married last name
    first_name = "JANE",
    date_of_birth = "1995-07-07"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "first_name_dob_match")
})


### ENSURE NO MATCH RETURNS EMPTY --------------------------------------------

test_that("multiPassMatching returns zero rows when no match exists", {
  df1 <- data.frame(
    last_name = "SMITH",
    first_name = "JOHN",
    date_of_birth = "2000-01-01"
  )

  df2 <- data.frame(
    last_name = "DOE",
    first_name = "JANE",
    date_of_birth = "2000-01-01"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 0)
})


### CUSTOM COLUMN NAME SUPPORT ------------------------------------------------

test_that("multiPassMatching works with custom column names", {
  df1 <- data.frame(
    ln = "BROWN TAYLOR",
    fn = "MARK",
    dob = "1982-03-03"
  )

  df2 <- data.frame(
    ln = "BROWN",
    fn = "MARK",
    dob = "1982-03-03"
  )

  result <- multiPassMatching(
    df1,
    df2,
    last_name_col = "ln",
    first_name_col = "fn",
    dob_col = "dob"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "compound_to_single_first")
})


### WHITESPACE HANDLING -------------------------------------------------------

test_that("multiPassMatching handles extra whitespace in names", {
  df1 <- data.frame(
    last_name = "  SMITH   JOHNSON ",
    first_name = "  ANA ",
    date_of_birth = "1999-09-09"
  )

  df2 <- data.frame(
    last_name = "JOHNSON",
    first_name = "ANA",
    date_of_birth = "1999-09-09"
  )

  result <- multiPassMatching(df1, df2)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "compound_to_single_last")
})
