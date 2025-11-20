#' Split Full Names into Component Parts
#'
#' This function takes a dataframe with a full name column and splits it into
#' separate first name, middle name, last name, and suffix columns. It handles
#' various edge cases including name prefixes (Van, Von, Mc, etc.), suffixes
#' (Jr, Sr, III, etc.), comma-separated names, and suffixes that appear early in the name.
#'
#' @param df A dataframe containing a column with full names to be split
#' @param name_column Character string specifying the name of the column containing
#'   the full names. Default is "Name".
#'
#' @return A dataframe with the original columns plus four new columns:
#'   \itemize{
#'     \item \code{first_name} - The first name
#'     \item \code{middle_name} - The middle name (empty string if no middle name)
#'     \item \code{last_name} - The last name, including prefixes like "Van" or "Mc"
#'     \item \code{suffix} - Name suffixes like "Jr", "Sr", "III", etc. (empty string if no suffix)
#'   }
#'
#' @details The function handles several complex naming patterns:
#'   \itemize{
#'     \item \strong{Comma-separated Names}: Properly parses "LASTNAME, FIRSTNAME MIDDLE" format
#'     \item \strong{Suffixes Before Comma}: Handles "LASTNAME SUFFIX, FIRSTNAME MIDDLE" format
#'     \item \strong{Standard Names}: Handles "FIRSTNAME MIDDLE LASTNAME" format
#'     \item \strong{Name Prefixes}: Keeps prefixes like Van, Von, Mc, Mac, St,
#'       De, Del, La, Le, Di, Da, O' as part of the last name
#'     \item \strong{Suffixes}: Properly extracts Jr, Sr, I, II, III, IV, V
#'       whether they appear at the end or early in the name
#'     \item \strong{Parenthetical Suffixes}: Handles suffixes in parentheses like "(Jr)"
#'     \item \strong{Case Insensitive}: Works with names in any case (ALL CAPS, mixed case, etc.)
#'     \item \strong{Extra Whitespace}: Cleans up extra spaces and formatting issues
#'   }
#'
#' @examples
#' # Basic usage - standard format
#' df <- data.frame(Name = c("JOHN SMITH", "MARY ELIZABETH JONES"))
#' result <- splitNames(df)
#'
#' # Comma-separated format
#' df <- data.frame(Name = c("SMITH, JOHN", "JONES, MARY ELIZABETH"))
#' result <- splitNames(df)
#'
#' # With prefixes and suffixes - standard format
#' df <- data.frame(Name = c("ROBERT VAN HOLLEN JR", "MARY VON TRAPP"))
#' result <- splitNames(df)
#'
#' # With prefixes and suffixes - comma format
#' df <- data.frame(Name = c("VAN HOLLEN JR, ROBERT", "VON TRAPP, MARY"))
#' result <- splitNames(df)
#'
#' # Suffix before comma
#' df <- data.frame(Name = c("SMITH JR, JOHN A", "DOE III, JANE BETH"))
#' result <- splitNames(df)
#'
#' # Edge case with suffix in second position
#' df <- data.frame(Name = c("JOHN JR A SMITH"))
#' result <- splitNames(df)
#'
#' # Custom column name
#' df <- data.frame(FullName = c("DOE, JANE"))
#' result <- splitNames(df, name_column = "FullName")
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
splitNames <- function(df, name_column, last_name_prefixes) {
  # Define prefixes that should stay with last name (case insensitive)
  last_name_prefixes <- c(
    "VAN",
    "VON",
    "MC",
    "MAC",
    "ST",
    "DE",
    "DEL",
    "LA",
    "LE",
    "DI",
    "DA",
    "O'"
  )
  # Step 1: mutate() block 1
  df1 <- mutate(
    df,
    # Clean the name column - remove extra spaces
    name_clean = str_squish(!!sym(name_column)),

    # Check if name contains comma (comma-separated format)
    has_comma = str_detect(name_clean, ","),

    # For comma-separated names, split on comma
    comma_last_raw = if_else(
      has_comma,
      str_trim(str_extract(name_clean, "^[^,]+")),
      ""
    ),
    comma_first_middle = if_else(
      has_comma,
      str_trim(str_extract(name_clean, "(?<=,).*")),
      ""
    ),

    # Check for suffix in the last name portion (before comma)
    suffix_from_lastname = case_when(
      has_comma & str_detect(comma_last_raw, "\\([^)]+\\)\\s*$") ~
        str_extract(comma_last_raw, "\\([^)]+\\)"),
      has_comma &
        str_detect(
          comma_last_raw,
          regex("\\b(JR|SR|III|II|IV|V|I)\\s*$", ignore_case = TRUE)
        ) ~
        str_extract(
          comma_last_raw,
          regex("\\b(JR|SR|III|II|IV|V|I)\\s*$", ignore_case = TRUE)
        ),
      TRUE ~ ""
    ),

    # Remove suffix from last name portion if found
    comma_last_clean = case_when(
      has_comma &
        suffix_from_lastname != "" &
        str_detect(suffix_from_lastname, "\\(") ~
        str_replace(comma_last_raw, "\\s*\\([^)]+\\)\\s*$", ""),
      has_comma & suffix_from_lastname != "" ~
        str_replace(
          comma_last_raw,
          regex(
            paste0("\\s*\\b", suffix_from_lastname, "\\s*$"),
            ignore_case = TRUE
          ),
          ""
        ),
      has_comma ~ comma_last_raw,
      TRUE ~ ""
    ),
    comma_last_clean = if_else(has_comma, str_trim(comma_last_clean), ""),

    # For processing, use either the comma-split or original format
    name_to_process = if_else(has_comma, comma_first_middle, name_clean),
    predetermined_last = comma_last_clean
  )

  # Step 2: mutate() block 2
  df2 <- mutate(
    df1,
    # Extract suffix from the first/middle portion
    suffix_from_firstmiddle = case_when(
      str_detect(name_to_process, "\\([^)]+\\)") ~
        str_extract(name_to_process, "\\([^)]+\\)"),
      str_detect(
        name_to_process,
        regex("\\b(JR|SR|III|II|IV|V|I)\\b", ignore_case = TRUE)
      ) ~
        str_extract(
          name_to_process,
          regex("\\b(JR|SR|III|II|IV|V|I)\\b", ignore_case = TRUE)
        ),
      TRUE ~ ""
    ),

    # Combine suffixes
    suffix = case_when(
      has_comma & suffix_from_lastname != "" ~ suffix_from_lastname,
      suffix_from_firstmiddle != "" ~ suffix_from_firstmiddle,
      TRUE ~ ""
    ),

    # Remove suffix from name_to_process
    name_no_suffix = case_when(
      suffix_from_firstmiddle != "" &
        str_detect(suffix_from_firstmiddle, "\\(") ~
        str_replace(name_to_process, "\\s*\\([^)]+\\)", ""),
      suffix_from_firstmiddle != "" ~
        str_replace(
          name_to_process,
          regex(
            paste0("\\s*\\b", suffix_from_firstmiddle, "\\b\\s*"),
            ignore_case = TRUE
          ),
          " "
        ),
      TRUE ~ name_to_process
    ),
    name_no_suffix = str_trim(str_squish(name_no_suffix))
  )

  # Step 3: separate() name parts
  df3 <- separate(
    df2,
    name_no_suffix,
    into = c("part1", "part2", "part3", "part4", "part5"),
    sep = "\\s+",
    fill = "right",
    extra = "merge"
  )

  # Step 4: mutate() block 3
  df4 <- mutate(
    df3,

    # Create last name prefix pattern
    prefix_pattern = paste(last_name_prefixes, collapse = "|"),

    # Uppercase parts for detection
    part2_upper = str_to_upper(part2),
    part3_upper = str_to_upper(part3),

    # Determine name structure
    first_name = part1,

    # Logic for middle names
    middle_name = case_when(
      has_comma & is.na(part3) ~ part2,
      has_comma & !is.na(part3) ~ str_squish(
        paste(
          coalesce(part2, ""),
          coalesce(part3, ""),
          coalesce(part4, ""),
          coalesce(part5, ""),
          sep = " "
        )
      ),

      # Standard format logic
      !has_comma & is.na(part3) ~ "",
      !has_comma &
        !is.na(part3) &
        is.na(part4) &
        str_detect(part2_upper, paste0("^(", prefix_pattern, ")$")) ~ "",
      !has_comma & !is.na(part3) & is.na(part4) ~ part2,
      !has_comma &
        !is.na(part4) &
        is.na(part5) &
        str_detect(part2_upper, paste0("^(", prefix_pattern, ")$")) ~ "",
      !has_comma &
        !is.na(part4) &
        is.na(part5) &
        str_detect(part3_upper, paste0("^(", prefix_pattern, ")$")) ~ part2,
      !has_comma & !is.na(part4) & is.na(part5) ~ part2,
      !has_comma & !is.na(part5) ~ part2,
      TRUE ~ ""
    ),

    # Logic for last names
    last_name = case_when(
      has_comma ~ predetermined_last,

      is.na(part3) ~ part2,
      !is.na(part3) &
        is.na(part4) &
        str_detect(part2_upper, paste0("^(", prefix_pattern, ")$")) ~
        paste(part2, part3),
      !is.na(part3) & is.na(part4) ~ part3,
      !is.na(part4) &
        is.na(part5) &
        str_detect(part2_upper, paste0("^(", prefix_pattern, ")$")) ~
        paste(part2, part3, part4),
      !is.na(part4) &
        is.na(part5) &
        str_detect(part3_upper, paste0("^(", prefix_pattern, ")$")) ~
        paste(part3, part4),
      !is.na(part4) & is.na(part5) ~ paste(part3, part4),
      !is.na(part5) ~ paste(part3, part4, part5),
      TRUE ~ ""
    ),

    # Clean pieces
    across(c(first_name, middle_name, last_name), ~ str_squish(.x)),
    across(c(first_name, middle_name, last_name), ~ replace_na(.x, "")),

    # Clean suffix
    suffix = str_replace_all(suffix, "[()]", ""),
    suffix = replace_na(suffix, "")
  )

  # Step 5: Final select()
  df_final <- select(
    df4,
    -name_clean,
    -has_comma,
    -comma_last_raw,
    -comma_first_middle,
    -suffix_from_lastname,
    -comma_last_clean,
    -name_to_process,
    -predetermined_last,
    -suffix_from_firstmiddle,
    -part1,
    -part2,
    -part3,
    -part4,
    -part5,
    -prefix_pattern,
    -part2_upper,
    -part3_upper
  )
  df_final
}


#' Remove Nicknames from Name Strings
#'
#' This function removes quoted nicknames from name strings before name processing.
#' It handles nicknames that appear in double quotes anywhere in the name string.
#'
#' @param df A dataframe containing a column with full names that may include nicknames
#' @param name_column Character string specifying the name of the column containing
#'   the full names. Default is "Name".
#'
#' @return A dataframe with the nickname-cleaned names, ready for further processing
#'
#' @examples
#' # Basic usage
#' df <- data.frame(Name = c("SMITH WILSON, JOHNATHAN \"JOHN\"",
#'                          "KEPPLER, WILLIAM HENRY \"BILL\"",
#'                          "NORMAL NAME WITHOUT NICKNAME"))
#' result <- removeNicknames(df)
#'
#' # Then process with splitNames
#' final_result <- splitNames(result)
#'
#' @import dplyr
#' @import stringr
#' @export
removeNicknames <- function(df, name_column = "Name") {
  df %>%
    mutate(
      # Remove quoted nicknames and clean up extra whitespace
      !!sym(name_column) := str_replace_all(
        !!sym(name_column),
        "\\s*\"[^\"]*\"\\s*",
        " "
      ),
      # Clean up any double spaces or leading/trailing whitespace
      !!sym(name_column) := str_trim(str_squish(!!sym(name_column)))
    )
}

#' Remove Hyphens, Spaces, and Apostrophes from Compound Last Names
#'
#' This function removes hyphens, spaces, and apostrophes from compound last names to create
#' a single continuous last name string. Useful for standardizing names like
#' "Van Der Berg" to "VanDerBerg", "Smith-Jones" to "SmithJones", or "O'Connor" to "OConnor".
#'
#' @param df A dataframe containing a last name column
#' @param last_name_column Character string specifying the name of the column
#'   containing the last names. Default is "last_name".
#'
#' @return A dataframe with the compound last name separators removed
#'
#' @examples
#' # After using splitNames function
#' df <- data.frame(
#'   first_name = c("JOHN", "MARY", "ROBERT"),
#'   last_name = c("VAN DER BERG", "SMITH-JONES", "O'CONNOR"),
#'   middle_name = c("", "ELIZABETH", ""),
#'   suffix = c("", "", "JR")
#' )
#' result <- removeCompoundSeparators(df)
#'
#' # Custom column name
#' df <- data.frame(surname = c("DE LA CRUZ", "MARTINEZ-RODRIGUEZ"))
#' result <- removeCompoundSeparators(df, last_name_column = "surname")
#'
#' @import dplyr
#' @import stringr
#' @export
removeCompoundSeparators <- function(df, last_name_column = "last_name") {
  df %>%
    mutate(
      # Remove hyphens, spaces, and apostrophes from last names
      !!sym(last_name_column) := str_replace_all(
        !!sym(last_name_column),
        "[-']",
        " "
      )
    )
}

#' Create Multiple Matching Keys from Last Names and First Names
#'
#' This function creates multiple variations of last names and first names to improve matching
#' between datasets where one might have compound names and another single names, or where
#' last names have changed (e.g., due to marriage) but first names remain the same.
#'
#' @param df A dataframe with last names, first names, and date of birth
#' @param last_name_col Character string specifying the last name column. Default is "last_name"
#' @param first_name_col Character string specifying the first name column. Default is "first_name"
#' @param dob_col Character string specifying the date of birth column. Default is "date_of_birth"
#'
#' @return A dataframe with additional matching key columns
#'
#' @import dplyr
#' @import stringr
#' @export
createMatchingKeys <- function(
  df,
  last_name_col = "last_name",
  first_name_col = "first_name",
  dob_col = "date_of_birth"
) {
  df %>%
    mutate(
      # Clean names for processing
      last_name_clean = str_trim(str_squish(!!sym(last_name_col))),
      first_name_clean = str_trim(str_squish(!!sym(first_name_col))),

      # Create various matching keys
      # Key 1: Full last name + DOB
      match_key_full = paste(last_name_clean, !!sym(dob_col), sep = "|"),

      # Key 2: First word of last name + DOB (for "SMITH WEST" -> "SMITH")
      last_name_first_word = str_extract(last_name_clean, "^\\S+"),
      match_key_first_word = paste(
        last_name_first_word,
        !!sym(dob_col),
        sep = "|"
      ),

      # Key 3: Last word of last name + DOB (for "SMITH WEST" -> "WEST")
      last_name_last_word = str_extract(last_name_clean, "\\S+$"),
      match_key_last_word = paste(
        last_name_last_word,
        !!sym(dob_col),
        sep = "|"
      ),

      # Key 4: First name + DOB (for maiden name changes: "DOE, JANE" -> "JOHNSON, JANE")
      match_key_first_name = paste(first_name_clean, !!sym(dob_col), sep = "|"),

      # Key 5: First name + first word of last name + DOB
      match_key_first_plus_first_last = paste(
        first_name_clean,
        last_name_first_word,
        !!sym(dob_col),
        sep = "|"
      ),

      # Key 6: First name + last word of last name + DOB
      match_key_first_plus_last_last = paste(
        first_name_clean,
        last_name_last_word,
        !!sym(dob_col),
        sep = "|"
      ),

      # Key 7: All individual words as separate keys (for multiple matching attempts)
      last_name_words = str_split(last_name_clean, "\\s+")
    ) %>%
    # Create a row for each word in compound last names
    unnest(last_name_words) %>%
    mutate(
      match_key_individual = paste(last_name_words, !!sym(dob_col), sep = "|")
    ) %>%
    # Group back to original structure but keep all matching keys
    group_by(across(-c(last_name_words, match_key_individual))) %>%
    summarise(
      individual_keys = list(unique(match_key_individual)),
      .groups = "drop"
    )
}

#' Perform Multi-Pass Name Matching with First Name Support
#'
#' This function performs hierarchical matching between two datasets, trying
#' exact matches first, then partial matches for compound names, then first name
#' matches for cases like maiden name changes.
#'
#' @param df1 First dataframe (e.g., the one with compound names)
#' @param df2 Second dataframe (e.g., the one with single names)
#' @param last_name_col Character string specifying the last name column
#' @param first_name_col Character string specifying the first name column
#' @param dob_col Character string specifying the date of birth column
#' @param id_col Character string specifying a unique ID column (optional)
#'
#' @return A dataframe with matched records and match type indicator
#'
#' @import dplyr
#' @import stringr
#' @export
multiPassMatching <- function(
  df1,
  df2,
  last_name_col = "last_name",
  first_name_col = "first_name",
  dob_col = "date_of_birth",
  id_col = NULL
) {
  # Prepare both datasets with matching keys
  df1_prep <- df1 %>%
    mutate(
      dataset = "df1",
      last_name_clean = str_trim(str_squish(!!sym(last_name_col))),
      first_name_clean = str_trim(str_squish(!!sym(first_name_col))),
      key_full = paste(last_name_clean, !!sym(dob_col), sep = "|"),
      key_first_word = paste(
        str_extract(last_name_clean, "^\\S+"),
        !!sym(dob_col),
        sep = "|"
      ),
      key_last_word = paste(
        str_extract(last_name_clean, "\\S+$"),
        !!sym(dob_col),
        sep = "|"
      ),
      key_first_name = paste(first_name_clean, !!sym(dob_col), sep = "|")
    )

  df2_prep <- df2 %>%
    mutate(
      dataset = "df2",
      last_name_clean = str_trim(str_squish(!!sym(last_name_col))),
      first_name_clean = str_trim(str_squish(!!sym(first_name_col))),
      key_full = paste(last_name_clean, !!sym(dob_col), sep = "|"),
      key_first_word = paste(
        str_extract(last_name_clean, "^\\S+"),
        !!sym(dob_col),
        sep = "|"
      ),
      key_last_word = paste(
        str_extract(last_name_clean, "\\S+$"),
        !!sym(dob_col),
        sep = "|"
      ),
      key_first_name = paste(first_name_clean, !!sym(dob_col), sep = "|")
    )

  # Pass 1: Exact full name + DOB match
  matches_exact <- df1_prep %>%
    inner_join(df2_prep, by = c("key_full"), suffix = c("_df1", "_df2")) %>%
    mutate(match_type = "exact_full_name")

  # Get unmatched records from both datasets
  matched_keys_df1 <- matches_exact$key_full
  matched_keys_df2 <- matches_exact$key_full

  unmatched_df1 <- df1_prep %>% filter(key_full %nin% matched_keys_df1)
  unmatched_df2 <- df2_prep %>% filter(key_full %nin% matched_keys_df2)

  # Pass 2: First word of df1 compound name matches full name in df2
  matches_first_word <- unmatched_df1 %>%
    inner_join(
      unmatched_df2,
      by = c("key_first_word" = "key_full"),
      suffix = c("_df1", "_df2")
    ) %>%
    mutate(match_type = "compound_to_single_first")

  # Update unmatched
  matched_keys_df1_p2 <- matches_first_word$key_first_word
  matched_keys_df2_p2 <- matches_first_word$key_first_word

  unmatched_df1 <- unmatched_df1 %>%
    filter(key_first_word %nin% matched_keys_df1_p2)
  unmatched_df2 <- unmatched_df2 %>% filter(key_full %nin% matched_keys_df2_p2)

  # Pass 3: Last word of df1 compound name matches full name in df2
  matches_last_word <- unmatched_df1 %>%
    inner_join(
      unmatched_df2,
      by = c("key_last_word" = "key_full"),
      suffix = c("_df1", "_df2")
    ) %>%
    mutate(match_type = "compound_to_single_last")

  # Update unmatched
  matched_keys_df1_p3 <- matches_last_word$key_last_word
  matched_keys_df2_p3 <- matches_last_word$key_last_word

  unmatched_df1 <- unmatched_df1 %>%
    filter(key_last_word %nin% matched_keys_df1_p3)
  unmatched_df2 <- unmatched_df2 %>% filter(key_full %nin% matched_keys_df2_p3)

  # Pass 4: Single name in df1 matches first word of compound name in df2
  matches_single_to_first <- unmatched_df1 %>%
    inner_join(
      unmatched_df2,
      by = c("key_full" = "key_first_word"),
      suffix = c("_df1", "_df2")
    ) %>%
    mutate(match_type = "single_to_compound_first")

  # Update unmatched
  matched_keys_df1_p4 <- matches_single_to_first$key_full
  matched_keys_df2_p4 <- matches_single_to_first$key_full_df2

  unmatched_df1 <- unmatched_df1 %>% filter(key_full %nin% matched_keys_df1_p4)
  unmatched_df2 <- unmatched_df2 %>% filter(key_full %nin% matched_keys_df2_p4)

  # Pass 5: Single name in df1 matches last word of compound name in df2
  matches_single_to_last <- unmatched_df1 %>%
    inner_join(
      unmatched_df2,
      by = c("key_full" = "key_last_word"),
      suffix = c("_df1", "_df2")
    ) %>%
    mutate(match_type = "single_to_compound_last")

  # Update unmatched
  matched_keys_df1_p5 <- matches_single_to_last$key_full
  matched_keys_df2_p5 <- matches_single_to_last$key_full_df2

  unmatched_df1 <- unmatched_df1 %>% filter(key_full %nin% matched_keys_df1_p5)
  unmatched_df2 <- unmatched_df2 %>% filter(key_full %nin% matched_keys_df2_p5)

  # Pass 6: First name + DOB match (for maiden name changes like "DOE, JANE" -> "JOHNSON, JANE")
  matches_first_name <- unmatched_df1 %>%
    inner_join(
      unmatched_df2,
      by = c("key_first_name"),
      suffix = c("_df1", "_df2")
    ) %>%
    mutate(match_type = "first_name_dob_match")

  # Combine all matches
  all_matches <- bind_rows(
    matches_exact,
    matches_first_word,
    matches_last_word,
    matches_single_to_first,
    matches_single_to_last,
    matches_first_name
  )

  return(all_matches)
}
