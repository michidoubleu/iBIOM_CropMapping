##### F2: Yield ordering check function
##### Verifies that for each pixel × crop, adjusted yields follow
##### the monotonic ordering: YLD(M5) >= YLD(M4) >= ... >= YLD(M1)

#### Author: Michael Wögerer
#### Date:   June 2025

#' @param alloc_df  A data.frame / tibble with columns:
#'   CELLCODE, lu.to, LUM.class, YLD
#' @return  Invisibly returns the wide-format rows that violate the ordering.

test_yield_ordering <- function(alloc_df) {

  # Only test actual crop rows (not OthAgr) with non-NA yields
  test.data <- alloc_df %>% na.omit() %>% distinct() %>%
    filter(lu.to != "OthAgr", !is.na(YLD)) %>%
    dplyr::select(CELLCODE, lu.to, LUM.class, YLD)

  # Expected LUM classes in decreasing order of intensity
  lum_cols <- c("M6.rf","M5.rf", "M4.rf", "M3.rf", "M2.rf", "M1.rf")

  # Keep only the relevant LUM classes
  test.data <- test.data %>%
    filter(LUM.class %in% lum_cols)

  duplicates <- test.data %>%
    count(CELLCODE, lu.to, LUM.class) %>%
    filter(n > 1)

  nrow(duplicates)

  duplicates

  # Pivot to wide format
  wide_df <- test.data %>%
    pivot_wider(
      id_cols = c(CELLCODE, lu.to),
      names_from = LUM.class,
      values_from = YLD
    )

  # Ensure all columns are present (fill with NA if completely missing from dataset)
  for (col in lum_cols) {
    if (!col %in% colnames(wide_df)) {
      wide_df[[col]] <- NA_real_
    }
  }

  # Reorder columns to check from M5.rf down to M1.rf
  wide_df <- wide_df %>%
    dplyr::select(CELLCODE, lu.to, all_of(lum_cols))

  # Extract yield matrix for row-wise check
  yield_matrix <- as.matrix(wide_df[, lum_cols])

  # A row is valid if the non-NA yields are decreasing (i.e. diff <= 0 in the order M5 -> M1)
  is_valid <- apply(yield_matrix, 1, function(v){
    v <- as.numeric(v)
    v_clean <- na.omit(v)
    if(length(v_clean) <= 1)
      return(TRUE)
    all(diff(v_clean) <= 0)
  })

  violations <- wide_df[!is_valid, ]

  n_violations <- nrow(violations)

  if (n_violations == 0) {
    message("  ✔ Yield ordering test PASSED: YLD(M5) >= YLD(M4) >= ... >= YLD(M1) holds for all pixels.")
  } else {
    warning(sprintf("  ✘ Yield ordering test: %d violation(s) detected where a higher LUM class has a LOWER yield.", n_violations))
    message("  Violations (up to 10 rows):")
    print(as.data.frame(head(violations, 10)))
  }

  return(invisible(violations))
}
