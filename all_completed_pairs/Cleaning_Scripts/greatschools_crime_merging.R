# greatschools_crime_merging.R
# Merge GreatSchools index and crime rate data from C&T replication files (recommended homes only)

library(dplyr)

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

collapse_ct_values <- function(df, key_cols, value_cols, label) {
  missing_cols <- setdiff(c(key_cols, value_cols), names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s missing columns: %s", label, paste(missing_cols, collapse = ", ")))
  }

  distinct_rows <- df %>%
    select(all_of(c(key_cols, value_cols))) %>%
    distinct()

  dup_counts <- distinct_rows %>%
    count(across(all_of(key_cols))) %>%
    filter(n > 1)

  if (nrow(dup_counts) > 0) {
    cat(sprintf("%s: %d keys have multiple distinct value rows; using first non-missing values.\n",
                label, nrow(dup_counts)))
  }

  collapsed <- distinct_rows %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(
      across(all_of(value_cols), first_non_na),
      .groups = "drop"
    )

  return(collapsed)
}

#' Merge GreatSchools and crime rate data from C&T replication files
#'
#' @param data Data frame containing at least CONTROL, TESTERID, SEQRH
#' @param recs_path Path to C&T recommended-home file (recsprocessed_JPE.rds)
#' @return Input data with appended columns: Elementary_School_Score_Rec, Assault_Rec
merge_ct_greatschools_crime <- function(
  data,
  recs_path = "Data/CT2022_Replication_Data/recsprocessed_JPE.rds"
) {
  required_cols <- c("CONTROL", "TESTERID", "SEQRH")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Input data missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  cat("Loading C&T GreatSchools/crime data...\n")
  ct_recs <- readRDS(recs_path)

  if (!"Elementary_School_Score_Rec" %in% names(ct_recs) && "Elementary_School_Score" %in% names(ct_recs)) {
    ct_recs <- ct_recs %>%
      mutate(Elementary_School_Score_Rec = Elementary_School_Score)
  }
  if (!"Assault_Rec" %in% names(ct_recs) && "Assault" %in% names(ct_recs)) {
    ct_recs <- ct_recs %>%
      mutate(Assault_Rec = Assault)
  }

  key_cols <- c("CONTROL", "TESTERID", "SEQRH")
  rec_value_cols <- c("Elementary_School_Score_Rec", "Assault_Rec")

  ct_recs_collapsed <- collapse_ct_values(ct_recs, key_cols, rec_value_cols, "C&T recs")

  merged <- data %>%
    left_join(ct_recs_collapsed, by = key_cols)

  any_match <- sum(!is.na(merged$Elementary_School_Score_Rec) | !is.na(merged$Assault_Rec))
  cat(sprintf("Rows with any GreatSchools/crime match: %d / %d (%.1f%%)\n",
              any_match, nrow(merged), 100 * any_match / nrow(merged)))

  return(merged)
}
