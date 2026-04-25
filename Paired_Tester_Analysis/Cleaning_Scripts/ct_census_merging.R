# ct_census_merging.R
# Merge ACS-derived census variables from C&T replication data (HUDprocessed_JPE_census_042021.rds)

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
    summarise(across(all_of(value_cols), first_non_na), .groups = "drop")

  return(collapsed)
}

#' Merge ACS-derived C&T census variables onto property data
#'
#' @param data Data frame containing at least CONTROL, TESTERID, SEQRH
#' @param ct_path Path to HUDprocessed_JPE_census_042021.rds
#' @return Input data with appended C&T census variables (Ad/Rec)
merge_ct_census_vars <- function(
  data,
  ct_path = "Data/CT2022_Replication_Data/HUDprocessed_JPE_census_042021.rds"
) {
  required_cols <- c("CONTROL", "TESTERID", "SEQRH")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Input data missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  cat("Loading C&T census data...\n")
  ct <- readRDS(ct_path)

  key_cols <- c("CONTROL", "TESTERID", "SEQRH")
  value_cols <- c(
    # ACS neighborhood characteristics
    "povrate_Ad", "povrate_Rec",
    "skill_Ad", "skill_Rec",
    "college_Ad", "college_Rec",
    "singlefamily_Ad", "singlefamily_Rec",
    "ownerocc_Ad", "ownerocc_Rec",
    "w2012pc_Ad", "w2012pc_Rec",
    "b2012pc_Ad", "b2012pc_Rec",
    "a2012pc_Ad", "a2012pc_Rec",
    "hisp2012pc_Ad", "hisp2012pc_Rec",
    "WhiteLI_Ad", "WhiteMI_Ad", "WhiteHI_Ad",
    "WhiteLI_Rec", "WhiteMI_Rec", "WhiteHI_Rec",
    "nodad_Ad", "nodad_Rec",
    "povnodad_Ad", "povnodad_Rec",
    "medincome_Ad", "medincome_Rec"
  )

  value_cols <- intersect(value_cols, names(ct))
  if (length(value_cols) == 0) {
    stop("No expected C&T census variables found in replication dataset.")
  }

  ct_collapsed <- collapse_ct_values(ct, key_cols, value_cols, "C&T census")

  merged <- data %>%
    left_join(ct_collapsed, by = key_cols)

  if ("povrate_Rec" %in% names(merged)) {
    rec_match <- sum(!is.na(merged$povrate_Rec))
    cat(sprintf("C&T census (povrate_Rec) matched: %d / %d (%.1f%%)\n",
                rec_match, nrow(merged), 100 * rec_match / nrow(merged)))
  }

  return(merged)
}
