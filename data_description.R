library(dplyr)
library(haven)
library(stringr)

# This file assumes it is run from the repository root. For example:
#   cd HUDreplication
#   Rscript data_description.R
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(repo_root, "Pooled_Analysis", "Output")

format_int <- function(x) {
  format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
}

format_pct <- function(x, digits = 1) {
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "\\%")
}

format_num <- function(x, digits = 3) {
  format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

write_city_fe_geometry_tex <- function(data, output_path) {
  dataset_labels <- c(
    adsprocessed = "Advertised-home file (Table 5)",
    census = "Census",
    testscores = "Test Scores",
    names = "Names"
  )

  table_data <- data %>%
    mutate(dataset_label = unname(dataset_labels[dataset]))

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{City fixed-effect geometry before and after city-name cleaning}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrrrrrrrrrr}",
    "\\toprule",
    " & & \\multicolumn{2}{c}{White Unique FEs} & \\multicolumn{2}{c}{Minority Unique FEs} & \\multicolumn{2}{c}{Mixed FE Groups} & \\multicolumn{2}{c}{White-only FE Groups} & \\multicolumn{2}{c}{Minority-only FE Groups} & \\multicolumn{2}{c}{Rows in Mixed FE Groups}\\\\",
    "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}\\cmidrule(lr){11-12}\\cmidrule(lr){13-14}",
    "Dataset & Rows & Before & After & Before & After & Before & After & Before & After & Before & After & Before & After\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_data))) {
    row <- table_data[i, ]
    lines <- c(
      lines,
      paste(
        row$dataset_label,
        format_int(row$rows),
        format_int(row$white_unique_fes_before),
        format_int(row$white_unique_fes_after),
        format_int(row$minority_unique_fes_before),
        format_int(row$minority_unique_fes_after),
        format_int(row$mixed_groups_before),
        format_int(row$mixed_groups_after),
        format_int(row$white_only_groups_before),
        format_int(row$white_only_groups_after),
        format_int(row$minority_only_groups_before),
        format_int(row$minority_only_groups_after),
        format_int(row$rows_in_mixed_groups_before),
        format_int(row$rows_in_mixed_groups_after),
        sep = " & "
      ) |> paste0("\\\\")
    )
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{table}"
  )

  writeLines(lines, output_path)
}

write_city_mixed_cell_tex <- function(data, output_path) {
  cell_labels <- c(
    mixed_before_only = "Mixed before cleaning only",
    mixed_before_plus_newly_attached = "Mixed before cleaning plus newly attached raw labels",
    purely_newly_mixed_after_cleaning = "Purely newly mixed after cleaning"
  )

  table_data <- data %>%
    mutate(cell_label = unname(cell_labels[cell_type]))

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Characteristics of mixed city cells in the Census file after city-name cleaning}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrrrrrrrr}",
    "\\toprule",
    "Cell type & Cells & Rows & Median rows / cell & Median raw labels / cell & Median block groups / cell & Share single block group & Median places / cell & Share single place & Share single county & Weighted white-share gap & Weighted income gap\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_data))) {
    row <- table_data[i, ]
    lines <- c(
      lines,
      paste(
        row$cell_label,
        format_int(row$cells),
        format_int(row$rows),
        format_int(row$median_rows_per_cell),
        format_int(row$median_raw_labels_per_cell),
        format_int(row$median_blockgroups_per_cell),
        format_pct(row$share_single_blockgroup),
        format_int(row$median_places_per_cell),
        format_pct(row$share_single_place),
        format_pct(row$share_single_county),
        format_num(row$weighted_gap_whitehi_rec, 3),
        format_int(row$weighted_gap_medincome_rec),
        sep = " & "
      ) |> paste0("\\\\")
    )
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{table}"
  )

  writeLines(lines, output_path)
}

if (basename(repo_root) != "HUDreplication") {
  stop("Run this script from the HUDreplication repository root")
}

# ---- Input files --------------------------------------------------------------

adsprocessed_path <- file.path(repo_root, "Data", "CT2022_Replication_Data", "adsprocessed_JPE_censor.rds")
recsprocessed_path <- file.path(repo_root, "Data", "CT2022_Replication_Data", "recsprocessed_JPE.rds")
hudprocessed_names_path <- file.path(repo_root, "Data", "CT2022_Replication_Data", "HUDprocessed_JPE_names_042021.rds")
hudprocessed_census_path <- file.path(repo_root, "Data", "CT2022_Replication_Data", "HUDprocessed_JPE_census_042021.rds")
hudprocessed_testscores_path <- file.path(repo_root, "Data", "CT2022_Replication_Data", "HUDprocessed_JPE_testscores_042021.rds")
hudprocessed_names_duplicates_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_names_correct_cities_with_duplicates.csv")
hudprocessed_census_duplicates_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_census_correct_cities_with_duplicates.csv")
hudprocessed_testscores_duplicates_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_testscores_correct_cities_with_duplicates.csv")
hudprocessed_names_processed_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_names_correct_cities_processed.csv")
hudprocessed_census_processed_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_census_correct_cities_processed.csv")
hudprocessed_testscores_processed_path <- file.path(repo_root, "Data", "Generated", "Pooled_Analysis", "HUDprocessed_testscores_correct_cities_processed.csv")
rechomes_path <- file.path(repo_root, "Data", "HDS2012_Raw_Data", "rechomes.sas7bdat")
sales_path <- file.path(repo_root, "Data", "HDS2012_Raw_Data", "sales.sas7bdat")
taf_path <- file.path(repo_root, "Data", "HDS2012_Raw_Data", "taf.sas7bdat")
tester_path <- file.path(repo_root, "Data", "HDS2012_Raw_Data", "tester_censored.sas7bdat")


# ---- Small text-normalization helpers ----------------------------------------

normalize_text <- function(x) {
  x <- coalesce(as.character(x), "")
  x <- str_to_upper(str_trim(x))
  str_replace_all(x, "[^A-Z0-9]", "")
}

normalize_zip <- function(x) {
  x <- coalesce(as.character(x), "")
  x <- str_replace_all(x, "[^0-9]", "")
  x <- ifelse(x == "", "", str_pad(x, width = 5, side = "left", pad = "0"))
  ifelse(x == "", "", str_sub(x, 1, 5))
}

normalize_price <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.na(out), NA_real_, round(out, 2))
}

normalize_ascii <- function(x) {
  x <- coalesce(as.character(x), "")
  iconv(x, to = "ASCII//TRANSLIT", sub = "")
}

normalize_city_match <- function(x) {
  x <- normalize_ascii(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  str_squish(x)
}

normalize_state_match <- function(x) {
  x <- normalize_ascii(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z]", "")
  str_squish(x)
}

normalize_street_match <- function(x) {
  x <- normalize_ascii(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, "&", " AND ")
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- str_replace_all(x, "\\bNORTH\\b", "N")
  x <- str_replace_all(x, "\\bSOUTH\\b", "S")
  x <- str_replace_all(x, "\\bEAST\\b", "E")
  x <- str_replace_all(x, "\\bWEST\\b", "W")
  x <- str_replace_all(x, "\\bNORTHEAST\\b", "NE")
  x <- str_replace_all(x, "\\bNORTHWEST\\b", "NW")
  x <- str_replace_all(x, "\\bSOUTHEAST\\b", "SE")
  x <- str_replace_all(x, "\\bSOUTHWEST\\b", "SW")
  x <- str_replace_all(x, "\\bSTREET\\b", "ST")
  x <- str_replace_all(x, "\\bAVENUE\\b", "AVE")
  x <- str_replace_all(x, "\\bBOULEVARD\\b", "BLVD")
  x <- str_replace_all(x, "\\bDRIVE\\b", "DR")
  x <- str_replace_all(x, "\\bROAD\\b", "RD")
  x <- str_replace_all(x, "\\bLANE\\b", "LN")
  x <- str_replace_all(x, "\\bCOURT\\b", "CT")
  x <- str_replace_all(x, "\\bCIRCLE\\b", "CIR")
  x <- str_replace_all(x, "\\bPARKWAY\\b", "PKWY")
  x <- str_replace_all(x, "\\bTERRACE\\b", "TER")
  x <- str_replace_all(x, "\\bPLACE\\b", "PL")
  x <- str_replace_all(x, "\\bTRAIL\\b", "TRL")
  x <- str_replace_all(x, "\\bHIGHWAY\\b", "HWY")
  x <- str_replace_all(x, "\\bMOUNT\\b", "MT")
  str_squish(x)
}

extract_house_number <- function(x) {
  x <- normalize_street_match(x)
  out <- str_extract(x, "\\b\\d+[A-Z]?\\b")
  out[is.na(out)] <- ""
  out
}

strip_house_number <- function(x) {
  x <- normalize_street_match(x)
  x <- str_replace(x, "^\\d+[A-Z]?\\s+", "")
  str_squish(x)
}

lev_similarity <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)
  a[is.na(a)] <- ""
  b[is.na(b)] <- ""
  out <- numeric(length(a))
  for (i in seq_along(a)) {
    ai <- a[[i]]
    bi <- b[[i]]
    if (ai == "" || bi == "") {
      out[[i]] <- NA_real_
    } else if (ai == bi) {
      out[[i]] <- 1
    } else {
      out[[i]] <- 1 - (utils::adist(ai, bi)[1] / max(nchar(ai), nchar(bi)))
    }
  }
  out
}

empty_to_na <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

pair_indicator_breakdown <- function(df, hadhome_col, available_col, label) {
  summary_table <- df %>%
    mutate(
      hadhome_flag = .data[[hadhome_col]],
      available_flag = .data[[available_col]],
      bucket = case_when(
        hadhome_flag & available_flag ~ "Both HADHOME == 1 and ad available",
        hadhome_flag & !available_flag ~ "HADHOME == 1 only",
        !hadhome_flag & available_flag ~ "Ad available only",
        TRUE ~ "Neither"
      )
    ) %>%
    count(bucket, name = "n_pairs") %>%
    mutate(share = round(n_pairs / sum(n_pairs), 4))

  cat(label, "\n", sep = "")
  print(summary_table)
  cat("\n")
}

summarize_hud_city_matches <- function(duplicates_path, processed_path, label) {
  duplicates_data <- read.csv(duplicates_path, stringsAsFactors = FALSE)
  processed_data <- read.csv(processed_path, stringsAsFactors = FALSE)

  both_hcityx_rec <- !is.na(duplicates_data$HCITY.x) & !is.na(duplicates_data$HCITY_Rec)
  both_hcityx_ad <- !is.na(processed_data$HCITY.x) & !is.na(processed_data$HCITY_Ad)

  cat(label, ":\n", sep = "")
  cat("Rows in with-duplicates file:", format(nrow(duplicates_data), big.mark = ","), "\n")
  cat(
    "HCITY.x == HCITY_Rec in with-duplicates file:",
    format(sum(duplicates_data$HCITY.x[both_hcityx_rec] == duplicates_data$HCITY_Rec[both_hcityx_rec]), big.mark = ","),
    "/",
    format(sum(both_hcityx_rec), big.mark = ","),
    "\n"
  )
  cat("Rows in processed file:", format(nrow(processed_data), big.mark = ","), "\n")
  cat(
    "HCITY.x == HCITY_Ad (normalized) in processed file:",
    format(sum(normalize_text(processed_data$HCITY.x[both_hcityx_ad]) == normalize_text(processed_data$HCITY_Ad[both_hcityx_ad])), big.mark = ","),
    "/",
    format(sum(both_hcityx_ad), big.mark = ","),
    "\n\n"
  )
}


# ---- Read and standardize the source files -----------------------------------

adsprocessed <- readRDS(adsprocessed_path) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID),
    HSITEAD = as.character(HSITEAD),
    HCITY = as.character(HCITY),
    HSTATE = as.character(HSTATE),
    HZIP = as.character(HZIP),
    HPRICE = normalize_price(HPRICE)
  ) %>%
  mutate(
    addr_norm = normalize_text(HSITEAD),
    city_norm = normalize_text(HCITY),
    state_norm = normalize_text(HSTATE),
    zip_norm = normalize_zip(HZIP)
  )

rechomes <- read_sas(rechomes_path) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID),
    HSITEAD = as.character(HSITEAD),
    HCITY = as.character(HCITY),
    HSTATE = as.character(HSTATE),
    HZIP = as.character(HZIP),
    HPRICE = normalize_price(HPRICE),
    HADHOME = suppressWarnings(as.numeric(HADHOME)),
    DELREC = suppressWarnings(as.numeric(DELREC))
  ) %>%
  mutate(
    addr_norm = normalize_text(HSITEAD),
    city_norm = normalize_text(HCITY),
    state_norm = normalize_text(HSTATE),
    zip_norm = normalize_zip(HZIP)
  )

sales_pairs <- read_sas(sales_path, col_select = c("CONTROL", "TESTERID", "SAVLBAD")) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID),
    ad_available = as.character(SAVLBAD) == "1"
  ) %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(ad_available = any(ad_available, na.rm = TRUE), .groups = "drop")

official_pass_controls <- read_sas(taf_path, col_select = c("CONTROL", "RELEASE", "FPASS")) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    RELEASE = as.character(RELEASE),
    FPASS = suppressWarnings(as.numeric(FPASS))
  ) %>%
  filter(str_detect(CONTROL, "-S[A-Z]-"), RELEASE == "1", FPASS == 1) %>%
  distinct(CONTROL)

tester_raw <- read_sas(tester_path, col_select = c("TesterID", "APRACE", "TNATORIG", "THISPUBG", "TASIANG")) %>%
  transmute(
    TESTERID = as.character(TesterID),
    APRACE_raw = suppressWarnings(as.numeric(APRACE)),
    TNATORIG = suppressWarnings(as.numeric(TNATORIG)),
    THISPUBG = suppressWarnings(as.numeric(THISPUBG)),
    TASIANG = suppressWarnings(as.numeric(TASIANG))
  ) %>%
  distinct(TESTERID, .keep_all = TRUE)


# ---- Build simple matching keys ----------------------------------------------

adsprocessed <- adsprocessed %>%
  mutate(
    pair_key = paste(CONTROL, TESTERID, sep = "||"),
    addr_key = paste(CONTROL, TESTERID, addr_norm, sep = "||"),
    full_key = paste(
      CONTROL, TESTERID, addr_norm, city_norm, state_norm, zip_norm,
      ifelse(is.na(HPRICE), "NA", format(HPRICE, trim = TRUE, scientific = FALSE)),
      sep = "||"
    )
  )

rechomes <- rechomes %>%
  mutate(
    pair_key = paste(CONTROL, TESTERID, sep = "||"),
    addr_key = paste(CONTROL, TESTERID, addr_norm, sep = "||"),
    full_key = paste(
      CONTROL, TESTERID, addr_norm, city_norm, state_norm, zip_norm,
      ifelse(is.na(HPRICE), "NA", format(HPRICE, trim = TRUE, scientific = FALSE)),
      sep = "||"
    )
  )


# ---- Collapse raw rechomes matches to the only facts we need ------------------

raw_by_full_key <- rechomes %>%
  group_by(full_key) %>%
  summarise(
    exact_rows = n(),
    exact_any_hadhome1 = any(HADHOME == 1, na.rm = TRUE),
    exact_any_hadhome0 = any(HADHOME == 0, na.rm = TRUE),
    .groups = "drop"
  )

raw_by_addr_key <- rechomes %>%
  group_by(addr_key) %>%
  summarise(
    addr_rows = n(),
    addr_any_hadhome1 = any(HADHOME == 1, na.rm = TRUE),
    addr_any_hadhome0 = any(HADHOME == 0, na.rm = TRUE),
    .groups = "drop"
  )


# ---- Classify each adsprocessed row ------------------------------------------

adsprocessed_audit <- adsprocessed %>%
  left_join(raw_by_full_key, by = "full_key") %>%
  left_join(raw_by_addr_key, by = "addr_key") %>%
  mutate(
    exact_match = !is.na(exact_rows),
    addr_match = !is.na(addr_rows),
    hadhome1_match_type = case_when(
      exact_match & exact_any_hadhome1 ~ "Exact match to rechomes with HADHOME == 1",
      !exact_match & addr_match & addr_any_hadhome1 ~ "Address-only match to rechomes with HADHOME == 1",
      exact_match & !exact_any_hadhome1 ~ "Exact match, but no HADHOME == 1 row",
      !exact_match & addr_match & !addr_any_hadhome1 ~ "Address-only match, but no HADHOME == 1 row",
      TRUE ~ "No rechomes match"
    )
  )


# ---- Print a compact, readable summary ---------------------------------------

summary_counts <- adsprocessed_audit %>%
  count(hadhome1_match_type, name = "n_rows") %>%
  mutate(share = round(n_rows / sum(n_rows), 4)) %>%
  arrange(desc(n_rows))

total_count <- nrow(adsprocessed_audit)
hadhome1_count <- sum(
  adsprocessed_audit$hadhome1_match_type %in% c(
    "Exact match to rechomes with HADHOME == 1",
    "Address-only match to rechomes with HADHOME == 1"
  )
)

cat("\n")
cat("adsprocessed / rechomes HADHOME provenance check\n")
cat("===============================================\n\n")

cat("Total rows in adsprocessed:", format(total_count, big.mark = ","), "\n")
cat("Rows that correspond to rechomes rows with HADHOME == 1:", format(hadhome1_count, big.mark = ","), "\n")
cat("Share of adsprocessed rows that correspond to HADHOME == 1:", round(hadhome1_count / total_count, 4), "\n")
print(summary_counts)


# ---- Reverse direction: raw HADHOME == 1 rows missing from adsprocessed ------

raw_hadhome_rows <- rechomes %>%
  filter(HADHOME == 1) %>%
  distinct(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP, HPRICE, DELREC, addr_key, full_key)

hadhome_pairs <- raw_hadhome_rows %>%
  distinct(CONTROL, TESTERID)

ct_pairs <- adsprocessed %>%
  distinct(CONTROL, TESTERID)

sales_pair_status <- sales_pairs %>%
  left_join(hadhome_pairs %>% mutate(has_hadhome = TRUE), by = c("CONTROL", "TESTERID")) %>%
  mutate(has_hadhome = ifelse(is.na(has_hadhome), FALSE, has_hadhome))

adsprocessed_keys <- adsprocessed %>%
  distinct(addr_key, full_key)

raw_hadhome_missing <- raw_hadhome_rows %>%
  anti_join(adsprocessed_keys, by = "full_key") %>%
  anti_join(adsprocessed_keys, by = "addr_key")

cat("\n\nRaw rechomes HADHOME == 1 rows missing from adsprocessed:\n")
cat("=========================================================\n\n")
cat("Unique raw HADHOME == 1 rows:", format(nrow(raw_hadhome_rows), big.mark = ","), "\n")
cat("Missing from adsprocessed:", format(nrow(raw_hadhome_missing), big.mark = ","), "\n")
cat("Share missing:", round(nrow(raw_hadhome_missing) / nrow(raw_hadhome_rows), 4), "\n\n")

print(
  as.data.frame(
    raw_hadhome_missing %>%
      select(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP, HPRICE, DELREC)
  ),
  row.names = FALSE
)


# ---- Relationship between HADHOME == 1 and ad availability -------------------

cat("\n\nRelationship between HADHOME == 1 and reported ad availability:\n")
cat("=========================================================\n\n")

pair_indicator_breakdown(
  sales_pair_status,
  hadhome_col = "has_hadhome",
  available_col = "ad_available",
  label = "All raw sales tester-trial pairs:"
)

pair_indicator_breakdown(
  sales_pair_status %>% semi_join(official_pass_controls, by = "CONTROL"),
  hadhome_col = "has_hadhome",
  available_col = "ad_available",
  label = "Official passed sales tester-trial pairs (RELEASE == 1, FPASS == 1):"
)


# ---- How many testers from official passed trials appear in adsprocessed? ----

official_sales_pairs <- sales_pairs %>%
  semi_join(official_pass_controls, by = "CONTROL")

official_pair_summary <- official_sales_pairs %>%
  left_join(ct_pairs %>% mutate(in_ct = 1), by = c("CONTROL", "TESTERID")) %>%
  mutate(in_ct = ifelse(is.na(in_ct), 0, in_ct))

official_complete_trial_summary <- official_pair_summary %>%
  count(CONTROL, wt = in_ct, name = "n_testers_in_ct") %>%
  inner_join(
    official_sales_pairs %>% count(CONTROL, name = "n_testers_in_trial") %>% filter(n_testers_in_trial == 2),
    by = "CONTROL"
  ) %>%
  count(n_testers_in_ct, name = "n_complete_passed_trials")

cat("\n\nCoverage of adsprocessed in the official passed sales-trial universe:\n")
cat("=========================================================\n\n")
cat("Unique tester-trial rows in official passed sales universe:", format(nrow(official_sales_pairs), big.mark = ","), "\n")
cat("Unique tester-trial rows in adsprocessed from that universe:", format(sum(official_pair_summary$in_ct), big.mark = ","), "\n")
cat("Share of tester-trial rows in adsprocessed:", round(sum(official_pair_summary$in_ct) / nrow(official_sales_pairs), 4), "\n\n")

cat("Official passed sales trials by number of testers appearing in adsprocessed:\n\n")
print(official_complete_trial_summary)


# ---- Check the HUDprocessed files used in later C&T analyses -----------------

hudprocessed_files <- tibble(
  file_label = c("HUDprocessed names", "HUDprocessed census", "HUDprocessed testscores"),
  file_path = c(hudprocessed_names_path, hudprocessed_census_path, hudprocessed_testscores_path)
)

cat("\n\nCoverage of HUDprocessed files in raw rechomes HADHOME == 1 pairs:\n")
cat("=========================================================\n\n")

for (i in seq_len(nrow(hudprocessed_files))) {
  hudprocessed_pairs <- readRDS(hudprocessed_files$file_path[i]) %>%
    transmute(
      CONTROL = as.character(CONTROL),
      TESTERID = as.character(TESTERID)
    ) %>%
    distinct()

  hudprocessed_hadhome_pairs <- hudprocessed_pairs %>%
    semi_join(hadhome_pairs, by = c("CONTROL", "TESTERID"))

  hudprocessed_official_pairs <- hudprocessed_pairs %>%
    semi_join(official_pass_controls, by = "CONTROL")

  hudprocessed_official_trial_summary <- official_sales_pairs %>%
    left_join(hudprocessed_official_pairs %>% mutate(in_file = 1), by = c("CONTROL", "TESTERID")) %>%
    mutate(in_file = ifelse(is.na(in_file), 0, in_file)) %>%
    count(CONTROL, wt = in_file, name = "n_testers_in_file") %>%
    count(n_testers_in_file, name = "n_complete_passed_trials")

  cat(hudprocessed_files$file_label[i], ":\n", sep = "")
  cat("Unique tester-trial rows:", format(nrow(hudprocessed_pairs), big.mark = ","), "\n")
  cat("Unique tester-trial rows with raw HADHOME == 1:", format(nrow(hudprocessed_hadhome_pairs), big.mark = ","), "\n")
  cat("Share with raw HADHOME == 1:", round(nrow(hudprocessed_hadhome_pairs) / nrow(hudprocessed_pairs), 4), "\n\n")
  cat("Unique tester-trial rows in official passed sales controls:", format(nrow(hudprocessed_official_pairs), big.mark = ","), "\n")
  cat(
    "Unique tester-trial rows outside official passed sales controls:",
    format(nrow(anti_join(hudprocessed_pairs, official_pass_controls, by = "CONTROL")), big.mark = ","),
    "\n\n"
  )
  cat("Official passed sales trials by number of testers appearing in this file:\n\n")
  print(hudprocessed_official_trial_summary)
  cat("\n")
}


# ---- Compare original HUD city FE with recommended/ad city fields ----------

cat("\n\nHow HCITY.x in the HUD files compares to HCITY_Rec and HCITY_Ad:\n")
cat("=========================================================\n\n")

summarize_hud_city_matches(hudprocessed_names_duplicates_path, hudprocessed_names_processed_path, "HUDprocessed names")
summarize_hud_city_matches(hudprocessed_census_duplicates_path, hudprocessed_census_processed_path, "HUDprocessed census")
summarize_hud_city_matches(hudprocessed_testscores_duplicates_path, hudprocessed_testscores_processed_path, "HUDprocessed testscores")


# ---- Duplicate provenance in the pooled Census file --------------------------

census_adsprocessed_source <- readRDS(adsprocessed_path) %>%
  mutate(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID)
  )

census_recsprocessed_source <- readRDS(recsprocessed_path) %>%
  mutate(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID)
  )

census_hud <- readRDS(hudprocessed_census_path) %>%
  mutate(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID)
  )

census_index_cols <- intersect(c("X.x", "X.y", "X.x.1", "X.y.1", "SEQRH"), names(census_hud))
census_ad_cols <- grep("_Ad$", names(census_hud), value = TRUE)
census_ad_price_cols <- intersect(c("AdPrice", "logAdPrice"), names(census_hud))
census_rec_cols <- grep("_Rec$", names(census_hud), value = TRUE)
census_rec_price_cols <- intersect(c("RecPrice", "logRecPrice"), names(census_hud))

# In the Census file, the final row definition keeps the ad-side variables with
# _Ad suffixes and the recommended-home side as the remaining non-index columns.
census_ads_exact_cols <- intersect(
  c("CONTROL", "TESTERID", census_ad_cols, census_ad_price_cols),
  names(census_adsprocessed_source)
)
census_recs_exact_cols <- intersect(
  setdiff(names(census_hud), c(census_index_cols, census_ad_cols, census_ad_price_cols)),
  names(census_recsprocessed_source)
)
census_hud_key_cols <- unique(c("CONTROL", "TESTERID", census_ad_cols, census_ad_price_cols, census_rec_cols, census_rec_price_cols))

census_ads_counts <- census_adsprocessed_source %>%
  group_by(CONTROL, TESTERID) %>%
  summarize(
    ads_rows = n(),
    ads_rows_under_census_definition =
      n_distinct(pick(all_of(setdiff(census_ads_exact_cols, c("CONTROL", "TESTERID"))))),
    .groups = "drop"
  )

census_recs_counts <- census_recsprocessed_source %>%
  group_by(CONTROL, TESTERID) %>%
  summarize(
    recs_rows = n(),
    recs_rows_under_census_definition =
      n_distinct(pick(all_of(setdiff(census_recs_exact_cols, c("CONTROL", "TESTERID"))))),
    .groups = "drop"
  )

census_final_counts <- census_hud %>%
  group_by(CONTROL, TESTERID) %>%
  summarize(
    final_rows = n(),
    distinct_rows_after_dedup =
      n_distinct(pick(all_of(setdiff(census_hud_key_cols, c("CONTROL", "TESTERID"))))),
    .groups = "drop"
  ) %>%
  left_join(census_ads_counts, by = c("CONTROL", "TESTERID")) %>%
  left_join(census_recs_counts, by = c("CONTROL", "TESTERID")) %>%
  mutate(
    observed_duplicates = final_rows - distinct_rows_after_dedup,
    expected_duplicates =
      final_rows - ads_rows_under_census_definition * recs_rows_under_census_definition
  )

cat("\n\nDuplicate provenance in the pooled Census file:\n")
cat("=========================================================\n\n")
cat("Rows in HUDprocessed_JPE_census_042021.rds:", format(sum(census_final_counts$final_rows), big.mark = ","), "\n")
cat("Rows remaining after exact and key deduplication:", format(sum(census_final_counts$distinct_rows_after_dedup), big.mark = ","), "\n")
cat("Observed duplicate rows removed:", format(sum(census_final_counts$observed_duplicates), big.mark = ","), "\n")
cat(
  "Expected duplicate rows from adsprocessed x recsprocessed multiplicity under the same Census row definition:",
  format(sum(census_final_counts$expected_duplicates), big.mark = ","),
  "\n"
)
cat(
  "Pairs where observed and expected duplicate counts agree exactly:",
  format(sum(census_final_counts$observed_duplicates == census_final_counts$expected_duplicates), big.mark = ","),
  "/",
  format(nrow(census_final_counts), big.mark = ","),
  "\n"
)


# ---- Reverse-engineer the APRACE rule used in C&T ---------------------------

ct_aprace_testers <- readRDS(adsprocessed_path) %>%
  transmute(
    TESTERID = as.character(TESTERID),
    APRACE_ct = suppressWarnings(as.numeric(APRACE))
  ) %>%
  filter(TESTERID != "", !is.na(APRACE_ct)) %>%
  distinct(TESTERID, .keep_all = TRUE)

aprace_rule_check <- ct_aprace_testers %>%
  inner_join(tester_raw, by = "TESTERID") %>%
  mutate(
    APRACE_from_subgroups = APRACE_raw,
    APRACE_from_subgroups = ifelse(!is.na(TASIANG) & TASIANG > 0, 4, APRACE_from_subgroups),
    APRACE_from_subgroups = ifelse(!is.na(THISPUBG) & THISPUBG > 0, 3, APRACE_from_subgroups),
    subgroup_rule_match = APRACE_from_subgroups == APRACE_ct
  )

asian_rule_cases <- aprace_rule_check %>%
  filter(!is.na(TASIANG) & TASIANG > 0, APRACE_raw != 4)

hispanic_rule_cases <- aprace_rule_check %>%
  mutate(aprace_after_asian = ifelse(!is.na(TASIANG) & TASIANG > 0, 4, APRACE_raw)) %>%
  filter(!is.na(THISPUBG) & THISPUBG > 0, aprace_after_asian != 3)

ct_other_cases <- aprace_rule_check %>%
  filter(APRACE_ct == 5)

cat("\n\nHow a simple subgroup-overwrite rule reproduces C&T's APRACE variable:\n")
cat("=========================================================\n\n")
cat("Unique C&T testers in pooled files:", format(nrow(ct_aprace_testers), big.mark = ","), "\n")
cat("Unique C&T testers matched to raw tester_censored:", format(nrow(aprace_rule_check), big.mark = ","), "\n")
cat(
  "Rule APRACE -> Asian if TASIANG > 0 -> Hispanic if THISPUBG > 0:",
  format(sum(aprace_rule_check$subgroup_rule_match, na.rm = TRUE), big.mark = ","),
  "/",
  format(nrow(aprace_rule_check), big.mark = ","),
  "=",
  round(mean(aprace_rule_check$subgroup_rule_match, na.rm = TRUE), 4),
  "\n"
)
cat("\nAffected by the Asian subgroup overwrite (TASIANG > 0 with raw APRACE != 4):\n\n")
cat("Count:", nrow(asian_rule_cases), "\n")
print(asian_rule_cases %>% select(TESTERID, APRACE_raw, TNATORIG, THISPUBG, TASIANG, APRACE_ct))

cat("\nAffected by the Hispanic subgroup overwrite (THISPUBG > 0 after Asian overwrite leaves APRACE != 3):\n\n")
cat("Count:", nrow(hispanic_rule_cases), "\n")
cat("With TNATORIG == 2:", sum(hispanic_rule_cases$TNATORIG == 2, na.rm = TRUE), "\n")
cat("Raw APRACE breakdown:\n")
print(hispanic_rule_cases %>% count(APRACE_raw, name = "n") %>% arrange(APRACE_raw))
cat("TNATORIG breakdown:\n")
print(hispanic_rule_cases %>% count(TNATORIG, name = "n") %>% arrange(TNATORIG))
cat("THISPUBG breakdown:\n")
print(hispanic_rule_cases %>% count(THISPUBG, name = "n") %>% arrange(THISPUBG))

cat("\nDescriptives for testers who remain APRACE == 5 in the C&T data:\n\n")
cat("Count:", nrow(ct_other_cases), "\n")
cat("With TNATORIG == 2:", sum(ct_other_cases$TNATORIG == 2, na.rm = TRUE), "\n")
cat("Raw APRACE breakdown:\n")
print(ct_other_cases %>% count(APRACE_raw, name = "n") %>% arrange(APRACE_raw))
cat("TNATORIG breakdown:\n")
print(ct_other_cases %>% count(TNATORIG, name = "n") %>% arrange(TNATORIG))
cat("THISPUBG breakdown:\n")
print(ct_other_cases %>% count(THISPUBG, name = "n") %>% arrange(THISPUBG))
cat("TASIANG breakdown:\n")
print(ct_other_cases %>% count(TASIANG, name = "n") %>% arrange(TASIANG))
print(ct_other_cases %>% select(TESTERID, APRACE_raw, TNATORIG, THISPUBG, TASIANG, APRACE_ct))


# ---- Examine address-only matches to see what differs from exact matches -----

cat("\n\nAddress-only matches:\n")
cat("=========================================================\n\n")

addr_only_matches <- adsprocessed_audit %>%
  filter(!exact_match & addr_match) %>%
  select(addr_key, HCITY, HSTATE, HZIP, HPRICE) %>%
  rename(
    ads_HCITY = HCITY,
    ads_HSTATE = HSTATE,
    ads_HZIP = HZIP,
    ads_HPRICE = HPRICE
  ) %>%
  inner_join(
    rechomes %>%
      filter(HADHOME == 1) %>%
      select(addr_key, HCITY, HSTATE, HZIP, HPRICE) %>%
      rename(
        rechomes_HCITY = HCITY,
        rechomes_HSTATE = HSTATE,
        rechomes_HZIP = HZIP,
        rechomes_HPRICE = HPRICE
      ) %>%
      distinct(),
    by = "addr_key"
  ) %>%
  select(-addr_key)

print(addr_only_matches)


# ---- Compare tester-identified advertised homes to TAF PADDR fields ---------

cat("\n\nHow well do TAF PADDR fields recover tester-identified advertised homes?\n")
cat("=========================================================\n\n")

taf_address_data <- read_sas(
  taf_path,
  col_select = c("CONTROL", "PADDRN", "PADDRS", "PADDRU", "PCITY", "PSTATE", "PZIP", "PHMPRI")
) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    taf_addr_raw = empty_to_na(str_squish(paste(PADDRN, PADDRS, PADDRU))),
    taf_city_raw = empty_to_na(PCITY),
    taf_state_raw = empty_to_na(PSTATE),
    taf_zip_raw = empty_to_na(as.character(PZIP)),
    taf_price = normalize_price(PHMPRI)
  ) %>%
  filter(CONTROL %in% unique(adsprocessed$CONTROL)) %>%
  mutate(
    taf_addr_std = normalize_street_match(taf_addr_raw),
    taf_city_std = normalize_city_match(taf_city_raw),
    taf_state_std = normalize_state_match(taf_state_raw),
    taf_zip_std = normalize_zip(taf_zip_raw),
    taf_house_num = extract_house_number(taf_addr_raw),
    taf_street_tail = strip_house_number(taf_addr_raw)
  ) %>%
  distinct(
    CONTROL, taf_addr_std, taf_city_std, taf_state_std, taf_zip_std,
    taf_house_num, taf_street_tail, taf_addr_raw, taf_city_raw, taf_state_raw,
    taf_zip_raw, taf_price, .keep_all = TRUE
  )

prep_ads_address_source <- function(data, address_col, city_col, state_col, zip_col, source_label) {
  data %>%
    transmute(
      CONTROL,
      TESTERID,
      source = source_label,
      ads_addr_raw = empty_to_na(.data[[address_col]]),
      ads_city_raw = empty_to_na(.data[[city_col]]),
      ads_state_raw = empty_to_na(.data[[state_col]]),
      ads_zip_raw = empty_to_na(.data[[zip_col]]),
      ads_price = HPRICE
    ) %>%
    mutate(
      ads_addr_std = normalize_street_match(ads_addr_raw),
      ads_city_std = normalize_city_match(ads_city_raw),
      ads_state_std = normalize_state_match(ads_state_raw),
      ads_zip_std = normalize_zip(ads_zip_raw),
      ads_house_num = extract_house_number(ads_addr_raw),
      ads_street_tail = strip_house_number(ads_addr_raw)
    ) %>%
    filter(ads_addr_std != "") %>%
    distinct(
      CONTROL, source, ads_addr_std, ads_city_std, ads_state_std, ads_zip_std,
      ads_house_num, ads_street_tail, ads_addr_raw, ads_city_raw, ads_state_raw,
      ads_zip_raw, ads_price, .keep_all = TRUE
    )
}

adsprocessed_for_taf_match <- readRDS(adsprocessed_path) %>%
  transmute(
    CONTROL = as.character(CONTROL),
    TESTERID = as.character(TESTERID),
    raw_addr = empty_to_na(str_squish(paste(HSITEAD, HUNITNO))),
    raw_city = empty_to_na(HCITY),
    raw_state = empty_to_na(HSTATE),
    raw_zip = empty_to_na(as.character(HZIP)),
    clean_addr = empty_to_na(Address),
    clean_city = empty_to_na(City),
    clean_state = empty_to_na(State),
    clean_zip = empty_to_na(as.character(Zip_Code)),
    HPRICE = normalize_price(HPRICE)
  )

ads_sources <- bind_rows(
  prep_ads_address_source(adsprocessed_for_taf_match, "raw_addr", "raw_city", "raw_state", "raw_zip", "ads_hds_fields"),
  prep_ads_address_source(adsprocessed_for_taf_match, "clean_addr", "clean_city", "clean_state", "clean_zip", "ads_ct_clean_fields")
)

taf_ads_pairs <- ads_sources %>%
  inner_join(taf_address_data, by = "CONTROL", relationship = "many-to-many") %>%
  mutate(
    exact_addr = ads_addr_std == taf_addr_std & ads_addr_std != "",
    exact_addr_city_state = exact_addr &
      ads_city_std == taf_city_std &
      ads_state_std == taf_state_std &
      ads_city_std != "" &
      ads_state_std != "",
    exact_addr_city_state_zip = exact_addr_city_state &
      ads_zip_std == taf_zip_std &
      ads_zip_std != "",
    house_number_match = ads_house_num == taf_house_num & ads_house_num != "",
    city_state_match = ads_city_std == taf_city_std &
      ads_state_std == taf_state_std &
      ads_city_std != "" &
      ads_state_std != "",
    zip_match = ads_zip_std == taf_zip_std & ads_zip_std != "",
    tail_similarity = lev_similarity(ads_street_tail, taf_street_tail),
    strong_fuzzy = !exact_addr & house_number_match & !is.na(tail_similarity) & tail_similarity >= 0.85,
    moderate_fuzzy = !exact_addr & house_number_match & !is.na(tail_similarity) & tail_similarity >= 0.70
  ) %>%
  mutate(
    match_rank = case_when(
      exact_addr_city_state_zip ~ 1L,
      exact_addr_city_state ~ 2L,
      exact_addr ~ 3L,
      strong_fuzzy & city_state_match ~ 4L,
      strong_fuzzy ~ 5L,
      moderate_fuzzy & city_state_match ~ 6L,
      moderate_fuzzy ~ 7L,
      TRUE ~ 8L
    )
  )

best_taf_match_per_ad <- taf_ads_pairs %>%
  arrange(source, CONTROL, ads_addr_std, match_rank, desc(tail_similarity), desc(zip_match)) %>%
  group_by(source, CONTROL, ads_addr_std) %>%
  slice_head(n = 1) %>%
  ungroup()

taf_ads_control_summary <- best_taf_match_per_ad %>%
  group_by(source, CONTROL) %>%
  summarize(
    n_ads_candidates = n(),
    any_exact_addr_city_state_zip = any(exact_addr_city_state_zip),
    any_exact_addr_city_state = any(exact_addr_city_state),
    any_exact_addr = any(exact_addr),
    any_strong_fuzzy = any(strong_fuzzy),
    any_moderate_fuzzy = any(moderate_fuzzy),
    any_taf_street_nonblank = any(taf_addr_std != ""),
    any_same_city_state = any(city_state_match),
    any_same_zip = any(zip_match),
    any_house_num = any(house_number_match),
    .groups = "drop"
  ) %>%
  mutate(
    control_match_class = case_when(
      any_exact_addr_city_state_zip ~ "exact_addr_city_state_zip",
      any_exact_addr_city_state ~ "exact_addr_city_state",
      any_exact_addr ~ "exact_addr_only",
      any_strong_fuzzy ~ "strong_fuzzy",
      any_moderate_fuzzy ~ "moderate_fuzzy",
      TRUE ~ "no_plausible_match"
    )
  )

taf_ads_match_summary <- taf_ads_control_summary %>%
  group_by(source) %>%
  summarize(
    controls = n(),
    controls_with_multiple_ads_candidates = sum(n_ads_candidates > 1),
    exact_addr_city_state_zip = sum(control_match_class == "exact_addr_city_state_zip"),
    exact_addr_city_state = sum(control_match_class == "exact_addr_city_state"),
    exact_addr_only = sum(control_match_class == "exact_addr_only"),
    strong_fuzzy = sum(control_match_class == "strong_fuzzy"),
    moderate_fuzzy = sum(control_match_class == "moderate_fuzzy"),
    no_plausible_match = sum(control_match_class == "no_plausible_match"),
    exact_or_better = sum(control_match_class %in% c(
      "exact_addr_city_state_zip",
      "exact_addr_city_state",
      "exact_addr_only"
    )),
    exact_or_strong_fuzzy = sum(control_match_class %in% c(
      "exact_addr_city_state_zip",
      "exact_addr_city_state",
      "exact_addr_only",
      "strong_fuzzy"
    )),
    exact_or_any_fuzzy = sum(control_match_class %in% c(
      "exact_addr_city_state_zip",
      "exact_addr_city_state",
      "exact_addr_only",
      "strong_fuzzy",
      "moderate_fuzzy"
    )),
    .groups = "drop"
  ) %>%
  mutate(
    exact_or_better_share = round(exact_or_better / controls, 4),
    exact_or_strong_fuzzy_share = round(exact_or_strong_fuzzy / controls, 4),
    exact_or_any_fuzzy_share = round(exact_or_any_fuzzy / controls, 4)
  )

taf_ads_evidence_summary <- taf_ads_control_summary %>%
  filter(source == "ads_hds_fields") %>%
  summarize(
    controls = n(),
    taf_nonblank_street = sum(any_taf_street_nonblank),
    exact_or_better = sum(control_match_class %in% c(
      "exact_addr_city_state_zip",
      "exact_addr_city_state",
      "exact_addr_only"
    )),
    exact_or_any_fuzzy = sum(control_match_class %in% c(
      "exact_addr_city_state_zip",
      "exact_addr_city_state",
      "exact_addr_only",
      "strong_fuzzy",
      "moderate_fuzzy"
    )),
    no_plausible_match = sum(control_match_class == "no_plausible_match"),
    no_match_with_nonblank_taf_street = sum(control_match_class == "no_plausible_match" & any_taf_street_nonblank),
    no_match_with_same_city_state = sum(control_match_class == "no_plausible_match" & any_same_city_state),
    no_match_with_same_zip = sum(control_match_class == "no_plausible_match" & any_same_zip),
    no_match_with_same_house_number = sum(control_match_class == "no_plausible_match" & any_house_num),
    controls_with_multiple_ads_candidates = sum(n_ads_candidates > 1),
    multiple_ads_exact_or_any_fuzzy = sum(
      n_ads_candidates > 1 &
        control_match_class %in% c(
          "exact_addr_city_state_zip",
          "exact_addr_city_state",
          "exact_addr_only",
          "strong_fuzzy",
          "moderate_fuzzy"
        )
    )
  )

no_match_best_rows <- best_taf_match_per_ad %>%
  inner_join(
    taf_ads_control_summary %>%
      filter(source == "ads_hds_fields", control_match_class == "no_plausible_match") %>%
      select(CONTROL),
    by = "CONTROL"
  ) %>%
  filter(source == "ads_hds_fields") %>%
  arrange(CONTROL, desc(city_state_match), desc(zip_match), desc(house_number_match), desc(tail_similarity)) %>%
  group_by(CONTROL) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    taf_suite_like = str_detect(
      str_to_upper(coalesce(taf_addr_raw, "")),
      "\\b(STE|SUITE|UNIT|OFFICE|APT)\\b"
    )
  )

taf_ads_evidence_summary <- taf_ads_evidence_summary %>%
  mutate(
    no_match_with_suite_or_office_token = sum(no_match_best_rows$taf_suite_like, na.rm = TRUE)
  )

exact_price_matches <- best_taf_match_per_ad %>%
  filter(source == "ads_hds_fields", exact_addr, !is.na(ads_price), !is.na(taf_price), ads_price > 0, taf_price > 0) %>%
  mutate(pct_price_diff = abs(ads_price - taf_price) / pmax(ads_price, taf_price))

taf_ads_evidence_summary <- taf_ads_evidence_summary %>%
  mutate(
    exact_matches_with_both_prices = nrow(exact_price_matches),
    exact_matches_price_within_5pct = sum(exact_price_matches$pct_price_diff <= 0.05, na.rm = TRUE),
    exact_matches_price_within_10pct = sum(exact_price_matches$pct_price_diff <= 0.10, na.rm = TRUE)
  )

taf_duplicate_controls <- taf_address_data %>%
  count(CONTROL, name = "n_taf_rows") %>%
  filter(n_taf_rows > 1)

taf_office_like_examples <- no_match_best_rows %>%
  filter(taf_suite_like) %>%
  select(CONTROL, ads_addr_raw, ads_city_raw, ads_zip_raw, taf_addr_raw, taf_city_raw, taf_zip_raw) %>%
  slice_head(n = 20)

source_labels <- c(
  ads_hds_fields = "HDS tester-recorded advertised-home fields",
  ads_ct_clean_fields = "C&T cleaned advertised-home fields"
)

cat("Control-level match summary:\n\n")
for (i in seq_len(nrow(taf_ads_match_summary))) {
  row <- taf_ads_match_summary[i, ]
  cat(source_labels[[row$source]], "\n", sep = "")
  cat("  Controls:", format(row$controls, big.mark = ","), "\n")
  cat("  Controls with multiple advertised-home candidates:", format(row$controls_with_multiple_ads_candidates, big.mark = ","), "\n")
  cat("  Exact match:", format(row$exact_or_better, big.mark = ","), "(", round(100 * row$exact_or_better_share, 1), "%)\n")
  cat("  Exact or strong fuzzy:", format(row$exact_or_strong_fuzzy, big.mark = ","), "(", round(100 * row$exact_or_strong_fuzzy_share, 1), "%)\n")
  cat("  Exact or any fuzzy:", format(row$exact_or_any_fuzzy, big.mark = ","), "(", round(100 * row$exact_or_any_fuzzy_share, 1), "%)\n")
  cat("  No plausible match:", format(row$no_plausible_match, big.mark = ","), "\n\n")
}

evidence <- taf_ads_evidence_summary[1, ]

cat("Supporting evidence summary (HDS tester-recorded address source):\n\n")
cat("  Controls in pooled sample:", format(evidence$controls, big.mark = ","), "\n")
cat("  Controls with nonblank TAF street:", format(evidence$taf_nonblank_street, big.mark = ","), "\n")
cat("  Exact matches:", format(evidence$exact_or_better, big.mark = ","), "\n")
cat("  Exact or fuzzy matches:", format(evidence$exact_or_any_fuzzy, big.mark = ","), "\n")
cat("  No plausible match:", format(evidence$no_plausible_match, big.mark = ","), "\n")
cat("  No plausible match despite nonblank TAF street:", format(evidence$no_match_with_nonblank_taf_street, big.mark = ","), "\n")
cat("  No plausible match but same city/state somewhere in control:", format(evidence$no_match_with_same_city_state, big.mark = ","), "\n")
cat("  No plausible match but same ZIP somewhere in control:", format(evidence$no_match_with_same_zip, big.mark = ","), "\n")
cat("  No plausible match but same house number somewhere in control:", format(evidence$no_match_with_same_house_number, big.mark = ","), "\n")
cat("  Controls with multiple advertised-home candidates:", format(evidence$controls_with_multiple_ads_candidates, big.mark = ","), "\n")
cat("  Multiple-candidate controls with any exact/fuzzy match:", format(evidence$multiple_ads_exact_or_any_fuzzy, big.mark = ","), "\n")
cat("  No-match controls with suite/office-style TAF tokens:", format(evidence$no_match_with_suite_or_office_token, big.mark = ","), "\n")
cat("  Exact matches with both prices observed:", format(evidence$exact_matches_with_both_prices, big.mark = ","), "\n")
cat("  Exact matches within 5% on price:", format(evidence$exact_matches_price_within_5pct, big.mark = ","), "\n")
cat("  Exact matches within 10% on price:", format(evidence$exact_matches_price_within_10pct, big.mark = ","), "\n\n")

cat("Controls with duplicated TAF rows:", nrow(taf_duplicate_controls), "\n")
if (nrow(taf_duplicate_controls) > 0) {
  print(taf_duplicate_controls)
}
cat("\n")

cat("Illustrative office-style nonmatches from TAF:\n\n")
print(taf_office_like_examples)


# ---- How city cleaning changes FE geometry and identification ---------------

derive_ofcolor_from_hds_race <- function(x) {
  race_num <- suppressWarnings(as.numeric(as.character(x)))
  aprace <- ifelse(is.na(race_num), NA_real_, race_num %% 10)
  case_when(
    aprace == 1 ~ 0,
    aprace %in% c(2, 3, 4) ~ 1,
    TRUE ~ NA_real_
  )
}

load_city_fe_file <- function(file_path, dataset_label, raw_city_var, race_var, require_ad_city) {
  data <- read_dta(file_path) %>%
    mutate(
      dataset = dataset_label,
      ofcolor = derive_ofcolor_from_hds_race(.data[[race_var]]),
      raw_city = as.character(.data[[raw_city_var]]),
      temp_city = as.character(temp_city),
      place_name = as.character(place_name),
      county_name = as.character(county_name)
    ) %>%
    filter(
      !is.na(ofcolor),
      !is.na(raw_city), !raw_city %in% c("", ".", "NA"),
      !is.na(temp_city), !temp_city %in% c("", ".", "NA"),
      !is.na(place_name), !place_name %in% c("", ".", "NA"),
      !is.na(county_name), !county_name %in% c("", ".", "NA")
    )

  if (require_ad_city) {
    data <- data %>%
      mutate(hcity_ad = as.character(hcity_ad)) %>%
      filter(!is.na(hcity_ad), !hcity_ad %in% c("", ".", "NA"))
  }

  data
}

summarize_fe_geometry <- function(data, fe_var) {
  grouped <- data %>%
    group_by(.data[[fe_var]]) %>%
    summarise(
      rows = n(),
      white_rows = sum(ofcolor == 0, na.rm = TRUE),
      minority_rows = sum(ofcolor == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cell_type = case_when(
        white_rows > 0 & minority_rows > 0 ~ "mixed",
        white_rows > 0 ~ "white_only",
        minority_rows > 0 ~ "minority_only",
        TRUE ~ "empty"
      )
    )

  tibble(
    groups = nrow(grouped),
    white_unique_fes = n_distinct(data[[fe_var]][data$ofcolor == 0]),
    minority_unique_fes = n_distinct(data[[fe_var]][data$ofcolor == 1]),
    mixed_groups = sum(grouped$cell_type == "mixed"),
    white_only_groups = sum(grouped$cell_type == "white_only"),
    minority_only_groups = sum(grouped$cell_type == "minority_only"),
    rows_in_mixed_groups = sum(grouped$rows[grouped$cell_type == "mixed"])
  )
}

city_fe_files <- list(
  list(
    dataset = "adsprocessed",
    path = file.path(output_dir, "adsprocessed_processed_hcity_cleaned.dta"),
    raw_city_var = "hcity",
    race_var = "race_ad",
    require_ad_city = FALSE
  ),
  list(
    dataset = "census",
    path = file.path(output_dir, "HUDprocessed_census_processed_hcityx_cleaned.dta"),
    raw_city_var = "hcityx",
    race_var = "race_rec",
    require_ad_city = TRUE
  ),
  list(
    dataset = "testscores",
    path = file.path(output_dir, "HUDprocessed_testscores_processed_hcityx_cleaned.dta"),
    raw_city_var = "hcityx",
    race_var = "race_rec",
    require_ad_city = TRUE
  ),
  list(
    dataset = "names",
    path = file.path(output_dir, "HUDprocessed_names_processed_hcityx_cleaned.dta"),
    raw_city_var = "hcityx",
    race_var = "race_rec",
    require_ad_city = TRUE
  )
)

city_fe_geometry_summary <- bind_rows(lapply(city_fe_files, function(spec) {
  data <- load_city_fe_file(
    spec$path,
    dataset_label = spec$dataset,
    raw_city_var = spec$raw_city_var,
    race_var = spec$race_var,
    require_ad_city = spec$require_ad_city
  )

  before <- summarize_fe_geometry(data, "raw_city")
  after <- summarize_fe_geometry(data, "temp_city")

  tibble(
    dataset = spec$dataset,
    rows = nrow(data),
    groups_before = before$groups,
    groups_after = after$groups,
    white_unique_fes_before = before$white_unique_fes,
    white_unique_fes_after = after$white_unique_fes,
    minority_unique_fes_before = before$minority_unique_fes,
    minority_unique_fes_after = after$minority_unique_fes,
    mixed_groups_before = before$mixed_groups,
    mixed_groups_after = after$mixed_groups,
    white_only_groups_before = before$white_only_groups,
    white_only_groups_after = after$white_only_groups,
    minority_only_groups_before = before$minority_only_groups,
    minority_only_groups_after = after$minority_only_groups,
    rows_in_mixed_groups_before = before$rows_in_mixed_groups,
    rows_in_mixed_groups_after = after$rows_in_mixed_groups
  )
}))

write.csv(
  city_fe_geometry_summary,
  file.path(output_dir, "city_fe_geometry_summary.csv"),
  row.names = FALSE
)
write_city_fe_geometry_tex(
  city_fe_geometry_summary,
  file.path(output_dir, "city_fe_geometry_summary.tex")
)

cat("\n\nCity FE geometry before and after cleaning:\n")
cat("=========================================================\n\n")
print(city_fe_geometry_summary)


# ---- Census mixed-cell comparison -------------------------------------------

census_city_data <- load_city_fe_file(
  file.path(output_dir, "HUDprocessed_census_processed_hcityx_cleaned.dta"),
  dataset_label = "census",
  raw_city_var = "hcityx",
  race_var = "race_rec",
  require_ad_city = TRUE
)

raw_mixed_flags <- census_city_data %>%
  group_by(raw_city) %>%
  summarise(raw_city_mixed = n_distinct(ofcolor) > 1, .groups = "drop")

temp_mixed_flags <- census_city_data %>%
  group_by(temp_city) %>%
  summarise(temp_city_mixed = n_distinct(ofcolor) > 1, .groups = "drop")

census_temp_city_components <- census_city_data %>%
  left_join(raw_mixed_flags, by = "raw_city") %>%
  left_join(temp_mixed_flags, by = "temp_city")

census_temp_city_types <- census_temp_city_components %>%
  group_by(temp_city, temp_city_mixed) %>%
  summarise(
    any_mixed_raw_component = any(raw_city_mixed, na.rm = TRUE),
    any_unmixed_raw_component = any(!raw_city_mixed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cell_type = case_when(
      !temp_city_mixed ~ "not_identifying_after_cleaning",
      any_mixed_raw_component & any_unmixed_raw_component ~ "mixed_before_plus_newly_attached",
      any_mixed_raw_component & !any_unmixed_raw_component ~ "mixed_before_only",
      !any_mixed_raw_component & any_unmixed_raw_component ~ "purely_newly_mixed_after_cleaning",
      TRUE ~ "not_identifying_after_cleaning"
    )
  )

census_city_cells <- census_temp_city_components %>%
  select(-temp_city_mixed) %>%
  left_join(
    census_temp_city_types %>% select(temp_city, cell_type),
    by = "temp_city"
  )

summarize_weighted_gap <- function(data, outcome_var) {
  outcome_data <- data %>%
    filter(!is.na(.data[[outcome_var]])) %>%
    group_by(cell_type, temp_city, ofcolor) %>%
    summarise(
      outcome_mean = mean(.data[[outcome_var]], na.rm = TRUE),
      rows = n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = ofcolor,
      values_from = c(outcome_mean, rows),
      names_sep = "_"
    ) %>%
    filter(!is.na(outcome_mean_0), !is.na(outcome_mean_1)) %>%
    mutate(
      gap = outcome_mean_1 - outcome_mean_0,
      total_rows = rows_0 + rows_1
    ) %>%
    group_by(cell_type) %>%
    summarise(
      weighted_gap = sum(gap * total_rows) / sum(total_rows),
      .groups = "drop"
    )

  names(outcome_data)[names(outcome_data) == "weighted_gap"] <- paste0("weighted_gap_", outcome_var)
  outcome_data
}

census_cell_structure <- census_city_cells %>%
  filter(cell_type %in% c(
    "mixed_before_only",
    "mixed_before_plus_newly_attached",
    "purely_newly_mixed_after_cleaning"
  )) %>%
  group_by(cell_type, temp_city) %>%
  summarise(
    rows_in_cell = n(),
    raw_labels_per_cell = n_distinct(raw_city),
    blockgroups_per_cell = n_distinct(blkgrp),
    places_per_cell = n_distinct(place_name),
    counties_per_cell = n_distinct(county_name),
    .groups = "drop"
  ) %>%
  group_by(cell_type) %>%
  summarise(
    cells = n(),
    rows = sum(rows_in_cell),
    median_rows_per_cell = median(rows_in_cell),
    median_raw_labels_per_cell = median(raw_labels_per_cell),
    median_blockgroups_per_cell = median(blockgroups_per_cell),
    share_single_blockgroup = mean(blockgroups_per_cell == 1),
    median_places_per_cell = median(places_per_cell),
    share_single_place = mean(places_per_cell == 1),
    share_single_county = mean(counties_per_cell == 1),
    .groups = "drop"
  )

census_mixed_cell_summary <- census_cell_structure %>%
  left_join(summarize_weighted_gap(census_city_cells, "whitehi_rec"), by = "cell_type") %>%
  left_join(summarize_weighted_gap(census_city_cells, "medincome_rec"), by = "cell_type")

write.csv(
  census_mixed_cell_summary,
  file.path(output_dir, "city_mixed_cell_summary.csv"),
  row.names = FALSE
)
write_city_mixed_cell_tex(
  census_mixed_cell_summary,
  file.path(output_dir, "city_mixed_cell_summary.tex")
)

cat("\n\nCensus mixed-cell comparison before and after city cleaning:\n")
cat("=========================================================\n\n")
print(census_mixed_cell_summary)

census_mixed_cell_console_examples <- census_city_cells %>%
  filter(cell_type %in% c(
    "mixed_before_only",
    "mixed_before_plus_newly_attached",
    "purely_newly_mixed_after_cleaning"
  )) %>%
  group_by(cell_type, temp_city) %>%
  summarise(
    rows = n(),
    raw_labels_per_cell = n_distinct(raw_city),
    raw_labels = paste(head(sort(unique(raw_city)), 8), collapse = " | "),
    blockgroups_per_cell = n_distinct(blkgrp),
    places_per_cell = n_distinct(place_name),
    controls = n_distinct(control),
    .groups = "drop"
  ) %>%
  arrange(cell_type, desc(rows), desc(raw_labels_per_cell)) %>%
  group_by(cell_type) %>%
  slice_head(n = 8) %>%
  ungroup()

cat("\n\nIllustrative cleaned city cells by overlap type:\n")
cat("=========================================================\n\n")
print(census_mixed_cell_console_examples)
