library(dplyr)
library(readr)
library(stringr)

# Paths for source data and diagnostics outputs.
input_folder <- "Data/Original"
output_folder <- "Output"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Normalize city strings for robust equality checks.
normalize_city <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(str_squish(x))
  x[x == ""] <- NA_character_
  x
}

# Format fractions as percentages for LaTeX output.
pct <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f\\%%", 100 * x))
}

# Escape LaTeX control characters in text fields (e.g., dataset names)
# so table rows compile safely.
escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%$#&{}])", "\\\\\\1", x)
  x
}

# Compute comparability/match statistics for two character vectors.
cmp_stats <- function(a, b) {
  comparable <- !is.na(a) & a != "" & !is.na(b) & b != ""
  n_comp <- sum(comparable)
  n_eq <- sum(a[comparable] == b[comparable])
  data.frame(
    comparable = n_comp,
    equal = n_eq,
    not_equal = n_comp - n_eq,
    share_equal = ifelse(n_comp > 0, n_eq / n_comp, NA_real_),
    share_not_equal = ifelse(n_comp > 0, (n_comp - n_eq) / n_comp, NA_real_)
  )
}

write_latex_table <- function(lines, file_path) {
  writeLines(lines, con = file_path)
  cat("Wrote", file_path, "\n")
}

# HUD files to audit for city-source consistency.
hud_files <- c(
  census = "HUDprocessed_JPE_census_042021.rds",
  names = "HUDprocessed_JPE_names_042021.rds",
  testscores = "HUDprocessed_JPE_testscores_042021.rds"
)

ads <- readRDS(file.path(input_folder, "adsprocessed_JPE.rds")) %>%
  mutate(TESTERID = as.character(TESTERID))
recs <- readRDS(file.path(input_folder, "recsprocessed_JPE.rds")) %>%
  mutate(TESTERID = as.character(TESTERID))

ad_keys <- c(
  "CONTROL", "TESTERID", "logAdPrice", "stfid_Ad",
  "w2012pc_Ad", "b2012pc_Ad", "a2012pc_Ad", "hisp2012pc_Ad", "oth2012pc_Ad"
)
rec_keys <- c(
  "CONTROL", "TESTERID", "logRecPrice",
  "w2012pc_Rec", "b2012pc_Rec", "a2012pc_Rec", "hisp2012pc_Rec", "oth2012pc_Rec"
)

ad_lookup <- ads %>%
  mutate(
    HCITY_Ad_raw = str_squish(as.character(HCITY)),
    HCITY_Ad_raw = ifelse(HCITY_Ad_raw == "", NA_character_, HCITY_Ad_raw),
    HCITY_Ad_norm = normalize_city(HCITY)
  ) %>%
  group_by(across(all_of(ad_keys))) %>%
  summarize(
    HCITY_Ad = {
      valid_idx <- which(!is.na(HCITY_Ad_norm))
      if (length(valid_idx) == 0) {
        NA_character_
      } else if (dplyr::n_distinct(HCITY_Ad_norm[valid_idx]) == 1) {
        HCITY_Ad_raw[valid_idx][1]
      } else {
        NA_character_
      }
    },
    ad_city_ambiguous = {
      valid <- HCITY_Ad_norm[!is.na(HCITY_Ad_norm)]
      length(valid) > 0 && dplyr::n_distinct(valid) > 1
    },
    .groups = "drop"
  ) %>%
  mutate(HCITY_Ad_norm = normalize_city(HCITY_Ad))

recs_lookup <- recs %>%
  mutate(
    HCITY_Rec_raw = str_squish(as.character(HCITY_Rec)),
    HCITY_Rec_raw = ifelse(HCITY_Rec_raw == "", NA_character_, HCITY_Rec_raw),
    HCITY_Rec_norm = normalize_city(HCITY_Rec)
  ) %>%
  group_by(across(all_of(rec_keys))) %>%
  summarize(
    HCITY_Rec_lookup = {
      valid_idx <- which(!is.na(HCITY_Rec_norm))
      if (length(valid_idx) == 0) {
        NA_character_
      } else if (dplyr::n_distinct(HCITY_Rec_norm[valid_idx]) == 1) {
        HCITY_Rec_raw[valid_idx][1]
      } else {
        NA_character_
      }
    },
    rec_city_ambiguous = {
      valid <- HCITY_Rec_norm[!is.na(HCITY_Rec_norm)]
      length(valid) > 0 && dplyr::n_distinct(valid) > 1
    },
    .groups = "drop"
  ) %>%
  mutate(HCITY_Rec_lookup_norm = normalize_city(HCITY_Rec_lookup))

cat("Ad-city lookup rows:", nrow(ad_lookup), "\n")
cat("Ad-city ambiguous keys:", sum(ad_lookup$ad_city_ambiguous), "\n")
cat("Rec-city lookup rows:", nrow(recs_lookup), "\n")
cat("Rec-city ambiguous keys:", sum(recs_lookup$rec_city_ambiguous), "\n")

summary_rows <- list()
within_control_rows <- list()
recs_match_rows <- list()
mismatch_examples <- list()

# Build per-dataset diagnostics and collect summary rows.
for (dataset_name in names(hud_files)) {
  hud <- readRDS(file.path(input_folder, hud_files[[dataset_name]])) %>%
    mutate(TESTERID = as.character(TESTERID))

  city_col <- if ("HCITY.x" %in% names(hud)) "HCITY.x" else "HCITY_x"

  hud_enriched <- hud %>%
    left_join(ad_lookup %>% select(all_of(ad_keys), HCITY_Ad, HCITY_Ad_norm), by = ad_keys) %>%
    left_join(
      recs_lookup %>% select(all_of(rec_keys), HCITY_Rec_lookup, HCITY_Rec_lookup_norm),
      by = rec_keys
    ) %>%
    mutate(
      HCITY_hud = as.character(.data[[city_col]]),
      HCITY_hud_norm = normalize_city(.data[[city_col]]),
      HCITY_Rec_hud_norm = normalize_city(HCITY_Rec)
    )

  eq_hud_rec <- cmp_stats(hud_enriched$HCITY_hud_norm, hud_enriched$HCITY_Rec_hud_norm)
  rec_vs_ad <- cmp_stats(hud_enriched$HCITY_Rec_hud_norm, hud_enriched$HCITY_Ad_norm)
  hud_vs_recs <- cmp_stats(hud_enriched$HCITY_hud_norm, hud_enriched$HCITY_Rec_lookup_norm)

  control_stats <- hud_enriched %>%
    group_by(CONTROL) %>%
    summarize(
      n_hcity = n_distinct(HCITY_hud_norm[!is.na(HCITY_hud_norm)]),
      .groups = "drop"
    )

  controls_total <- nrow(control_stats)
  controls_varying <- sum(control_stats$n_hcity > 1)

  ad_matched_rows <- sum(!is.na(hud_enriched$HCITY_Ad_norm))
  recs_matched_rows <- sum(!is.na(hud_enriched$HCITY_Rec_lookup_norm))

  summary_rows[[dataset_name]] <- data.frame(
    dataset = dataset_name,
    rows = nrow(hud_enriched),
    hud_rec_comparable = eq_hud_rec$comparable,
    hud_rec_equal = eq_hud_rec$equal,
    hud_rec_share_equal = eq_hud_rec$share_equal,
    rec_ad_comparable = rec_vs_ad$comparable,
    rec_ad_not_equal = rec_vs_ad$not_equal,
    rec_ad_share_not_equal = rec_vs_ad$share_not_equal,
    ad_matched_rows = ad_matched_rows
  )

  within_control_rows[[dataset_name]] <- data.frame(
    dataset = dataset_name,
    controls_total = controls_total,
    controls_varying = controls_varying,
    controls_varying_share = controls_varying / controls_total
  )

  recs_match_rows[[dataset_name]] <- data.frame(
    dataset = dataset_name,
    recs_matched_rows = recs_matched_rows,
    hud_recs_comparable = hud_vs_recs$comparable,
    hud_recs_equal = hud_vs_recs$equal,
    hud_recs_share_equal = hud_vs_recs$share_equal
  )

  mismatch_examples[[dataset_name]] <- hud_enriched %>%
    filter(
      !is.na(HCITY_Rec_hud_norm), !is.na(HCITY_Ad_norm),
      HCITY_Rec_hud_norm != HCITY_Ad_norm
    ) %>%
    mutate(dataset = dataset_name) %>%
    select(
      dataset,
      CONTROL, TESTERID, HCITY_hud, HCITY_Rec, HCITY_Ad
    ) %>%
    distinct(dataset, CONTROL, TESTERID, HCITY_hud, HCITY_Rec, HCITY_Ad) %>%
    slice_head(n = 20)
}

summary_df <- bind_rows(summary_rows)
within_control_df <- bind_rows(within_control_rows)
recs_match_df <- bind_rows(recs_match_rows)
mismatch_df <- bind_rows(mismatch_examples)

# Save a small set of concrete row-level examples where rec and ad cities differ.
write_csv(mismatch_df, file.path(output_folder, "hcity_rec_vs_ad_mismatch_examples.csv"))

# Table 1: direct comparisons among HUD HCITY.x, HUD HCITY_Rec, and merged HCITY_Ad.
table1_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{HCITY diagnostics in original HUD files}",
  "\\label{tab:hcity_diagnostics_main}",
  "\\begin{tabular}{lrrrrrr}",
  "\\hline",
  "Dataset & Rows & $HCITY.x=HCITY\\_Rec$ & Share & $HCITY\\_Rec \\neq HCITY\\_Ad$ & Share & Rows with $HCITY\\_Ad$ \\\\",
  "\\hline"
)

for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  table1_lines <- c(
    table1_lines,
    sprintf(
      "%s & %d & %d/%d & %s & %d/%d & %s & %d \\\\",
      escape_latex(r$dataset),
      r$rows,
      r$hud_rec_equal, r$hud_rec_comparable, pct(r$hud_rec_share_equal),
      r$rec_ad_not_equal, r$rec_ad_comparable, pct(r$rec_ad_share_not_equal),
      r$ad_matched_rows
    )
  )
}

table1_lines <- c(
  table1_lines,
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)
write_latex_table(table1_lines, file.path(output_folder, "hcity_diagnostics_main.tex"))

# Table 2: within-trial variation of HUD HCITY.x by CONTROL.
table2_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Within-control variation in HUD $HCITY.x$}",
  "\\label{tab:hcity_within_control}",
  "\\begin{tabular}{lrrr}",
  "\\hline",
  "Dataset & Controls & Controls with $>1$ city & Share \\\\",
  "\\hline"
)

for (i in seq_len(nrow(within_control_df))) {
  r <- within_control_df[i, ]
  table2_lines <- c(
    table2_lines,
    sprintf(
      "%s & %d & %d & %s \\\\",
      escape_latex(r$dataset),
      r$controls_total,
      r$controls_varying,
      pct(r$controls_varying_share)
    )
  )
}

table2_lines <- c(
  table2_lines,
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)
write_latex_table(table2_lines, file.path(output_folder, "hcity_within_control.tex"))

# Table 3: recsprocessed cross-check that HUD HCITY.x aligns with rec-side city.
table3_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Cross-check with recsprocessed}",
  "\\label{tab:hcity_recs_crosscheck}",
  "\\begin{tabular}{lrrr}",
  "\\hline",
  "Dataset & Rows matched to recs & $HCITY.x = HCITY\\_Rec$ from recs & Share \\\\",
  "\\hline"
)

for (i in seq_len(nrow(recs_match_df))) {
  r <- recs_match_df[i, ]
  table3_lines <- c(
    table3_lines,
    sprintf(
      "%s & %d & %d/%d & %s \\\\",
      escape_latex(r$dataset),
      r$recs_matched_rows,
      r$hud_recs_equal,
      r$hud_recs_comparable,
      pct(r$hud_recs_share_equal)
    )
  )
}

table3_lines <- c(
  table3_lines,
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)
write_latex_table(table3_lines, file.path(output_folder, "hcity_recs_crosscheck.tex"))

cat("HCITY diagnostics complete.\n")
