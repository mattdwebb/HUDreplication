# matching_diagnosis.R
# Standardized validation of external dataset merges vs Christensen & Timmins (2022) replication data

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(readxl)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg_value <- function(prefix) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(NULL)
  sub(prefix, "", hit[1], fixed = TRUE)
}

resolve_repo_root <- function(repo_root_arg = NULL) {
  if (!is.null(repo_root_arg) && nzchar(repo_root_arg)) {
    return(normalizePath(repo_root_arg, winslash = "/", mustWork = TRUE))
  }

  env_root <- Sys.getenv("HUD_REPLICATION_ROOT", "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (tolower(basename(cwd)) == "hudreplication") return(cwd)
  if (basename(cwd) == "reconstructed_sample") return(dirname(cwd))
  candidate <- file.path(cwd, "HUDreplication")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  candidate <- file.path(cwd, "HUDReplication")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  stop("Could not infer repo_root. Run from HUDreplication or reconstructed_sample, set HUD_REPLICATION_ROOT, or pass --repo-root=/path/to/HUDreplication.")
}

repo_root <- resolve_repo_root(get_arg_value("--repo-root="))
setwd(repo_root)

data_dir <- file.path(repo_root, "Data")
ct_data_dir <- file.path(data_dir, "CT2022_Replication_Data")
non_hds_data_dir <- file.path(data_dir, "Non_HDS_Data")
reconstructed_sample_generated_dir <- file.path(data_dir, "Generated", "reconstructed_sample")
cleaning_scripts_dir <- file.path(repo_root, "reconstructed_sample", "Cleaning_Scripts")

appendix_table_dir <- get_arg_value("--output-dir=")
if (is.null(appendix_table_dir) || !nzchar(appendix_table_dir)) {
  appendix_table_dir <- file.path(repo_root, "reconstructed_sample", "Appendix_Tables")
}

if (!dir.exists(appendix_table_dir)) {
  dir.create(appendix_table_dir, recursive = TRUE)
}
if (!dir.exists(reconstructed_sample_generated_dir)) {
  dir.create(reconstructed_sample_generated_dir, recursive = TRUE)
}

cat("Repository root:", repo_root, "\n")
cat("Appendix table output directory:", appendix_table_dir, "\n")

ensure_table_rows <- function(rows) {
  rows <- as.character(rows)
  needs_slashes <- !grepl("\\\\\\\\s*$", rows)
  rows[needs_slashes] <- paste0(rows[needs_slashes], " \\\\")
  rows
}

write_latex_table <- function(rows,
                              filename,
                              caption,
                              label,
                              col_spec,
                              header,
                              placement = "h",
                              landscape = FALSE,
                              small = FALSE,
                              note = NULL) {
  rows <- ensure_table_rows(rows)
  header <- ensure_table_rows(header)
  lines <- c(
    if (landscape) "\\begin{landscape}",
    sprintf("\\begin{table}[%s]", placement),
    "\\centering",
    if (small) "\\small",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\toprule",
    header,
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}"
  )

  if (!is.null(note)) {
    lines <- c(
      lines,
      "\\vspace{0.5em}",
      "\\begin{minipage}{0.95\\linewidth}",
      paste0("\\footnotesize\\textit{Note:} ", note),
      "\\end{minipage}"
    )
  }

  lines <- c(
    lines,
    "\\end{table}",
    if (landscape) "\\end{landscape}"
  )

  out_path <- file.path(appendix_table_dir, filename)
  writeLines(lines, out_path, useBytes = TRUE)
  cat("Wrote LaTeX table to:", out_path, "\n")
}

match_stats <- function(ct_vals, our_vals, exact_tol = 0.001) {
  ct_ok <- !is.na(ct_vals)
  both_ok <- ct_ok & !is.na(our_vals)

  total <- sum(ct_ok)
  matched <- sum(both_ok)
  exact <- sum(both_ok & abs(our_vals - ct_vals) <= exact_tol)

  match_rate <- if (total > 0) 100 * matched / total else NA_real_
  exact_rate <- if (total > 0) 100 * exact / total else NA_real_
  exact_rate_matched <- if (matched > 0) 100 * exact / matched else NA_real_

  cor_val <- if (matched > 1) {
    cor(our_vals[both_ok], ct_vals[both_ok])
  } else {
    NA_real_
  }

  list(
    total = total,
    matched = matched,
    exact = exact,
    match_rate = match_rate,
    exact_rate = exact_rate,
    exact_rate_matched = exact_rate_matched,
    correlation = cor_val
  )
}

fmt_num <- function(x, digits = 1) {
  if (is.na(x)) "--" else sprintf(paste0("%.", digits, "f"), x)
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

collapse_ct_values <- function(df, key_cols, value_cols) {
  df %>%
    select(all_of(c(key_cols, value_cols))) %>%
    distinct() %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(across(all_of(value_cols), first_non_na), .groups = "drop")
}

normalize_address <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]", " ")
  str_squish(x)
}

make_addr_key <- function(df,
                          addr_col = "HSITEAD",
                          city_col = "HCITY",
                          state_col = "HSTATE",
                          zip_col = "HZIP") {
  safe <- function(x) ifelse(is.na(x), "", x)
  df %>%
    mutate(
      addr_key = normalize_address(paste(
        safe(.data[[addr_col]]),
        safe(.data[[city_col]]),
        safe(.data[[state_col]]),
        safe(.data[[zip_col]]),
        sep = "|"
      ))
    )
}

coverage_stats <- function(our_df, ct_df, by) {
  total <- nrow(our_df)
  matched <- nrow(semi_join(our_df, ct_df, by = by))
  rate <- if (total > 0) 100 * matched / total else NA_real_
  list(total = total, matched = matched, rate = rate)
}

matching_summary <- tibble(
  Dataset = character(),
  N_total = integer(),
  N_matched = integer(),
  Match_rate = double(),
  Exact_rate = double(),
  Exact_rate_matched = double(),
  Correlation = double()
)

# ==============================================================================
# SCHOOL MATCHING VALIDATION
# ==============================================================================

cat("=== SCHOOL MATCHING VALIDATION ===\n")

source(file.path(cleaning_scripts_dir, "school_score_merging.R"))

# Load C&T recommended properties with coordinates
ct_recs <- readRDS(file.path(ct_data_dir, "recsprocessed_JPE.rds")) %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, RecPrice, Sqft_Rec)

cat(sprintf("  C&T recommended properties: %d\n", nrow(ct_recs)))

# Load C&T scores (property-level)
ct_hud <- readRDS(file.path(ct_data_dir, "HUDprocessed_JPE_testscores_042021.rds"))

ct_scores <- ct_hud %>%
  select(CONTROL, TESTERID, RecPrice, Sqft_Rec,
         ct_elem_score = mn_avg_ol_elem_Rec,
         ct_middle_score = mn_avg_ol_middle_Rec) %>%
  distinct()

ct_with_scores <- ct_recs %>%
  left_join(ct_scores,
            by = c("CONTROL", "TESTERID", "RecPrice", "Sqft_Rec"),
            relationship = "many-to-many") %>%
  group_by(CONTROL, TESTERID, SEQRH) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("  Properties with C&T scores: %d\n", nrow(ct_with_scores)))

# Apply our matching function to C&T coordinates
ct_matching_input <- ct_with_scores %>%
  rename(lat = Latitude, long = Longitude)

ct_matched <- merge_school_scores(ct_matching_input, lat_col = "lat", lon_col = "long")

comparison <- ct_matched %>%
  select(CONTROL, TESTERID, SEQRH, ct_elem_score, ct_middle_score,
         our_elem_score = elementary_school_score,
         our_middle_score = middle_school_score)

# Elementary school stats
stats_elem <- match_stats(comparison$ct_elem_score, comparison$our_elem_score, exact_tol = 0.001)
cat(sprintf("Elementary schools: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_elem$matched, stats_elem$total, stats_elem$match_rate,
            stats_elem$exact_rate, stats_elem$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "School scores (Elementary)",
    N_total = stats_elem$total,
    N_matched = stats_elem$matched,
    Match_rate = stats_elem$match_rate,
    Exact_rate = stats_elem$exact_rate,
    Exact_rate_matched = stats_elem$exact_rate_matched,
    Correlation = stats_elem$correlation
  )

# Middle school stats
stats_middle <- match_stats(comparison$ct_middle_score, comparison$our_middle_score, exact_tol = 0.001)
cat(sprintf("Middle schools: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_middle$matched, stats_middle$total, stats_middle$match_rate,
            stats_middle$exact_rate, stats_middle$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "School scores (Middle)",
    N_total = stats_middle$total,
    N_matched = stats_middle$matched,
    Match_rate = stats_middle$match_rate,
    Exact_rate = stats_middle$exact_rate,
    Exact_rate_matched = stats_middle$exact_rate_matched,
    Correlation = stats_middle$correlation
  )

# Save detailed school comparison results
school_validation_path <- file.path(reconstructed_sample_generated_dir, "school_matching_validation.csv")
write_csv(comparison, school_validation_path)
cat("Detailed school validation results saved to:", school_validation_path, "\n")

# ==============================================================================
# GREATSCHOOLS + CRIME COVERAGE (C&T REPLICATION, ALL ROWS)
# ==============================================================================

cat("\n=== GREATSCHOOLS + CRIME COVERAGE ===\n")

our_path <- file.path(reconstructed_sample_generated_dir, "sales_tester_rechomes_merged.csv")
ct_recs_path <- file.path(ct_data_dir, "recsprocessed_JPE.rds")

if (!file.exists(our_path)) {
  cat("Skipping GreatSchools/crime coverage:", our_path, "not found.\n")
} else {
  our_homes <- read_csv(our_path, show_col_types = FALSE)
  ct_recs <- readRDS(ct_recs_path)

  if (!"Elementary_School_Score_Rec" %in% names(ct_recs) && "Elementary_School_Score" %in% names(ct_recs)) {
    ct_recs <- ct_recs %>% mutate(Elementary_School_Score_Rec = Elementary_School_Score)
  }
  if (!"Assault_Rec" %in% names(ct_recs) && "Assault" %in% names(ct_recs)) {
    ct_recs <- ct_recs %>% mutate(Assault_Rec = Assault)
  }
  key_cols <- c("CONTROL", "TESTERID", "SEQRH")
  ct_recs_key <- collapse_ct_values(
    ct_recs,
    key_cols,
    c("Elementary_School_Score_Rec", "Assault_Rec")
  )

  baseline_all <- coverage_stats(our_homes, ct_recs_key, by = key_cols)

  ct_recs_addr <- ct_recs %>%
    select(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP) %>%
    distinct()

  addr_all <- coverage_stats(
    make_addr_key(our_homes),
    make_addr_key(ct_recs_addr),
    by = c("CONTROL", "TESTERID", "addr_key")
  )

  loose_all <- coverage_stats(
    our_homes,
    ct_recs_key %>% select(CONTROL, TESTERID) %>% distinct(),
    by = c("CONTROL", "TESTERID")
  )

  gs_variants <- tibble(
    Method = c(
      "Baseline: CONTROL+TESTERID+SEQRH",
      "Address: CONTROL+TESTERID+HSITEAD+HCITY+HSTATE+HZIP",
      "Loose: CONTROL+TESTERID"
    ),
    All_match = c(baseline_all$rate, addr_all$rate, loose_all$rate)
  )

  gs_rows <- sprintf("%s & %s \\\\",
                     gs_variants$Method,
                     vapply(gs_variants$All_match, fmt_num, character(1), digits = 1))
  if (length(gs_rows) > 0) {
    gs_rows[length(gs_rows)] <- sub("\\\\\\\\$", "", gs_rows[length(gs_rows)])
  }
  write_latex_table(
    gs_rows,
    "greatschools_crime_matching_variants.tex",
    "GreatSchools/crime merge coverage under alternative matching rules",
    "tab:gs-crime-coverage",
    "lr",
    "Matching rule & Homes matched (\\%)"
  )

  # Add coverage rows to master summary (coverage against our dataset)
  merged_vals <- our_homes %>%
    left_join(ct_recs_key, by = key_cols) %>%
    mutate(
      gs_value = Elementary_School_Score_Rec,
      assault_value = Assault_Rec
    )

  gs_total <- nrow(merged_vals)
  gs_matched <- sum(!is.na(merged_vals$gs_value))
  gs_rate <- if (gs_total > 0) 100 * gs_matched / gs_total else NA_real_

  assault_total <- nrow(merged_vals)
  assault_matched <- sum(!is.na(merged_vals$assault_value))
  assault_rate <- if (assault_total > 0) 100 * assault_matched / assault_total else NA_real_

  matching_summary <- matching_summary %>%
    add_row(
      Dataset = "GreatSchools index (C\\&T merge)",
      N_total = gs_total,
      N_matched = gs_matched,
      Match_rate = gs_rate,
      Exact_rate = NA_real_,
      Exact_rate_matched = NA_real_,
      Correlation = NA_real_
    ) %>%
    add_row(
      Dataset = "Crime rate (Assaults, C\\&T merge)",
      N_total = assault_total,
      N_matched = assault_matched,
      Match_rate = assault_rate,
      Exact_rate = NA_real_,
      Exact_rate_matched = NA_real_,
      Correlation = NA_real_
    )
}

# ==============================================================================
# SUPERFUND MATCHING DIAGNOSIS
# ==============================================================================

cat("\n=== SUPERFUND MATCHING DIAGNOSIS ===\n")

source(file.path(cleaning_scripts_dir, "superfund_merging.R"))

recs_path <- file.path(ct_data_dir, "recsprocessed_JPE.rds")
sf_excel_path <- file.path(non_hds_data_dir, "Superfund", "epa-national-priorities-list-ciesin-mod-v2-2014.xls")

# Load replication data
cat("Loading C&T replication data...\n")
recs <- readRDS(recs_path)
valid_idx <- !is.na(recs$Latitude) & !is.na(recs$Longitude) & !is.na(recs$SFcount)
recs <- recs[valid_idx, ]
cat("Valid observations:", nrow(recs), "\n")

# Load Superfund data (full list for diagnostics)
cat("Loading Superfund site data...\n")
sf <- read_excel(sf_excel_path, sheet = "EPA_NPL_Sites_asof_27Feb2014")
sf$year <- as.numeric(format(sf$NPL_STATUS_DATE, "%Y"))

# Masks for year cutoffs
mask_2011 <- sf$year < 2011
mask_2012 <- sf$year < 2012
mask_2013 <- sf$year < 2013

# Masks for status filters (using 2012 cutoff)
mask_final <- sf$NPL_STATUS == "Currently on the Final NPL"
mask_deleted <- sf$NPL_STATUS == "Deleted from the Final NPL"
mask_proposed <- sf$NPL_STATUS == "Proposed for NPL"
mask_final_only <- mask_2012 & mask_final
mask_final_deleted <- mask_2012 & (mask_final | mask_deleted)
mask_final_proposed <- mask_2012 & (mask_final | mask_proposed)

# Allocate vectors
n <- nrow(recs)
calc_2012_5 <- integer(n)
calc_2012_5mi <- integer(n)
calc_2012_4p9 <- integer(n)
calc_2012_5p1 <- integer(n)
calc_2012_5p2 <- integer(n)
calc_2011_5 <- integer(n)
calc_2013_5 <- integer(n)
calc_final_only <- integer(n)
calc_final_deleted <- integer(n)
calc_final_proposed <- integer(n)

calc_eq_2012_5 <- integer(n)
calc_square_2012_5 <- integer(n)
calc_manh_2012_5 <- integer(n)

miles_to_km <- 1.60934
progress_interval <- max(1, floor(n / 20))

cat("Computing distances and counts...\n")
for (i in 1:n) {
  lon0 <- recs$Longitude[i]
  lat0 <- recs$Latitude[i]

  # Haversine distances (km)
  d_hav <- haversine_km(lon0, lat0, sf$LONGITUDE, sf$LATITUDE)

  calc_2012_5[i] <- sum(d_hav[mask_2012] <= 5, na.rm = TRUE)
  calc_2012_5mi[i] <- sum(d_hav[mask_2012] <= 5 * miles_to_km, na.rm = TRUE)
  calc_2012_4p9[i] <- sum(d_hav[mask_2012] <= 4.9, na.rm = TRUE)
  calc_2012_5p1[i] <- sum(d_hav[mask_2012] <= 5.1, na.rm = TRUE)
  calc_2012_5p2[i] <- sum(d_hav[mask_2012] <= 5.2, na.rm = TRUE)
  calc_2011_5[i] <- sum(d_hav[mask_2011] <= 5, na.rm = TRUE)
  calc_2013_5[i] <- sum(d_hav[mask_2013] <= 5, na.rm = TRUE)

  calc_final_only[i] <- sum(d_hav[mask_final_only] <= 5, na.rm = TRUE)
  calc_final_deleted[i] <- sum(d_hav[mask_final_deleted] <= 5, na.rm = TRUE)
  calc_final_proposed[i] <- sum(d_hav[mask_final_proposed] <= 5, na.rm = TRUE)

  # Planar approximations in km (local scaling)
  lat_rad <- lat0 * pi / 180
  dx <- (sf$LONGITUDE - lon0) * 111.32 * cos(lat_rad)
  dy <- (sf$LATITUDE - lat0) * 111.32
  d_eq <- sqrt(dx^2 + dy^2)

  calc_eq_2012_5[i] <- sum(d_eq[mask_2012] <= 5, na.rm = TRUE)
  calc_square_2012_5[i] <- sum(abs(dx[mask_2012]) <= 5 & abs(dy[mask_2012]) <= 5, na.rm = TRUE)
  calc_manh_2012_5[i] <- sum((abs(dx[mask_2012]) + abs(dy[mask_2012])) <= 5, na.rm = TRUE)

  if (i %% progress_interval == 0) {
    cat("  Processed", i, "/", n, "\n")
  }
}

sfcount <- recs$SFcount

summarize_match <- function(calc_vec) {
  exact <- mean(calc_vec == sfcount) * 100
  within1 <- mean(abs(calc_vec - sfcount) <= 1) * 100
  return(c(exact = exact, within1 = within1))
}

results <- rbind(
  c("Baseline: Haversine, 5 km, NPL<2012", summarize_match(calc_2012_5)),
  c("Haversine, 5 miles, NPL<2012", summarize_match(calc_2012_5mi)),
  c("Haversine, 4.9 km, NPL<2012", summarize_match(calc_2012_4p9)),
  c("Haversine, 5.1 km, NPL<2012", summarize_match(calc_2012_5p1)),
  c("Haversine, 5.2 km, NPL<2012", summarize_match(calc_2012_5p2)),
  c("Haversine, 5 km, NPL<2011", summarize_match(calc_2011_5)),
  c("Haversine, 5 km, NPL<2013", summarize_match(calc_2013_5)),
  c("Haversine, 5 km, Final only", summarize_match(calc_final_only)),
  c("Haversine, 5 km, Final+Deleted", summarize_match(calc_final_deleted)),
  c("Haversine, 5 km, Final+Proposed", summarize_match(calc_final_proposed)),
  c("Planar circle, 5 km, NPL<2012", summarize_match(calc_eq_2012_5)),
  c("Axis-aligned square, 5 km", summarize_match(calc_square_2012_5)),
  c("Manhattan diamond, 5 km", summarize_match(calc_manh_2012_5))
)

results <- data.frame(
  Method = results[, 1],
  Exact_Match = as.numeric(results[, 2]),
  Within_1 = as.numeric(results[, 3]),
  stringsAsFactors = FALSE
)

results$Exact_Match <- round(results$Exact_Match, 2)
results$Within_1 <- round(results$Within_1, 2)

# Render comparison symbols reliably in LaTeX tables
results$Method <- gsub("<", "$<$", results$Method, fixed = TRUE)

cat("\n=== Match Rate Summary (percent) ===\n")
print(results, row.names = FALSE)

superfund_rows <- sprintf("%s & %.1f & %.1f \\\\",
                          results$Method, results$Exact_Match, results$Within_1)
if (length(superfund_rows) > 0) {
  superfund_rows[length(superfund_rows)] <- sub("\\\\\\\\$", "", superfund_rows[length(superfund_rows)])
}
write_latex_table(
  superfund_rows,
  "superfund_matching_variants.tex",
  "Superfund matching validation against C\\&T replication data",
  "tab:superfund-matching",
  "lrr",
  "Specification & Exact match (\\%) & Within +/-1 (\\%)"
)

# Add baseline superfund results to the master summary
stats_sf <- match_stats(sfcount, calc_2012_5, exact_tol = 0)
cat(sprintf("Superfund baseline: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_sf$matched, stats_sf$total, stats_sf$match_rate,
            stats_sf$exact_rate, stats_sf$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "Superfund count (5 km)",
    N_total = stats_sf$total,
    N_matched = stats_sf$matched,
    Match_rate = stats_sf$match_rate,
    Exact_rate = stats_sf$exact_rate,
    Exact_rate_matched = stats_sf$exact_rate_matched,
    Correlation = stats_sf$correlation
  )

# ==============================================================================
# RSEI (TRI TOXIC CONCENTRATION) MATCHING DIAGNOSIS
# ==============================================================================

cat("\n=== RSEI MATCHING DIAGNOSIS ===\n")

source(file.path(cleaning_scripts_dir, "rsei_merging.R"))

ct_rsei <- readRDS(file.path(ct_data_dir, "recsprocessed_JPE.rds")) %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, RSEI) %>%
  filter(!is.na(RSEI), !is.na(Latitude), !is.na(Longitude))

cat("  C&T properties with RSEI and coordinates:", nrow(ct_rsei), "\n")

ct_rsei_input <- ct_rsei %>%
  rename(lat = Latitude, long = Longitude)

grid_lookup <- build_rsei_grid_lookup(
  agg_path = file.path(non_hds_data_dir, "RSEI", "aggmicro2022_2012.csv"),
  agg_cache = file.path(non_hds_data_dir, "RSEI", "rsei_agg_2012.rds"),
  lookup_cache = file.path(non_hds_data_dir, "RSEI", "rsei_grid_lookup_2012.rds")
)

grid_wgs <- prepare_rsei_grid(
  lookup = grid_lookup,
  distance_crs = NULL,
  coords_cache = file.path(non_hds_data_dir, "RSEI", "rsei_grid_coords_wgs84_2012.rds")
)

grid_albers <- prepare_rsei_grid(
  lookup = grid_lookup,
  distance_crs = 5070,
  coords_cache = file.path(non_hds_data_dir, "RSEI", "rsei_grid_coords_5070_2012.rds")
)

variants <- tibble(
  Method = c(
    "Centroid NN (WGS84), toxconc",
    "Centroid NN (WGS84), ctconc",
    "Centroid NN (WGS84), nctconc",
    "Centroid NN (WGS84), score",
    "Centroid NN (EPSG:5070), toxconc",
    "Centroid NN (EPSG:5070), ctconc",
    "Centroid NN (EPSG:5070), nctconc",
    "Centroid NN (EPSG:5070), score"
  ),
  grid = c(
    rep("wgs", 4),
    rep("albers", 4)
  ),
  value_col = rep(c("toxconc", "ctconc", "nctconc", "score"), 2)
)

run_rsei_variant <- function(grid_prep, value_col) {
  matched <- match_rsei_centroid(
    ct_rsei_input,
    grid = grid_prep$lookup,
    coords = grid_prep$coords,
    lat_col = "lat",
    lon_col = "long",
    value_cols = value_col,
    prefix = "rsei_",
    keep_cell = FALSE,
    distance_crs = grid_prep$distance_crs
  )

  our_vals <- matched[[paste0("rsei_", value_col)]]
  stats <- match_stats(ct_rsei$RSEI, our_vals, exact_tol = 0.001)
  pearson_log <- cor(log1p(our_vals), log1p(ct_rsei$RSEI), use = "pairwise.complete.obs")
  spearman <- cor(our_vals, ct_rsei$RSEI, use = "pairwise.complete.obs", method = "spearman")

  list(matched = matched, stats = stats, our_vals = our_vals,
       pearson_log = pearson_log, spearman = spearman)
}

variant_results <- vector("list", nrow(variants))
for (i in seq_len(nrow(variants))) {
  grid_prep <- if (variants$grid[i] == "wgs") grid_wgs else grid_albers
  variant_results[[i]] <- run_rsei_variant(grid_prep, variants$value_col[i])
}

variants$Exact_Match_raw <- vapply(variant_results, function(x) x$stats$exact_rate, numeric(1))
variants$Match_rate <- vapply(variant_results, function(x) x$stats$match_rate, numeric(1))
variants$Exact_rate_matched <- vapply(variant_results, function(x) x$stats$exact_rate_matched, numeric(1))
variants$Pearson_raw <- vapply(variant_results, function(x) x$stats$correlation, numeric(1))
variants$Log1p_raw <- vapply(variant_results, function(x) x$pearson_log, numeric(1))
variants$Spearman_raw <- vapply(variant_results, function(x) x$spearman, numeric(1))

variants$Exact_Match <- round(variants$Exact_Match_raw, 2)
variants$Pearson <- round(variants$Pearson_raw, 4)
variants$Log1p <- round(variants$Log1p_raw, 4)
variants$Spearman <- round(variants$Spearman_raw, 4)
variants$Method <- gsub("<", "$<$", variants$Method, fixed = TRUE)

cat("\n=== RSEI Match Rate Summary ===\n")
print(variants[, c("Method", "Exact_Match", "Pearson", "Log1p", "Spearman")], row.names = FALSE)

rsei_rows <- sprintf("%s & %.1f & %.3f & %.3f & %.3f \\\\",
                     variants$Method, variants$Exact_Match,
                     variants$Pearson, variants$Log1p, variants$Spearman)
if (length(rsei_rows) > 0) {
  rsei_rows[length(rsei_rows)] <- sub("\\\\\\\\$", "", rsei_rows[length(rsei_rows)])
}
write_latex_table(
  rsei_rows,
  "rsei_matching_variants.tex",
  "RSEI matching validation against C\\&T replication data (2012 grid)",
  "tab:rsei-matching",
  "lrrrr",
  "Specification & Exact match (\\%) & Pearson r & log1p Pearson r & Spearman r"
)

best_idx <- variants %>%
  mutate(idx = row_number(),
         best_score = ifelse(is.na(Pearson_raw), -Inf, Pearson_raw)) %>%
  arrange(desc(best_score), desc(Exact_Match_raw), desc(Match_rate)) %>%
  slice(1) %>%
  pull(idx)

best_variant <- variants[best_idx, ]
best_stats <- variant_results[[best_idx]]$stats

best_grid_prep <- if (best_variant$grid == "wgs") grid_wgs else grid_albers
best_matched <- run_rsei_variant(best_grid_prep, best_variant$value_col)$matched

best_value <- paste0("rsei_", best_variant$value_col)
comparison_rsei <- best_matched %>%
  select(CONTROL, TESTERID, SEQRH, RSEI,
         our_rsei = all_of(best_value))

rsei_validation_path <- file.path(reconstructed_sample_generated_dir, "rsei_matching_validation.csv")
write_csv(comparison_rsei, rsei_validation_path)
cat("Detailed RSEI validation results saved to:", rsei_validation_path, "\n")

cat(sprintf("RSEI best match: %s (matched %d/%d, exact %.1f%%, r = %.4f)\n",
            best_variant$Method, best_stats$matched, best_stats$total,
            best_stats$exact_rate, best_stats$correlation))
cat(sprintf("RSEI best match (log1p Pearson = %.4f, Spearman = %.4f)\n",
            variants$Log1p_raw[best_idx], variants$Spearman_raw[best_idx]))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "RSEI toxic concentration",
    N_total = best_stats$total,
    N_matched = best_stats$matched,
    Match_rate = best_stats$match_rate,
    Exact_rate = best_stats$exact_rate,
    Exact_rate_matched = best_stats$exact_rate_matched,
    Correlation = best_stats$correlation
  )

# ==============================================================================
# PM2.5 MATCHING DIAGNOSIS
# ==============================================================================

cat("\n=== PM2.5 MATCHING DIAGNOSIS ===\n")

source(file.path(cleaning_scripts_dir, "pm25_merging.R"))

pm25_path_v5 <- file.path(non_hds_data_dir, "PM2_5", "V5.NA.04.02", "V5NA04.02.HybridPM25.NorthAmerica.2012001-2012364.nc")
pm25_path_v4 <- file.path(non_hds_data_dir, "PM2_5", "V4.NA.02", "GWRwSPEC_PM25_NA_201201_201212-RH35.nc")
pm25_path_v4_maple <- file.path(non_hds_data_dir, "PM2_5", "V4.NA.02", "GWRwSPEC.HEI.ELEVandURB_PM25_NA_201201_201212-RH35.nc")
pm25_path_v403 <- file.path(non_hds_data_dir, "PM2_5", "V4.NA.02", "V4NA03_PM25_NA_201201_201212-RH35.nc")

ct_pm25 <- readRDS(file.path(ct_data_dir, "recsprocessed_JPE.rds")) %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, PM25_Rec)

cat(sprintf("  C&T PM2.5 observations: %d\n", nrow(ct_pm25)))

pm25_grid <- load_pm25_grid(
  pm25_path_v5,
  nc_pm_var = "GWRPM25",
  nc_lat_var = "lat",
  nc_lon_var = "lon"
)
pm25_scale <- pm25_grid$scale_factor_default

# Baseline: nearest-neighbor lookup, rounded to 1 decimal (V5)
pm25_baseline <- pm25_lookup(
  lat_vals = ct_pm25$Latitude,
  lon_vals = ct_pm25$Longitude,
  grid = pm25_grid,
  method = "nearest",
  scale_factor = pm25_scale,
  round_digits = 1,
  round_mode = "even"
)

stats_pm25 <- match_stats(ct_pm25$PM25_Rec, pm25_baseline, exact_tol = 0.001)
cat(sprintf("PM2.5 baseline: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_pm25$matched, stats_pm25$total, stats_pm25$match_rate,
            stats_pm25$exact_rate, stats_pm25$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "PM2.5 (Recommended, V5)",
    N_total = stats_pm25$total,
    N_matched = stats_pm25$matched,
    Match_rate = stats_pm25$match_rate,
    Exact_rate = stats_pm25$exact_rate,
    Exact_rate_matched = stats_pm25$exact_rate_matched,
    Correlation = stats_pm25$correlation
  )

# Methodological variants for PM2.5 matching (V5 baseline)
pm25_variants <- list(
  list(label = "Nearest (round 0.1)", method = "nearest", round_digits = 1, round_mode = "even", coord_round = FALSE, lat_shift = 0, lon_shift = 0, grid = pm25_grid),
  list(label = "Nearest (round 0.1, half-up)", method = "nearest", round_digits = 1, round_mode = "half_up", coord_round = FALSE, lat_shift = 0, lon_shift = 0, grid = pm25_grid),
  list(label = "Bilinear (round 0.1)", method = "bilinear", round_digits = 1, round_mode = "even", coord_round = FALSE, lat_shift = 0, lon_shift = 0, grid = pm25_grid),
  list(label = "Nearest (shift -0.005,+0.005)", method = "nearest", round_digits = 1, round_mode = "even", coord_round = FALSE, lat_shift = -0.005, lon_shift = 0.005, grid = pm25_grid)
)

pm25_results <- do.call(rbind, lapply(pm25_variants, function(v) {
  lat_vals <- ct_pm25$Latitude
  lon_vals <- ct_pm25$Longitude
  if (v$coord_round) {
    lat_vals <- round(lat_vals, 2)
    lon_vals <- round(lon_vals, 2)
  }
  if (!is.null(v$lat_shift) && !is.null(v$lon_shift)) {
    lat_vals <- lat_vals + v$lat_shift
    lon_vals <- lon_vals + v$lon_shift
  }

  rd <- v$round_digits
  if (is.na(rd)) rd <- NULL

  vals <- pm25_lookup(
    lat_vals = lat_vals,
    lon_vals = lon_vals,
    grid = v$grid,
    method = v$method,
    scale_factor = v$grid$scale_factor_default,
    round_digits = rd,
    round_mode = v$round_mode
  )

  stats <- match_stats(ct_pm25$PM25_Rec, vals, exact_tol = 0.001)

  data.frame(
    Method = v$label,
    Match_rate = stats$match_rate,
    Exact_rate = stats$exact_rate,
    Correlation = stats$correlation,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== PM2.5 Match Rate Summary (percent) ===\n")
print(pm25_results, row.names = FALSE)

pm25_rows <- sprintf("%s & %s & %s & %s \\\\",
                     pm25_results$Method,
                     vapply(pm25_results$Match_rate, fmt_num, character(1), digits = 1),
                     vapply(pm25_results$Exact_rate, fmt_num, character(1), digits = 1),
                     vapply(pm25_results$Correlation, fmt_num, character(1), digits = 3))
if (length(pm25_rows) > 0) {
  pm25_rows[length(pm25_rows)] <- sub("\\\\\\\\$", "", pm25_rows[length(pm25_rows)])
}
write_latex_table(
  pm25_rows,
  "pm25_matching_variants.tex",
  "PM2.5 matching variants (V5 baseline) against C\\&T replication data",
  "tab:pm25-matching",
  "lrrr",
  "Specification & Match rate (\\%) & Exact match (\\%) & Correlation"
)

# Dataset comparison across available PM2.5 products (nearest, round 0.1)
pm25_datasets <- list(
  list(label = "V5 HybridPM25 (baseline)", path = pm25_path_v5, nc_pm_var = "GWRPM25", nc_lat_var = "lat", nc_lon_var = "lon"),
  list(label = "V4.NA.02 GWRwSPEC", path = pm25_path_v4, nc_pm_var = "PM25", nc_lat_var = "LAT", nc_lon_var = "LON"),
  list(label = "V4.NA.02 MAPLE (HEI.ELEVandURB)", path = pm25_path_v4_maple, nc_pm_var = "PM25", nc_lat_var = "LAT", nc_lon_var = "LON"),
  list(label = "V4.NA.03", path = pm25_path_v403, nc_pm_var = "PM25", nc_lat_var = "LAT", nc_lon_var = "LON")
)

pm25_dataset_results <- do.call(rbind, lapply(pm25_datasets, function(d) {
  if (!file.exists(d$path)) {
    return(data.frame(Method = d$label, Match_rate = NA_real_, Exact_rate = NA_real_, Correlation = NA_real_))
  }

  grid_d <- load_pm25_grid(d$path, nc_pm_var = d$nc_pm_var, nc_lat_var = d$nc_lat_var, nc_lon_var = d$nc_lon_var)
  vals <- pm25_lookup(ct_pm25$Latitude, ct_pm25$Longitude, grid_d, method = "nearest", round_digits = 1)
  stats <- match_stats(ct_pm25$PM25_Rec, vals, exact_tol = 0.001)

  data.frame(
    Method = d$label,
    Match_rate = stats$match_rate,
    Exact_rate = stats$exact_rate,
    Correlation = stats$correlation,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== PM2.5 Dataset Comparison (percent) ===\n")
print(pm25_dataset_results, row.names = FALSE)

pm25_dataset_rows <- sprintf("%s & %s & %s & %s \\\\",
                             pm25_dataset_results$Method,
                             vapply(pm25_dataset_results$Match_rate, fmt_num, character(1), digits = 1),
                             vapply(pm25_dataset_results$Exact_rate, fmt_num, character(1), digits = 1),
                             vapply(pm25_dataset_results$Correlation, fmt_num, character(1), digits = 3))
if (length(pm25_dataset_rows) > 0) {
  pm25_dataset_rows[length(pm25_dataset_rows)] <- sub("\\\\\\\\$", "", pm25_dataset_rows[length(pm25_dataset_rows)])
}
write_latex_table(
  pm25_dataset_rows,
  "pm25_dataset_comparison.tex",
  "PM2.5 dataset comparison against C\\&T replication data",
  "tab:pm25-datasets",
  "lrrr",
  "Dataset & Match rate (\\%) & Exact match (\\%) & Correlation"
)

# ==============================================================================
# COORDINATE VALIDATION (OPTIONAL)
# ==============================================================================

cat("\n=== COORDINATE VALIDATION (OPTIONAL) ===\n")
geocode_path <- file.path(reconstructed_sample_generated_dir, "sales_tester_rechomes_geocoded.csv")

if (file.exists(geocode_path)) {
  geo <- read_csv(geocode_path, show_col_types = FALSE)
  needed_keys <- c("CONTROL", "TESTERID", "SEQRH")

  if (!all(needed_keys %in% names(geo)) || !all(needed_keys %in% names(ct_recs))) {
    cat("Skipping coordinate validation: missing join keys (CONTROL/TESTERID/SEQRH).\n")
  } else if (!all(c("lat", "long") %in% names(geo))) {
    cat("Skipping coordinate validation: missing lat/long columns in geocoded data.\n")
  } else {
    geo_joined <- geo %>%
      select(all_of(needed_keys), lat, long) %>%
      inner_join(ct_recs, by = needed_keys)

    cat("Matched rows for coordinate check:", nrow(geo_joined), "\n")

    dist_km <- haversine_km(
      geo_joined$long,
      geo_joined$lat,
      geo_joined$Longitude,
      geo_joined$Latitude
    )

    dist_m <- dist_km * 1000
    coord_summary <- tibble(
      N = length(dist_m),
      Mean_m = mean(dist_m, na.rm = TRUE),
      Median_m = median(dist_m, na.rm = TRUE),
      Pct_within_50m = mean(dist_m <= 50, na.rm = TRUE) * 100,
      Pct_within_100m = mean(dist_m <= 100, na.rm = TRUE) * 100,
      Pct_within_500m = mean(dist_m <= 500, na.rm = TRUE) * 100,
      Pct_within_1km = mean(dist_m <= 1000, na.rm = TRUE) * 100
    )

    coord_row <- sprintf(
      "%d & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f",
      coord_summary$N,
      coord_summary$Mean_m,
      coord_summary$Median_m,
      coord_summary$Pct_within_50m,
      coord_summary$Pct_within_100m,
      coord_summary$Pct_within_500m,
      coord_summary$Pct_within_1km
    )
    write_latex_table(
      coord_row,
      "coordinate_match.tex",
      "Geocoding alignment with C\\&T coordinates",
      "tab:coordinate-match",
      "rrrrrrr",
      "N & Mean distance (m) & Median distance (m) & Within 50m (\\%) & Within 100m (\\%) & Within 500m (\\%) & Within 1 km (\\%)",
      placement = "p",
      landscape = TRUE,
      small = TRUE
    )
  }
} else {
  cat("Skipping coordinate validation:", geocode_path, "not found.\n")
}

# ==============================================================================
# MASTER SUMMARY TABLE
# ==============================================================================

summary_rows <- sprintf(
  "%s & %d & %d & %s & %s & %s & %s \\\\",
  matching_summary$Dataset,
  matching_summary$N_total,
  matching_summary$N_matched,
  vapply(matching_summary$Match_rate, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Exact_rate, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Exact_rate_matched, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Correlation, fmt_num, character(1), digits = 3)
)
if (length(summary_rows) > 0) {
  summary_rows[length(summary_rows)] <- sub("\\\\\\\\$", "", summary_rows[length(summary_rows)])
}

write_latex_table(
  summary_rows,
  "matching_summary.tex",
  "External merge validation summary against C\\&T replication data",
  "tab:matching-summary",
  "lrrrrrr",
  "Dataset & N (C\\&T) & N matched & Match rate (\\%) & Exact match (\\%) & Exact match (matched \\%) & Correlation",
  placement = "p",
  landscape = TRUE,
  small = TRUE,
  note = "N (C\\&T) is the number of C\\&T observations with a non-missing value. N matched is the number of those observations where our reconstruction also has a non-missing value. Match rate equals N matched divided by N (C\\&T). Exact match is computed within the tolerance used by the validation script. Correlation is computed on the matched subset. For the GreatSchools/crime merge, exact-match and correlation columns are left blank because values are imported directly from the replication archive."
)

cat("\nValidation complete!\n")
