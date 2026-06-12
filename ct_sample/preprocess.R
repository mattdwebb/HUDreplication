# Match block groups to place/county names and de-duplicate adsprocessed/HUD datasets
# before saving corrected outputs.

# Load necessary libraries
library(readr)
library(dplyr)
library(tigris)
library(sf)
library(haven)
library(stringr)

# Important! Set sf to not use the spherical s2 geometry, opting instead for planar geometry to reduce computation time with negligible accuracy changes.
sf_use_s2(FALSE)

resolve_repo_root <- function() {
  env_root <- Sys.getenv("HUD_REPLICATION_ROOT", Sys.getenv("REPO_ROOT", ""))
  if (nzchar(env_root)) {
    env_root <- normalizePath(env_root, winslash = "/", mustWork = TRUE)
    if (dir.exists(file.path(env_root, "ct_sample")) && dir.exists(file.path(env_root, "Data"))) {
      return(env_root)
    }
    stop("HUD_REPLICATION_ROOT/REPO_ROOT does not point to the HUDReplication repository.")
  }

  cmd_args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  start_dirs <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (length(file_arg) == 1) {
    script_arg <- sub("^--file=", "", file_arg)
    script_path <- normalizePath(script_arg, winslash = "/", mustWork = TRUE)
    start_dirs <- c(dirname(script_path), start_dirs)
  }

  for (start_dir in unique(start_dirs)) {
    candidate <- start_dir
    while (candidate != dirname(candidate)) {
      if (dir.exists(file.path(candidate, "ct_sample")) && dir.exists(file.path(candidate, "Data"))) {
        return(candidate)
      }
      candidate <- dirname(candidate)
    }
  }

  stop("Could not infer HUDReplication repository root. Run from HUDReplication/ or set HUD_REPLICATION_ROOT.")
}

REPO_ROOT <- resolve_repo_root()

# Define the C&T-sample output folder and shared generated root
generated_root <- file.path(REPO_ROOT, "Data", "Generated")
output_folder <- file.path(generated_root, "ct_sample")
input_folder <- file.path(REPO_ROOT, "Data", "CT2022_Replication_Data")

# Place/county fixed-effect construction is no longer used in the paper.
# Keep the implementation below for reference, but do not run the expensive
# spatial joins or require place/county columns in generated C&T-sample inputs.
ENABLE_PLACE_COUNTY_FIXED_EFFECTS <- FALSE

ensure_output_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Helper functions for de-duplication
normalize_text <- function(x) {
  x <- iconv(x, to = "UTF-8", sub = "")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  str_squish(x)
}

normalize_geoid <- function(x, width = 15) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  x_chr <- ifelse(
    !is.na(x_num),
    format(round(x_num), scientific = FALSE, trim = TRUE),
    x_chr
  )
  x_chr <- str_replace_all(x_chr, "[^0-9]", "")
  x_chr[x_chr == ""] <- NA_character_
  str_pad(x_chr, width = width, side = "left", pad = "0")
}

new_dedup_log <- function() {
  data.frame(
    step = character(),
    before = integer(),
    after = integer(),
    dropped = integer(),
    stringsAsFactors = FALSE
  )
}

dedup_log <- new_dedup_log()

write_dedup_log <- function(output_dir, log_name) {
  cat("\nDe-duplication summary:\n")
  print(dedup_log)
  write.csv(dedup_log, file.path(output_dir, log_name), row.names = FALSE)
}

prepare_stata_dataset <- function(dataset) {
  dataset %>%
    rename_all(~ gsub("\\.$", "", .)) %>%
    rename_all(~ gsub("\\.", "_", .)) %>%
    rename_all(~ gsub("\\-", "_", .)) %>%
    rename_all(~ gsub("[^a-zA-Z0-9_]", "_", .)) %>%
    rename_all(~ ifelse(nchar(.) > 32, substr(., 1, 32), .)) %>%
    select_if(~ !is.list(.))
}

filter_processed_geo <- function(dataset, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(dataset))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns for", label, ":", paste(missing_cols, collapse = ", ")))
  }

  rows_before <- nrow(dataset)
  keep <- Reduce(`&`, lapply(required_cols, function(col) {
    x <- dataset[[col]]
    x_chr <- str_trim(as.character(x))
    !is.na(x) & !(x_chr %in% c("", ".", "NA"))
  }))
  filtered_dataset <- dataset[keep, , drop = FALSE]
  rows_after <- nrow(filtered_dataset)

  cat(
    "Filtered", label, "rows with invalid geography:",
    rows_before - rows_after, "dropped;", rows_after, "remaining.\n"
  )

  filtered_dataset
}

write_ads_outputs <- function(dataset, output_dir, file_stem, csv_na = "NA") {
  ensure_output_dir(output_dir)
  
  rds_output_path <- file.path(output_dir, paste0(file_stem, ".rds"))
  saveRDS(dataset, rds_output_path)
  cat("Saved processed data to RDS:", rds_output_path, "\n")
  
  csv_output_path <- file.path(output_dir, paste0(file_stem, ".csv"))
  write.csv(dataset, csv_output_path, row.names = FALSE, na = csv_na)
  cat("Saved processed data to CSV format:", csv_output_path, "\n")
  
  list_columns <- names(dataset)[sapply(dataset, is.list)]
  cat("Removing the following list columns for Stata compatibility:\n")
  if (length(list_columns) > 0) {
    cat(paste("- ", list_columns, collapse = "\n"), "\n")
  } else {
    cat("No list columns found to remove.\n")
  }
  
  dataset_stata <- prepare_stata_dataset(dataset)
  if ("TesterID" %in% names(dataset_stata)) {
    dataset_stata <- dataset_stata %>% select(-TesterID)
  }
  
  dta_output_path <- file.path(output_dir, paste0(file_stem, ".dta"))
  haven::write_dta(dataset_stata, dta_output_path)
  cat("Saved processed data to Stata format:", dta_output_path, "\n")
}

write_hud_outputs <- function(hud_census_with_place,
                              hud_names_with_place,
                              hud_testscores_with_place,
                              output_dir,
                              file_suffix,
                              csv_na = "NA") {
  ensure_output_dir(output_dir)
  
  cat("\nSaving merged datasets...\n")
  saveRDS(hud_census_with_place, file.path(output_dir, paste0("HUDprocessed_census_correct_cities_", file_suffix, ".rds")))
  saveRDS(hud_names_with_place, file.path(output_dir, paste0("HUDprocessed_names_correct_cities_", file_suffix, ".rds")))
  saveRDS(hud_testscores_with_place, file.path(output_dir, paste0("HUDprocessed_testscores_correct_cities_", file_suffix, ".rds")))
  cat("Merged datasets saved to RDS format\n")
  
  cat("\nSaving merged datasets to CSV format...\n")
  write.csv(hud_census_with_place, file.path(output_dir, paste0("HUDprocessed_census_correct_cities_", file_suffix, ".csv")), row.names = FALSE, na = csv_na)
  write.csv(hud_names_with_place, file.path(output_dir, paste0("HUDprocessed_names_correct_cities_", file_suffix, ".csv")), row.names = FALSE, na = csv_na)
  write.csv(hud_testscores_with_place, file.path(output_dir, paste0("HUDprocessed_testscores_correct_cities_", file_suffix, ".csv")), row.names = FALSE, na = csv_na)
  cat("Merged datasets saved to CSV format\n")
  
  cat("\nPreparing census dataset for Stata...\n")
  hud_census_stata <- prepare_stata_dataset(hud_census_with_place)
  
  cat("Preparing names dataset for Stata...\n")
  hud_names_stata <- prepare_stata_dataset(hud_names_with_place)
  
  cat("Preparing testscores dataset for Stata...\n")
  hud_testscores_stata <- prepare_stata_dataset(hud_testscores_with_place)
  
  haven::write_dta(hud_census_stata, file.path(output_dir, paste0("HUDprocessed_census_correct_cities_", file_suffix, ".dta")))
  haven::write_dta(hud_names_stata, file.path(output_dir, paste0("HUDprocessed_names_correct_cities_", file_suffix, ".dta")))
  haven::write_dta(hud_testscores_stata, file.path(output_dir, paste0("HUDprocessed_testscores_correct_cities_", file_suffix, ".dta")))
  cat("Merged datasets saved to Stata format\n")
}

log_dedup <- function(label, before, after) {
  dedup_log <<- rbind(
    dedup_log,
    data.frame(
      step = label,
      before = before,
      after = after,
      dropped = before - after,
      stringsAsFactors = FALSE
    )
  )
}

build_address_key <- function(df) {
  addr_raw <- if ("HSITEAD" %in% names(df)) df$HSITEAD else NA
  addr_alt <- if ("Address" %in% names(df)) df$Address else NA
  addr_raw <- ifelse(!is.na(addr_raw) & addr_raw != "", addr_raw, addr_alt)
  
  unit_raw <- if ("HUNITNO" %in% names(df)) df$HUNITNO else ""
  
  city_raw <- if ("HCITY" %in% names(df)) df$HCITY else NA
  city_alt <- if ("City" %in% names(df)) df$City else NA
  city_raw <- ifelse(!is.na(city_raw) & city_raw != "", city_raw, city_alt)
  
  state_raw <- if ("HSTATE" %in% names(df)) df$HSTATE else NA
  state_alt <- if ("State" %in% names(df)) df$State else NA
  state_raw <- ifelse(!is.na(state_raw) & state_raw != "", state_raw, state_alt)
  
  zip_raw <- if ("HZIP" %in% names(df)) df$HZIP else NA
  zip_alt <- if ("Zip_Code" %in% names(df)) df$Zip_Code else NA
  zip_raw <- ifelse(!is.na(zip_raw) & zip_raw != "", zip_raw, zip_alt)
  
  paste(
    normalize_text(addr_raw),
    normalize_text(unit_raw),
    normalize_text(city_raw),
    normalize_text(state_raw),
    normalize_text(zip_raw)
  )
}

dedup_exact_ignore <- function(df, ignore_cols, label) {
  before <- nrow(df)
  if (length(ignore_cols) > 0) {
    df <- df %>% distinct(across(-all_of(ignore_cols)), .keep_all = TRUE)
  } else {
    df <- df %>% distinct()
  }
  after <- nrow(df)
  cat(label, "- exact de-dup (ignoring", paste(ignore_cols, collapse = ", "), "):", before, "->", after, "\n")
  log_dedup(label, before, after)
  df
}

dedup_by_key <- function(df, key_cols, label) {
  if (length(key_cols) == 0) return(df)
  before <- nrow(df)
  df <- df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  after <- nrow(df)
  cat(label, "- key de-dup on", paste(key_cols, collapse = ", "), ":", before, "->", after, "\n")
  log_dedup(label, before, after)
  df
}

merge_canonical_ads_into_hud <- function(dataset, dataset_name, canonical_hud_lookup) {
  cat("\nMerging canonical advertised-home values into", dataset_name, "...\n")

  missing_join_cols <- setdiff(c("CONTROL", "TESTERID"), names(dataset))
  if (length(missing_join_cols) > 0) {
    stop(paste("Missing required join columns in", dataset_name, ":", paste(missing_join_cols, collapse = ", ")))
  }

  overlapping_cols <- intersect(setdiff(names(canonical_hud_lookup), c("CONTROL", "TESTERID")), names(dataset))
  rows_before <- nrow(dataset)

  merged_dataset <- dataset %>%
    mutate(TESTERID = as.character(TESTERID)) %>%
    select(-any_of(overlapping_cols)) %>%
    inner_join(canonical_hud_lookup, by = c("CONTROL", "TESTERID"))

  cat("Rows before canonical advertised-home merge:", rows_before, "\n")
  cat("Rows after canonical advertised-home merge:", nrow(merged_dataset), "\n")
  cat("Rows dropped because no canonical advertised-home assignment was available:",
      rows_before - nrow(merged_dataset), "\n")

  merged_dataset
}

# Load the adsprocessed_JPE data
adsprocessed_data <- readRDS(paste0(input_folder, "/adsprocessed_JPE_censor.rds"))

# De-duplicate adsprocessed data
ads_index_cols <- intersect(c("X.x", "X.y", "X.x.1", "X.y.1", "SEQRH"), names(adsprocessed_data))
adsprocessed_data <- dedup_exact_ignore(adsprocessed_data, ads_index_cols, "adsprocessed")
adsprocessed_place_lookup_source <- adsprocessed_data

# De-duplicate near-duplicates by normalized address within CONTROL
adsprocessed_data <- adsprocessed_data %>%
  mutate(
    addr_key = build_address_key(.),
    addr_key_dedup = ifelse(is.na(addr_key) | addr_key == "", paste0("missing_", row_number()), addr_key)
  )
adsprocessed_key <- intersect(c("CONTROL", "TESTERID", "addr_key_dedup"), names(adsprocessed_data))

# Preserve Table 5 outcomes before collapsing repeated advertised-home rows.
# Multiple rows with the same advertised-home address usually represent multiple
# appointments, so recommendation counts should be accumulated and advertised
# home availability should be recorded if it occurred in any appointment.
if ("STOTUNIT" %in% names(adsprocessed_data)) {
  stotunit_variation <- adsprocessed_data %>%
    group_by(across(all_of(adsprocessed_key))) %>%
    summarize(
      rows = n(),
      distinct_stotunit = {
        stotunit_clean <- suppressWarnings(as.numeric(STOTUNIT))
        stotunit_clean[stotunit_clean < 0] <- NA_real_
        n_distinct(stotunit_clean, na.rm = FALSE)
      },
      .groups = "drop"
    )
  cat(
    "adsprocessed address-key groups with varying STOTUNIT:",
    sum(stotunit_variation$rows > 1 & stotunit_variation$distinct_stotunit > 1),
    "\n"
  )

  adsprocessed_data <- adsprocessed_data %>%
    group_by(across(all_of(adsprocessed_key))) %>%
    mutate(
      STOTUNIT = {
        stotunit_clean <- suppressWarnings(as.numeric(STOTUNIT))
        stotunit_clean[stotunit_clean < 0] <- NA_real_
        if (all(is.na(stotunit_clean))) NA_real_ else sum(stotunit_clean, na.rm = TRUE)
      }
    ) %>%
    ungroup()
}

if ("SAVLBAD" %in% names(adsprocessed_data)) {
  savlbad_variation <- adsprocessed_data %>%
    group_by(across(all_of(adsprocessed_key))) %>%
    summarize(
      rows = n(),
      distinct_savlbad = {
        savlbad_clean <- suppressWarnings(as.numeric(as.character(SAVLBAD)))
        savlbad_clean[savlbad_clean < 0] <- NA_real_
        savlbad_clean[savlbad_clean > 1] <- 0
        n_distinct(savlbad_clean, na.rm = FALSE)
      },
      .groups = "drop"
    )
  cat(
    "adsprocessed address-key groups with varying SAVLBAD:",
    sum(savlbad_variation$rows > 1 & savlbad_variation$distinct_savlbad > 1),
    "\n"
  )

  adsprocessed_data <- adsprocessed_data %>%
    group_by(across(all_of(adsprocessed_key))) %>%
    mutate(
      SAVLBAD = {
        savlbad_clean <- suppressWarnings(as.numeric(as.character(SAVLBAD)))
        savlbad_clean[savlbad_clean < 0] <- NA_real_
        savlbad_clean[savlbad_clean > 1] <- 0
        if (any(savlbad_clean == 1, na.rm = TRUE)) {
          1
        } else if (all(is.na(savlbad_clean))) {
          NA_real_
        } else {
          0
        }
      }
    ) %>%
    ungroup()
}

adsprocessed_data <- dedup_by_key(adsprocessed_data, adsprocessed_key, "adsprocessed address key")
adsprocessed_data <- adsprocessed_data %>% select(-addr_key, -addr_key_dedup)

# Summarize how many trials still contain multiple distinct advertised-home
# addresses after address-based de-duplication.
if ("CONTROL" %in% names(adsprocessed_data)) {
  addr_key <- build_address_key(adsprocessed_data)
  control_ad_counts <- adsprocessed_data %>%
    mutate(addr_key = addr_key) %>%
    group_by(CONTROL) %>%
    summarize(distinct_ads = n_distinct(addr_key, na.rm = TRUE), .groups = "drop")
  multi_ad_controls <- sum(control_ad_counts$distinct_ads > 1, na.rm = TRUE)
  cat("adsprocessed - controls with >1 distinct ad address:", multi_ad_controls, "of", nrow(control_ad_counts), "\n")
}

# Load census block groups and places data
# Using tigris package to get census geography data
# Note: Using cache = TRUE to avoid re-downloading data

# Set the year for the data
census_year <- 2010 
# Define the states we want to process
states_to_process <- c(
  "06" = "CA", # California
  "11" = "DC", # District of Columbia
  "12" = "FL", # Florida
  "13" = "GA", # Georgia
  "17" = "IL", # Illinois
  "20" = "KS", # Kansas
  "24" = "MD", # Maryland
  "25" = "MA", # Massachusetts
  "26" = "MI", # Michigan
  "29" = "MO", # Missouri
  "34" = "NJ", # New Jersey
  "35" = "NM", # New Mexico
  "36" = "NY", # New York
  "37" = "NC", # North Carolina
  "39" = "OH", # Ohio
  "42" = "PA", # Pennsylvania
  "45" = "SC", # South Carolina
  "48" = "TX", # Texas
  "51" = "VA", # Virginia
  "53" = "WA"  # Washington
)

if (ENABLE_PLACE_COUNTY_FIXED_EFFECTS) {

add_block_group_area_to_intersection <- function(block_group_place_intersection,
                                                 state_abbr,
                                                 census_year,
                                                 intersection_file = NULL) {
  if (
    "block_group_area" %in% names(block_group_place_intersection) &&
    !all(is.na(block_group_place_intersection$block_group_area))
  ) {
    return(block_group_place_intersection)
  }

  cat("Adding full block-group area denominator for", state_abbr, "...\n")
  block_groups_area <- block_groups(state = state_abbr, cb = TRUE, year = census_year)
  st_crs(block_groups_area) <- st_crs(block_group_place_intersection)
  block_groups_area$block_group_area <- st_area(block_groups_area)

  block_group_place_intersection <- block_group_place_intersection %>%
    select(-any_of("block_group_area")) %>%
    left_join(
      block_groups_area %>%
        st_drop_geometry() %>%
        select(GEO_ID, block_group_area),
      by = "GEO_ID"
    )

  if (!is.null(intersection_file)) {
    saveRDS(block_group_place_intersection, file = intersection_file)
    cat("Updated cached intersection with block_group_area:", intersection_file, "\n")
  }

  block_group_place_intersection
}

# Initialize an empty dataframe to store all block group-place mappings
all_block_group_place_mappings <- data.frame()

# Process each state
for (state_fips in names(states_to_process)) {
  state_abbr <- states_to_process[state_fips]
  
  cat(paste0("\n\nProcessing state: ", state_abbr, " (FIPS: ", state_fips, ")\n"))
  
  # Define file paths for saving intermediate results
  intersection_file <- paste0(generated_root, "/Intersection Files/block_group_place_intersection_", state_abbr, ".rds")

  # Check if the intersection file already exists to avoid reprocessing
  used_cached_intersection <- FALSE
  if (file.exists(intersection_file)) {
    cat("Loading existing block group-place intersection for", state_abbr, "...\n")
    block_group_place_intersection <- readRDS(intersection_file)
    used_cached_intersection <- TRUE
  } else {
    # Get census block groups for the current state
    cat("Loading census block groups data for", state_abbr, "...\n")
    block_groups_sf <- block_groups(state = state_abbr, cb = TRUE, year = census_year)
    cat(state_abbr, "census block groups loaded:", nrow(block_groups_sf), "block groups\n")

    # Get places for the current state
    cat("Loading places data for", state_abbr, "...\n")
    places <- places(state = state_abbr, cb = TRUE, year = 2020)
    cat(state_abbr, "places loaded:", nrow(places), "places\n")
    
    # Ensure both datasets have the same CRS
    st_crs(block_groups_sf) <- st_crs(places)
    block_groups_sf$block_group_area <- st_area(block_groups_sf)
    
    # Perform spatial intersection
    cat("Performing spatial intersection between block groups and places for", state_abbr, "...\n")
    block_group_place_intersection <- st_intersection(block_groups_sf, places)
    
    # Create the directory for intersection files if it doesn't exist
    intersection_dir <- paste0(generated_root, "/Intersection Files")
    if (!dir.exists(intersection_dir)) {
      cat("Creating directory for intersection files:", intersection_dir, "\n")
      dir.create(intersection_dir, recursive = TRUE)
    }

    # Save the intersection to avoid recomputing if the process is interrupted
    saveRDS(block_group_place_intersection, file = intersection_file)
    cat("Saved intersection to", intersection_file, "\n")
  }
  
  # Calculate areas and coverage percentages
  cat("Calculating coverage percentages for", state_abbr, "...\n")

  # Ensure cached and fresh paths use the full block-group area denominator.
  # Older cached intersections lacked this column and used the sum of place
  # intersections instead, which changed county/place fallback behavior.
  block_group_place_intersection <- add_block_group_area_to_intersection(
    block_group_place_intersection,
    state_abbr,
    census_year,
    intersection_file
  )
  
  # Make geometries valid
  block_group_place_intersection <- st_make_valid(block_group_place_intersection)
  
  # Calculate intersection areas
  block_group_place_intersection$intersection_area <- st_area(block_group_place_intersection)

  if (any(is.na(block_group_place_intersection$block_group_area))) {
    stop("Missing block_group_area after area-denominator repair for ", state_abbr)
  }
  
  # Calculate coverage percentage
  block_group_place_intersection$coverage_pct <- as.numeric(block_group_place_intersection$intersection_area / block_group_place_intersection$block_group_area)
  
  # For each block group, find the place with the largest coverage
  best_matches <- block_group_place_intersection %>%
    group_by(GEO_ID) %>%
    arrange(desc(coverage_pct)) %>%
    slice(1) %>%
    ungroup()
  
  # Create clean dataset with block-group-to-place mapping
  state_block_group_place_mapping <- best_matches %>%
    st_drop_geometry() %>%
    select(block_group_geoid = GEO_ID, place_geoid = GEOID, place_name = NAME.1, coverage_pct) %>%
    mutate(
      block_group_geoid = sub("^.*US", "", block_group_geoid),
      state_fips = state_fips,
      state_abbr = state_abbr
    )
  
  cat("Completed processing for", state_abbr, "\n")
  cat("Number of block groups matched to places:", nrow(state_block_group_place_mapping), "\n")
  
  # Append to the combined dataframe
  all_block_group_place_mappings <- rbind(all_block_group_place_mappings, state_block_group_place_mapping)
  
  # Clean up to free memory
  rm(block_group_place_intersection, best_matches, state_block_group_place_mapping)
  if (exists("block_groups_sf")) rm(block_groups_sf)
  if (exists("places")) rm(places)
  gc()
}

cat("\nSpatial merge completed for all states. Each block group is matched to at most one place (the one with largest coverage).\n")
cat("Total number of block groups matched to places:", nrow(all_block_group_place_mappings), "\n")

intersection_dir <- paste0(generated_root, "/Intersection Files")

# Save the combined block-group-place mapping
saveRDS(all_block_group_place_mappings, file = paste0(intersection_dir, "/all_block_group_place_mappings.rds"))

# Load the block-group-place mapping, to run the file after it has been generated
block_group_place_mapping <- readRDS(paste0(intersection_dir, "/all_block_group_place_mappings.rds"))

# Display the first few rows of the mapping
cat("\nFirst few rows of the block-group-to-place mapping:\n")
print(head(block_group_place_mapping))




# Build county lookup from built-in FIPS codes (offline-safe)
county_lookup <- tigris::fips_codes %>%
  filter(state %in% unname(states_to_process)) %>%
  transmute(
    county_geoid = paste0(state_code, county_code),
    county_name = paste(county, " County")
  ) %>%
  distinct(county_geoid, .keep_all = TRUE)

add_place_county_info <- function(dataset, label) {
  cat("\nProcessing", label, "and merging with place information...\n")
  
  dataset <- dataset %>%
    mutate(
      GEOID10 = normalize_geoid(GEOID10, width = 15),
      block_group_geoid = substr(GEOID10, 1, 12)
    ) %>%
    left_join(block_group_place_mapping, by = "block_group_geoid") %>%
    mutate(county_geoid = substr(GEOID10, 1, 5)) %>%
    left_join(county_lookup, by = "county_geoid") %>%
    mutate(
      # If place_geoid is missing or has negligible/unknown coverage, fall back to county geography.
      place_geoid = ifelse(is.na(place_geoid) | is.na(coverage_pct) | coverage_pct <= 0.01, county_geoid, place_geoid),
      # If place_name is missing or has negligible/unknown coverage, fall back to county name.
      place_name = ifelse(is.na(place_name) | is.na(coverage_pct) | coverage_pct <= 0.01, county_name, place_name)
    )
  
  cat("\nMerge completed for", label, ". All observations now have place or county information.\n")
  cat("Number of observations with place information:", sum(!is.na(dataset$coverage_pct)), "\n")
  cat("Number of observations with county fallback:", sum(is.na(dataset$coverage_pct)), "\n")
  
  dataset
}

} else {
  cat("\nPlace/county fixed-effect construction disabled; skipping spatial place/county enrichment.\n")
  add_place_county_info <- function(dataset, label) {
    cat("\nSkipping place/county enrichment for", label, "\n")
    dataset
  }
  block_group_place_mapping <- data.frame()
}

adsprocessed_place_lookup_source <- add_place_county_info(
  adsprocessed_place_lookup_source,
  "pre-address-dedup ads lookup source"
)

adsprocessed_data_processed <- add_place_county_info(
  adsprocessed_data,
  "deduplicated adsprocessed output"
)

core_ad_vars <- intersect(
  c("w2012pc_Ad", "b2012pc_Ad", "a2012pc_Ad", "hisp2012pc_Ad"),
  names(adsprocessed_data_processed)
)

# Resolve each CONTROL x TESTERID pair to one canonical advertised-home record.
# Priority:
# 1. If the tester reported one advertised home, keep it.
# 2. If the other tester in the completed pair reported exactly one advertised home, use that listing.
# 3. If the tester's multiple listings agree on the core racial-composition ad controls,
#    keep one row and average price on the dollar scale before logging again.
# 4. Otherwise, drop the ambiguous tester-trial pair.
race_candidate_cols <- intersect(
  c("APRACE", "aprace", "APRACE.x", "apracex", "RACEID.x", "HRACE"),
  names(adsprocessed_data_processed)
)

pair_ad_cols <- intersect(
  unique(c(
    grep("_Ad$", names(adsprocessed_data_processed), value = TRUE),
    "AdPrice", "logAdPrice",
    "HSITEAD", "HUNITNO", "HCITY", "HSTATE", "HZIP",
    "blkgrp", "GEOID10", "STATEFP10", "COUNTYFP10", "block_group_geoid",
    "place_geoid", "place_name", "county_geoid", "county_name", "coverage_pct"
  )),
  names(adsprocessed_data_processed)
)

pair_data <- as.data.frame(
  adsprocessed_data_processed[, unique(c("CONTROL", "TESTERID", pair_ad_cols, race_candidate_cols)), drop = FALSE]
)
pair_data$TESTERID <- as.character(pair_data$TESTERID)

addr_raw <- if ("HSITEAD" %in% names(pair_data)) pair_data$HSITEAD else rep(NA_character_, nrow(pair_data))
addr_alt <- if ("Address" %in% names(pair_data)) pair_data$Address else rep(NA_character_, nrow(pair_data))
unit_raw <- if ("HUNITNO" %in% names(pair_data)) pair_data$HUNITNO else rep("", nrow(pair_data))
city_raw <- if ("HCITY" %in% names(pair_data)) pair_data$HCITY else rep(NA_character_, nrow(pair_data))
city_alt <- if ("City" %in% names(pair_data)) pair_data$City else rep(NA_character_, nrow(pair_data))
state_raw <- if ("HSTATE" %in% names(pair_data)) pair_data$HSTATE else rep(NA_character_, nrow(pair_data))
state_alt <- if ("State" %in% names(pair_data)) pair_data$State else rep(NA_character_, nrow(pair_data))
zip_raw <- if ("HZIP" %in% names(pair_data)) pair_data$HZIP else rep(NA_character_, nrow(pair_data))
zip_alt <- if ("Zip_Code" %in% names(pair_data)) pair_data$Zip_Code else rep(NA_character_, nrow(pair_data))

pair_data <- pair_data %>%
  mutate(
    addr_raw = ifelse(!is.na(addr_raw) & addr_raw != "", addr_raw, addr_alt),
    unit_raw = unit_raw,
    city_raw = ifelse(!is.na(city_raw) & city_raw != "", city_raw, city_alt),
    state_raw = ifelse(!is.na(state_raw) & state_raw != "", state_raw, state_alt),
    zip_raw = ifelse(!is.na(zip_raw) & zip_raw != "", zip_raw, zip_alt),
    addr_key_resolve = paste(
      normalize_text(addr_raw),
      normalize_text(unit_raw),
      normalize_text(city_raw),
      normalize_text(state_raw),
      normalize_text(zip_raw),
      sep = "|"
    )
  )
pair_data$addr_key_resolve[pair_data$addr_key_resolve == "na|na|na|na|na"] <- NA_character_
pair_data <- pair_data %>% select(-any_of(c("addr_raw", "unit_raw", "city_raw", "state_raw", "zip_raw")))

preferred_race_var <- if (length(race_candidate_cols) > 0) race_candidate_cols[[1]] else NA_character_
pair_groups <- split(pair_data, paste(pair_data$CONTROL, pair_data$TESTERID, sep = "||"), drop = TRUE)

pair_meta <- bind_rows(lapply(pair_groups, function(rows) {
  distinct_addrs <- unique(rows$addr_key_resolve)
  race_value <- NA_character_

  if (!is.na(preferred_race_var)) {
    race_codes <- str_squish(str_to_upper(as.character(rows[[preferred_race_var]])))
    race_codes[race_codes %in% c("", "NA", "<NA>")] <- NA_character_
    race_labels <- case_when(
      race_codes %in% c("1", "WHITE", "NON-HISPANIC WHITE", "W") ~ "White",
      race_codes %in% c("2", "BLACK", "AFRICAN AMERICAN", "B") ~ "Black",
      race_codes %in% c("3", "HISPANIC", "LATINO", "LATINA", "H") ~ "Hispanic",
      race_codes %in% c("4", "ASIAN", "A") ~ "Asian",
      is.na(race_codes) ~ NA_character_,
      TRUE ~ "Other"
    )
    race_labels <- race_labels[!is.na(race_labels)]
    if (length(race_labels) > 0) {
      race_value <- race_labels[[1]]
    }
  }

  data.frame(
    CONTROL = rows$CONTROL[[1]],
    TESTERID = rows$TESTERID[[1]],
    n_addr = length(distinct_addrs),
    single_addr = if (length(distinct_addrs) == 1) distinct_addrs[[1]] else NA_character_,
    race = race_value,
    stringsAsFactors = FALSE
  )
}))

canonical_rows <- list()
status_rows <- list()

for (i in seq_len(nrow(pair_meta))) {
  control_value <- pair_meta$CONTROL[[i]]
  tester_value <- pair_meta$TESTERID[[i]]
  pair_key <- paste(control_value, tester_value, sep = "||")
  own_rows <- pair_groups[[pair_key]]
  source_rows <- NULL
  status <- NA_character_

  if (pair_meta$n_addr[[i]] == 1) {
    source_rows <- own_rows
    status <- "self_unique_advertised_home"
  } else {
    partner_meta <- pair_meta %>%
      filter(CONTROL == control_value, TESTERID != tester_value, n_addr == 1)
    partner_single_addrs <- unique(partner_meta$single_addr)

    if (length(partner_single_addrs) == 1) {
      partner_keys <- paste(partner_meta$CONTROL, partner_meta$TESTERID, sep = "||")
      partner_keys <- intersect(partner_keys, names(pair_groups))
      if (length(partner_keys) > 0) {
        source_rows <- bind_rows(pair_groups[partner_keys])
        status <- "paired_tester_unique_advertised_home"
      }
    }

    if (is.null(source_rows)) {
      available_core_vars <- intersect(core_ad_vars, names(own_rows))
      core_agreement <- length(available_core_vars) > 0 &&
        all(vapply(available_core_vars, function(var) length(unique(own_rows[[var]])) <= 1, logical(1)))
      if (core_agreement) {
        source_rows <- own_rows
        status <- "within_tester_core_agreement"
      }
    }

    if (is.null(source_rows)) {
      status <- "dropped_unresolved_advertised_home"
    }
  }

  status_rows[[length(status_rows) + 1]] <- data.frame(
    CONTROL = control_value,
    TESTERID = tester_value,
    race = pair_meta$race[[i]],
    minority = ifelse(is.na(pair_meta$race[[i]]), NA, pair_meta$race[[i]] != "White"),
    status = status,
    stringsAsFactors = FALSE
  )

  if (!is.null(source_rows)) {
    keep_cols <- unique(c("CONTROL", "TESTERID", pair_ad_cols))
    canonical_row <- source_rows[1, intersect(keep_cols, names(source_rows)), drop = FALSE]
    canonical_row$CONTROL <- control_value
    canonical_row$TESTERID <- tester_value

    avg_price <- NA_real_
    if ("logAdPrice" %in% names(source_rows)) {
      valid_log <- suppressWarnings(as.numeric(source_rows$logAdPrice))
      valid_log <- valid_log[!is.na(valid_log)]
      if (length(valid_log) > 0) {
        avg_price <- mean(exp(valid_log))
      }
    }
    if ((is.na(avg_price) || !is.finite(avg_price)) && "AdPrice" %in% names(source_rows)) {
      valid_price <- suppressWarnings(as.numeric(source_rows$AdPrice))
      valid_price <- valid_price[!is.na(valid_price)]
      if (length(valid_price) > 0) {
        avg_price <- mean(valid_price)
      }
    }
    avg_log_price <- if (is.finite(avg_price) && !is.na(avg_price) && avg_price > 0) log(avg_price) else NA_real_
    if ("AdPrice" %in% names(canonical_row)) canonical_row$AdPrice <- avg_price
    if ("logAdPrice" %in% names(canonical_row)) canonical_row$logAdPrice <- avg_log_price

    canonical_rows[[length(canonical_rows) + 1]] <- canonical_row
  }
}

status_df <- bind_rows(status_rows)
canonical_ads_lookup <- bind_rows(canonical_rows) %>%
  distinct(CONTROL, TESTERID, .keep_all = TRUE)

cat("\nCanonical advertised-home assignment summary:\n")
print(status_df %>% count(status, name = "pairs"))
cat("Canonical advertised-home rows retained:", nrow(canonical_ads_lookup), "\n")
cat("Dropped CONTROL x TESTERID pairs with unresolved advertised homes:",
    sum(status_df$status == "dropped_unresolved_advertised_home"), "\n")

race_summary <- status_df %>%
  count(race, status, name = "pairs") %>%
  arrange(race, status)

minority_summary <- status_df %>%
  filter(!is.na(minority)) %>%
  group_by(minority) %>%
  summarize(
    pairs = n(),
    dropped = sum(status == "dropped_unresolved_advertised_home"),
    drop_rate = dropped / pairs,
    .groups = "drop"
  )

control_drop_summary <- status_df %>%
  filter(!is.na(minority)) %>%
  group_by(CONTROL) %>%
  summarize(
    white_dropped = sum(status == "dropped_unresolved_advertised_home" & minority == FALSE),
    minority_dropped = sum(status == "dropped_unresolved_advertised_home" & minority == TRUE),
    .groups = "drop"
  ) %>%
  summarize(
    controls_with_any_drop = sum(white_dropped > 0 | minority_dropped > 0),
    controls_minority_only = sum(minority_dropped > 0 & white_dropped == 0),
    controls_white_only = sum(white_dropped > 0 & minority_dropped == 0),
    controls_both = sum(white_dropped > 0 & minority_dropped > 0)
  )

fisher_p <- NA_real_
discordant_p <- NA_real_
if (nrow(minority_summary) == 2 &&
    all(c(FALSE, TRUE) %in% minority_summary$minority)) {
  minority_summary_ordered <- minority_summary %>% arrange(minority)
  fisher_p <- fisher.test(
    matrix(
      c(
        minority_summary_ordered$dropped[[2]],
        minority_summary_ordered$pairs[[2]] - minority_summary_ordered$dropped[[2]],
        minority_summary_ordered$dropped[[1]],
        minority_summary_ordered$pairs[[1]] - minority_summary_ordered$dropped[[1]]
      ),
      nrow = 2,
      byrow = TRUE
    )
  )$p.value

  discordant_total <- control_drop_summary$controls_minority_only + control_drop_summary$controls_white_only
  if (discordant_total > 0) {
    discordant_p <- binom.test(
      control_drop_summary$controls_minority_only,
      discordant_total,
      p = 0.5
    )$p.value
  }
}

cat("\nAdvertised-home ambiguity drop summary by preferred race variable:\n")
if (!is.na(preferred_race_var)) {
  cat("Preferred race variable:", preferred_race_var, "\n")
} else {
  cat("Preferred race variable: none found\n")
}
print(race_summary)
cat("\nDrop rates by minority status:\n")
print(minority_summary)
cat("\nWithin-control drop imbalance summary:\n")
print(control_drop_summary)
cat("Minority-versus-white Fisher exact p-value:", fisher_p, "\n")
cat("Discordant-control exact binomial p-value:", discordant_p, "\n")

log_dedup(
  "adsprocessed canonical advertised-home assignment",
  nrow(pair_meta),
  nrow(canonical_ads_lookup)
)

if (nrow(minority_summary %>% filter(minority == FALSE)) == 1 &&
    nrow(minority_summary %>% filter(minority == TRUE)) == 1) {
  white_row <- minority_summary %>% filter(minority == FALSE)
  minority_row <- minority_summary %>% filter(minority == TRUE)
  cat("\nAdvertised-home ambiguity race-balance check:\n")
  cat(sprintf(
    "Dropped unresolved advertised-home pairs: %d / %d\n",
    sum(status_df$status == "dropped_unresolved_advertised_home"),
    nrow(status_df)
  ))
  cat(sprintf(
    "White: %d / %d = %.3f%%\n",
    white_row$dropped[[1]],
    white_row$pairs[[1]],
    100 * white_row$drop_rate[[1]]
  ))
  cat(sprintf(
    "Minority: %d / %d = %.3f%%\n",
    minority_row$dropped[[1]],
    minority_row$pairs[[1]],
    100 * minority_row$drop_rate[[1]]
  ))
  cat(sprintf("Fisher exact p = %.7f\n", fisher_p))
}

# Prepare the canonical advertised-home lookup for the HUD files and overwrite
# the ad-side fields in the adsprocessed output with the resolved values.
canonical_hud_lookup <- canonical_ads_lookup %>%
  mutate(TESTERID = as.character(TESTERID))
if ("HCITY" %in% names(canonical_hud_lookup)) {
  canonical_hud_lookup <- canonical_hud_lookup %>% rename(HCITY_Ad = HCITY)
}
canonical_hud_keep_cols <- intersect(
  unique(c(
    "CONTROL", "TESTERID", "HCITY_Ad",
    "blkgrp", "GEOID10", "place_name", "place_geoid", "county_name", "county_geoid",
    grep("_Ad$", names(canonical_hud_lookup), value = TRUE),
    "AdPrice", "logAdPrice"
  )),
  names(canonical_hud_lookup)
)
canonical_hud_lookup <- canonical_hud_lookup %>% select(all_of(canonical_hud_keep_cols))

canonical_cols <- setdiff(names(canonical_ads_lookup), c("CONTROL", "TESTERID"))
adsprocessed_base <- adsprocessed_data_processed %>%
  mutate(TESTERID = as.character(TESTERID)) %>%
  group_by(CONTROL, TESTERID) %>%
  slice(1) %>%
  ungroup() %>%
  select(-any_of(canonical_cols))
ads_rows_before <- nrow(adsprocessed_base)
adsprocessed_data_processed <- adsprocessed_base %>%
  inner_join(canonical_ads_lookup %>% mutate(TESTERID = as.character(TESTERID)),
             by = c("CONTROL", "TESTERID"))

cat("\nApplying canonical advertised-home values to adsprocessed output...\n")
cat("Tester-control rows before canonical merge:", ads_rows_before, "\n")
cat("Tester-control rows after canonical merge:", nrow(adsprocessed_data_processed), "\n")

ads_geo_required_cols <- if (ENABLE_PLACE_COUNTY_FIXED_EFFECTS) {
  c("HCITY", "place_name", "county_name")
} else {
  c("HCITY")
}
adsprocessed_data_processed <- filter_processed_geo(
  adsprocessed_data_processed,
  ads_geo_required_cols,
  "deduplicated adsprocessed output"
)

write_ads_outputs(
  adsprocessed_data_processed,
  output_folder,
  "adsprocessed_correct_cities_processed",
  csv_na = ""
)




########################################## VERIFYING AD VARIABLES AS BLOCK GROUP IDENTIFIERS ##############################################
# This section checks if ad variables uniquely identify block groups

# Load the censored adsprocessed data for block-group identifier checks and
# HCITY_Ad lookup construction.
cat("\nLoading adsprocessed_JPE_censor data...\n")
adsprocessed <- readRDS(paste0(input_folder, "/adsprocessed_JPE_censor.rds"))

# Define ad_only_variables with specific variable names
ad_only_variables <- c("logAdPrice", "stfid_Ad", "w2012pc_Ad", "b2012pc_Ad", 
                      "a2012pc_Ad", "hisp2012pc_Ad", "oth2012pc_Ad", "CONTROL")

# Create an index based on unique combinations of these ad variables
cat("\nCreating an index based on unique combinations of ad variables...\n")

# Select only the ad-related variables and create a unique identifier
ad_combinations <- adsprocessed %>%
  select(CONTROL, blkgrp, all_of(ad_only_variables)) %>%
  # Create a hash or identifier for each unique combination of ad variables
  mutate(ad_combo_id = group_indices(., across(all_of(ad_only_variables)))) %>%
  arrange(ad_combo_id)

# Create indicators for block groups with multiple ad combinations and vice versa
cat("\nIdentifying block groups with multiple ad combinations and vice versa...\n")

# Block groups with multiple ad combinations
blkgrp_with_multiple_ads <- ad_combinations %>%
  group_by(blkgrp) %>%
  summarize(
    unique_ad_combos = n_distinct(ad_combo_id),
    ad_combo_ids = paste(sort(unique(ad_combo_id)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(unique_ad_combos > 1) %>%
  mutate(has_multiple_ads = TRUE)

# Ad combinations spanning multiple block groups
ad_combos_with_multiple_blkgrps <- ad_combinations %>%
  group_by(ad_combo_id) %>%
  summarize(
    unique_blkgrps = n_distinct(blkgrp),
    blkgrp_list = paste(sort(unique(blkgrp)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(unique_blkgrps > 1) %>%
  mutate(spans_multiple_blkgrps = TRUE)

# Print summary statistics
cat("Found", nrow(blkgrp_with_multiple_ads), "block groups with multiple ad combinations\n")
cat("Found", nrow(ad_combos_with_multiple_blkgrps), "ad combinations spanning multiple block groups\n")



# Add indicators back to the original ad_combinations dataframe
ad_combinations <- ad_combinations %>%
  left_join(blkgrp_with_multiple_ads %>% select(blkgrp, has_multiple_ads), by = "blkgrp") %>%
  left_join(ad_combos_with_multiple_blkgrps %>% select(ad_combo_id, spans_multiple_blkgrps), by = "ad_combo_id") %>%
  mutate(
    has_multiple_ads = ifelse(is.na(has_multiple_ads), FALSE, has_multiple_ads),
    spans_multiple_blkgrps = ifelse(is.na(spans_multiple_blkgrps), FALSE, spans_multiple_blkgrps)
  )

# For the ad combinations that span multiple block groups, print the details
cat("\nPrinting details of ad combinations that span multiple block groups...\n")

# Get the ad combinations that span multiple block groups
ad_combos_spanning_multiple_blkgrps <- ad_combinations %>%
  filter(spans_multiple_blkgrps == TRUE) %>%
  arrange(ad_combo_id, blkgrp)

# Print the first few rows of these combinations
cat("\nAd combinations spanning multiple block groups:\n")
print(head(ad_combos_spanning_multiple_blkgrps, 20))

if (ENABLE_PLACE_COUNTY_FIXED_EFFECTS) {
  # For the two blkgrp values that have the same ad characteristics, we show that they will be assigned the same city in the end
  cat("\nSubsetting to specific block_group_geoid values (39055312203 and 39055312202)...\n")
  selected_block_groups <- block_group_place_mapping %>%
    filter(block_group_geoid %in% c("39055312203", "39055312202"))

  # Display the results
  cat("Found", nrow(selected_block_groups), "matching records\n")
  if(nrow(selected_block_groups) > 0) {
    cat("\nSelected block group information:\n")
    print(selected_block_groups %>%
          as.data.frame())
  } else {
    cat("No matching records found for the specified GEOIDs\n")
  }

  # Note that both block_group_geoids point to the place_name Chardon, so we can use either one without inaccuracy in our generated place names
}



# Check if the same ad variables are used across different datasets for identification
cat("\nChecking consistency of ad variables across datasets...\n")

# Function to compare ad variables in a dataset with the reference list
compare_ad_variables <- function(data, dataset_name) {
  # Get all column names from the dataset
  all_vars <- names(data)
  
  # Check which reference ad variables exist in this dataset
  present_vars <- intersect(ad_only_variables, all_vars)
  missing_vars <- setdiff(ad_only_variables, all_vars)
  
  cat("\nResults for", dataset_name, ":\n")
  cat("- Contains", length(present_vars), "out of", length(ad_only_variables), "reference ad variables\n")
  
  if (length(missing_vars) > 0) {
    cat("- Missing ad variables:", paste(missing_vars, collapse=", "), "\n")
  } else {
    cat("- Contains all reference ad variables\n")
  }
  
  # Check for additional ad-related variables not in the reference list
  potential_extra_ad_vars <- grep("^(ad|Ad|AD).*|.*(_ad|_Ad|_AD)$", all_vars, value = TRUE)
  extra_vars <- setdiff(potential_extra_ad_vars, ad_only_variables)
  
  if (length(extra_vars) > 0) {
    cat("- Contains", length(extra_vars), "additional potential ad variables not in reference list:\n")
    cat("  ", paste(extra_vars, collapse=", "), "\n")
  }
  
  return(list(present = present_vars, missing = missing_vars, extra = extra_vars))
}


# HUD de-duplication helper
dedup_hud_dataset <- function(df, dataset_name) {
  index_cols <- intersect(c("X.x", "X.y", "X.x.1", "X.y.1", "SEQRH"), names(df))
  df <- dedup_exact_ignore(df, index_cols, paste(dataset_name, "exact"))
  
  rec_cols <- grep("_Rec$", names(df), value = TRUE)
  rec_price_cols <- intersect(c("RecPrice", "logRecPrice"), names(df))
  recommendation_key <- unique(c("CONTROL", "TESTERID", rec_cols, rec_price_cols))
  df <- dedup_by_key(df, recommendation_key, paste(dataset_name, "recommendation"))
  
  df
}

# Load and check HUD_processed_JPE_census_042021.rds
cat("\nChecking HUD_processed_JPE_census_042021.rds...\n")
hud_census <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_census_042021.rds"))
hud_census <- dedup_hud_dataset(hud_census, "HUDprocessed_JPE_census_042021.rds")
hud_census_results <- compare_ad_variables(hud_census, "HUDprocessed_JPE_census_042021.rds")

# Load and check HUD_processed_JPE_names_042021.rds
cat("\nChecking HUD_processed_JPE_names_042021.rds...\n")
hud_names <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_names_042021.rds"))
hud_names <- dedup_hud_dataset(hud_names, "HUDprocessed_JPE_names_042021.rds")
hud_names_results <- compare_ad_variables(hud_names, "HUDprocessed_JPE_names_042021.rds")

# Load and check HUD_processed_JPE_testscores_042021.rds
cat("\nChecking HUD_processed_JPE_testscores_042021.rds...\n")
hud_testscores <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_testscores_042021.rds"))
hud_testscores <- dedup_hud_dataset(hud_testscores, "HUDprocessed_JPE_testscores_042021.rds")
hud_testscores_results <- compare_ad_variables(hud_testscores, "HUDprocessed_JPE_testscores_042021.rds")


# Merge canonical advertised-home values into the three datasets
cat("\nMerging canonical advertised-home values into the three datasets...\n")
hud_census_with_place <- merge_canonical_ads_into_hud(
  hud_census,
  "HUDprocessed_JPE_census_042021.rds",
  canonical_hud_lookup
)
hud_names_with_place <- merge_canonical_ads_into_hud(
  hud_names,
  "HUDprocessed_JPE_names_042021.rds",
  canonical_hud_lookup
)
hud_testscores_with_place <- merge_canonical_ads_into_hud(
  hud_testscores,
  "HUDprocessed_JPE_testscores_042021.rds",
  canonical_hud_lookup
)

hud_geo_required_cols <- if (ENABLE_PLACE_COUNTY_FIXED_EFFECTS) {
  c("HCITY.x", "HCITY_Ad", "place_name", "county_name")
} else {
  c("HCITY.x", "HCITY_Ad")
}
hud_census_with_place <- filter_processed_geo(
  hud_census_with_place,
  hud_geo_required_cols,
  "deduplicated HUD census output"
)
hud_names_with_place <- filter_processed_geo(
  hud_names_with_place,
  hud_geo_required_cols,
  "deduplicated HUD names output"
)
hud_testscores_with_place <- filter_processed_geo(
  hud_testscores_with_place,
  hud_geo_required_cols,
  "deduplicated HUD testscores output"
)

write_hud_outputs(
  hud_census_with_place,
  hud_names_with_place,
  hud_testscores_with_place,
  output_folder,
  "processed",
  csv_na = ""
)
write_dedup_log(output_folder, "dedup_log_processed.csv")

cat("\nGenerating non-deduplicated C&T-sample outputs...\n")
dedup_log <- new_dedup_log()

adsprocessed_data_nodedup <- readRDS(paste0(input_folder, "/adsprocessed_JPE_censor.rds"))
cat("\nWriting minimally formatted non-deduplicated ads data without place/county enrichment...\n")
write_ads_outputs(adsprocessed_data_nodedup, output_folder, "adsprocessed_correct_cities_with_duplicates")

cat("\nChecking HUD_processed_JPE_census_042021.rds for non-deduplicated output...\n")
hud_census_nodedup <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_census_042021.rds"))
compare_ad_variables(hud_census_nodedup, "HUDprocessed_JPE_census_042021.rds")

cat("\nChecking HUD_processed_JPE_names_042021.rds for non-deduplicated output...\n")
hud_names_nodedup <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_names_042021.rds"))
compare_ad_variables(hud_names_nodedup, "HUDprocessed_JPE_names_042021.rds")

cat("\nChecking HUD_processed_JPE_testscores_042021.rds for non-deduplicated output...\n")
hud_testscores_nodedup <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_testscores_042021.rds"))
compare_ad_variables(hud_testscores_nodedup, "HUDprocessed_JPE_testscores_042021.rds")

cat("\nWriting minimally formatted non-deduplicated HUD outputs without place/county enrichment or HCITY_Ad...\n")
write_hud_outputs(
  hud_census_nodedup,
  hud_names_nodedup,
  hud_testscores_nodedup,
  output_folder,
  "with_duplicates"
)
write_dedup_log(output_folder, "dedup_log_with_duplicates.csv")
