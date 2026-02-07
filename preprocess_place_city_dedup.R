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

# Set the working directory to the directory location of the github repository 
# This will be appended to the front of all addresses in the file
WORKING_DIRECTORY = ""

# Define the path to the output folder
output_folder <- paste0(WORKING_DIRECTORY, "Data/Generated")
input_folder <- paste0(WORKING_DIRECTORY, "Data/Original")

# Helper functions for de-duplication
normalize_text <- function(x) {
  x <- iconv(x, to = "UTF-8", sub = "")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  str_squish(x)
}

normalize_city_for_join <- function(x) {
  x <- as.character(x)
  x <- str_squish(str_to_upper(x))
  x[x == ""] <- NA_character_
  x
}

dedup_log <- data.frame(
  step = character(),
  before = integer(),
  after = integer(),
  dropped = integer(),
  stringsAsFactors = FALSE
)

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

log_control_ad_variation <- function(df, label) {
  if (!("CONTROL" %in% names(df))) return(invisible(NULL))
  addr_key <- build_address_key(df)
  tmp <- df %>%
    mutate(addr_key = addr_key) %>%
    group_by(CONTROL) %>%
    summarize(distinct_ads = n_distinct(addr_key, na.rm = TRUE), .groups = "drop")
  count_multi <- sum(tmp$distinct_ads > 1, na.rm = TRUE)
  cat(label, "- controls with >1 distinct ad address:", count_multi, "of", nrow(tmp), "\n")
}

# Load the adsprocessed_JPE data
adsprocessed_data <- readRDS(paste0(input_folder, "/adsprocessed_JPE_censor.rds"))

# De-duplicate adsprocessed data
ads_index_cols <- intersect(c("X.x", "X.y", "X.x.1", "X.y.1", "SEQRH"), names(adsprocessed_data))
adsprocessed_data <- dedup_exact_ignore(adsprocessed_data, ads_index_cols, "adsprocessed")

# De-duplicate near-duplicates by normalized address within CONTROL
adsprocessed_data <- adsprocessed_data %>%
  mutate(
    addr_key = build_address_key(.),
    addr_key_dedup = ifelse(is.na(addr_key) | addr_key == "", paste0("missing_", row_number()), addr_key)
  )
adsprocessed_key <- intersect(c("CONTROL", "TESTERID", "addr_key_dedup"), names(adsprocessed_data))
adsprocessed_data <- dedup_by_key(adsprocessed_data, adsprocessed_key, "adsprocessed address key")
adsprocessed_data <- adsprocessed_data %>% select(-addr_key, -addr_key_dedup)
log_control_ad_variation(adsprocessed_data, "adsprocessed")

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

# Initialize an empty dataframe to store all block group-place mappings
all_block_group_place_mappings <- data.frame()

# Process each state
for (state_fips in names(states_to_process)) {
  state_abbr <- states_to_process[state_fips]
  
  cat(paste0("\n\nProcessing state: ", state_abbr, " (FIPS: ", state_fips, ")\n"))
  
  # Define file paths for saving intermediate results
  intersection_file <- paste0(output_folder, "/Intersection Files/block_group_place_intersection_", state_abbr, ".rds")

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
    
    # Perform spatial intersection
    cat("Performing spatial intersection between block groups and places for", state_abbr, "...\n")
    block_group_place_intersection <- st_intersection(block_groups_sf, places)
    
    # Create the directory for intersection files if it doesn't exist
    intersection_dir <- paste0(output_folder, "/Intersection Files")
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
  
  # Make geometries valid
  block_group_place_intersection <- st_make_valid(block_group_place_intersection)
  
  # Calculate intersection areas
  block_group_place_intersection$intersection_area <- st_area(block_group_place_intersection)
  
  if (used_cached_intersection) {
    # When reusing cached intersections, derive relative coverage from intersections alone.
    block_group_place_intersection <- block_group_place_intersection %>%
      group_by(GEO_ID) %>%
      mutate(block_group_area = sum(intersection_area, na.rm = TRUE)) %>%
      ungroup()
  } else {
    # Calculate block group areas
    block_groups_sf$block_group_area <- st_area(block_groups_sf)
    
    # Join block group areas to intersection data
    block_group_place_intersection <- block_group_place_intersection %>%
      left_join(block_groups_sf %>% 
                  st_drop_geometry() %>% 
                  select(GEO_ID, block_group_area), 
                by = "GEO_ID")
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

intersection_dir <- paste0(output_folder, "/Intersection Files")

# Save the combined block-group-place mapping
saveRDS(all_block_group_place_mappings, file = paste0(intersection_dir, "/all_block_group_place_mappings.rds"))

# Load the block-group-place mapping, to run the file after it has been generated
block_group_place_mapping <- readRDS(paste0(intersection_dir, "/all_block_group_place_mappings.rds"))

# Display the first few rows of the mapping
cat("\nFirst few rows of the block-group-to-place mapping:\n")
print(head(block_group_place_mapping))




# Process all observations in adsprocessed_data
cat("\nProcessing all observations and merging with place information...\n")

# Extract block group GEOID from block GEOID (drop block suffix)
cat("\nExtracting block group GEOID from block GEOID and merging with place information...\n")
adsprocessed_data <- adsprocessed_data %>%
  mutate(block_group_geoid = substr(GEOID10, 1, nchar(GEOID10)-3)) %>%
  mutate(block_group_geoid = sprintf("%012s", block_group_geoid))
  

# Merge with block_group_place_mapping to get place information
adsprocessed_data <- adsprocessed_data %>%
  left_join(block_group_place_mapping, by = "block_group_geoid")

# For blocks without a corresponding place, fill in with county information
cat("\nFilling in county information for blocks without a corresponding place...\n")

# Extract county FIPS code (first 5 characters of GEOID10)
adsprocessed_data <- adsprocessed_data %>%
  mutate(county_geoid = substr(GEOID10, 1, 5))

# Build county lookup from built-in FIPS codes (offline-safe)
county_lookup <- tigris::fips_codes %>%
  filter(state %in% unname(states_to_process)) %>%
  transmute(
    county_geoid = paste0(state_code, county_code),
    county_name = paste(county, " County")
  ) %>%
  distinct(county_geoid, .keep_all = TRUE)

# Join county names to the data
adsprocessed_data_processed <- adsprocessed_data %>%
  left_join(county_lookup, by = "county_geoid") %>%
  mutate(
    # If place_geoid is NA, use county_geoid instead
    place_geoid = ifelse(is.na(place_geoid) | coverage_pct <= 0.01, county_geoid, place_geoid),
    # If place_name is NA, use county_name instead
    place_name = ifelse(is.na(place_name) | coverage_pct <= 0.01, county_name, place_name)
  )

test <- adsprocessed_data_processed %>% 
  select(HCITY, GEOID10, CONTROL, STATEFP10, COUNTYFP10, block_group_geoid, place_geoid, place_name, coverage_pct) %>%
  filter(is.na(place_geoid))


cat("\nMerge completed. All observations now have place or county information.\n")
cat("Number of observations with place information:", sum(!is.na(adsprocessed_data$coverage_pct)), "\n")
cat("Number of observations with county fallback:", sum(is.na(adsprocessed_data$coverage_pct)), "\n")


# Save the processed data to RDS format
rds_output_path <- file.path(output_folder, "adsprocessed_correct_cities.rds")
saveRDS(adsprocessed_data_processed, rds_output_path)
cat("Saved processed data to RDS:", rds_output_path, "\n")

# Save the processed data to CSV format
csv_output_path <- file.path(output_folder, "adsprocessed_correct_cities.csv")
write.csv(adsprocessed_data_processed, csv_output_path, row.names = FALSE)
cat("Saved processed data to CSV format:", csv_output_path, "\n")




# First, identify list columns that will be removed
list_columns <- names(adsprocessed_data_processed)[sapply(adsprocessed_data_processed, is.list)]
cat("Removing the following list columns for Stata compatibility:\n")
if(length(list_columns) > 0) {
  cat(paste("- ", list_columns, collapse = "\n"), "\n")
} else {
  cat("No list columns found to remove.\n")
}

# Clean variable names to make them Stata-compatible (max 32 chars, no special chars)
adsprocessed_data_stata <- adsprocessed_data_processed %>%
  # Remove trailing periods
  rename_all(~ gsub("\\.$", "", .)) %>%
  # Replace dots and other illegal characters with underscores
  rename_all(~ gsub("\\.", "_", .)) %>%
  rename_all(~ gsub("\\-", "_", .)) %>%
  # Replace any other special characters that might cause issues
  rename_all(~ gsub("[^a-zA-Z0-9_]", "_", .)) %>%
  # Truncate to 32 chars
  rename_all(~ ifelse(nchar(.) > 32, substr(., 1, 32), .)) %>%
  # Drop any columns that include lists
  select_if(~ !is.list(.))

# Drop duplicate TesterID if present (TESTERID is the canonical string version)
if ("TesterID" %in% names(adsprocessed_data_stata)) {
  adsprocessed_data_stata <- adsprocessed_data_stata %>% select(-TesterID)
}

# Save the processed data to Stata .dta format
dta_output_path <- file.path(output_folder, "adsprocessed_correct_cities.dta")
haven::write_dta(adsprocessed_data_stata, dta_output_path)
cat("Saved processed data to Stata format:", dta_output_path, "\n")




########################################## VERIFYING AD VARIABLES AS BLOCK GROUP IDENTIFIERS ##############################################
# This section checks if ad variables uniquely identify block groups

# Load the adsprocessed_JPE data
cat("\nLoading adsprocessed_JPE data...\n")
adsprocessed <- readRDS(paste0(input_folder, "/adsprocessed_JPE.rds"))

# Define ad_only_variables with specific variable names
ad_only_variables <- c("logAdPrice", "stfid_Ad", "w2012pc_Ad", "b2012pc_Ad", 
                      "a2012pc_Ad", "hisp2012pc_Ad", "oth2012pc_Ad", "CONTROL")
ad_city_join_keys <- c(ad_only_variables, "TESTERID")

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



# Check if the same ad variables are used across different datasets for identification
cat("\nChecking consistency of ad variables across datasets...\n")

# Build an ad-city lookup keyed to ad characteristics + tester for later HUD merge
cat("\nBuilding HCITY_Ad lookup using ad variables + TESTERID...\n")
ad_city_lookup <- adsprocessed %>%
  mutate(
    TESTERID = as.character(TESTERID),
    hcity_ad_raw = str_squish(as.character(HCITY)),
    hcity_ad_raw = ifelse(hcity_ad_raw == "", NA_character_, hcity_ad_raw),
    hcity_ad_norm = normalize_city_for_join(HCITY)
  ) %>%
  group_by(across(all_of(ad_city_join_keys))) %>%
  summarize(
    HCITY_Ad = {
      valid_idx <- which(!is.na(hcity_ad_norm))
      valid_norm <- hcity_ad_norm[valid_idx]
      valid_raw <- hcity_ad_raw[valid_idx]
      if (length(valid_norm) == 0) NA_character_
      else if (dplyr::n_distinct(valid_norm) == 1) valid_raw[[1]]
      else NA_character_
    },
    HCITY_Ad_ambiguous = {
      valid <- hcity_ad_norm[!is.na(hcity_ad_norm)]
      length(valid) > 0 && dplyr::n_distinct(valid) > 1
    },
    .groups = "drop"
  )

cat("HCITY_Ad lookup rows:", nrow(ad_city_lookup), "\n")
cat("HCITY_Ad lookup keys with ambiguous cities:", sum(ad_city_lookup$HCITY_Ad_ambiguous), "\n")

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
  
  ad_cols <- grep("_Ad$", names(df), value = TRUE)
  rec_cols <- grep("_Rec$", names(df), value = TRUE)
  ad_price_cols <- intersect(c("AdPrice", "logAdPrice"), names(df))
  rec_price_cols <- intersect(c("RecPrice", "logRecPrice"), names(df))
  combined_key <- unique(c("CONTROL", "TESTERID", ad_cols, ad_price_cols, rec_cols, rec_price_cols))
  df <- dedup_by_key(df, combined_key, paste(dataset_name, "ad+rec"))
  
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


# Merge place information from adsprocessed_data into the three datasets
cat("\nMerging place information from adsprocessed_data into the three datasets...\n")

# First, create a lookup dataframe with ad variables, place info, and county info
cat("Creating lookup dataframe with ad variables, place info, and county info...\n")
 place_lookup <- adsprocessed_data_processed %>%
  select(all_of(ad_only_variables), blkgrp, place_name, place_geoid, county_name, county_geoid) %>%
  mutate(blkgrp = ifelse(blkgrp == "390553122033", "390553122021", blkgrp)) %>%
  distinct() %>%
  distinct(across(all_of(ad_only_variables)), .keep_all = TRUE)

# Check the lookup dataframe
cat("Lookup dataframe created with", nrow(place_lookup), "rows\n")
cat("Number of unique combinations of ad variables:", 
    place_lookup %>% select(all_of(ad_only_variables)) %>% distinct() %>% nrow(), "\n")

# Check for any duplicated ad variable combinations
duplicate_ad_combos <- place_lookup %>%
  group_by_at(ad_only_variables) %>%
  filter(n() > 1) %>%
  ungroup()

if(nrow(duplicate_ad_combos) > 0) {
  cat("WARNING:", nrow(duplicate_ad_combos), "rows have duplicate ad variable combinations\n")
  cat("This may cause issues with the merge as the key is not unique\n")
  
  # Show a sample of duplicates
  cat("Sample of duplicated ad variable combinations:\n")
  print(head(duplicate_ad_combos))
} else {
  cat("All ad variable combinations are unique in the lookup dataframe\n")
}

# Function to merge place information into a dataset
merge_place_info <- function(dataset, dataset_name) {
  cat("\nMerging place information into", dataset_name, "...\n")
  
  # Perform the merge using all ad variables
  rows_before <- nrow(dataset)
  
  merged_dataset <- dataset %>%
    left_join(place_lookup, by = ad_only_variables)
  
  rows_after <- nrow(merged_dataset)
  
  cat("Rows before merge:", rows_before, "\n")
  cat("Rows after merge:", rows_after, "\n")
  
  if(rows_after > rows_before) {
    cat("WARNING: The number of rows increased after the merge\n")
    cat("This indicates a one-to-many join occurred\n")
  }
  
  # Check how many rows got place information
  rows_with_place <- merged_dataset %>%
    filter(!is.na(place_name)) %>%
    nrow()
  
  rows_without_place <- merged_dataset %>%
    filter(is.na(place_name)) %>%
    nrow()
  
  cat("Rows with place information:", rows_with_place, 
      "(", round(rows_with_place/rows_after*100, 2), "%)\n")
  
  cat("Rows without place information:", rows_without_place, 
      "(", round(rows_without_place/rows_after*100, 2), "%)\n")
  
  # Print sample of rows missing place information
  if(rows_without_place > 0) {
    cat("Sample of rows missing place information:\n")
    print(merged_dataset %>%
          filter(is.na(place_name)) %>%
          select(all_of(ad_only_variables), blkgrp) %>%
          head(30))
  }
  
  return(merged_dataset)
}

# Function to merge ad city information into a dataset
merge_ad_city <- function(dataset, dataset_name) {
  cat("\nMerging HCITY_Ad into", dataset_name, "...\n")

  missing_join_cols <- setdiff(ad_city_join_keys, names(dataset))
  if (length(missing_join_cols) > 0) {
    cat("WARNING:", dataset_name, "is missing required join columns:",
        paste(missing_join_cols, collapse = ", "), "\n")
    dataset$HCITY_Ad <- NA_character_
    return(dataset)
  }

  rows_before <- nrow(dataset)

  merged_dataset <- dataset %>%
    mutate(TESTERID = as.character(TESTERID)) %>%
    left_join(ad_city_lookup, by = ad_city_join_keys)

  rows_after <- nrow(merged_dataset)
  rows_with_hcity_ad <- merged_dataset %>%
    filter(!is.na(HCITY_Ad) & HCITY_Ad != "") %>%
    nrow()
  rows_ambiguous <- merged_dataset %>%
    filter(HCITY_Ad_ambiguous %in% TRUE) %>%
    nrow()

  cat("Rows before merge:", rows_before, "\n")
  cat("Rows after merge:", rows_after, "\n")
  cat("Rows with HCITY_Ad:", rows_with_hcity_ad,
      "(", round(rows_with_hcity_ad / rows_after * 100, 2), "%)\n")
  cat("Rows tied to ambiguous ad-city lookup keys:", rows_ambiguous,
      "(", round(rows_ambiguous / rows_after * 100, 2), "%)\n")

  merged_dataset <- merged_dataset %>%
    select(-HCITY_Ad_ambiguous)

  return(merged_dataset)
}

# Merge place information into each dataset
hud_census_with_place <- merge_place_info(hud_census, "HUDprocessed_JPE_census_042021.rds")
hud_names_with_place <- merge_place_info(hud_names, "HUDprocessed_JPE_names_042021.rds")
hud_testscores_with_place <- merge_place_info(hud_testscores, "HUDprocessed_JPE_testscores_042021.rds")

# Merge ad city into each dataset
hud_census_with_place <- merge_ad_city(hud_census_with_place, "HUDprocessed_JPE_census_042021.rds")
hud_names_with_place <- merge_ad_city(hud_names_with_place, "HUDprocessed_JPE_names_042021.rds")
hud_testscores_with_place <- merge_ad_city(hud_testscores_with_place, "HUDprocessed_JPE_testscores_042021.rds")

# Save the merged datasets
cat("\nSaving merged datasets...\n")

# Save RDS files
saveRDS(hud_census_with_place, file.path(output_folder, "HUDprocessed_census_correct_cities.rds"))
saveRDS(hud_names_with_place, file.path(output_folder, "HUDprocessed_names_correct_cities.rds"))
saveRDS(hud_testscores_with_place, file.path(output_folder, "HUDprocessed_testscores_correct_cities.rds"))

cat("Merged datasets saved to RDS format\n")

# Save CSV files
cat("\nSaving merged datasets to CSV format...\n")

# Save CSV files
write.csv(hud_census_with_place, file.path(output_folder, "HUDprocessed_census_correct_cities.csv"), row.names = FALSE)
write.csv(hud_names_with_place, file.path(output_folder, "HUDprocessed_names_correct_cities.csv"), row.names = FALSE)
write.csv(hud_testscores_with_place, file.path(output_folder, "HUDprocessed_testscores_correct_cities.csv"), row.names = FALSE)

cat("Merged datasets saved to CSV format\n")



# Save Stata files
# Process census dataset for Stata compatibility
cat("\nPreparing census dataset for Stata...\n")
hud_census_stata <- hud_census_with_place %>%
  # Remove trailing periods
  rename_all(~ gsub("\\.$", "", .)) %>%
  # Replace dots and other illegal characters with underscores
  rename_all(~ gsub("\\.", "_", .)) %>%
  rename_all(~ gsub("\\-", "_", .)) %>%
  # Replace any other special characters that might cause issues
  rename_all(~ gsub("[^a-zA-Z0-9_]", "_", .)) %>%
  # Truncate to 32 chars
  rename_all(~ ifelse(nchar(.) > 32, substr(., 1, 32), .)) %>%
  # Drop any columns that include lists
  select_if(~ !is.list(.))

# Process names dataset for Stata compatibility
cat("Preparing names dataset for Stata...\n")
hud_names_stata <- hud_names_with_place %>%
  rename_all(~ gsub("\\.$", "", .)) %>%
  rename_all(~ gsub("\\.", "_", .)) %>%
  rename_all(~ gsub("\\-", "_", .)) %>%
  rename_all(~ gsub("[^a-zA-Z0-9_]", "_", .)) %>%
  rename_all(~ ifelse(nchar(.) > 32, substr(., 1, 32), .)) %>%
  select_if(~ !is.list(.))

# Process testscores dataset for Stata compatibility
cat("Preparing testscores dataset for Stata...\n")
hud_testscores_stata <- hud_testscores_with_place %>%
  rename_all(~ gsub("\\.$", "", .)) %>%
  rename_all(~ gsub("\\.", "_", .)) %>%
  rename_all(~ gsub("\\-", "_", .)) %>%
  rename_all(~ gsub("[^a-zA-Z0-9_]", "_", .)) %>%
  rename_all(~ ifelse(nchar(.) > 32, substr(., 1, 32), .)) %>%
  select_if(~ !is.list(.))

# Save to Stata format
haven::write_dta(hud_census_stata, file.path(output_folder, "HUDprocessed_census_correct_cities.dta"))
haven::write_dta(hud_names_stata, file.path(output_folder, "HUDprocessed_names_correct_cities.dta"))
haven::write_dta(hud_testscores_stata, file.path(output_folder, "HUDprocessed_testscores_correct_cities.dta"))

cat("Merged datasets saved to Stata format\n")

# Save de-duplication log
cat("\nDe-duplication summary:\n")
print(dedup_log)
write.csv(dedup_log, file.path(output_folder, "dedup_log.csv"), row.names = FALSE)
