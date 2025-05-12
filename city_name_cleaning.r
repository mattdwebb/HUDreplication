# Load necessary libraries
library(readr)
library(dplyr)
library(tigris)
library(sf)
library(haven)

# Set the working directory to the directory location of the github repository 
# This will be appended to the front of all addresses in the file
WORKING_DIRECTORY = "cities-from-geoid"

# Define the path to the output folder
output_folder <- paste0(WORKING_DIRECTORY, "/Data/Generated")
input_folder <- paste0(WORKING_DIRECTORY, "/Data/Original")


# Load the adsprocessed_JPE data
adsprocessed_data <- readRDS(paste0(input_folder, "/adsprocessed_JPE.rds"))

# Load census tracts and places data
# Using tigris package to get census geography data
# Note: Using cache = TRUE to avoid re-downloading data

# Set the year for the data (adjust as needed)
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

# Initialize an empty dataframe to store all tract-place mappings
all_tract_place_mappings <- data.frame()

# Process each state
for (state_fips in names(states_to_process)) {
  state_abbr <- states_to_process[state_fips]
  
  cat(paste0("\n\nProcessing state: ", state_abbr, " (FIPS: ", state_fips, ")\n"))
  
  # Define file paths for saving intermediate results
  intersection_file <- paste0(output_folder, "/Intersection Files/tract_place_intersection_", state_abbr, ".rds")
  
  # Get census tracts for the current state
  cat("Loading census tracts data for", state_abbr, "...\n")
  tracts <- tracts(state = state_abbr, cb = TRUE, year = census_year)
  cat(state_abbr, "census tracts loaded:", nrow(tracts), "tracts\n")

  # Check if the intersection file already exists to avoid reprocessing
  if (file.exists(intersection_file)) {
    cat("Loading existing tract-place intersection for", state_abbr, "...\n")
    tract_place_intersection <- readRDS(intersection_file)
  } else {
    # Get places for the current state
    cat("Loading places data for", state_abbr, "...\n")
    places <- places(state = state_abbr, cb = TRUE, year = 2020)
    cat(state_abbr, "places loaded:", nrow(places), "places\n")
    
    # Ensure both datasets have the same CRS
    st_crs(tracts) <- st_crs(places)
    
    # Perform spatial intersection
    cat("Performing spatial intersection between tracts and places for", state_abbr, "...\n")
    tract_place_intersection <- st_intersection(tracts, places)
    
    # Create the directory for intersection files if it doesn't exist
    intersection_dir <- paste0(output_folder, "/Intersection Files")
    if (!dir.exists(intersection_dir)) {
      cat("Creating directory for intersection files:", intersection_dir, "\n")
      dir.create(intersection_dir, recursive = TRUE)
    }

    # Save the intersection to avoid recomputing if the process is interrupted
    saveRDS(tract_place_intersection, file = intersection_file)
    cat("Saved intersection to", intersection_file, "\n")
  }
  
  # Calculate areas and coverage percentages
  cat("Calculating coverage percentages for", state_abbr, "...\n")
  
  # Make geometries valid
  tract_place_intersection <- st_make_valid(tract_place_intersection)
  
  # Calculate intersection areas
  tract_place_intersection$intersection_area <- st_area(tract_place_intersection)
  
  # Calculate tract areas
  tracts$tract_area <- st_area(tracts)
  
  # Join tract areas to intersection data
  tract_place_intersection <- tract_place_intersection %>%
    left_join(tracts %>% 
                st_drop_geometry() %>% 
                select(GEO_ID, tract_area), 
              by = "GEO_ID")
  
  # Calculate coverage percentage
  tract_place_intersection$coverage_pct <- as.numeric(tract_place_intersection$intersection_area / tract_place_intersection$tract_area)
  
  # For each tract, find the place with the largest coverage
  best_matches <- tract_place_intersection %>%
    group_by(GEO_ID) %>%
    arrange(desc(coverage_pct)) %>%
    slice(1) %>%
    ungroup()
  
  # Create clean dataset with tract-to-place mapping
  state_tract_place_mapping <- best_matches %>%
    st_drop_geometry() %>%
    select(tract_geoid = GEO_ID, place_geoid = GEOID, place_name = NAME.1, coverage_pct) %>%
    mutate(
      tract_geoid = sub("^.*US", "", tract_geoid),
      state_fips = state_fips,
      state_abbr = state_abbr
    )
  
  cat("Completed processing for", state_abbr, "\n")
  cat("Number of tracts matched to places:", nrow(state_tract_place_mapping), "\n")
  
  # Append to the combined dataframe
  all_tract_place_mappings <- rbind(all_tract_place_mappings, state_tract_place_mapping)
  
  # Clean up to free memory
  rm(tract_place_intersection, best_matches, state_tract_place_mapping)
  if (exists("tracts")) rm(tracts)
  if (exists("places")) rm(places)
  gc()
}

cat("\nSpatial merge completed for all states. Each tract is matched to at most one place (the one with largest coverage).\n")
cat("Total number of tracts matched to places:", nrow(all_tract_place_mappings), "\n")

intersection_dir <- paste0(output_folder, "/Intersection Files")

# Save the combined tract-place mapping
saveRDS(all_tract_place_mappings, file = paste0(intersection_dir, "/all_tract_place_mappings.rds"))

# Load the tract-place mapping, to run the file after it has been generated
tract_place_mapping <- readRDS(paste0(intersection_dir, "/all_tract_place_mappings.rds"))

# Display the first few rows of the mapping
cat("\nFirst few rows of the tract-to-place mapping:\n")
print(head(tract_place_mapping))




# Process all observations in adsprocessed_data
cat("\nProcessing all observations and merging with place information...\n")

# Extract tract GEOID from block GEOID (first 11 characters)
cat("\nExtracting tract GEOID from block GEOID and merging with place information...\n")
adsprocessed_data <- adsprocessed_data %>%
  mutate(tract_geoid = substr(GEOID10, 1, nchar(GEOID10)-4)) %>%
  mutate(tract_geoid = sprintf("%011s", tract_geoid))
  

# Merge with tract_place_mapping to get place information
adsprocessed_data <- adsprocessed_data %>%
  left_join(tract_place_mapping, by = c("tract_geoid"))

# For blocks without a corresponding place, fill in with county information
cat("\nFilling in county information for blocks without a corresponding place...\n")

# Extract county FIPS code (first 5 characters of GEOID10)
adsprocessed_data <- adsprocessed_data %>%
  mutate(county_geoid = substr(GEOID10, 1, 5))

# Get county names from tigris for all states
# Create an empty dataframe to store county information
county_lookup <- data.frame()

# Use the states_to_process list instead of extracting from data
# states_to_process contains state abbreviations as strings

# Loop through each state to get county information
for (state_abbr in states_to_process) {
  # Get state FIPS code from the state abbreviation
  state_info <- tigris::fips_codes %>%
    filter(state == state_abbr) %>%
    distinct(state, state_code) %>%
    slice(1)
  
  if (nrow(state_info) > 0) {
    # Get counties for this state
    counties_state <- tigris::counties(state = state_abbr, cb = TRUE, year = census_year)
    
    # Create county lookup for this state
    state_county_lookup <- counties_state %>%
      st_drop_geometry() %>%
      select(county_geoid = GEO_ID, county_name = NAME) %>%
      mutate(
        county_geoid = sub("^.*US", "", county_geoid),
        county_name = paste(county_name, " County"))
    
    # Append to the combined dataframe
    county_lookup <- rbind(county_lookup, state_county_lookup)
  }
}

# Join county names to the data
adsprocessed_data_processed <- adsprocessed_data %>%
  left_join(county_lookup, by = "county_geoid") %>%
  mutate(
    # If place_geoid is NA, use county_geoid instead
    place_geoid = ifelse(is.na(place_geoid), county_geoid, place_geoid),
    # If place_name is NA, use county_name instead
    place_name = ifelse(is.na(place_name), county_name, place_name)
  )

test <- adsprocessed_data_processed %>% 
  select(HCITY, GEOID10, CONTROL, STATEFP10, COUNTYFP10, tract_geoid, place_geoid, place_name, coverage_pct) %>%
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

# Create an index based on unique combinations of these ad variables
cat("\nCreating an index based on unique combinations of ad variables...\n")

# Select only the ad-related variables and create a unique identifier
ad_combinations <- adsprocessed_data_processed %>%
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
cat("\nSubsetting to specific tract_geoid values (39055312203 and 39055312202)...\n")
selected_tracts <- tract_place_mapping %>%
  filter(tract_geoid %in% c("39055312203", "39055312202"))

# Display the results
cat("Found", nrow(selected_tracts), "matching records\n")
if(nrow(selected_tracts) > 0) {
  cat("\nSelected tract information:\n")
  print(selected_tracts %>% 
        as.data.frame())
} else {
  cat("No matching records found for the specified GEOIDs\n")
}

# Note that both tract_geoids point to the place_name Chardon, so we can use either one without inaccuracy in our generated place names



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


# Load and check HUD_processed_JPE_census_042021.rds
cat("\nChecking HUD_processed_JPE_census_042021.rds...\n")
hud_census <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_census_042021.rds"))
hud_census_results <- compare_ad_variables(hud_census, "HUDprocessed_JPE_census_042021.rds")

# Load and check HUD_processed_JPE_names_042021.rds
cat("\nChecking HUD_processed_JPE_names_042021.rds...\n")
hud_names <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_names_042021.rds"))
hud_names_results <- compare_ad_variables(hud_names, "HUDprocessed_JPE_names_042021.rds")

# Load and check HUD_processed_JPE_testscores_042021.rds
cat("\nChecking HUD_processed_JPE_testscores_042021.rds...\n")
hud_testscores <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_testscores_042021.rds"))
hud_testscores_results <- compare_ad_variables(hud_testscores, "HUDprocessed_JPE_testscores_042021.rds")


# Merge place information from adsprocessed_data into the three datasets
cat("\nMerging place information from adsprocessed_data into the three datasets...\n")

# First, create a lookup dataframe with ad variables and place information
cat("Creating lookup dataframe with ad variables and place information...\n")
 place_lookup <- adsprocessed_data_processed %>%
  select(all_of(ad_only_variables), blkgrp, place_name, place_geoid) %>%
  mutate(blkgrp = ifelse(blkgrp == "390553122033", "390553122021", blkgrp)) %>%
  distinct()

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

# Merge place information into each dataset
hud_census_with_place <- merge_place_info(hud_census, "HUDprocessed_JPE_census_042021.rds")
hud_names_with_place <- merge_place_info(hud_names, "HUDprocessed_JPE_names_042021.rds")
hud_testscores_with_place <- merge_place_info(hud_testscores, "HUDprocessed_JPE_testscores_042021.rds")

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

















# Load the HUD processed tract data
HUDprocessed_tract <- readRDS("ChristensenandTimmins2022Data/HuD_Replication/Final Data Sets/HUDprocessed_tract.rds")
HUDprocessed_census <- readRDS("ChristensenandTimmins2022Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_census_042021.rds")









# Count the number of unique rows in HUDprocessed_tract
cat("\nCounting unique rows in HUDprocessed_tract...\n")

# Total number of rows
total_rows_hud <- nrow(HUDprocessed_tract)
cat("Total rows in HUDprocessed_tract:", total_rows_hud, "\n")

# Count distinct rows
distinct_rows_hud <- HUDprocessed_tract %>%
  distinct() %>%
  nrow()
cat("Number of unique rows in HUDprocessed_tract:", distinct_rows_hud, "\n")

# Calculate duplicates
duplicate_rows_hud <- total_rows_hud - distinct_rows_hud
cat("Number of duplicate rows:", duplicate_rows_hud, "\n")

# Calculate percentage of unique rows
pct_unique <- round((distinct_rows_hud / total_rows_hud) * 100, 2)
cat("Percentage of unique rows:", pct_unique, "%\n")



# Create a dataframe with only unique rows from HUDprocessed_tract
HUDprocessed_tract_unique <- HUDprocessed_tract %>%
  distinct()

# Count how many combinations of CONTROL and TESTERID have multiple rows
cat("\nChecking for multiple rows per CONTROL and TESTERID combination in unique HUD data...\n")

# Count occurrences of each CONTROL and TESTERID combination
control_tester_counts <- HUDprocessed_tract_unique %>%
  group_by(CONTROL, TESTERID) %>%
  summarize(count = n(), .groups = "drop")

# Find combinations that appear more than once
multiple_rows <- control_tester_counts %>%
  filter(count > 1)

# Display results
cat("Total unique CONTROL and TESTERID combinations:", nrow(control_tester_counts), "\n")
cat("Combinations with multiple rows:", nrow(multiple_rows), "\n")

if(nrow(multiple_rows) > 0) {
  cat("Distribution of row counts per combination:\n")
  multiple_rows %>%
    group_by(count) %>%
    summarize(combinations = n(), .groups = "drop") %>%
    arrange(count) %>%
    print(n = Inf)
  
  # Create a frequency table of differing columns
  cat("\nFrequency table of columns that differ across combinations:\n")
  
  # Initialize an empty vector to store all differing columns
  all_differing_columns <- character()
  
  # Loop through each combination with multiple rows
  for(i in 1:nrow(multiple_rows)) {
    # Get the rows for this combination
    example_rows <- HUDprocessed_tract_unique %>%
      filter(CONTROL == multiple_rows$CONTROL[i], 
             TESTERID == multiple_rows$TESTERID[i])
    
    # Find columns that differ
    differing_cols <- sapply(names(example_rows), function(col) {
      length(unique(example_rows[[col]])) > 1
    })
    
    # Add the differing column names to our vector
    if(sum(differing_cols) > 0) {
      all_differing_columns <- c(all_differing_columns, names(example_rows)[differing_cols])
    }
  }
  
  # Create and display the frequency table
  if(length(all_differing_columns) > 0) {
    diff_col_table <- table(all_differing_columns)
    diff_col_df <- as.data.frame(diff_col_table)
    names(diff_col_df) <- c("Column", "Frequency")
    diff_col_df <- diff_col_df %>%
      arrange(desc(Frequency))
    
    print(diff_col_df, row.names = FALSE)
    
    # Calculate percentage of combinations where each column differs
    total_combinations <- nrow(multiple_rows)
    cat("\nPercentage of combinations where each column differs:\n")
    diff_col_df %>%
      mutate(Percentage = round(Frequency / total_combinations * 100, 2)) %>%
      select(Column, Frequency, Percentage) %>%
      arrange(desc(Percentage)) %>%
      print(row.names = FALSE)
  } else {
    cat("No differing columns found despite multiple rows.\n")
  }
  
  # Show a few examples of combinations with multiple rows
  cat("\nExample combinations with multiple rows:\n")
  sample_combinations <- multiple_rows %>%
    slice_head(n = 5)
  
  for(i in 1:nrow(sample_combinations)) {
    cat("\nCombination", i, "- CONTROL:", sample_combinations$CONTROL[i], 
        "TESTERID:", sample_combinations$TESTERID[i], 
        "Row count:", sample_combinations$count[i], "\n")
    
    # Show what differs between these rows
    example_rows <- HUDprocessed_tract_unique %>%
      filter(CONTROL == sample_combinations$CONTROL[i], 
             TESTERID == sample_combinations$TESTERID[i])
    
    # Find columns that differ
    differing_cols <- sapply(names(example_rows), function(col) {
      length(unique(example_rows[[col]])) > 1
    })
    
    if(sum(differing_cols) > 0) {
      cat("Columns with different values:", paste(names(example_rows)[differing_cols], collapse = ", "), "\n")
    } else {
      cat("No differing columns found despite multiple rows.\n")
    }
  }
}

# Create a dataframe for rows where CONTROL and TESTERID are the same, 
# but HCITY.x differs, while logAdPrice is the same
cat("\nCreating dataframe for rows where CONTROL and TESTERID are the same, but HCITY.x differs, while logAdPrice is the same...\n")

# First, identify the combinations of CONTROL and TESTERID that have multiple rows
control_tester_combos <- HUDprocessed_tract_unique %>%
  group_by(CONTROL, TESTERID) %>%
  filter(n() > 1) %>%
  # Check if HCITY.x differs within the group
  filter(length(unique(HCITY.x)) > 1) %>%
  # Check if logAdPrice is the same within the group
  filter(length(unique(logAdPrice)) == 1) %>%
  filter(length(unique(logRecPrice)) == 1) %>%
  ungroup()

# Count how many such combinations we found
num_combos <- control_tester_combos %>%
  select(CONTROL, TESTERID) %>%
  distinct() %>%
  nrow()

cat("Found", num_combos, "combinations of CONTROL and TESTERID where HCITY.x differs but logAdPrice is the same.\n")

# If we found any such combinations, create a dataframe with just those rows
if(num_combos > 0) {
  # Create the dataframe
  city_diff_price_same_df <- HUDprocessed_tract_unique %>%
    semi_join(control_tester_combos, by = c("CONTROL", "TESTERID"))
  
  # Display summary information
  cat("Created dataframe with", nrow(city_diff_price_same_df), "rows.\n")
  cat("Number of unique CONTROL values:", length(unique(city_diff_price_same_df$CONTROL)), "\n")
  cat("Number of unique TESTERID values:", length(unique(city_diff_price_same_df$TESTERID)), "\n")
  
  # Show a few examples
  cat("\nExample rows from the dataframe:\n")
  example_controls <- city_diff_price_same_df %>%
    select(CONTROL) %>%
    distinct() %>%
    slice_head(n = 3) %>%
    pull(CONTROL)
  
  for(ctrl in example_controls) {
    cat("\nCONTROL:", ctrl, "\n")
    city_diff_price_same_df %>%
      filter(CONTROL == ctrl) %>%
      select(CONTROL, TESTERID, HCITY.x, logAdPrice) %>%
      print()
  }
} else {
  cat("No combinations found where CONTROL and TESTERID are the same, HCITY.x differs, but logAdPrice is the same.\n")
}



# Check if tester_assignment uniquely identifies each row by blkgrp and TESTERID
cat("\nChecking if tester_assignment uniquely identifies each row by blkgrp and TESTERID...\n")

# Count total rows
total_rows <- nrow(tester_assignment)
cat("Total rows in tester_assignment:", total_rows, "\n")

# Count unique combinations of blkgrp and TESTERID
unique_combinations <- tester_assignment %>%
  group_by(blkgrp, TESTERID) %>%
  summarize(count = n(), .groups = "drop")

# Count how many combinations appear more than once
duplicates <- unique_combinations %>% 
  filter(count > 1)

# For each duplicate combination, identify which columns differ
if(nrow(duplicates) > 0) {
  # Function to find differing columns for a specific blkgrp and TESTERID
  find_differing_columns <- function(bg, tid) {
    # Get all rows with this combination
    rows <- tester_assignment %>% 
      filter(blkgrp == bg, TESTERID == tid)
    
    # Check each column for differences
    diff_cols <- sapply(names(rows), function(col) {
      # Check if values in this column are all identical
      length(unique(rows[[col]])) > 1
    })
    
    # Return names of columns that differ
    paste(names(rows)[diff_cols], collapse = ", ")
  }
  
  # Apply the function to each duplicate combination
  duplicates <- duplicates %>%
    rowwise() %>%
    mutate(differing_columns = find_differing_columns(blkgrp, TESTERID))
}

# Create a data frame with just the duplicate rows
if(nrow(duplicates) > 0) {
  # Extract the differing columns from each row
  all_differing_cols <- duplicates %>%
    pull(differing_columns) %>%
    paste(collapse = ", ") %>%
    strsplit(", ") %>%
    unlist() %>%
    unique()
  
  # Count how often each column differs
  col_counts <- sapply(all_differing_cols, function(col) {
    sum(grepl(paste0("\\b", col, "\\b"), duplicates$differing_columns)) / nrow(duplicates)
  })
  
  # Find columns that differ in 30% or more of duplicate cases
  frequent_diff_cols <- names(col_counts[col_counts >= 0.3])
  
  cat("\nColumns that differ in 30% or more of duplicate cases:\n")
  print(frequent_diff_cols)
  
  # Create a data frame with just the duplicate rows and the frequently differing columns
  duplicate_rows <- data.frame()
  
  for(i in 1:nrow(duplicates)) {
    bg <- duplicates$blkgrp[i]
    tid <- duplicates$TESTERID[i]
    
    # Get all rows with this combination
    rows <- tester_assignment %>% 
      filter(blkgrp == bg, TESTERID == tid)
    
    # Add to the duplicate_rows data frame
    duplicate_rows <- bind_rows(duplicate_rows, rows)
  }
  
  # Select only the columns we want to keep
  duplicate_rows_subset <- duplicate_rows %>%
    select(blkgrp, TESTERID, all_of(frequent_diff_cols))
  
  cat("\nCreated data frame with", nrow(duplicate_rows_subset), "duplicate rows and", 
      length(frequent_diff_cols) + 2, "columns (blkgrp, TESTERID, and frequently differing columns).\n")
  
  # Display the first few rows
  cat("\nFirst few rows of the duplicate data:\n")
  print(head(duplicate_rows_subset))
} else {
  cat("No duplicates found, so no duplicate data frame created.\n")
}




if(nrow(duplicates) > 0) {
  cat("CONTROL and TESTERID do NOT uniquely identify rows.\n")
  cat("Number of duplicate combinations:", nrow(duplicates), "\n")
  cat("First few duplicate combinations:\n")
  print(head(duplicates))
} else {
  cat("CONTROL and TESTERID uniquely identify each row in the dataset.\n")
  cat("Number of unique CONTROL-TESTERID combinations:", nrow(unique_combinations), "\n")
}

# Additional check: distribution of testers per control
testers_per_control <- tester_assignment %>%
  group_by(CONTROL) %>%
  summarize(num_testers = n_distinct(TESTERID), .groups = "drop")

cat("\nDistribution of testers per control:\n")
print(table(testers_per_control$num_testers))
cat("Mean number of testers per control:", mean(testers_per_control$num_testers), "\n")
cat("Max number of testers per control:", max(testers_per_control$num_testers), "\n")


# Check if blkgrp uniquely identifies rows in tester_assignment
cat("\nChecking if blkgrp uniquely identifies rows in tester_assignment...\n")

total_rows_ta <- nrow(tester_assignment)
unique_blkgrp_ta <- length(unique(tester_assignment$blkgrp))

cat("Total rows in tester_assignment:", total_rows_ta, "\n")
cat("Number of unique blkgrp values:", unique_blkgrp_ta, "\n")

if(unique_blkgrp_ta == total_rows_ta) {
  cat("blkgrp uniquely identifies each row in tester_assignment.\n")
} else {
  cat("blkgrp does NOT uniquely identify rows in tester_assignment.\n")
  
  # Count occurrences of each blkgrp
  blkgrp_counts <- tester_assignment %>%
    group_by(blkgrp) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  cat("Distribution of rows per blkgrp value:\n")
  print(table(blkgrp_counts$count))
  cat("Top 5 most frequent blkgrp values:\n")
  print(head(blkgrp_counts, 5))
}



# Drop X and X.x columns if they exist
if("X" %in% names(tester_assignment)) {
  cat("\nDropping column 'X' from tester_assignment...\n")
  tester_assignment <- tester_assignment %>% select(-X)
}

if("X.x" %in% names(tester_assignment)) {
  cat("\nDropping column 'X.x' from tester_assignment...\n")
  tester_assignment <- tester_assignment %>% select(-X.x)
}

# Check for fully unique rows in tester_assignment
cat("\nChecking for fully unique rows in tester_assignment...\n")

total_rows <- nrow(tester_assignment)
unique_rows <- nrow(distinct(tester_assignment))

cat("Total rows in tester_assignment:", total_rows, "\n")
cat("Number of unique rows:", unique_rows, "\n")

if(unique_rows == total_rows) {
  cat("All rows in tester_assignment are unique.\n")
} else {
  cat("There are duplicate rows in tester_assignment.\n")
  cat("Number of duplicate rows:", total_rows - unique_rows, "\n")
  
  # Find duplicate rows
  duplicate_rows <- tester_assignment %>%
    group_by_all() %>%
    filter(n() > 1) %>%
    ungroup()
  
  cat("Number of distinct duplicate row patterns:", nrow(distinct(duplicate_rows)), "\n")
  
  if(nrow(duplicate_rows) > 0) {
    cat("First few duplicate rows:\n")
    print(head(duplicate_rows))
  }
}


# Identify testers who visited multiple cities
cat("\nIdentifying testers who visited multiple cities...\n")

# First, find all control-tester combinations with multiple distinct city names
multi_city_testers <- HUDprocessed_census %>% 
  group_by(CONTROL, TESTERID) %>% 
  summarize(unique_cities = n_distinct(HCITY.x), .groups = "drop") %>% 
  filter(unique_cities > 1)

cat("Number of tester-control combinations with multiple cities:", nrow(multi_city_testers), "\n")

# Now extract all rows for these testers to see the different cities they visited
multi_city_details <- HUDprocessed_census %>%
  inner_join(multi_city_testers, by = c("CONTROL", "TESTERID")) %>%
  select(CONTROL, TESTERID, HCITY.x, 
         matches("_Ad$|Ad$")) %>%
  distinct() %>%
  arrange(CONTROL, TESTERID)

if(nrow(multi_city_details) > 0) {
  cat("\nDetails of testers who visited multiple cities:\n")
  print(multi_city_details)
}

# Check if duplicate rows differ by ad variables in HUDprocessed_census
cat("\nChecking if duplicate rows differ by ad variables in HUDprocessed_census...\n")

# Identify all ad-related variables in the dataset, excluding those with "rec"
ad_variables <- names(HUDprocessed_census)[grepl("ad", names(HUDprocessed_census), ignore.case = TRUE) & 
                                          !grepl("rec", names(HUDprocessed_census), ignore.case = TRUE)]

cat("Ad-related variables found:", length(ad_variables), "\n")
if(length(ad_variables) > 0) {
  cat("Ad variables:", paste(ad_variables, collapse=", "), "\n")
}

# Find duplicate combinations of CONTROL and TESTERID in HUDprocessed_census
census_duplicates <- HUDprocessed_census %>%
  group_by(CONTROL, TESTERID) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)
# For each duplicate combination of CONTROL and TESTERID, check if ad variables differ
if(nrow(census_duplicates) > 0) {
  # Function to check if ad variables differ for a specific CONTROL and TESTERID
  check_ad_variables_differ <- function(ctrl, tid) {
    # Get all rows with this combination
    rows <- HUDprocessed_census %>% 
      filter(CONTROL == ctrl, TESTERID == tid)
    
    # Check each ad variable for differences
    diff_ad_vars <- sapply(ad_variables, function(col) {
      if(col %in% names(rows)) {
        # Check if values in this column are all identical
        return(length(unique(rows[[col]])) > 1)
      } else {
        return(FALSE)
      }
    })
    
    # Return names of ad variables that differ
    differing_vars <- names(diff_ad_vars)[diff_ad_vars]
    if(length(differing_vars) > 0) {
      return(paste(differing_vars, collapse=", "))
    } else {
      return("None")
    }
  }
  
  # Apply the function to each duplicate combination
  census_duplicates_with_ad_diffs <- census_duplicates %>%
    rowwise() %>%
    mutate(differing_ad_vars = check_ad_variables_differ(CONTROL, TESTERID))
  
  # Count how many duplicate combinations have differing ad variables
  ad_diff_count <- sum(census_duplicates_with_ad_diffs$differing_ad_vars != "None")
  
  cat("\nNumber of duplicate combinations with differing ad variables:", ad_diff_count, 
      "(", round(ad_diff_count/nrow(census_duplicates)*100, 2), "% of all duplicates)\n")
  
  # Create a dataframe with all rows that differ by ad columns
  if(ad_diff_count > 0) {
    # Get the combinations with differing ad variables
    diff_combinations <- census_duplicates_with_ad_diffs %>%
      filter(differing_ad_vars != "None")
    
    # Create an empty dataframe to store all rows with differing ad variables
    all_diff_rows <- data.frame()
    
    # For each combination, get all rows and add them to the dataframe
    for(i in 1:nrow(diff_combinations)) {
      ctrl <- diff_combinations$CONTROL[i]
      tid <- diff_combinations$TESTERID[i]
      
      # Get all rows for this combination
      rows <- HUDprocessed_census %>%
        filter(CONTROL == ctrl, TESTERID == tid) %>%
        select(CONTROL, TESTERID, HCITY.x, all_of(ad_variables))
      
      # Add to the dataframe
      all_diff_rows <- bind_rows(all_diff_rows, rows)
    }
    
    # Save the dataframe with all rows that differ by ad columns
    cat("\nCreated dataframe with", nrow(all_diff_rows), "rows that differ by ad columns\n")
    cat("Number of unique CONTROL-TESTERID combinations:", 
        all_diff_rows %>% distinct(CONTROL, TESTERID) %>% nrow(), "\n")
    
    # Show a sample of the dataframe
    cat("\nSample of rows that differ by ad columns:\n")
    print(head(all_diff_rows))
    
    # Show examples of duplicates with differing ad variables
    cat("\nExamples of duplicates with differing ad variables:\n")
    examples <- diff_combinations %>%
      slice_head(n = 5)
    
    for(i in 1:nrow(examples)) {
      cat("\nCONTROL:", examples$CONTROL[i], "TESTERID:", examples$TESTERID[i], "\n")
      cat("Differing ad variables:", examples$differing_ad_vars[i], "\n")
      
      # Show the actual rows
      example_rows <- HUDprocessed_census %>%
        filter(CONTROL == examples$CONTROL[i], TESTERID == examples$TESTERID[i]) %>%
        select(CONTROL, TESTERID, all_of(unlist(strsplit(examples$differing_ad_vars[i], ", "))))
      
      print(example_rows)
    }
  } else {
    cat("No duplicate combinations have differing ad variables.\n")
  }
} else {
  cat("No duplicate combinations of CONTROL and TESTERID found in HUDprocessed_census to check for ad variable differences.\n")
}


