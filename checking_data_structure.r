# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Define paths
input_folder <- "cities-from-geoid/Data/Generated"
output_folder <- "cities-from-geoid/Data/Generated"

# Load the datasets
cat("Loading HUDprocessed datasets...\n")
census <- read_csv(file.path(input_folder, "HUDprocessed_census_correct_cities_cleaned.csv"))
names <- read_csv(file.path(input_folder, "HUDprocessed_names_correct_cities_cleaned.csv"))
testscores <- read_csv(file.path(input_folder, "HUDprocessed_testscores_correct_cities_cleaned.csv"))

# Check how often HCITY and HCITY_REC match in the census dataset
cat("Checking HCITY and HCITY_REC match in census dataset...\n")

# Count total rows and check HCITY/HCITY_REC matches for census dataset
cat("\n=== CENSUS DATASET ===\n")
total_rows_census <- nrow(census)
cat("Total rows in census dataset:", total_rows_census, "\n")

# Count rows where both fields exist
rows_with_both_census <- census %>%
    filter(!is.na(hcity) & !is.na(hcity_rec)) %>%
    nrow()
cat("Rows with both HCITY and HCITY_REC:", rows_with_both_census, "\n")

# Count exact matches
exact_matches_census <- census %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity == hcity_rec) %>%
    nrow()
cat("Rows where HCITY exactly matches HCITY_REC:", exact_matches_census, 
        "(", round(exact_matches_census/rows_with_both_census*100, 2), "% of rows with both fields)\n")

# Find mismatches for examination
mismatches_census <- census %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity != hcity_rec) %>%
    select(hcity, hcity_rec) %>%
    distinct() %>%
    head(10)

if(nrow(mismatches_census) > 0) {
    cat("Sample of mismatched HCITY and HCITY_REC values:\n")
    print(mismatches_census)
}

# Check for names dataset
cat("\n=== NAMES DATASET ===\n")
total_rows_names <- nrow(names)
cat("Total rows in names dataset:", total_rows_names, "\n")

rows_with_both_names <- names %>%
    filter(!is.na(hcity) & !is.na(hcity_rec)) %>%
    nrow()
cat("Rows with both HCITY and HCITY_REC:", rows_with_both_names, "\n")

exact_matches_names <- names %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity == hcity_rec) %>%
    nrow()
cat("Rows where HCITY exactly matches HCITY_REC:", exact_matches_names, 
        "(", round(exact_matches_names/rows_with_both_names*100, 2), "% of rows with both fields)\n")

mismatches_names <- names %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity != hcity_rec) %>%
    select(hcity, hcity_rec) %>%
    distinct() %>%
    head(10)

if(nrow(mismatches_names) > 0) {
    cat("Sample of mismatched HCITY and HCITY_REC values:\n")
    print(mismatches_names)
}

# Check for testscores dataset
cat("\n=== TESTSCORES DATASET ===\n")
total_rows_testscores <- nrow(testscores)
cat("Total rows in testscores dataset:", total_rows_testscores, "\n")

rows_with_both_testscores <- testscores %>%
    filter(!is.na(hcity) & !is.na(hcity_rec)) %>%
    nrow()
cat("Rows with both HCITY and HCITY_REC:", rows_with_both_testscores, "\n")

exact_matches_testscores <- testscores %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity == hcity_rec) %>%
    nrow()
cat("Rows where HCITY exactly matches HCITY_REC:", exact_matches_testscores, 
        "(", round(exact_matches_testscores/rows_with_both_testscores*100, 2), "% of rows with both fields)\n")

mismatches_testscores <- testscores %>%
    filter(!is.na(hcity) & !is.na(hcity_rec) & hcity != hcity_rec) %>%
    select(hcity, hcity_rec) %>%
    distinct() %>%
    head(10)

if(nrow(mismatches_testscores) > 0) {
    cat("Sample of mismatched HCITY and HCITY_REC values:\n")
    print(mismatches_testscores)
}

# Load address data
cat("Loading address data...\n")
ads_addresses <- read_csv(file.path(input_folder, "adsprocessed_JPE_clean_addresses.csv"))
# Find all entries with CONTROL = "AQ-SB-4104-1"
aq_sb_entries_ads <- ads_addresses %>%
    filter(CONTROL == "AQ-SB-4104-1")

# Display the number of matching entries
cat("Number of entries with CONTROL = 'AQ-SB-4104-1':", nrow(aq_sb_entries), "\n")

# Show the first few rows if any matches found
if(nrow(aq_sb_entries) > 0) {
    cat("First few matching entries:\n")
    print(head(aq_sb_entries))
} else {
    cat("No entries found with CONTROL = 'AQ-SB-4104-1'\n")
}



# Define ad variables used for identification
ad_only_variables <- c("logAdPrice", "w2012pc_Ad", "b2012pc_Ad", 
                      "a2012pc_Ad", "hisp2012pc_Ad", "HCITY", "CONTROL")

# Create a lookup dataframe with ad variables and standardized addresses
cat("Creating lookup dataframe with ad variables and address information...\n")
address_lookup <- ads_addresses %>%
    select(logAdPrice, w2012pc_Ad, b2012pc_Ad, a2012pc_Ad, hisp2012pc_Ad, HCITY, CONTROL, standardized_address) %>%
    distinct(logAdPrice, w2012pc_Ad, b2012pc_Ad, a2012pc_Ad, hisp2012pc_Ad, CONTROL, .keep_all = TRUE)

# Check for duplicate ad variable combinations
duplicate_ad_combos <- address_lookup %>%
  group_by_at(ad_only_variables) %>%
  filter(n() > 1) %>%
  ungroup()

# Check for duplicate ad variable combinations
duplicate_ad_combos <- address_lookup %>%
  group_by_at(c("logAdPrice", "w2012pc_Ad", "b2012pc_Ad", 
                      "a2012pc_Ad", "hisp2012pc_Ad", "CONTROL")) %>%
  filter(n() > 1) %>%
  ungroup()

if(nrow(duplicate_ad_combos) > 0) {
  cat("WARNING:", nrow(duplicate_ad_combos), "rows have duplicate ad variable combinations\n")
} else {
  cat("All ad variable combinations are unique in the lookup dataframe\n")
}

# Function to merge address information into a dataset and save
merge_and_save <- function(dataset, dataset_name) {
  cat("\nProcessing", dataset_name, "...\n")
  
  # Perform the merge using all ad variables
  rows_before <- nrow(dataset)
  
  merged_dataset <- dataset %>%
    left_join(address_lookup, by = ad_only_variables)
  
  rows_after <- nrow(merged_dataset)
  
  cat("Rows before merge:", rows_before, "\n")
  cat("Rows after merge:", rows_after, "\n")
  
  # Check how many rows got address information
  rows_with_address <- merged_dataset %>%
    filter(!is.na(standardized_address)) %>%
    nrow()
  
  cat("Rows with address information:", rows_with_address, 
      "(", round(rows_with_address/rows_after*100, 2), "%)\n")
  
  # Save the merged dataset
  output_file <- file.path(output_folder, paste0(dataset_name, "_check.csv"))
  write_csv(merged_dataset, output_file)
  cat("Saved to", output_file, "\n")
  
  return(merged_dataset)
}

# Process each dataset
cities_checked <- merge_and_save(cities, "HUDprocessed_census")
cities_names_checked <- merge_and_save(cities_names, "HUDprocessed_census_names")
cities_testscores_checked <- merge_and_save(cities_testscores, "HUDprocessed_census_testscores")

cat("\nAll processing complete.\n")