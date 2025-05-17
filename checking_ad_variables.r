
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

########################################## VERIFYING AD VARIABLES AS CONTROL IDENTIFIERS ##############################################
# This section checks if CONTROL values uniquely identify combinations of ad variables (excluding CONTROL itself), and vice versa

# Load the HUDprocessed_JPE_census_042021 data
cat("\nLoading HUDprocessed_JPE_census_042021 data...\n")
hudprocessed <- readRDS(paste0(input_folder, "/HUDprocessed_JPE_census_042021.rds"))

# Define ad variables (excluding CONTROL)
ad_vars <- c("logAdPrice", "stfid_Ad", "w2012pc_Ad", "b2012pc_Ad", 
             "a2012pc_Ad", "hisp2012pc_Ad", "oth2012pc_Ad")

cat("\nCreating an index based on unique combinations of ad variables (excluding CONTROL) in HUDprocessed...\n")

# Create a unique identifier for each ad variable combination (excluding CONTROL)
ad_combinations <- hudprocessed %>%
  select(CONTROL, all_of(ad_vars)) %>%
  mutate(ad_combo_id = group_indices(., across(all_of(ad_vars)))) %>%
  arrange(ad_combo_id)

cat("\nIdentifying CONTROL values with multiple ad variable combinations and vice versa in HUDprocessed...\n")

# CONTROLs with multiple ad variable combinations
control_with_multiple_combos <- ad_combinations %>%
  group_by(CONTROL) %>%
  summarize(
    unique_ad_combos = n_distinct(ad_combo_id),
    ad_combo_ids = paste(sort(unique(ad_combo_id)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(unique_ad_combos > 1) %>%
  mutate(has_multiple_ad_combos = TRUE)

# Ad variable combinations associated with multiple CONTROLs
ad_combos_with_multiple_controls <- ad_combinations %>%
  group_by(ad_combo_id) %>%
  summarize(
    unique_controls = n_distinct(CONTROL),
    control_list = paste(sort(unique(CONTROL)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(unique_controls > 1) %>%
  mutate(spans_multiple_controls = TRUE)

# Print summary statistics
cat("Found", nrow(control_with_multiple_combos), "CONTROL values with multiple ad variable combinations in HUDprocessed\n")
cat("Found", nrow(ad_combos_with_multiple_controls), "ad variable combinations spanning multiple CONTROL values in HUDprocessed\n")

# For the CONTROL values that have multiple ad variable combinations, add the ad variable columns back in and print only the first 20 rows
cat("\nPrinting details of CONTROL values with multiple ad variable combinations in HUDprocessed (with ad variable columns)...\n")
if (nrow(control_with_multiple_combos) > 0) {
  control_multi_combos_details <- ad_combinations %>%
    filter(CONTROL %in% control_with_multiple_combos$CONTROL) %>%
    select(CONTROL, all_of(ad_vars), ad_combo_id) %>%
    arrange(CONTROL, ad_combo_id)
  print(head(control_multi_combos_details, 20))
} else {
  cat("No CONTROL values with multiple ad variable combinations found.\n")
}

# For the ad variable combinations that span multiple CONTROL values, add the ad variable columns back in and print only the first 20 rows
cat("\nPrinting details of ad variable combinations spanning multiple CONTROL values in HUDprocessed (with ad variable columns)...\n")
if (nrow(ad_combos_with_multiple_controls) > 0) {
  ad_combos_multi_controls_details <- ad_combinations %>%
    filter(ad_combo_id %in% ad_combos_with_multiple_controls$ad_combo_id) %>%
    select(ad_combo_id, all_of(ad_vars), CONTROL) %>%
    arrange(ad_combo_id, CONTROL)
  print(head(ad_combos_multi_controls_details, 20))
} else {
  cat("No ad variable combinations spanning multiple CONTROL values found.\n")
}

########################################## CHECKING IF AD VARIABLES UNIQUELY IDENTIFY ADDRESSES ##############################################
# Load the clean addresses data
ads_clean_addresses <- readr::read_csv("cities-from-geoid/Data/Generated/adsprocessed_JPE_clean_addresses.csv", show_col_types = FALSE)

# Identify CONTROLs with multiple standardized_address values
control_with_multiple_addresses <- ads_clean_addresses %>%
  group_by(CONTROL) %>%
  summarize(
    unique_addresses = n_distinct(standardized_address),
    address_list = paste(sort(unique(standardized_address)), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(unique_addresses > 1) %>%
  mutate(has_multiple_addresses = TRUE)

cat("\nFound", nrow(control_with_multiple_addresses), "CONTROL values with multiple standardized_address values in adsprocessed_JPE_clean_addresses.csv\n")

# For these CONTROLs, print a dataframe with, for each CONTROL, the unique list of standardized addresses and the ad variables defined in ad_vars.
# Also, check if for different addresses under the same CONTROL, the ad variables are identical.

ad_vars <- c("logAdPrice", "w2012pc_Ad", "b2012pc_Ad", 
             "a2012pc_Ad", "hisp2012pc_Ad")

if (nrow(control_with_multiple_addresses) > 0) {
  cat("\nSummary of standardized_address and ad variables for each CONTROL with multiple addresses:\n")
  
  same_ad_vars_count <- 0
  diff_ad_vars_count <- 0
  total_controls <- length(control_with_multiple_addresses$CONTROL)
  
  for (ctrl in control_with_multiple_addresses$CONTROL) {
    subset_df <- ads_clean_addresses %>%
      filter(CONTROL == ctrl) %>%
      select(CONTROL, standardized_address, all_of(ad_vars))
    
    # Remove duplicate rows (in case the same address/ad_vars combo appears more than once)
    unique_rows <- subset_df %>%
      distinct()
    
    # Check if the ad variables are identical for all addresses under this CONTROL
    ad_vars_only <- unique_rows %>%
      select(all_of(ad_vars)) %>%
      distinct()
    ad_vars_identical <- nrow(ad_vars_only) == 1
    
    cat("\nCONTROL:", ctrl, "\n")
    print(unique_rows)
    if (ad_vars_identical) {
      cat("All standardized_address values for this CONTROL have identical ad variable values.\n")
      same_ad_vars_count <- same_ad_vars_count + 1
    } else {
      cat("Different standardized_address values for this CONTROL have DIFFERENT ad variable values.\n")
      diff_ad_vars_count <- diff_ad_vars_count + 1
    }
  }
  
  cat("\nSummary across all CONTROLs with multiple addresses:\n")
  cat("Number with identical ad variables across addresses:", same_ad_vars_count, "\n")
  cat("Number with different ad variables across addresses:", diff_ad_vars_count, "\n")
  cat("Ratio (identical/different): ", 
      ifelse(diff_ad_vars_count > 0, 
             paste0(round(same_ad_vars_count / diff_ad_vars_count, 3)), 
             "All identical (no different)"), "\n")
  cat("Fraction identical: ", round(same_ad_vars_count / total_controls, 3), 
      " (", same_ad_vars_count, "/", total_controls, ")\n")
  cat("Fraction different: ", round(diff_ad_vars_count / total_controls, 3), 
      " (", diff_ad_vars_count, "/", total_controls, ")\n")
  
} else {
  cat("No CONTROL values with multiple standardized_address values found.\n")
}
