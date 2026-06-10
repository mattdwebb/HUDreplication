# ACS 2008-2012 5-Year Tract Data Merger
# Author: Anthony McCanny
# Date: 2025-09-21
# Purpose: Pull ACS tract data and merge with existing geocoded HDS data

# =================================================================================================== #
# SETUP: LOAD REQUIRED PACKAGES AND API KEYS
# =================================================================================================== #

packages <- c(
  "tidyverse",    # Data manipulation
  "tidycensus",   # Census API interface
  "readr"         # File I/O
)

# Install missing packages and load all
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load API keys
api_keys_path <- file.path("reconstructed_sample", "api_keys.R")
if (!file.exists(api_keys_path)) {
  stop("Missing reconstructed_sample/api_keys.R. Copy reconstructed_sample/api_keys_template.R to api_keys.R and add your Census API key.")
}
source(api_keys_path)

# Set Census API key for tidycensus only if not already set
if (is.null(Sys.getenv("CENSUS_API_KEY")) || Sys.getenv("CENSUS_API_KEY") == "") {
  census_api_key(CENSUS_API_KEY, install = TRUE)
}

# =================================================================================================== #
# ACS VARIABLE DEFINITIONS (CHRISTENSEN & TIMMINS 2022)
# =================================================================================================== #

# Define ACS variables matching Christensen & Timmins (2022) methodology
acs_variables <- c(
  # Poverty (Table B17001 - Poverty Status)
  poverty_total = "B17001_001",      # Total population for poverty determination
  poverty_below = "B17001_002",      # Income below poverty level

  # Education (Table B15003 - Educational Attainment)
  education_total = "B15003_001",    # Total population 25 years and over
  education_bachelors = "B15003_022", # Bachelor's degree
  education_masters = "B15003_023",   # Master's degree
  education_professional = "B15003_024", # Professional degree
  education_doctorate = "B15003_025", # Doctorate degree

  # Occupation (Table C24010 - Occupation by Sex)
  occupation_total = "C24010_001",   # Total civilian employed population 16+
  occupation_mgmt_male = "C24010_003",   # Management, business, science, arts - Male
  occupation_mgmt_female = "C24010_039", # Management, business, science, arts - Female

  # Household Type (Table B11001 - Household Type)
  total_households = "B11001_001",   # Total households
  family_households = "B11001_002",  # Family households
  single_parent_male = "B11001_005",   # Male householder, no spouse, with children
  single_parent_female = "B11001_006", # Female householder, no spouse, with children

  # Housing Tenure (Table B25003 - Tenure)
  tenure_total = "B25003_001",       # Total occupied housing units
  tenure_owned = "B25003_002",       # Owner occupied

  # Median Household Income (Table B19013)
  median_income = "B19013_001",      # Median household income (past 12 months, inflation-adjusted)

  # Household Income Distribution (Table B19001)
  income_total = "B19001_001",
  income_lt10 = "B19001_002",
  income_10_14 = "B19001_003",
  income_15_19 = "B19001_004",
  income_20_24 = "B19001_005",
  income_25_29 = "B19001_006",
  income_30_34 = "B19001_007",
  income_35_39 = "B19001_008",
  income_40_44 = "B19001_009",
  income_45_49 = "B19001_010",
  income_50_59 = "B19001_011",
  income_60_74 = "B19001_012",
  income_75_99 = "B19001_013",
  income_100_124 = "B19001_014",
  income_125_149 = "B19001_015",
  income_150_199 = "B19001_016",
  income_200_plus = "B19001_017",

  # Household Income Distribution by Race of Householder (White Alone, Table B19001A)
  # C&T's Table 7 variables decompose the neighborhood white share by the income
  # distribution of white-alone householders, so the three components sum to
  # the neighborhood white population share.
  white_income_total = "B19001A_001",
  white_income_lt10 = "B19001A_002",
  white_income_10_14 = "B19001A_003",
  white_income_15_19 = "B19001A_004",
  white_income_20_24 = "B19001A_005",
  white_income_25_29 = "B19001A_006",
  white_income_30_34 = "B19001A_007",
  white_income_35_39 = "B19001A_008",
  white_income_40_44 = "B19001A_009",
  white_income_45_49 = "B19001A_010",
  white_income_50_59 = "B19001A_011",
  white_income_60_74 = "B19001A_012",
  white_income_75_99 = "B19001A_013",
  white_income_100_124 = "B19001A_014",
  white_income_125_149 = "B19001A_015",
  white_income_150_199 = "B19001A_016",
  white_income_200_plus = "B19001A_017",

  # Race and Hispanic Origin (Table B03002)
  race_total = "B03002_001",         # Total population
  race_white_alone = "B03002_003",   # Not Hispanic or Latino, White alone
  race_black_alone = "B03002_004",   # Black or African American alone
  race_asian_alone = "B03002_006",   # Asian alone
  race_hispanic = "B03002_012"       # Hispanic or Latino
)

# =================================================================================================== #
# MAIN FUNCTION: GET ACS DATA FOR SPECIFIC BLOCK GROUPS
# =================================================================================================== #

get_acs_data_for_geoids <- function(geoid_list, year = 2012) {
  # Pull ACS 2008-2012 5-year data for specific block group GEOIDs

  # Args:
  #   geoid_list: Vector of 12-digit block group GEOIDs
  #   year: ACS year (2012 for 2008-2012 5-year estimates)

  # Returns:
  #   Tibble with processed ACS variables for the specified block groups

  # Extract unique states from GEOIDs
  unique_states <- unique(str_sub(geoid_list, 1, 2))
  cat(sprintf("Fetching ACS data for %d block groups across %d states...\n",
              length(unique(geoid_list)), length(unique_states)))

  all_data <- tibble()

  for (state in unique_states) {
    cat(sprintf("Processing state %s...\n", state))

    # Get all block group data for this state
    state_data <- get_acs(
      geography = "tract",
      variables = acs_variables,
      state = state,
      year = year,
      survey = "acs5",
      output = "wide"
    )

    all_data <- bind_rows(all_data, state_data)
    Sys.sleep(0.1)  # Rate limiting
  }

  # Filter to only the GEOIDs we need and process variables
  filtered_data <- all_data %>%
    filter(GEOID %in% geoid_list) %>%
    process_acs_variables()

  cat(sprintf("Retrieved data for %d of %d requested tracts\n",
              nrow(filtered_data), length(unique(geoid_list))))

  return(filtered_data)
}

# =================================================================================================== #
# PROCESS ACS VARIABLES TO MATCH CHRISTENSEN & TIMMINS
# =================================================================================================== #

process_acs_variables <- function(acs_raw) {
  
  # Calculate derived variables matching Christensen & Timmins (2022) methodology
  

  acs_processed <- acs_raw %>%
    # Clean GEOID to ensure 12 digits
    rename(
      tract_geoid = GEOID
    ) %>%

    # Calculate Christensen & Timmins variables
    mutate(
      # 1. Poverty rate (share of households at/below poverty line)
      poverty_rate = ifelse(poverty_totalE > 0, poverty_belowE / poverty_totalE, NA),

      # 2. College graduate rate (bachelor's degree or higher)
      college_graduate_rate = ifelse(
        education_totalE > 0,
        (education_bachelorsE + education_mastersE +
         education_professionalE + education_doctorateE) / education_totalE,
        NA
      ),

      # 3. High-skilled occupation rate (management, business, science, arts)
      high_skilled_rate = ifelse(
        occupation_totalE > 0,
        (occupation_mgmt_maleE + occupation_mgmt_femaleE) / occupation_totalE,
        NA
      ),

      # 4. Single-parent household rate
      single_parent_rate = ifelse(
        total_householdsE > 0,
        (single_parent_maleE + single_parent_femaleE) / total_householdsE,
        NA
      ),

      # 4b. Share of family households without a father (female householder, no spouse)
      nodad_rate = ifelse(
        family_householdsE > 0,
        single_parent_femaleE / family_householdsE,
        NA
      ),

      # 5. Home ownership rate
      ownership_rate = ifelse(tenure_totalE > 0, tenure_ownedE / tenure_totalE, NA),

      # 5b. Median household income (ACS estimate)
      median_income = ifelse(median_incomeE > 0, median_incomeE, NA),

      # 5c. Household income distribution shares
      income_low_share = ifelse(
        income_totalE > 0,
        (income_lt10E + income_10_14E + income_15_19E + income_20_24E +
         income_25_29E + income_30_34E + income_35_39E + income_40_44E +
         income_45_49E) / income_totalE,
        NA
      ),
      income_mid_share = ifelse(
        income_totalE > 0,
        (income_50_59E + income_60_74E + income_75_99E) / income_totalE,
        NA
      ),
      income_high_share = ifelse(
        income_totalE > 0,
        (income_100_124E + income_125_149E + income_150_199E + income_200_plusE) / income_totalE,
        NA
      ),

      # 6. Racial composition (as shares of total population)
      percent_white = ifelse(race_totalE > 0, race_white_aloneE / race_totalE, NA),
      percent_black = ifelse(race_totalE > 0, race_black_aloneE / race_totalE, NA),
      percent_asian = ifelse(race_totalE > 0, race_asian_aloneE / race_totalE, NA),
      percent_hispanic = ifelse(race_totalE > 0, race_hispanicE / race_totalE, NA),

      # 7. White population share decomposed by household income bins
      white_income_low_share = ifelse(
        white_income_totalE > 0,
        (white_income_lt10E + white_income_10_14E + white_income_15_19E +
         white_income_20_24E + white_income_25_29E + white_income_30_34E +
         white_income_35_39E + white_income_40_44E + white_income_45_49E) /
          white_income_totalE,
        NA
      ),
      white_income_mid_share = ifelse(
        white_income_totalE > 0,
        (white_income_50_59E + white_income_60_74E + white_income_75_99E) /
          white_income_totalE,
        NA
      ),
      white_income_high_share = ifelse(
        white_income_totalE > 0,
        (white_income_100_124E + white_income_125_149E +
         white_income_150_199E + white_income_200_plusE) /
          white_income_totalE,
        NA
      ),
      white_low_income_component = percent_white * white_income_low_share,
      white_mid_income_component = percent_white * white_income_mid_share,
      white_high_income_component = percent_white * white_income_high_share
    ) %>%

    # Select final columns matching Christensen & Timmins variable names
    select(
      tract_geoid, NAME,
      poverty_rate, college_graduate_rate, high_skilled_rate,
      single_parent_rate, nodad_rate, ownership_rate, median_income,
      income_low_share, income_mid_share, income_high_share,
      white_income_low_share, white_income_mid_share, white_income_high_share,
      white_low_income_component, white_mid_income_component, white_high_income_component,
      percent_white, percent_black, percent_asian, percent_hispanic,
      # Keep raw counts for reference
      poverty_totalE, education_totalE, occupation_totalE,
      total_householdsE, tenure_totalE, race_totalE, white_income_totalE
    )

  return(acs_processed)
}

# =================================================================================================== #
# MERGE FUNCTION FOR HDS DATA
# =================================================================================================== #

merge_acs <- function(hds_data, geoid_col = "tract_geoid") {
  
  # Merge HDS property data with ACS tract data

  # Args:
  #   hds_data: Data frame with HDS property data including geocoded block group IDs
  #   geoid_col: Column name containing 11-digit tract GEOIDs

  # Returns:
  #   Data frame with HDS data merged with ACS variables
  

  # Extract unique GEOIDs from HDS data
  geoid_list <- hds_data %>%
    pull(!!sym(geoid_col)) %>%
    unique() %>%
    na.omit() 

  cat(sprintf("Found %d unique tracts in HDS data\n", length(geoid_list)))

  # Get ACS data for these specific block groups
  acs_data <- get_acs_data_for_geoids(geoid_list, year = 2012)

  # Check matching GEOIDs between ACS and HDS data
  matched_geoids <- intersect(acs_data$tract_geoid, hds_data$tract_geoid)
  unmatched_geoids <- setdiff(hds_data[[geoid_col]], acs_data$tract_geoid)
  cat(sprintf("Matched GEOIDs: %d\n", length(matched_geoids)))
  cat(sprintf("Unmatched GEOIDs: %d\n", length(unmatched_geoids)))
  if (length(unmatched_geoids) > 0) {
    cat("Unmatched GEOIDs (first 10):\n")
    print(head(unmatched_geoids, 10))
  }

  # Merge with HDS data
  merged_data <- hds_data %>%
    left_join(acs_data, by = geoid_col)

  

  # Report merge success
  n_matched <- sum(!is.na(merged_data$poverty_rate))
  n_total <- nrow(merged_data)
  cat(sprintf("Successfully merged ACS data for %d of %d properties (%.1f%%)\n",
              n_matched, n_total, 100 * n_matched / n_total))

  return(merged_data)
}


# =================================================================================================== #
# EXAMPLE: RUN THE ACS MATCHING PROCESS
# =================================================================================================== #

# # 1. Load your geocoded HDS data (sample for testing)
# cat("Loading HDS data sample...\n")
# hds_data_full <- read_csv("Data/sales_tester_rechomes_geocoded.csv")

# # Take a sample of 100 rows for testing
# set.seed(123)  # For reproducible sampling
# hds_data <- hds_data_full %>%
#   slice_sample(n = min(100, nrow(hds_data_full))) %>%
#   mutate(tract_geoid = str_sub(blockgroup_geoid, 1, 11))

# cat(sprintf("Using sample of %d rows from %d total rows\n", 
#             nrow(hds_data), nrow(hds_data_full)))

# # 2. Merge with ACS data
# cat("Starting ACS data merge...\n")
# hds_with_acs <- merge_acs(hds_data, geoid_col = "tract_geoid")

# # # 3. Save results
# # output_file <- "Data/hds_sample_with_acs_variables.csv"
# # write_csv(hds_with_acs, output_file)
# # cat(sprintf("Results saved to: %s\n", output_file))

# # 4. Summary of ACS variables
# cat("\nSummary of ACS variables:\n")
# hds_with_acs %>%
#   select(poverty_rate, college_graduate_rate, high_skilled_rate,
#          single_parent_rate, ownership_rate, percent_white, percent_black,
#          percent_asian, percent_hispanic) %>%
#   summary() %>%
#   print()

# cat("\nACS matching process completed successfully!\n")


# available_variables <- tidycensus::load_variables(2012, "acs5")
# library(stringdist)

# # Get ACS variable codes from acs_variables
# acs_codes <- unname(acs_variables)

# # Fuzzy match: subset available_variables to those with codes close to acs_codes
# matched_vars <- available_variables %>%
#   filter(
#     sapply(name, function(nm) min(stringdist::stringdist(nm, acs_codes, method = "jw")) < 0.1)
#   )

# print(matched_vars)
