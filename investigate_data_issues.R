# Investigate data quality issues with CONTROL and AdPrice
library(dplyr)
library(readr)

# Load both datasets
ads_data <- readRDS("Data/Original/adsprocessed_JPE.rds")
clean_addresses <- read_csv("Data/Generated/adsprocessed_JPE_clean_addresses.csv")

cat("INVESTIGATING DATA QUALITY ISSUES\n")
cat("=================================\n\n")

# 1. Examine multiple controls per address issue
cat("1. MULTIPLE CONTROLS PER ADDRESS ANALYSIS:\n")

if("standardized_address" %in% colnames(clean_addresses)) {
  address_control_counts <- clean_addresses %>%
    filter(!is.na(standardized_address) & !is.na(CONTROL)) %>%
    group_by(standardized_address) %>%
    summarise(
      num_controls = n_distinct(CONTROL),
      controls = paste(unique(CONTROL), collapse = "; "),
      .groups = "drop"
    ) %>%
    arrange(desc(num_controls))
  
  multi_control_addresses <- address_control_counts %>%
    filter(num_controls > 1)
  
  cat(sprintf("Total addresses with multiple CONTROLs: %d\n", nrow(multi_control_addresses)))
  cat(sprintf("Max CONTROLs per address: %d\n", max(address_control_counts$num_controls)))
  
  cat("\nTop 10 addresses with most CONTROLs:\n")
  print(head(multi_control_addresses, 10))
  
} else {
  cat("standardized_address column not found in clean_addresses file\n")
}

cat("\n2. ADPRICE VARIATIONS WITHIN SAME CONTROL:\n")

# Check for AdPrice variations within same CONTROL
price_variations <- ads_data %>%
  filter(!is.na(CONTROL) & !is.na(AdPrice)) %>%
  group_by(CONTROL) %>%
  summarise(
    num_prices = n_distinct(AdPrice),
    min_price = min(AdPrice),
    max_price = max(AdPrice),
    price_range = max_price - min_price,
    prices = paste(unique(AdPrice), collapse = "; "),
    num_testers = n(),
    .groups = "drop"
  ) %>%
  filter(num_prices > 1) %>%
  arrange(desc(price_range))

cat(sprintf("CONTROLs with multiple AdPrices: %d\n", nrow(price_variations)))

if(nrow(price_variations) > 0) {
  cat("\nTop 10 CONTROLs with largest price variations:\n")
  print(head(price_variations, 10))
  
  # Look at a specific example in detail
  example_control <- price_variations$CONTROL[1]
  cat(sprintf("\nDetailed look at CONTROL '%s':\n", example_control))
  
  example_details <- ads_data %>%
    filter(CONTROL == example_control) %>%
    select(CONTROL, TESTERID, AdPrice, logAdPrice, HCITY, Address, 
           w2012pc_Ad, b2012pc_Ad, a2012pc_Ad, Latitude, Longitude) %>%
    arrange(AdPrice)
  
  print(example_details)
}

cat("\n3. IDENTIFYING TRULY STABLE PROPERTY CHARACTERISTICS:\n")

# Look for characteristics that are truly property-specific (not tester-specific)
property_characteristics <- c("Latitude", "Longitude", "w2012pc_Ad", "b2012pc_Ad", 
                             "a2012pc_Ad", "hisp2012pc_Ad", "stfid_Ad", "zip_Ad")

# Test if these vary within CONTROL groups
characteristic_stability <- list()

for(char in property_characteristics) {
  if(char %in% colnames(ads_data)) {
    variations <- ads_data %>%
      filter(!is.na(CONTROL) & !is.na(.data[[char]])) %>%
      group_by(CONTROL) %>%
      summarise(
        num_unique = n_distinct(.data[[char]]),
        .groups = "drop"
      ) %>%
      filter(num_unique > 1)
    
    characteristic_stability[[char]] <- nrow(variations)
    cat(sprintf("%-15s: %d CONTROLs with multiple values\n", char, nrow(variations)))
  }
}

cat("\n4. ALTERNATIVE PROPERTY IDENTIFICATION STRATEGY:\n")

# Try combinations that might be more stable
cat("Testing coordinate-based identification:\n")

coord_combinations <- ads_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    num_controls = n_distinct(CONTROL, na.rm = TRUE),
    num_testers = n(),
    price_variations = n_distinct(AdPrice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(num_controls))

cat(sprintf("Unique coordinate pairs: %d\n", nrow(coord_combinations)))
cat(sprintf("Coordinate pairs with multiple CONTROLs: %d\n", 
            sum(coord_combinations$num_controls > 1)))

cat("\nTop coordinate pairs with most CONTROLs:\n")
print(head(coord_combinations, 5))

cat("\n5. DEMOGRAPHIC + COORDINATE COMBINATION:\n")

# Try demographic characteristics + coordinates for property ID
demo_coord_test <- ads_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude) & 
         !is.na(w2012pc_Ad) & !is.na(b2012pc_Ad)) %>%
  group_by(Latitude, Longitude, w2012pc_Ad, b2012pc_Ad, a2012pc_Ad) %>%
  summarise(
    num_controls = n_distinct(CONTROL, na.rm = TRUE),
    num_testers = n(),
    controls = paste(unique(CONTROL), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(num_controls))

cat(sprintf("Unique demographic+coordinate combinations: %d\n", nrow(demo_coord_test)))
cat(sprintf("With multiple CONTROLs: %d\n", sum(demo_coord_test$num_controls > 1)))

if(sum(demo_coord_test$num_controls > 1) > 0) {
  cat("\nCombinations still with multiple CONTROLs:\n")
  multi_demo_coord <- demo_coord_test %>% filter(num_controls > 1)
  print(head(multi_demo_coord, 5))
}