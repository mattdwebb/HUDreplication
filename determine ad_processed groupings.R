# Load necessary libraries
library(readr)
library(dplyr)
library(tigris)
library(sf)
library(haven)
library(tidyr)

# Set the working directory to the directory location of the github repository 
# This will be appended to the front of all addresses in the file
setwd("~/GitHub/HUDreplication")
WORKING_DIRECTORY = getwd()

# Define the path to the output folder
output_folder <- paste0(WORKING_DIRECTORY, "/Data/Generated")
input_folder <- paste0(WORKING_DIRECTORY, "/Data/Original")

# Load the adsprocessed_JPE data
adsprocessed_data <- read_csv(paste0(input_folder, "/adsprocessed_JPE.csv"))

# filter out visits after the first one?
filtered_data <- adsprocessed_data %>% filter(SEQUENCE.x == 1)

# group by all values and count when values are not the same across grouped rows
groups = filtered_data %>% 
  group_by(TESTERID, CONTROL, blkgrp, HPRICE) %>% 
  summarise(across(everything(), ~n_distinct(.)), .groups = "drop") %>% 
  mutate(max_distinct = do.call(pmax, select(., -TESTERID, -CONTROL, -blkgrp, -HPRICE)))

# keep only counts > 1 (counts == 1 mean there is only one value)
filtered_groups = groups %>% filter(max_distinct > 1)

# get list of column names where more than 1 distinct value
col_names = filtered_groups %>%
  select(-TESTERID, -CONTROL, -blkgrp, -HPRICE, -max_distinct) %>%
  pivot_longer(everything(), names_to = "col", values_to = "max_val") %>%
  filter(max_val > 1) %>% pull(col)

col_names = unique(col_names)

filtered_groups_w_cols = filtered_groups %>% 
  select(TESTERID, CONTROL, blkgrp, HPRICE, all_of(col_names), max_distinct)

# check if values differ across Table 5 controls. only differ 9 times on HCITY 
# (we know HCITY is bad) and HHMTYPE (categorical variable defining house type?)
# across 5814 rows.

instances = filtered_groups_w_cols %>% 
  summarise(across(-c(TESTERID, CONTROL, blkgrp, HPRICE), ~ sum(. > 1, na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "col", values_to = "times") %>%
  arrange(times) %>%
  filter(col %in% c(
    
                  "SEQUENCE.x",
                  "month",
                  "HCITY",
                  "market",
                  "ARELATE2",
                  "HHMTYPE",
                  "SAPPTAM",
                  "TSEX.x",
                  "THHEGAI",
                  "TPEGAI",
                  "THIGHEDU",
                  "TCURTENR",
                  "ALGNCUR",
                  "AELNG1",
                  "DPMTEXP",
                  "AMOVERS",
                  "age",
                  "ALEASETP",
                  "ACAROWN"))
  
# unique rows: 5832, down from 7026
unique_data = filtered_data %>% 
  select(
    c("TESTERID",
      "Longitude",
      "Latitude",
      "blkgrp",
      "HPRICE",
      "CONTROL",
      "SEQUENCE.x",
      "month",
      "HCITY",
      "ARELATE2",
      "HHMTYPE",
      "SAPPTAM",
      "TSEX.x",
      "THHEGAI",
      "TPEGAI",
      "THIGHEDU",
      "TCURTENR",
      "ALGNCUR",
      "AELNG1",
      "DPMTEXP",
      "AMOVERS",
      "age",
      "ALEASETP",
      "ACAROWN")) %>%
  rename_with(~c("SEQUENCE", "TSEX"), c("SEQUENCE.x", "TSEX.x")) %>%
  distinct()

haven::write_dta(unique_data, file.path(output_folder, "unique_adprocessed_JPE.dta"))