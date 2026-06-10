# HDS 2012 Data Cleaning and Merging Script
# Author: Anthony McCanny
# Date: 2025-06-04
# Purpose: Merge HDS raw data files, specifically taf, sales, tester, rhgeo, assignment by CONTROL and TESTERID

# =================================================================================================== #
# SETUP: LOAD REQUIRED PACKAGES, SET UP ENVIRONMENT, DECLARE FUNCTIONS
# =================================================================================================== #

# Required packages
packages <- c(
  "haven",      # Reading SAS files (.sas7bdat format)
  "dplyr",      # Data manipulation and transformation
  "readr",      # Reading and writing CSV files
  "stringr",    # String manipulation and regex operations
  "purrr",      # For map functions
  "readxl",     # Reading Excel files (Superfund data)
  "stringdist"  # Fuzzy string matching for address deduplication
)

# Install missing packages and load all
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
skip_geocoding <- "--skip-geocoding" %in% args
stop_before_geocoding <- "--stop-before-geocoding" %in% args

if (skip_geocoding) {
  cat("=== GEOCODING WILL BE SKIPPED (--skip-geocoding flag detected) ===\n")
}
if (stop_before_geocoding) {
  cat("=== SCRIPT WILL STOP BEFORE GEOCODING (--stop-before-geocoding flag detected) ===\n")
}

resolve_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(cwd) == "HUDreplication") return(cwd)
  if (basename(cwd) == "reconstructed_sample") return(dirname(cwd))
  candidate <- file.path(cwd, "HUDreplication")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  stop("Could not infer repo_root. Run from HUDreplication or reconstructed_sample.")
}

repo_root <- resolve_repo_root()
setwd(repo_root)

# Define data paths
raw_data_path <- "Data/HDS2012_Raw_Data"
reconstructed_sample_generated_path <- "Data/Generated/reconstructed_sample"

dir.create(reconstructed_sample_generated_path, showWarnings = FALSE, recursive = TRUE)

generated_file <- function(filename) {
  file.path(reconstructed_sample_generated_path, filename)
}


# Function to read SAS files with UTF-8 encoding, and convert empty strings to NA
import_sas <- function(file_path) {
  tryCatch({
    data <- read_sas(file_path, encoding = "UTF-8")                               # Read SAS file with UTF-8 encoding
    data <- data %>% mutate(across(everything(), ~ ifelse(.x == "", NA, .x)))     # Convert empty strings to NA
    cat("Successfully imported", basename(file_path), ":", nrow(data), "rows x", ncol(data), "columns\n")
    return(data)
  }, error = function(e) {
    # Return error if UTF-8 encoding fails
    stop("Failed to read ", basename(file_path), " with UTF-8 encoding: ", e$message)
  })
}

# =================================================================================================== #
# IMPORT SAS DATA FILES
# =================================================================================================== #

cat("=== READING SAS FILES AND FILTERING TO SALES DATA ===\n")

# Read assignment file and filter to sales + RELEASE="1"
assignment_raw <- import_sas(file.path(raw_data_path, "assignment.sas7bdat"))
assignment <- assignment_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL)) %>%  # Filter to sales tests (SA/SB/SH)
  filter(RELEASE == "1" & !is.na(TESTERID)) %>%  # Only released tests with valid tester IDs
  mutate(
    has_child = (ARELATE2 == 3) | (ARELATE3 == 3) | (ARELATE4 == 3) | (ARELATE5 == 3),
    all_rel_na = is.na(ARELATE2) & is.na(ARELATE3) & is.na(ARELATE4) & is.na(ARELATE5),
    kids = if_else(all_rel_na, NA_real_, if_else(has_child, 1, 0))
  ) %>%
  select(CONTROL, TESTERID, kids, SEQUENCE, ARELATE2, AMOVERS, AELNG1, ALGNCUR, ALEASETP, ACAROWN, DPMTEXP)
cat("Assignment: ", nrow(assignment_raw), "→", nrow(assignment), "rows after filtering to only sales and released tests\n")

# Read taf file and filter to sales
taf_raw <- import_sas(file.path(raw_data_path, "taf.sas7bdat"))
taf <- taf_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("TAF: ", nrow(taf_raw), "→", nrow(taf), "rows after sales filter\n")

# Read sales file (already sales data by definition)
sales <- import_sas(file.path(raw_data_path, "sales.sas7bdat"))

# Read tester file and standardize variable name
tester <- import_sas(file.path(raw_data_path, "tester_public.sas7bdat")) %>% 
  rename(TESTERID = TesterID)  # Standardize variable name

# Read rhgeo file and filter to sales
rhgeo_raw <- import_sas(file.path(raw_data_path, "rhgeo.sas7bdat"))
rhgeo <- rhgeo_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("RHGEO: ", nrow(rhgeo_raw), "→", nrow(rhgeo), "rows after sales filter\n")

# Read rechomes file and filter to sales
rechomes_raw <- import_sas(file.path(raw_data_path, "rechomes.sas7bdat"))
rechomes <- rechomes_raw %>%
    filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("RECHOMES: ", nrow(rechomes_raw), "→", nrow(rechomes), "rows after sales filter\n")


# =================================================================================================== #
# DATA CLEANING FUNCTIONS
# =================================================================================================== #

# Function to parse various date formats
parse_date_string <- function(date_string, is_birth_date = FALSE) {
  date_clean <- trimws(gsub("|[()]", "", as.character(date_string)))
  # First standardize separators - replace dashes and dots with forward slash
  date_standardized <- gsub("[-\\.]", "/", date_clean)
  # Then handle spaces and whitespace separately  
  date_standardized <- gsub("\\s+", "/", date_standardized)
  # Replace multiple slashes with single slash
  date_standardized <- gsub("//+", "/", date_standardized)

  # Manual corrections for known invalid dates
  corrections <- list(
    "6/31/12" = "2012-06-30",
    "02/29/2011" = "2011-02-28",
    "09/0/1983" = "1983-09-01",
    "08/18/7977" = "1977-08-18",
    "03/15/2948" = "1948-03-15",
    "01/08/1064" = "1964-01-08",
    "03/27/1066" = "1966-03-27",
    "08/02/1874" = "1974-08-02",
    "03/05/1012" = "2012-03-05",
    "05/17" = "2012-05-17"
  )
  
  if (date_standardized %in% names(corrections)) {
    return(as.Date(corrections[[date_clean]]))
  }

  # Date parsing patterns 
  patterns <- list(
    # Standard formats with full 4-digit years
    "^\\d{1,2}/\\d{1,2}/\\d{4}$" = "%m/%d/%Y",
    
    # 2-digit year formats (need special handling for birth vs appointment dates)
    "^\\d{1,2}/\\d{1,2}/\\d{2}$" = function(x) {
      if (is_birth_date) {
        # Birth dates: assume 19xx century
        year_prefix <- "19"
      } else {
        # Appointment dates: assume 20xx century  
        year_prefix <- "20"
      }
      full_date <- paste0(substr(x, 1, nchar(x)-2), year_prefix, substr(x, nchar(x)-1, nchar(x)))
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # No separator - 8 digits (MMDDYYYY)
    "^\\d{8}$" = "%m%d%Y",
    
    # No separator - 6 digits (MMDDYY)
    "^\\d{6}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 5, 6)
      
      if (is_birth_date) {
        year_full <- paste0("19", year_suffix)
      } else {
        year_full <- paste0("20", year_suffix)
      }
      
      full_date <- paste0(month, "/", day, "/", year_full)
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # MMDD/YY format
    "^\\d{4}/\\d{2}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 6, 7)
      
      if (is_birth_date) {
        year_full <- paste0("19", year_suffix)
      } else {
        year_full <- paste0("20", year_suffix)
      }
      
      full_date <- paste0(month, "/", day, "/", year_full)
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # MMDD/YYYY format (parse as MM/DD/YY)
    "^\\d{4}/\\d{4}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 8, 9)  # Take last 2 digits of year
      
      full_date <- paste0(month, "/", day, "/", year_suffix)
      as.Date(full_date, format = "%m/%d/%y")
    },
    
    # Ambiguous MM/YY or DD/MM formats - need intelligent parsing
    "^\\d{1,2}/\\d{2}$" = function(x) {
      parts <- strsplit(x, "/")[[1]]
      first_num <- as.numeric(parts[1])
      second_num <- as.numeric(parts[2])
      
      # Helper functions for validation
      could_be_month <- function(n) !is.na(n) && n >= 1 && n <= 12
      could_be_day <- function(n) !is.na(n) && n >= 1 && n <= 31
      
      # Scenario 1: First number too large for month (13-31) - likely DD/MM format
      if (!could_be_month(first_num) && could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          warning("Birth date appears to be DD/MM format - not implemented")
          return(NA)
        }
        # Treat as DD/MM/2000 for appointment dates - this will be caught and corrected by the correct_appointment_date function later
        return(as.Date(sprintf("%02d/%02d/2000", second_num, first_num), format = "%m/%d/%Y"))
      }
      
      # Scenario 2: Both numbers valid for month/day - need context clues
      if (could_be_month(first_num) && could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          # Try interpreting as MM/YY birth date
          birth_year <- 1900 + second_num
          age_in_2011 <- 2011 - birth_year
          if (age_in_2011 >= 18 && age_in_2011 <= 90) {
            return(as.Date(sprintf("%02d/01/%d", first_num, birth_year), format = "%m/%d/%Y"))
          }
        } else {
          # For appointment dates, check if second number suggests year (11-12)
          if (second_num >= 11 && second_num <= 12) {
            # Treat as MM/YY format
            return(as.Date(sprintf("%02d/01/20%02d", first_num, second_num), format = "%m/%d/%Y"))
          } else {
            # Treat as DD/MM/2000 for appointment dates - this will be caught and corrected by the correct_appointment_date function latert
            return(as.Date(sprintf("%02d/%02d/2000", second_num, first_num), format = "%m/%d/%Y"))
          }
        }
      }
      
      # Scenario 3: First number too large for month/day - might be YY/MM
      if (!could_be_month(first_num) && !could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          birth_year <- 1900 + first_num
          age_in_2011 <- 2011 - birth_year
          if (age_in_2011 >= 18 && age_in_2011 <= 90) {
            return(as.Date(sprintf("%02d/01/%d", second_num, birth_year), format = "%m/%d/%Y"))
          }
        } else if (first_num >= 11 && first_num <= 12) {
          # YY/MM format for 2011-2012
          return(as.Date(sprintf("%02d/01/20%02d", second_num, first_num), format = "%m/%d/%Y"))
        }
      }
      
      # If we reach here, couldn't determine format
      warning(paste("Could not parse ambiguous date format:", x))
      return(NA)
    }
  )
  
  # Try each pattern on the standardized date
  for (pattern in names(patterns)) {
    if (grepl(pattern, date_standardized)) {
      if (is.function(patterns[[pattern]])) {
        return(patterns[[pattern]](date_standardized))
      } else {
        return(as.Date(date_standardized, format = patterns[[pattern]]))
      }
    }
  }
  
  # If we reach here, couldn't determine format
  if (!is.na(date_string) && date_string != "") {
    warning(paste("Could not parse date format. Original:", date_string, "Standardized:", date_standardized))
  }
  return(NA)
}

# Function to parse time strings
parse_time_string <- function(time_string, am_pm_indicator = NULL) {
  time_clean <- trimws(as.character(time_string))

  hour <- NA
  minute <- NA
  
  # Check for invalid time
  if (is.na(time_clean) || time_clean == "" || 
      grepl("^(N/A|NA|n/a|na)$", time_clean, ignore.case = TRUE) ||
      grepl("^[^0-9:;.]+$", time_clean)) {
    return(list(hour = 0, minute = 0))
  }
  
  # Extract hour and minute based on format
  if (grepl("^\\d{4}$", time_clean)) {
    # Military time (e.g., "0315", "1430")
    hour <- as.numeric(substr(time_clean, 1, 2))
    minute <- as.numeric(substr(time_clean, 3, 4))
  } else if (grepl("^\\d{3}$", time_clean)) {
    # 3-digit format (e.g., "357" = 3:57)
    hour <- as.numeric(substr(time_clean, 1, 1))
    minute <- as.numeric(substr(time_clean, 2, 3))
  } else if (grepl("[:;.]", time_clean)) {
    # Separated format (colon, semicolon, or period)
    parts <- strsplit(time_clean, "[:;.]")[[1]]
    hour <- as.numeric(parts[1])
    minute <- if(length(parts) > 1) as.numeric(parts[2]) else 0
  } else {
    return(list(hour = 0, minute = 0))
  }
  
  # Validate and clean minute
  if (is.na(minute) || minute >= 60) minute <- 0
  
  # Handle AM/PM indicator with corrections for extreme cases
  if (!is.null(am_pm_indicator)) {
    # Handle missing AM/PM indicator (-1 or NULL) by guessing based on hour
    if (is.na(am_pm_indicator) || am_pm_indicator == -1) {
      if (!is.na(hour) && hour >= 9 && hour < 12) {
        am_pm_indicator <- 1  # AM
      } else if (!is.na(hour) && (hour == 12 || (hour >= 1 && hour <= 8))) {
        am_pm_indicator <- 2  # PM
      }

    # Correct obviously wrong AM/PM indicators (only if hour is not NA)
    } else if (!is.na(hour) && am_pm_indicator == 1 && hour >= 1 && hour <= 5) {
      am_pm_indicator <- 2  # Change to PM

    } else if (!is.na(hour) && am_pm_indicator == 2 && hour >= 10 && hour <= 11) {
      am_pm_indicator <- 1  # Change to AM
    }
    
    # Convert to 24-hour format (only if hour is not NA)
    if (!is.na(hour) && am_pm_indicator == 2 && hour < 12) {
      hour <- hour + 12  # PM hours (1-11) + 12
    }
  } 

  # Handle invalid hours
  if (is.na(hour) || hour >= 24) hour <- 0
  
  return(list(hour = hour, minute = minute))
}

# Function to correct incorrect appointment dates
correct_appointment_date <- function(df) {
  # Identify rows that need correction
  df <- df %>%
    mutate(
    needs_correction = !is.na(appointment_date) & 
              (appointment_date < as.Date("2011-07-01") | 
               appointment_date > as.Date("2012-10-31"))
    )
  
  # For each row that needs correction, find the correct year from same CONTROL
  for (i in which(df$needs_correction)) {
    current_control <- df$CONTROL[i]
    current_date <- df$appointment_date[i]
    
    # Find other rows with same CONTROL that have valid dates
    same_control_rows <- df %>%
    filter(CONTROL == current_control, 
       !is.na(appointment_date),
       appointment_date >= as.Date("2011-07-01"),
       appointment_date <= as.Date("2012-10-31"))
    
    # If we found valid dates in the same control, use the first one's year
    if (nrow(same_control_rows) > 0) {
    reference_date <- same_control_rows$appointment_date[1]
    reference_year <- format(reference_date, "%Y")
    
    # Keep original month and day, but change year
    corrected_date <- as.Date(paste0(reference_year, "-", 
            format(current_date, "%m-%d")))
    
    # If the corrected year is the same as original, use reference month instead
    if (format(current_date, "%Y") == reference_year) {
    reference_month <- format(reference_date, "%m")
    corrected_date <- as.Date(paste0(reference_year, "-", 
            reference_month, "-", 
            format(current_date, "%d")))
    }
    
    df$appointment_date[i] <- corrected_date
    
    cat("Corrected CONTROL", current_control, ":", 
    format(current_date, "%Y-%m-%d"), "->", 
    format(corrected_date, "%Y-%m-%d"), "\n")
    } else {
    # Set to NA if no valid reference date found
    df$appointment_date[i] <- NA
    warning("No valid reference date found for CONTROL ", current_control, 
      ". Setting appointment_date to NA for incorrect date: ", 
      format(current_date, "%Y-%m-%d"))
    }
  }
  
  # Remove the temporary column
  df <- df %>% select(-needs_correction)
  
  return(df)
}

# Function to correct invalid appointment times (when 00:00 or some non-time value is entered)
correct_invalid_times <- function(df) {
  # Identify rows with 00:00:00 times
  df <- df %>%
    mutate(
      is_midnight = !is.na(appointment_datetime) & 
                    format(appointment_datetime, "%H:%M:%S") == "00:00:00"
    )
  
  # For each row with midnight time, check if other rows exist on same day in same CONTROL
  for (i in which(df$is_midnight)) {
    current_control <- df$CONTROL[i]
    current_date <- as.Date(df$appointment_datetime[i])
    
    # Find other rows with same CONTROL on same date (excluding the current row)
    same_control_same_day <- df %>%
      filter(CONTROL == current_control,
              !is.na(appointment_datetime),
              as.Date(appointment_datetime) == current_date,
              row_number() != i)
    
    # If other appointments exist on same day, set time to NA
    if (nrow(same_control_same_day) > 0) {
      df$appointment_datetime[i] <- NA
      cat("Set appointment_datetime to NA for CONTROL", current_control, 
          "on", format(current_date, "%Y-%m-%d"), 
          "due to other appointments on same day\n")
    }
  }
  
  # Remove temporary column
  df <- df %>% select(-is_midnight)
  
  return(df)
}

# Function to correct AM/PM indicators based on hour values
correct_am_pm_indicator <- function(hour, am_pm_indicator) {
  # Handle missing AM/PM indicator (-1 or NA) by guessing based on hour
  if_else(is.na(am_pm_indicator) | am_pm_indicator == -1,
    case_when(
      !is.na(hour) & hour >= 9 & hour < 12 ~ TRUE,  # AM
      !is.na(hour) & (hour == 12 | (hour >= 1 & hour <= 8)) ~ FALSE,  # PM
      TRUE ~ NA  # Cannot determine, leave as NA
    ),
    # Correct obviously wrong AM/PM indicators (only if hour is not NA)
    case_when(
      !is.na(hour) & am_pm_indicator == 1 & hour >= 1 & hour <= 5 ~ FALSE,  # Change to PM
      !is.na(hour) & am_pm_indicator == 2 & hour >= 10 & hour <= 11 ~ TRUE,  # Change to AM
      am_pm_indicator == 1 ~ TRUE,   # Convert existing codes to TRUE/FALSE
      am_pm_indicator == 2 ~ FALSE,  # Convert existing codes to TRUE/FALSE
      TRUE ~ NA
    )
  )
}

first_by_order <- function(value, order) {
  idx <- which(!is.na(order))
  if (length(idx) == 0) return(value[NA_integer_][1])
  value[idx[which.min(order[idx])]]
}

# --- Address normalization and fuzzy deduplication functions ---

normalize_address_field <- function(x) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x <- toupper(trimws(x))
  x <- gsub("[.,#'\"()]+", "", x)
  x <- gsub("\\s+", " ", x)
  ifelse(x == "" | x == "NA", NA_character_, x)
}

normalize_street <- function(x) {
  x <- normalize_address_field(x)
  x <- gsub("\\bSTREET\\b", "ST", x)
  x <- gsub("\\bAVENUE\\b", "AVE", x)
  x <- gsub("\\bDRIVE\\b", "DR", x)
  x <- gsub("\\bBOULEVARD\\b", "BLVD", x)
  x <- gsub("\\bROAD\\b", "RD", x)
  x <- gsub("\\bLANE\\b", "LN", x)
  x <- gsub("\\bCOURT\\b", "CT", x)
  x <- gsub("\\bCIRCLE\\b", "CIR", x)
  x <- gsub("\\bPLACE\\b", "PL", x)
  x <- gsub("\\bTERRACE\\b", "TER", x)
  x <- gsub("\\bPARKWAY\\b", "PKWY", x)
  x <- gsub("\\bHIGHWAY\\b", "HWY", x)
  x <- gsub("\\bAPARTMENT\\b", "APT", x)
  x <- gsub("\\bSUITE\\b", "STE", x)
  x <- gsub("\\bNORTH\\b", "N", x)
  x <- gsub("\\bSOUTH\\b", "S", x)
  x <- gsub("\\bEAST\\b", "E", x)
  x <- gsub("\\bWEST\\b", "W", x)
  x
}

# Build a composite normalized address string from all address fields for fuzzy comparison
build_composite_addr <- function(norm_street, norm_sitetyp, norm_unitno, norm_city, norm_state, norm_zip) {
  df <- data.frame(norm_street, norm_sitetyp, norm_unitno, norm_city, norm_state, norm_zip,
                   stringsAsFactors = FALSE)
  apply(df, 1, function(row) {
    parts <- row[!is.na(row)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = " | ")
  })
}

# Compute a row-level quality score for choosing which row to keep during dedup.
# Higher = better: not-deleted >> more complete overall >> later sequence number.
compute_row_quality <- function(df) {
  not_deleted <- if ("DELREC" %in% names(df)) {
    is.na(df$DELREC) | df$DELREC != 1
  } else {
    rep(TRUE, nrow(df))
  }
  seqrh <- if ("SEQRH" %in% names(df)) {
    coalesce(as.numeric(df$SEQRH), 0)
  } else {
    rep(0, nrow(df))
  }

  (as.numeric(not_deleted) * 1000) +
    rowSums(!is.na(df) & df != "") +
    seqrh / 100
}

extract_house_number <- function(street) {
  str_extract(street, "^\\s*[0-9]+[A-Z]?(?:-[0-9]+[A-Z]?)?") %>%
    str_squish()
}

strip_house_number <- function(street) {
  str_remove(street, "^\\s*[0-9]+[A-Z]?(?:-[0-9]+[A-Z]?)?\\s*") %>%
    str_squish()
}

same_present_value <- function(x, y) {
  !is.na(x) && !is.na(y) && identical(x, y)
}

same_optional_value <- function(x, y) {
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(x, y))
}

is_bad_normalized_street <- function(x) {
  is.na(x) |
    x %in% c("UNKNOWN", "DID NOT PROVIDE", "MODEL HOME") |
    str_detect(
      x,
      regex("\\b(UNKNOWN|NOT PROVIDE(?:D)?|N/?A|MODEL HOME|NO ADDRESS|NONE|BLANK|MISSING|REFUSE|DID NOT GET)\\b")
    ) |
    str_detect(x, "^\\s*$|^-+$|^0+$") |
    str_detect(x, "^\\d*\\s*AVAILABLE$") |
    str_detect(x, "^\\d+$") |
    nchar(trimws(x)) <= 3
}

bad_normalized_street_reason <- function(x) {
  case_when(
    is.na(x) ~ "missing street address",
    x %in% c("UNKNOWN", "DID NOT PROVIDE", "MODEL HOME") |
      str_detect(
        x,
        regex("\\b(UNKNOWN|NOT PROVIDE(?:D)?|N/?A|MODEL HOME|NO ADDRESS|NONE|BLANK|MISSING|REFUSE|DID NOT GET)\\b")
      ) ~ "placeholder street address",
    str_detect(x, "^\\s*$|^-+$|^0+$") ~ "empty street address",
    str_detect(x, "^\\d*\\s*AVAILABLE$") ~ "availability placeholder",
    str_detect(x, "^\\d+$") ~ "numeric-only street address",
    nchar(trimws(x)) <= 3 ~ "overly short street address",
    TRUE ~ NA_character_
  )
}

drop_bad_rechomes_addresses <- function(df) {
  df <- df %>%
    mutate(
      norm_HSITEAD = normalize_street(HSITEAD),
      bad_rechomes_address = is_bad_normalized_street(norm_HSITEAD),
      bad_rechomes_address_reason = bad_normalized_street_reason(norm_HSITEAD)
    )

  dropped <- df %>% filter(bad_rechomes_address)
  write_csv(
    dropped %>%
      select(CONTROL, TESTERID, SEQRH, HSITEAD, HSITETYP, HUNITNO,
             HCITY, HSTATE, HZIP, bad_rechomes_address_reason),
    generated_file("bad_rechomes_address_drop_log.csv")
  )

  cat("Dropped rechomes rows with bad street-address inputs:", nrow(dropped), "\n")
  if (nrow(dropped) > 0) {
    cat("Bad rechomes address reasons:\n")
    print(dropped %>% count(bad_rechomes_address_reason, sort = TRUE))
  }

  df %>%
    filter(!bad_rechomes_address) %>%
    select(-norm_HSITEAD, -bad_rechomes_address, -bad_rechomes_address_reason)
}

has_address_field_conflict <- function(x) {
  dplyr::n_distinct(x, na.rm = FALSE) > 1
}

compatible_except_missing_address_fields <- function(x, y) {
  same_present_value(x[["norm_HSITEAD"]], y[["norm_HSITEAD"]]) &&
    !is_bad_normalized_street(x[["norm_HSITEAD"]]) &&
    !is_bad_normalized_street(y[["norm_HSITEAD"]]) &&
    all(is.na(x) | is.na(y) | x == y) &&
    any((is.na(x) & !is.na(y)) | (!is.na(x) & is.na(y)))
}

partial_missing_address_dedup <- function(df, source_name = "rechomes") {
  norm_cols <- c("norm_HSITEAD", "norm_HSITETYP", "norm_HUNITNO",
                 "norm_HCITY", "norm_HSTATE", "norm_HZIP")

  df$norm_HSITEAD  <- normalize_street(df$HSITEAD)
  df$norm_HSITETYP <- normalize_address_field(df$HSITETYP)
  df$norm_HUNITNO  <- normalize_address_field(df$HUNITNO)
  df$norm_HCITY    <- normalize_address_field(df$HCITY)
  df$norm_HSTATE   <- normalize_address_field(df$HSTATE)
  df$norm_HZIP     <- normalize_address_field(df$HZIP)
  df$composite_addr <- build_composite_addr(
    df$norm_HSITEAD, df$norm_HSITETYP, df$norm_HUNITNO,
    df$norm_HCITY, df$norm_HSTATE, df$norm_HZIP
  )
  df$addr_completeness <- rowSums(!is.na(df[, norm_cols, drop = FALSE]))
  df$row_quality <- compute_row_quality(df)

  rows_before <- nrow(df)
  log_entries <- list()

  df <- df %>%
    group_by(CONTROL, TESTERID) %>%
    group_modify(function(grp, key) {
      if (nrow(grp) <= 1) return(grp)

      candidates <- list()
      norm_mat <- grp[, norm_cols, drop = FALSE]

      for (i in seq_len(nrow(grp) - 1)) {
        for (j in (i + 1):nrow(grp)) {
          x <- as.character(unlist(norm_mat[i, ], use.names = FALSE))
          y <- as.character(unlist(norm_mat[j, ], use.names = FALSE))
          names(x) <- norm_cols
          names(y) <- norm_cols

          if (!compatible_except_missing_address_fields(x, y)) next

          if (grp$addr_completeness[i] > grp$addr_completeness[j]) {
            candidates[[length(candidates) + 1]] <- tibble(drop_idx = j, keep_idx = i)
          } else if (grp$addr_completeness[j] > grp$addr_completeness[i]) {
            candidates[[length(candidates) + 1]] <- tibble(drop_idx = i, keep_idx = j)
          }
        }
      }

      if (length(candidates) == 0) return(grp)

      candidates <- bind_rows(candidates) %>%
        mutate(
          keep_addr_completeness = grp$addr_completeness[keep_idx],
          max_keep_addr_completeness = ave(keep_addr_completeness, drop_idx, FUN = max)
        ) %>%
        filter(keep_addr_completeness == max_keep_addr_completeness)

      unambiguous <- candidates %>%
        group_by(drop_idx) %>%
        filter(n_distinct(keep_idx) == 1) %>%
        slice(1) %>%
        ungroup()

      if (nrow(unambiguous) == 0) return(grp)

      drop_idx <- unique(unambiguous$drop_idx)

      for (row in seq_len(nrow(unambiguous))) {
        to_drop <- unambiguous$drop_idx[row]
        kept <- unambiguous$keep_idx[row]
        log_entries[[length(log_entries) + 1]] <<- tibble(
          source          = source_name,
          CONTROL         = key$CONTROL,
          TESTERID        = key$TESTERID,
          match_type      = "partial_missing_address_fields_keep_more_complete",
          kept_addr       = grp$composite_addr[kept],
          dropped_addr    = grp$composite_addr[to_drop],
          kept_quality    = grp$row_quality[kept],
          dropped_quality = grp$row_quality[to_drop],
          kept_addr_completeness = grp$addr_completeness[kept],
          dropped_addr_completeness = grp$addr_completeness[to_drop],
          street_distance = NA_real_,
          kept_unit       = grp$norm_HUNITNO[kept],
          dropped_unit    = grp$norm_HUNITNO[to_drop],
          kept_HSITEAD    = grp$HSITEAD[kept],
          dropped_HSITEAD = grp$HSITEAD[to_drop],
          kept_HCITY      = grp$HCITY[kept],
          dropped_HCITY   = grp$HCITY[to_drop],
          kept_HSTATE     = grp$HSTATE[kept],
          dropped_HSTATE  = grp$HSTATE[to_drop],
          kept_HZIP       = grp$HZIP[kept],
          dropped_HZIP    = grp$HZIP[to_drop]
        )
      }

      grp[-drop_idx, ]
    }) %>%
    ungroup()

  if (length(log_entries) > 0) {
    new_log <- bind_rows(log_entries)
    if (exists("fuzzy_match_log", envir = .GlobalEnv)) {
      existing <- get("fuzzy_match_log", envir = .GlobalEnv)
      assign("fuzzy_match_log", bind_rows(existing, new_log), envir = .GlobalEnv)
    } else {
      assign("fuzzy_match_log", new_log, envir = .GlobalEnv)
    }
  }

  df <- df %>%
    select(-composite_addr, -row_quality, -addr_completeness, -all_of(norm_cols))

  cat(source_name, "partial-missing address deduplication:",
      rows_before, "->", nrow(df), "rows (", rows_before - nrow(df), "removed)\n")
  df
}

latest_rechomes_position <- function(grp) {
  seqrh <- if ("SEQRH" %in% names(grp)) {
    suppressWarnings(as.numeric(grp$SEQRH))
  } else {
    rep(NA_real_, nrow(grp))
  }

  if (any(!is.na(seqrh))) {
    latest_seqrh <- max(seqrh, na.rm = TRUE)
    candidates <- which(!is.na(seqrh) & seqrh == latest_seqrh)
  } else {
    candidates <- seq_len(nrow(grp))
  }

  candidates[which.max(grp$source_row_order[candidates])]
}

# When the same tester/trial records the same street/type/unit but conflicting
# city/state/ZIP, treat the later recommendation-home record as the corrected
# one. This is rechomes-only because rechomes is the merge spine.
same_actual_address_conflict_dedup <- function(df, source_name = "rechomes") {
  norm_cols <- c("norm_HSITEAD", "norm_HSITETYP", "norm_HUNITNO",
                 "norm_HCITY", "norm_HSTATE", "norm_HZIP")

  df$source_row_order <- seq_len(nrow(df))
  df$norm_HSITEAD  <- normalize_street(df$HSITEAD)
  df$norm_HSITETYP <- normalize_address_field(df$HSITETYP)
  df$norm_HUNITNO  <- normalize_address_field(df$HUNITNO)
  df$norm_HCITY    <- normalize_address_field(df$HCITY)
  df$norm_HSTATE   <- normalize_address_field(df$HSTATE)
  df$norm_HZIP     <- normalize_address_field(df$HZIP)
  df$composite_addr <- build_composite_addr(
    df$norm_HSITEAD, df$norm_HSITETYP, df$norm_HUNITNO,
    df$norm_HCITY, df$norm_HSTATE, df$norm_HZIP
  )
  df$row_quality <- compute_row_quality(df)

  rows_before <- nrow(df)
  log_entries <- list()

  df <- df %>%
    group_by(CONTROL, TESTERID, norm_HSITEAD, norm_HSITETYP, norm_HUNITNO) %>%
    group_modify(function(grp, key) {
      if (nrow(grp) <= 1) return(grp)
      if (is.na(key$norm_HSITEAD)) return(grp)
      if (is_bad_normalized_street(key$norm_HSITEAD)) return(grp)

      location_conflict <- has_address_field_conflict(grp$norm_HCITY) ||
        has_address_field_conflict(grp$norm_HSTATE) ||
        has_address_field_conflict(grp$norm_HZIP)

      if (!location_conflict) return(grp)

      keep_idx <- latest_rechomes_position(grp)
      drop_idx <- setdiff(seq_len(nrow(grp)), keep_idx)

      for (to_drop in drop_idx) {
        log_entries[[length(log_entries) + 1]] <<- tibble(
          source          = source_name,
          CONTROL         = key$CONTROL,
          TESTERID        = key$TESTERID,
          match_type      = "same_address_city_state_zip_conflict_keep_latest_seqrh",
          kept_addr       = grp$composite_addr[keep_idx],
          dropped_addr    = grp$composite_addr[to_drop],
          kept_quality    = grp$row_quality[keep_idx],
          dropped_quality = grp$row_quality[to_drop],
          street_distance = NA_real_,
          kept_unit       = grp$norm_HUNITNO[keep_idx],
          dropped_unit    = grp$norm_HUNITNO[to_drop],
          kept_HSITEAD    = grp$HSITEAD[keep_idx],
          dropped_HSITEAD = grp$HSITEAD[to_drop],
          kept_HCITY      = grp$HCITY[keep_idx],
          dropped_HCITY   = grp$HCITY[to_drop],
          kept_HSTATE     = grp$HSTATE[keep_idx],
          dropped_HSTATE  = grp$HSTATE[to_drop],
          kept_HZIP       = grp$HZIP[keep_idx],
          dropped_HZIP    = grp$HZIP[to_drop]
        )
      }

      grp[keep_idx, ]
    }) %>%
    ungroup()

  if (length(log_entries) > 0) {
    new_log <- bind_rows(log_entries)
    if (exists("fuzzy_match_log", envir = .GlobalEnv)) {
      existing <- get("fuzzy_match_log", envir = .GlobalEnv)
      assign("fuzzy_match_log", bind_rows(existing, new_log), envir = .GlobalEnv)
    } else {
      assign("fuzzy_match_log", new_log, envir = .GlobalEnv)
    }
  }

  df <- df %>%
    select(-source_row_order, -composite_addr, -row_quality, -all_of(norm_cols))

  cat(source_name, "same-address city/state/ZIP conflict deduplication:",
      rows_before, "->", nrow(df), "rows (", rows_before - nrow(df), "removed)\n")
  df
}

# Strict fuzzy deduplication within each (CONTROL, TESTERID) group.
# This intentionally catches only apparent street spelling/abbreviation errors:
# same house number, same city/state/ZIP, same unit/type fields, and a small
# edit distance in the remaining street text. It does not collapse different
# units, different house numbers, or rows that merely have missing address data.
strict_street_spelling_dedup_within_tester <- function(df, max_street_dist = 2, source_name = "unknown") {
  addr_fields <- c("HSITEAD", "HSITETYP", "HUNITNO", "HCITY", "HSTATE", "HZIP")
  norm_cols   <- paste0("norm_", addr_fields)

  # Add normalized columns
  df$norm_HSITEAD  <- normalize_street(df$HSITEAD)
  df$norm_HSITETYP <- normalize_address_field(df$HSITETYP)
  df$norm_HUNITNO  <- normalize_address_field(df$HUNITNO)
  df$norm_HCITY    <- normalize_address_field(df$HCITY)
  df$norm_HSTATE   <- normalize_address_field(df$HSTATE)
  df$norm_HZIP     <- normalize_address_field(df$HZIP)

  df$composite_addr <- build_composite_addr(
    df$norm_HSITEAD, df$norm_HSITETYP, df$norm_HUNITNO,
    df$norm_HCITY, df$norm_HSTATE, df$norm_HZIP
  )
  df$street_number <- extract_house_number(df$norm_HSITEAD)
  df$street_name <- strip_house_number(df$norm_HSITEAD)
  df$addr_completeness <- rowSums(!is.na(df[, norm_cols, drop = FALSE]))
  df$row_quality <- compute_row_quality(df)

  rows_before <- nrow(df)
  log_entries <- list()

  df <- df %>%
    group_by(CONTROL, TESTERID) %>%
    group_modify(function(grp, key) {
      if (nrow(grp) <= 1) return(grp)
      n <- nrow(grp)
      drop_idx <- integer(0)

      for (i in seq_len(n - 1)) {
        if (i %in% drop_idx) next
        for (j in (i + 1):n) {
          if (j %in% drop_idx) next
          if (is.na(grp$composite_addr[i]) || is.na(grp$composite_addr[j])) next

          match_type <- NA_character_
          to_drop <- NA_integer_
          street_dist <- NA_real_

          same_required_address <- same_present_value(grp$street_number[i], grp$street_number[j]) &&
            same_present_value(grp$norm_HCITY[i], grp$norm_HCITY[j]) &&
            same_present_value(grp$norm_HSTATE[i], grp$norm_HSTATE[j]) &&
            same_present_value(grp$norm_HZIP[i], grp$norm_HZIP[j]) &&
            same_optional_value(grp$norm_HSITETYP[i], grp$norm_HSITETYP[j]) &&
            same_optional_value(grp$norm_HUNITNO[i], grp$norm_HUNITNO[j]) &&
            !is.na(grp$street_name[i]) &&
            !is.na(grp$street_name[j]) &&
            grp$street_name[i] != grp$street_name[j]

          if (same_required_address) {
            street_dist <- stringdist::stringdist(grp$street_name[i], grp$street_name[j], method = "lv")
            if (street_dist > 0 && street_dist <= max_street_dist) {
              match_type <- paste0("street_spelling_lv_", street_dist)
              to_drop <- if (grp$row_quality[i] >= grp$row_quality[j]) j else i
            }
          }

          if (!is.na(match_type)) {
            kept <- if (to_drop == i) j else i
            log_entries[[length(log_entries) + 1]] <<- tibble(
              source        = source_name,
              CONTROL       = key$CONTROL,
              TESTERID      = key$TESTERID,
              match_type    = match_type,
              kept_addr     = grp$composite_addr[kept],
              dropped_addr  = grp$composite_addr[to_drop],
              kept_quality  = grp$row_quality[kept],
              dropped_quality = grp$row_quality[to_drop],
              street_distance = street_dist,
              kept_unit = grp$norm_HUNITNO[kept],
              dropped_unit = grp$norm_HUNITNO[to_drop],
              kept_HSITEAD  = grp$HSITEAD[kept],
              dropped_HSITEAD = grp$HSITEAD[to_drop],
              kept_HCITY    = grp$HCITY[kept],
              dropped_HCITY = grp$HCITY[to_drop],
              kept_HSTATE   = grp$HSTATE[kept],
              dropped_HSTATE = grp$HSTATE[to_drop],
              kept_HZIP     = grp$HZIP[kept],
              dropped_HZIP  = grp$HZIP[to_drop]
            )
            drop_idx <- c(drop_idx, to_drop)
          }
        }
      }
      if (length(drop_idx) > 0) grp[-unique(drop_idx), ] else grp
    }) %>%
    ungroup()

  # Append log entries to global accumulator
  if (length(log_entries) > 0) {
    new_log <- bind_rows(log_entries)
    if (exists("fuzzy_match_log", envir = .GlobalEnv)) {
      existing <- get("fuzzy_match_log", envir = .GlobalEnv)
      assign("fuzzy_match_log", bind_rows(existing, new_log), envir = .GlobalEnv)
    } else {
      assign("fuzzy_match_log", new_log, envir = .GlobalEnv)
    }
  }

  df <- df %>% select(-composite_addr, -street_number, -street_name, -row_quality, -addr_completeness, -all_of(norm_cols))
  cat(source_name, "strict street-spelling deduplication:", rows_before, "->", nrow(df), "rows (",
      rows_before - nrow(df), "removed)\n")
  df
}

# =================================================================================================== #
# SALES DATA CLEANING
# =================================================================================================== #

cat("=== CLEANING SALES DATA ===\n")

# Basic cleaning and column selection
sales_clean <- sales %>%
  mutate(
    STOTUNIT = as.numeric(STOTUNIT),
    SAVLBAD_BINARY = ifelse(SAVLBAD == "1", 1, 0)
  ) %>%
  select(CONTROL, TESTERID, RACEID, SEQUENCE, SSNDVIS, SAPPTD, SAPPTDY, 
         SBEGH, SBEGAM, STOTUNIT, SAVLBAD_BINARY) %>%
  filter(!is.na(CONTROL) & !is.na(TESTERID))

# Parse appointment dates and times
sales_with_time <- sales_clean %>%
  mutate(
    appointment_date = map_dbl(SAPPTD, ~ as.numeric(parse_date_string(.x, is_birth_date = FALSE))),
    appointment_date = as.Date(appointment_date, origin = "1970-01-01")
  ) %>% 
  correct_appointment_date() %>% # correct dates outside of the study period by setting year to year of visits from same trial
  mutate(
    time_parsed = map2(SBEGH, SBEGAM, parse_time_string),
    hour_24 = map_dbl(time_parsed, ~ .x$hour),
    minute_part = map_dbl(time_parsed, ~ .x$minute),
    appointment_datetime = as.POSIXct(
      paste(appointment_date, sprintf("%02d:%02d:00", hour_24, minute_part)),
      format = "%Y-%m-%d %H:%M:%S"
    ),
    am_indicator = correct_am_pm_indicator(hour = hour_24, am_pm_indicator = SBEGAM)
  ) %>%
  select(-time_parsed, -hour_24, -minute_part) %>%
  correct_invalid_times() # correct cases where appointment time is 00:00 or some non-time value

# Summary of appointment date and time issues
cat("=== APPOINTMENT DATE AND TIME SUMMARY ===\n")

sales_with_time %>%
  summarise(
    na_dates = sum(is.na(appointment_date)),
    na_datetimes = sum(is.na(appointment_datetime)),
    unusual_times = sum(!is.na(appointment_datetime) & 
                        (as.numeric(format(appointment_datetime, "%H")) < 6 | 
                        as.numeric(format(appointment_datetime, "%H")) >= 21)),
    midnight_times = sum(!is.na(appointment_datetime) & 
                        as.numeric(format(appointment_datetime, "%H")) == 0 & 
                        as.numeric(format(appointment_datetime, "%M")) == 0),
    midnight_with_others = sum(!is.na(appointment_datetime) & 
                              as.numeric(format(appointment_datetime, "%H")) == 0 & 
                              as.numeric(format(appointment_datetime, "%M")) == 0 &
                              CONTROL %in% (sales_with_time %>% 
                                          filter(!is.na(appointment_datetime)) %>%
                                          group_by(CONTROL, as.Date(appointment_datetime)) %>%
                                          filter(n() > 1) %>% pull(CONTROL)))
  ) %>%
  {
    cat("NA appointment dates:", .$na_dates, "\n")
    cat("NA appointment datetimes:", .$na_datetimes, "\n") 
    cat("Appointments before 6 AM or after 9 PM:", .$unusual_times, "\n")
    cat("Appointments at exactly 00:00 (midnight):", .$midnight_times, "\n")
    cat("NOTE: The", .$midnight_times, "midnight appointments have no other appointments on the same day,")
    cat(" so they won't impact appointment sequence determination.\n")
  }

# Print data on the CONTROL with rows where datetime is NA but date is not NA
sales_with_time %>%
  filter(CONTROL %in% (sales_with_time %>% 
                        filter(!is.na(appointment_date) & is.na(appointment_datetime)) %>% 
                        pull(CONTROL))) %>%
  {
    cat("All rows in CONTROL(s) where some rows have date but missing datetime:\n")
    print(., n = Inf, width = Inf)
  }

# NOTE: the only case where appointment_datetime is NA but appointment_date is not NA is when it's clear that
# row with the invalid time (e.g., 00:00) is clearly an error corrected in the next row (time: 12:00),
# so there is no information lost when dropping this row in later analyses.

# Determine visit order, aggregate and rename variables
sales_final <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(
    visit_order = ifelse(is.na(appointment_datetime), NA, row_number()), # only rows with non-NA datetime will have visit_order, NA values ordered at the end of each CONTROL
    substantive_visit = !is.na(STOTUNIT) | !is.na(SAVLBAD_BINARY),
    substantive_visit_order = if_else(substantive_visit, as.integer(visit_order), NA_integer_)
  ) %>%
  ungroup() %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(
    RACEID = first(RACEID),
    am_indicator_first = first_by_order(am_indicator, substantive_visit_order),
    STOTUNIT_FIRST = first_by_order(STOTUNIT, substantive_visit_order),
    SAVLBAD_FIRST = first_by_order(SAVLBAD_BINARY, substantive_visit_order),
    first_visit_datetime = first_by_order(appointment_datetime, substantive_visit_order),
    first_substantive_visit_order = suppressWarnings(min(substantive_visit_order, na.rm = TRUE)),
    num_visits = sum(!is.na(substantive_visit_order)),
    STOTUNIT_TOTAL = if(all(is.na(STOTUNIT))) NA_real_ else sum(STOTUNIT, na.rm = TRUE),
    SAVLBAD_ANY = if(all(is.na(SAVLBAD_BINARY))) NA_real_ else as.numeric(any(SAVLBAD_BINARY == 1, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    RACE = as.numeric(str_extract(as.character(RACEID), "\\d$")),
    # Rechomes has no appointment/date key, so recommendation-level data can
    # only inherit timing from the tester-trial visit record.
    month = as.integer(format(first_visit_datetime, "%m")),
    first_substantive_visit_order = if_else(is.infinite(first_substantive_visit_order), NA_real_, first_substantive_visit_order)
  ) %>%
  group_by(CONTROL) %>%
  mutate(
    first_substantive_order_in_trial = suppressWarnings(min(first_substantive_visit_order, na.rm = TRUE)),
    first_substantive_order_in_trial = if_else(is.infinite(first_substantive_order_in_trial), NA_real_, first_substantive_order_in_trial),
    was_first_visitor = if_else(
      is.na(first_substantive_visit_order) | is.na(first_substantive_order_in_trial),
      NA,
      first_substantive_visit_order == first_substantive_order_in_trial
    )
  ) %>%
  ungroup() %>%
  select(-RACEID, -first_substantive_order_in_trial)

cat("Final sales dataset:", nrow(sales_final), "rows (one per tester per control)\n")

# Count distinct controls with non-missing outcomes and two testers, by first two letters
outcome_summary <- sales_final %>%
  filter(complete.cases(.)) %>%  # Exclude rows with any NA values in any column
  group_by(CONTROL) %>%
  filter(n() == 2) %>%  # Exactly two testers
  ungroup() %>%
  mutate(control_prefix = substr(CONTROL, 1, 2)) %>%
  group_by(control_prefix) %>%
  summarise(distinct_controls = n_distinct(CONTROL), .groups = 'drop') %>%
  arrange(control_prefix)

  cat("Distinct controls with non-missing outcomes and exactly two testers:\n")
  print(outcome_summary)
  cat("Total distinct controls:", sum(outcome_summary$distinct_controls), "\n")

# Create a separate dataset with one row for each appointment; alternate specification
sales_appointments <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(
    visit_order = ifelse(is.na(appointment_datetime), NA, row_number()),  # only rows with non-NA datetime will have visit_order, NA values ordered at the end of each CONTROL
    month = as.integer(format(appointment_datetime, "%m")),
    RACE = as.numeric(str_extract(as.character(RACEID), "\\d$"))
  ) %>% 
  ungroup()

# Export cleaned sales data
write_csv(sales_final, generated_file("sales_cleaned.csv"))
cat("Exported cleaned sales data to ", generated_file("sales_cleaned.csv"), "\n", sep = "")

# =================================================================================================== #
# TESTER & ASSIGNMENT DATA CLEANING
# =================================================================================================== #

cat("=== CLEANING TESTER DATA ===\n")

tester_clean <- tester %>%
  mutate(
    birth_date = map_dbl(TDOB, ~ as.numeric(parse_date_string(.x, is_birth_date = TRUE))),
    birth_date = as.Date(birth_date, origin = "1970-01-01"),
    
    # Calculate age as of January 1, 2012
    age = as.numeric(difftime(as.Date("2012-01-01"), birth_date, units = "days")) / 365.25,
    age = round(age, 0),
    age = if_else(age <= 17, NA_real_, age)  # Remove unrealistic ages
  )

cat("Cleaned birth dates for", nrow(tester_clean), "testers\n")
cat("Valid birth dates:", sum(!is.na(tester_clean$birth_date)), "out of", nrow(tester_clean), "\n")

# Show invalid birth dates without saving to memory
tester_clean %>%
  filter(is.na(birth_date)) %>%
  select(TESTERID, TDOB, birth_date, age) %>%
  {
    cat("Testers with invalid birth dates:\n")
    if(nrow(.) > 0) {
      print(.)
    }
  }

# Note: Only four testers have invalid birth dates due to a lack of data entry, with TDOB = "00/00/0000".

# Merge sales and tester data
sales_and_tester <- sales_final %>%
  left_join(tester_clean, by = "TESTERID") %>%
  left_join(assignment, by = c("CONTROL", "TESTERID")) %>%
  mutate(
    mother = if_else(kids == 1 & TSEX == 0, 1, if_else(is.na(kids) | is.na(TSEX), NA_real_, 0))
  )

write_csv(sales_and_tester, generated_file("sales_and_tester_merged.csv"))
cat("Exported merged data to ", generated_file("sales_and_tester_merged.csv"), " (", nrow(sales_and_tester), " rows)\n", sep = "")

# Alternate specification keeping each appointment as a separate row
sales_and_tester_appointments <- sales_appointments %>%
  left_join(tester_clean, by = "TESTERID") %>%
  left_join(assignment, by = c("CONTROL", "TESTERID")) %>%
  mutate(
    mother = if_else(kids == 1 & TSEX == 0, 1, if_else(is.na(kids) | is.na(TSEX), NA_real_, 0))
  )
write_csv(sales_and_tester_appointments, generated_file("sales_and_tester_appointments.csv"))
cat("Exported sales and tester appointments data to ", generated_file("sales_and_tester_appointments.csv"), " (", nrow(sales_and_tester_appointments), " rows)\n", sep = "")

# =================================================================================================== #
# MERGE SALES, TESTER AND RECHOMES DATA
# =================================================================================================== #

# Initialize global fuzzy match log
fuzzy_match_log <- tibble(
  source = character(), CONTROL = character(), TESTERID = character(),
  match_type = character(), kept_addr = character(), dropped_addr = character(),
  kept_quality = numeric(), dropped_quality = numeric(),
  street_distance = numeric(),
  kept_unit = character(), dropped_unit = character(),
  kept_HSITEAD = character(), dropped_HSITEAD = character(),
  kept_HCITY = character(), dropped_HCITY = character(),
  kept_HSTATE = character(), dropped_HSTATE = character(),
  kept_HZIP = character(), dropped_HZIP = character()
)

# --- STEP 1: Rechomes deduplication (exact on normalized keys, then fuzzy) ---
cat("=== DEDUPLICATING RECHOMES ===\n")

rechomes_completeness_score <- function(df) {
  rowSums(!is.na(df) & df != "")
}

rechomes_deduplicated <- rechomes %>%
  {
    cat("Initial rechomes rows:", nrow(.), "\n")
    .
  } %>%
  # Add normalized address columns for grouping
  mutate(
    norm_HSITEAD  = normalize_street(HSITEAD),
    norm_HSITETYP = normalize_address_field(HSITETYP),
    norm_HUNITNO  = normalize_address_field(HUNITNO),
    norm_HCITY    = normalize_address_field(HCITY),
    norm_HSTATE   = normalize_address_field(HSTATE),
    norm_HZIP     = normalize_address_field(HZIP)
  ) %>%
  # Exact dedup on normalized keys (keeps best row per group)
  group_by(CONTROL, TESTERID, norm_HSITEAD, norm_HSITETYP, norm_HUNITNO,
           norm_HCITY, norm_HSTATE, norm_HZIP) %>%
  mutate(
    prefer_not_deleted = is.na(DELREC) | DELREC != 1,
    completeness_score = rechomes_completeness_score(across(everything())),
    seqrh_order = coalesce(SEQRH, -Inf)
  ) %>%
  arrange(
    desc(prefer_not_deleted),
    desc(completeness_score),
    desc(seqrh_order),
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-prefer_not_deleted, -completeness_score, -seqrh_order,
         -norm_HSITEAD, -norm_HSITETYP, -norm_HUNITNO,
         -norm_HCITY, -norm_HSTATE, -norm_HZIP) %>%
  {
    rows_lost <- nrow(rechomes) - nrow(.)
    cat("After exact dedup on normalized address keys:", nrow(.), "rows\n")
    cat("Rows collapsed by normalization:", rows_lost, "\n")
    .
  }

rechomes_deduplicated <- partial_missing_address_dedup(
  rechomes_deduplicated, source_name = "rechomes"
)

# If the tester/trial has the same street/type/unit but conflicting
# city/state/ZIP, keep the later rechomes recommendation record.
rechomes_deduplicated <- same_actual_address_conflict_dedup(
  rechomes_deduplicated, source_name = "rechomes"
)

# Strict fuzzy dedup: only collapse apparent street spelling errors within each
# CONTROL x TESTERID, never different unit numbers or different house numbers.
rechomes_deduplicated <- strict_street_spelling_dedup_within_tester(
  rechomes_deduplicated, max_street_dist = 2, source_name = "rechomes"
)

rechomes_deduplicated <- drop_bad_rechomes_addresses(rechomes_deduplicated)

# --- STEP 2: Rhgeo deduplication (exact on normalized keys, then fuzzy) ---
cat("=== DEDUPLICATING RHGEO ===\n")

rhgeo_deduplicated <- rhgeo %>%
  {
    cat("Initial rhgeo rows:", nrow(.), "\n")
    .
  } %>%
  distinct() %>%
  {
    cat("After removing exact duplicates:", nrow(.), "rows\n")
    .
  } %>%
  filter(
    !(
      HSITEAD %in% c("unknown", "did not provide", "UNKNOWN", "MODEL HOME") |
      str_detect(
        str_squish(str_to_lower(HSITEAD)),
        regex("\\b(unknown|not provide(?:d)?|n/?a|model home|no address|none|blank|missing|refuse)\\b")
      ) |
      str_detect(HSITEAD, "^\\s*$|^-+$|^0+$") |
      nchar(trimws(HSITEAD)) <= 3
    )
  ) %>%
  {
    filtered_addresses <- rhgeo %>%
      filter(
        HSITEAD %in% c("unknown", "did not provide", "UNKNOWN", "MODEL HOME") |
        str_detect(
          str_squish(str_to_lower(HSITEAD)),
          regex("\\b(unknown|not provide(?:d)?|n/?a|model home|no address|none|blank|missing|refuse|did not get)\\b")
        ) |
        str_detect(HSITEAD, "^\\s*$|^-+$|^0+$") |
        nchar(trimws(HSITEAD)) <= 3
      ) %>%
      pull(HSITEAD) %>%
      unique() %>%
      sort()

    cat("After removing misformed addresses:", nrow(.), "rows\n")
    cat("Filtered addresses:", paste(filtered_addresses, collapse = ", "), "\n")
    .
  } %>%
  # Exact dedup on normalized keys
  mutate(
    norm_HSITEAD  = normalize_street(HSITEAD),
    norm_HSITETYP = normalize_address_field(HSITETYP),
    norm_HUNITNO  = normalize_address_field(HUNITNO),
    norm_HCITY    = normalize_address_field(HCITY),
    norm_HSTATE   = normalize_address_field(HSTATE),
    norm_HZIP     = normalize_address_field(HZIP)
  ) %>%
  group_by(CONTROL, TESTERID, norm_HSITEAD, norm_HSITETYP, norm_HUNITNO,
           norm_HCITY, norm_HSTATE, norm_HZIP) %>%
  slice(1) %>%
  ungroup() %>%
  select(-norm_HSITEAD, -norm_HSITETYP, -norm_HUNITNO,
         -norm_HCITY, -norm_HSTATE, -norm_HZIP) %>%
  {
    cat("After exact dedup on normalized keys:", nrow(.), "rows\n")
    .
  }

cat("Skipping fuzzy deduplication for rhgeo; rechomes is the row-preserving merge spine.\n")

# --- STEP 3: Merge rechomes with rhgeo, ensuring at most one rhgeo row per rechomes row ---
cat("=== MERGING RECHOMES WITH RHGEO (1:1) ===\n")

# Add normalized keys to both sides for matching
rechomes_for_merge <- rechomes_deduplicated %>%
  mutate(
    norm_HSITEAD  = normalize_street(HSITEAD),
    norm_HSITETYP = normalize_address_field(HSITETYP),
    norm_HUNITNO  = normalize_address_field(HUNITNO),
    norm_HCITY    = normalize_address_field(HCITY),
    norm_HSTATE   = normalize_address_field(HSTATE),
    norm_HZIP     = normalize_address_field(HZIP),
    rechomes_row_id = row_number()
  )

rhgeo_for_merge <- rhgeo_deduplicated %>%
  select(-SEQRH, -RACEID) %>%
  mutate(
    norm_HSITEAD  = normalize_street(HSITEAD),
    norm_HSITETYP = normalize_address_field(HSITETYP),
    norm_HUNITNO  = normalize_address_field(HUNITNO),
    norm_HCITY    = normalize_address_field(HCITY),
    norm_HSTATE   = normalize_address_field(HSTATE),
    norm_HZIP     = normalize_address_field(HZIP)
  )

# Exact join on normalized address keys, including type and unit.
merge_keys <- c("CONTROL", "TESTERID", "norm_HSITEAD", "norm_HSITETYP",
                "norm_HUNITNO", "norm_HCITY", "norm_HSTATE", "norm_HZIP")

# Identify rhgeo columns that would conflict with rechomes columns.
rhgeo_only_cols <- setdiff(names(rhgeo_for_merge),
                           c(names(rechomes_for_merge), merge_keys))

rechomes_rhgeo <- rechomes_for_merge %>%
  left_join(
    rhgeo_for_merge %>% select(all_of(merge_keys), all_of(rhgeo_only_cols)),
    by = merge_keys
  ) %>%
  # If a rechomes row matched multiple rhgeo rows, keep the first one (most complete)
  group_by(rechomes_row_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-rechomes_row_id, -all_of(setdiff(merge_keys, c("CONTROL", "TESTERID")))) %>%
  {
    rechomes_missing_in_rhgeo <- rechomes_for_merge %>%
      anti_join(rhgeo_for_merge, by = merge_keys)
    rhgeo_missing_in_rechomes <- rhgeo_for_merge %>%
      anti_join(rechomes_for_merge, by = merge_keys)

    cat("Rechomes deduplicated rows:", nrow(rechomes_deduplicated), "\n")
    cat("Rhgeo deduplicated rows:", nrow(rhgeo_deduplicated), "\n")
    cat("Merged rows (1:1):", nrow(.), "\n")
    cat("Rechomes rows missing in rhgeo:", nrow(rechomes_missing_in_rhgeo), "\n")
    cat("Rhgeo rows missing in rechomes:", nrow(rhgeo_missing_in_rechomes), "\n")
    cat("Pct rechomes missing in rhgeo:", round(nrow(rechomes_missing_in_rhgeo) / nrow(rechomes_deduplicated) * 100, 2), "%\n")
    cat("Pct rhgeo missing in rechomes:", round(nrow(rhgeo_missing_in_rechomes) / nrow(rhgeo_deduplicated) * 100, 2), "%\n")
    .
  }

# Export the fuzzy match log for later review
write_csv(fuzzy_match_log, generated_file("fuzzy_dedup_log.csv"))
cat("Exported fuzzy dedup log to ", generated_file("fuzzy_dedup_log.csv"),
    " (", nrow(fuzzy_match_log), " matches)\n", sep = "")

cat("=== MERGING SALES, TESTER, RECHOMES & RHGEO DATA ===\n")

# First, refine to analytic sample then merge sales_and_tester with rechomes data

# Analytical sample: non-missing outcomes and exactly 2 testers per control
analytic_sales_tester <- sales_and_tester %>%
  filter(!is.na(STOTUNIT_TOTAL), !is.na(SAVLBAD_ANY)) %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%  # Exactly 2 unique testers per control
  ungroup() %>%
  {
    # Report analytical sample progression
    base_rows <- nrow(sales_and_tester)
    complete_cases <- sum(!is.na(sales_and_tester$STOTUNIT_TOTAL) & !is.na(sales_and_tester$SAVLBAD_ANY))
    analytical_rows <- nrow(.)

    cat("=== ANALYTICAL SAMPLE PROGRESSION ===\n")
    cat("Original sales_and_tester rows:", base_rows, "\n")
    cat("After requiring non-missing outcomes:", complete_cases, "\n") 
    cat("After requiring exactly 2 testers per control:", analytical_rows, "\n")
    cat("Unique controls in analytical sample:", n_distinct(.$CONTROL), "\n")
    
    # Return the filtered data
    .
  }

# Now merge analytical sample with rechomes data
sales_tester_rechomes <- analytic_sales_tester %>%
  left_join(rechomes_rhgeo, by = c("CONTROL", "TESTERID")) %>%
  mutate(has_rechomes_data = rowSums(!is.na(select(., HSITEAD, HZIP))) > 0) %>%
  {
    total_rows <- nrow(.)
    total_controls <- n_distinct(.$CONTROL)
    with_rechomes <- sum(.$has_rechomes_data)
    without_rechomes <- sum(!.$has_rechomes_data)
    
    cat("\n=== RECHOMES MERGE ANALYSIS ===\n")
    cat("Total analytical sample controls:", total_controls, "\n")
    
    # Count CONTROLs with at least one tester with no rechomes data
    controls_with_missing_rechomes <- {
      current_data <- .
      current_data %>%
        group_by(CONTROL) %>%
        summarise(has_missing_rechomes = any(!has_rechomes_data), .groups = 'drop') %>%
        summarise(n_controls = sum(has_missing_rechomes)) %>%
        pull(n_controls) %>%
        as.numeric()
    }
    cat("CONTROLs with at least one tester with no rechomes data:", controls_with_missing_rechomes, "\n")

    # Count controls where neither tester has STOTUNIT_TOTAL == 0
    controls_with_no_zero_recommendations <- {
    current_data <- .
    current_data %>%
      group_by(CONTROL) %>%
      summarise(has_zero_recommendations = any(STOTUNIT_TOTAL == 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(!has_zero_recommendations, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    }
    cat("CONTROLs where neither tester has STOTUNIT_TOTAL == 0:", controls_with_no_zero_recommendations, "\n")
    
    # Check for rows with recommended homes but no rechomes data
    recommends_no_rechomes <- sum(!.$has_rechomes_data & .$STOTUNIT_TOTAL > 0, na.rm = TRUE)
    cat("Rows with recommendations but missing rechomes data:", recommends_no_rechomes, "\n")
    cat("Percentage of rows with recommendations but missing rechomes data:", round(recommends_no_rechomes / total_rows * 100, 2), "%\n")

    # Count CONTROLs with at least one tester having recommendations but missing rechomes data
    controls_with_recommendations_no_rechomes <- {
    current_data <- .
    current_data %>%
      group_by(CONTROL) %>%
      summarise(has_missing_rechomes = any(!has_rechomes_data & STOTUNIT_TOTAL > 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(has_missing_rechomes, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    }
    cat("CONTROLs with at least one tester having recommendations but missing rechomes data:", controls_with_recommendations_no_rechomes, "\n")
    
    invisible(.)
  } %>%
  {
    # Identify rows that need rechomes data but are missing it
    needs_rechomes_fix <- !.$has_rechomes_data & 
          .$STOTUNIT_TOTAL == 1 & .$SAVLBAD_ANY == 1
    
    missing_data <- .[needs_rechomes_fix, ]
    
    if (nrow(missing_data) > 0) {
    # Find potential donor rows (same CONTROL, HADHOME == 1)
    donor_rows <- rechomes %>%
      filter(CONTROL %in% missing_data$CONTROL, HADHOME == 1) %>%
      select(-TESTERID) %>%  # Remove TESTERID 
      group_by(CONTROL) %>%
      slice(1) %>%  # Take first matching row per CONTROL
      ungroup()

    cat(nrow(missing_data), " of the missing rows above are supposed to only have the advertised home recommended (STOTUNIT_TOTAL=1 & SAVLBAD_ANY=1). We check if their partnered tester recorded the information about the advertised home.\n")
    cat("Found HADHOME == 1 donor rows for", nrow(donor_rows), "of these CONTROLs\n")
    
    # For each missing row, try to fill with donor data
    rows_fixed <- 0
    for (i in which(needs_rechomes_fix)) {
      current_control <- .$CONTROL[i]
      donor_match <- donor_rows[donor_rows$CONTROL == current_control, ]
      
      if (nrow(donor_match) > 0) {
      # Fill in all rechomes columns except CONTROL and TESTERID
      rechomes_cols <- names(donor_match)[!names(donor_match) %in% c("CONTROL")]
      for (col in rechomes_cols) {
        .[[col]][i] <- donor_match[[col]][1]
      }
      # Update has_rechomes_data flag
      .$has_rechomes_data[i] <- TRUE
      rows_fixed <- rows_fixed + 1
      }
    }
    
    cat("Successfully fixed", rows_fixed, "rows with donor rechomes data\n")
    
    if (rows_fixed > 0) {
      # Show sample of actually fixed rows by tracking which ones were successfully fixed
      actually_fixed_indices <- c()
      for (i in which(needs_rechomes_fix)) {
        current_control <- .$CONTROL[i]
        donor_match <- donor_rows[donor_rows$CONTROL == current_control, ]
        if (nrow(donor_match) > 0) {
      actually_fixed_indices <- c(actually_fixed_indices, i)
        }
      }
      
      # Show first 3 actually fixed rows
      sample_indices <- head(actually_fixed_indices, 3)
      fixed_rows_sample <- .[sample_indices, ]
      cat("\nSample of actually fixed rows (showing first 3):\n")
      rechomes_sample_cols <- names(rechomes)[!names(rechomes) %in% c("CONTROL", "TESTERID")][1:5]
      print_cols <- c("CONTROL", "TESTERID", "STOTUNIT_TOTAL", "SAVLBAD_ANY", "has_rechomes_data", 
          rechomes_sample_cols)
      print(fixed_rows_sample[, print_cols], n = Inf, width = Inf)
    }
    
    unfixed_count <- nrow(missing_data) - rows_fixed
    if (unfixed_count > 0) {
      cat("Could not fix", unfixed_count, "rows (no HADHOME == 1 donor found)\n")
    }
    } else {
    cat("No rows found missing rechomes data with STOTUNIT_TOTAL=1 & SAVLBAD_ANY=1\n")
    }
    
    # Return the modified dataframe
    .
  } %>%
  {
    # Recount CONTROLs with missing rechomes data after the fix
    current_data <- .
    controls_with_recommendations_no_rechomes_after_fix <- current_data %>%
      group_by(CONTROL) %>%
      summarise(has_missing_rechomes = any(!has_rechomes_data & STOTUNIT_TOTAL > 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(has_missing_rechomes, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    
    cat("CONTROLs with at least one tester having recommendations but missing rechomes data (after fix):", controls_with_recommendations_no_rechomes_after_fix, "\n")
    
    # Return the modified dataframe
    .
  } %>%
  # Subset to rows that have rechomes data
  filter(has_rechomes_data) %>%
  # Only keep controls with exactly two testers (both having rechomes data)
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%
  ungroup() %>%
  {
    total_controls <- n_distinct(.$CONTROL)
    total_rows <- nrow(.)
    
    cat("Final analytical sample with rechomes data:\n")
    cat("Total controls:", total_controls, "\n")
    cat("Total rows:", total_rows, "\n")
    
    # Return the filtered dataset
    .
  }
    
# Export merged dataset
write_csv(sales_tester_rechomes, generated_file("sales_tester_rechomes_merged.csv"))
cat("Exported merged data to ", generated_file("sales_tester_rechomes_merged.csv"), "\n", sep = "")

if (stop_before_geocoding) {
  cat("Stopping before geocoding as requested.\n")
  quit(save = "no", status = 0)
}

# =================================================================================================== #
# GEOCODING 
# =================================================================================================== #

cat("=== GEOCODING  ===\n")
source(file.path("reconstructed_sample", "Cleaning_Scripts", "address_geocoding.R"))
manual_geocode_override_path <- file.path(
  "reconstructed_sample",
  "Cleaning_Scripts",
  "manual_geocoded_addresses.csv"
)
legacy_geocoded_path <- file.path("Data", "sales_tester_rechomes_geocoded.csv")

if (skip_geocoding) {
  cat("Skipping geocoding step (--skip-geocoding flag set)\n")
  if (file.exists(generated_file("sales_tester_rechomes_geocoded.csv"))) {
    cat("Loading previously geocoded data from ", generated_file("sales_tester_rechomes_geocoded.csv"), "\n", sep = "")
    sales_tester_rechomes_geocoded <- read_csv(generated_file("sales_tester_rechomes_geocoded.csv"))
  } else if (file.exists(legacy_geocoded_path)) {
    cat("Loading legacy geocoded data from ", legacy_geocoded_path, " and migrating output to ", generated_file("sales_tester_rechomes_geocoded.csv"), "\n", sep = "")
    sales_tester_rechomes_geocoded <- read_csv(legacy_geocoded_path)
    # Backfill kids/mother for older cached geocoding files
    if (!("kids" %in% names(sales_tester_rechomes_geocoded))) {
      sales_tester_rechomes_geocoded <- sales_tester_rechomes_geocoded %>%
        left_join(assignment_kids, by = c("CONTROL", "TESTERID"))
    }
    if (!("mother" %in% names(sales_tester_rechomes_geocoded))) {
      sales_tester_rechomes_geocoded <- sales_tester_rechomes_geocoded %>%
        mutate(
          mother = if_else(kids == 1 & TSEX == 0, 1,
                           if_else(is.na(kids) | is.na(TSEX), NA_real_, 0))
        )
    }
    if (!("month" %in% names(sales_tester_rechomes_geocoded))) {
      sales_tester_rechomes_geocoded <- sales_tester_rechomes_geocoded %>%
        left_join(
          sales_final %>% select(CONTROL, TESTERID, month),
          by = c("CONTROL", "TESTERID")
        )
    }
  } else {
    stop("Cannot skip geocoding: neither ", generated_file("sales_tester_rechomes_geocoded.csv"), " nor ", legacy_geocoded_path, " exists. Run without --skip-geocoding first.")
  }
} else {
  # Ensure UTF-8 encoding for address fields before geocoding
  sales_tester_rechomes <- sales_tester_rechomes %>%
    mutate(
      HSITEAD = iconv(HSITEAD, to = "UTF-8", sub = ""),
      HCITY = iconv(HCITY, to = "UTF-8", sub = ""),
      HSTATE = iconv(HSTATE, to = "UTF-8", sub = ""),
      HZIP = iconv(HZIP, to = "UTF-8", sub = "")
    )

  # Run geocoding on sales_tester_rechomes data
  sales_tester_rechomes_geocoded <- geocode_addresses(
    df = sales_tester_rechomes,
    street_col = "HSITEAD",
    city_col = "HCITY", 
    state_col = "HSTATE",
    postalcode_col = "HZIP",
    geoid_col = "stfid"
  )
}

sales_tester_rechomes_geocoded <- apply_manual_geocode_overrides(
  sales_tester_rechomes_geocoded,
  manual_geocode_override_path
)

assignment_control_vars <- c("kids", "SEQUENCE", "ARELATE2", "AMOVERS", "AELNG1",
                             "ALGNCUR", "ALEASETP", "ACAROWN", "DPMTEXP")

# Cached geocoded files can predate newer assignment controls. Refresh these
# trial-level variables from assignment.sas7bdat before building cleaned_hds.csv.
sales_tester_rechomes_geocoded <- sales_tester_rechomes_geocoded %>%
  select(-any_of(c(assignment_control_vars, "month"))) %>%
  left_join(assignment, by = c("CONTROL", "TESTERID")) %>%
  left_join(
    sales_final %>% select(CONTROL, TESTERID, month),
    by = c("CONTROL", "TESTERID")
  ) %>%
  mutate(
    mother = if_else(kids == 1 & TSEX == 0, 1,
                     if_else(is.na(kids) | is.na(TSEX), NA_real_, 0))
  )

sales_tester_rechomes_geocoded <- flag_bad_address_inputs(
  sales_tester_rechomes_geocoded,
  street_col = "HSITEAD"
)
cat(
  "Flagged",
  sum(sales_tester_rechomes_geocoded$bad_address_input, na.rm = TRUE),
  "rows with clearly bad address inputs before exporting geocoded data.\n"
)

# Export the geocoded data after applying any manual overrides
write_csv(sales_tester_rechomes_geocoded, generated_file("sales_tester_rechomes_geocoded.csv"))
cat("Exported geocoded data to ", generated_file("sales_tester_rechomes_geocoded.csv"), "\n", sep = "")

# =================================================================================================== #
# MERGE WITH EXTERNAL (NON-HDS) DATASETS
# =================================================================================================== #

cat("=== MERGING EXTERNAL DATASETS (ACS + NON-HDS) ===\n")

# Reload CSV from file in case you want to run the file from here.
if (exists("sales_tester_rechomes_geocoded")) {
  merged_external_data <- sales_tester_rechomes_geocoded
} else {
  merged_external_data <- read_csv(generated_file("sales_tester_rechomes_geocoded.csv"))
}

# Load merge helpers
source(file.path("reconstructed_sample", "Cleaning_Scripts", "acs_merging.R"))
source(file.path("reconstructed_sample", "Cleaning_Scripts", "superfund_merging.R"))
source(file.path("reconstructed_sample", "Cleaning_Scripts", "school_score_merging.R"))
source(file.path("reconstructed_sample", "Cleaning_Scripts", "greatschools_crime_merging.R"))
source(file.path("reconstructed_sample", "Cleaning_Scripts", "rsei_merging.R"))
source(file.path("reconstructed_sample", "Cleaning_Scripts", "pm25_merging.R"))

# Prepare tract GEOID for ACS merging
merged_external_data <- merged_external_data %>%
  mutate(tract_geoid = str_sub(blockgroup_geoid, 1, 11))

cat("\n--- ACS tract-level demographics ---\n")
merged_external_data <- merge_acs(
  merged_external_data,
  geoid_col = "tract_geoid"
)

# Map ACS variables into C&T-style names expected by analysis.R (Rec only)
merged_external_data <- merged_external_data %>%
  mutate(
    w2012pc_Rec = percent_white,
    povrate_Rec = poverty_rate,
    skill_Rec = high_skilled_rate,
    college_Rec = college_graduate_rate,
    singlefamily_Rec = single_parent_rate,
    ownerocc_Rec = ownership_rate,
    medincome_Rec = median_income,
    nodad_Rec = nodad_rate,
    WhiteLI_Rec = white_low_income_component,
    WhiteMI_Rec = white_mid_income_component,
    WhiteHI_Rec = white_high_income_component
  )

cat("\n--- Superfund site counts (5 km) ---\n")
merged_external_data <- merge_superfund_counts(
  merged_external_data,
  lat_col = "lat",
  lon_col = "long"
)
if ("SFcount_5km" %in% names(merged_external_data) && !"SFcount_Rec" %in% names(merged_external_data)) {
  merged_external_data <- merged_external_data %>%
    rename(SFcount_Rec = SFcount_5km)
}

cat("\n--- SEDA school test scores ---\n")
merged_external_data <- merge_school_scores(
  merged_external_data,
  lat_col = "lat",
  lon_col = "long"
)

cat("\n--- GreatSchools index + crime rate (C&T replication) ---\n")
merged_external_data <- merge_ct_greatschools_crime(merged_external_data)

cat("\n--- RSEI toxic concentration (TRI) ---\n")
merged_external_data <- merge_rsei_toxic_conc(
  merged_external_data,
  lat_col = "lat",
  lon_col = "long",
  method = "centroid_nn",
  value_cols = c("toxconc", "ctconc", "nctconc", "score"),
  prefix = "rsei_",
  keep_cell = FALSE,
  agg_path = "Data/Non_HDS_Data/RSEI/aggmicro2022_2012.csv",
  agg_cache = "Data/Non_HDS_Data/RSEI/rsei_agg_2012.rds",
  lookup_cache = "Data/Non_HDS_Data/RSEI/rsei_grid_lookup_2012.rds",
  coords_cache = "Data/Non_HDS_Data/RSEI/rsei_grid_coords_wgs84_2012.rds"
)
if ("rsei_toxconc" %in% names(merged_external_data) && !"RSEI_Rec" %in% names(merged_external_data)) {
  merged_external_data <- merged_external_data %>%
    mutate(RSEI_Rec = rsei_toxconc)
}

cat("\n--- PM2.5 modeled concentrations (2012) ---\n")
merged_external_data <- merge_pm25(
  merged_external_data,
  lat_col = "lat",
  lon_col = "long",
  pm25_path = "Data/Non_HDS_Data/PM2_5/V5.NA.04.02/V5NA04.02.HybridPM25.NorthAmerica.2012001-2012364.nc"
)

# Print summary of final dataset
controls_with_complete_outcomes <- merged_external_data %>%
  filter(!is.na(STOTUNIT_TOTAL), !is.na(SAVLBAD_ANY), !is.na(poverty_rate)) %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%
  ungroup() %>%
  summarise(num_controls = n_distinct(CONTROL))
cat("Number of CONTROLs with complete outcomes and exactly two testers:", controls_with_complete_outcomes$num_controls, "\n")

# Export merged data
write_csv(merged_external_data, generated_file("cleaned_hds.csv"))
cat("Exported merged data with external datasets to ", generated_file("cleaned_hds.csv"), "\n", sep = "")
