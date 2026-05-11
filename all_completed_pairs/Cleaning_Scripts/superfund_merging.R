# superfund_merging.R
# Functions for merging HDS observations with nearby Superfund site counts

# Load required library
library(readxl)

#' Load Superfund Site Data from Excel
#'
#' Loads EPA National Priorities List (NPL) Superfund site data from Excel file
#' and filters to sites added before a specified year cutoff.
#'
#' @param excel_path Path to the Excel file containing Superfund data
#' @param sheet_name Name of the sheet containing the data (default: "EPA_NPL_Sites_asof_27Feb2014")
#' @param year_cutoff Year cutoff for filtering sites (default: 2012). Only sites with
#'                    NPL_STATUS_DATE before this year will be included.
#' @param status_filter Optional character vector of NPL_STATUS values to keep.
#'                      If NULL (default), all statuses are retained.
#' @return Data frame with columns: EPA_SITEID, SITE_NAME, LATITUDE, LONGITUDE, NPL_STATUS_DATE, year
load_superfund_data <- function(excel_path,
                                 sheet_name = "EPA_NPL_Sites_asof_27Feb2014",
                                 year_cutoff = 2012,
                                 status_filter = NULL) {

  # Read the Excel file
  sf_data <- read_excel(excel_path, sheet = sheet_name)

  # Extract year from NPL_STATUS_DATE
  sf_data$year <- as.numeric(format(sf_data$NPL_STATUS_DATE, "%Y"))

  # Filter to sites before year cutoff
  sf_data <- sf_data[sf_data$year < year_cutoff, ]

  # Optional filter by NPL status
  if (!is.null(status_filter)) {
    sf_data <- sf_data[sf_data$NPL_STATUS %in% status_filter, ]
  }

  cat("Loaded", nrow(sf_data), "Superfund sites with NPL_STATUS_DATE <", year_cutoff, "\n")

  return(sf_data)
}


#' Calculate Haversine Distance Between Two Points
#'
#' Computes the great-circle distance between two points on Earth using the
#' Haversine formula. Uses Earth radius of 6371 km.
#'
#' @param lon1 Longitude of first point (degrees)
#' @param lat1 Latitude of first point (degrees)
#' @param lon2 Longitude of second point (degrees, can be vector)
#' @param lat2 Latitude of second point (degrees, can be vector)
#' @return Distance in kilometers
haversine_km <- function(lon1, lat1, lon2, lat2) {
  # Convert degrees to radians
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180

  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))

  # Earth's radius in kilometers
  r <- 6371

  return(c * r)
}


#' Count Superfund Sites Within Radius
#'
#' Counts the number of Superfund sites within a specified radius of a point.
#'
#' @param lat Latitude of the point
#' @param lon Longitude of the point
#' @param sf_data Data frame with Superfund site locations (must have LATITUDE, LONGITUDE columns)
#' @param radius_km Radius in kilometers (default: 5)
#' @return Integer count of Superfund sites within the radius
count_superfund_within_radius <- function(lat, lon, sf_data, radius_km = 5) {
  # Calculate distances to all Superfund sites
  distances <- haversine_km(lon, lat, sf_data$LONGITUDE, sf_data$LATITUDE)

  # Count sites within radius
  count <- sum(distances <= radius_km, na.rm = TRUE)

  return(count)
}


#' Add Superfund Counts to Data Frame
#'
#' Adds a Superfund site count column to a data frame. Calculates counts for
#' a 5km radius.
#'
#' @param df Data frame containing observations with lat/long coordinates
#' @param lat_col Name of the latitude column (default: "lat")
#' @param lon_col Name of the longitude column (default: "long")
#' @param sf_data Data frame with Superfund site locations (from load_superfund_data()).
#'                If NULL, the data are loaded internally using the default file path.
#' @param excel_path Path to the Excel file containing Superfund data (used if sf_data is NULL)
#' @param sheet_name Name of the sheet containing the data (used if sf_data is NULL)
#' @param year_cutoff Year cutoff for filtering sites (used if sf_data is NULL)
#' @param status_filter Optional character vector of NPL_STATUS values to keep (used if sf_data is NULL)
#' @return Data frame with added column: SFcount_5km
merge_superfund_counts <- function(df,
                                  lat_col = "lat",
                                  lon_col = "long",
                                  sf_data = NULL,
                                  excel_path = "Data/Non_HDS_Data/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls",
                                  sheet_name = "EPA_NPL_Sites_asof_27Feb2014",
                                  year_cutoff = 2012,
                                  status_filter = NULL) {

  # Check that required columns exist
  if (!lat_col %in% names(df)) {
    stop(paste("Latitude column", lat_col, "not found in data frame"))
  }
  if (!lon_col %in% names(df)) {
    stop(paste("Longitude column", lon_col, "not found in data frame"))
  }
  if (is.null(sf_data)) {
    sf_data <- load_superfund_data(
      excel_path = excel_path,
      sheet_name = sheet_name,
      year_cutoff = year_cutoff,
      status_filter = status_filter
    )
  }
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(sf_data))) {
    stop("Superfund data must have LATITUDE and LONGITUDE columns")
  }

  cat("Calculating Superfund counts for", nrow(df), "observations...\n")

  # Calculate counts for each row
  # Initialize columns
  df$SFcount_5km <- NA

  # Progress indicator
  progress_interval <- max(1, floor(nrow(df) / 20))

  for (i in 1:nrow(df)) {
    # Skip if coordinates are missing
    if (is.na(df[[lat_col]][i]) || is.na(df[[lon_col]][i])) {
      next
    }

    # Calculate distances to all Superfund sites
    distances <- haversine_km(df[[lon_col]][i], df[[lat_col]][i],
                              sf_data$LONGITUDE, sf_data$LATITUDE)

    # Count sites within 5km
    df$SFcount_5km[i] <- sum(distances <= 5, na.rm = TRUE)

    # Progress indicator
    if (i %% progress_interval == 0) {
      cat("  Processed", i, "/", nrow(df), "observations\n")
    }
  }

  cat("Done! Added SFcount_5km column\n")
  cat("Summary of SFcount_5km:\n")
  print(summary(df$SFcount_5km))

  return(df)
}
