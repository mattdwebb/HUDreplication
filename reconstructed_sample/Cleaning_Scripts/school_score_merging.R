# school_score_merging.R
# Functions for merging school attendance boundary scores (SEDA) into properties
# Uses SABS 2015-16 boundaries and SEDA v3.0 standardized test scores

library(sf)
library(dplyr)
library(readr)

#' Match properties to elementary and middle school test scores
#'
#' Takes a dataframe with property coordinates and returns it with added columns
#' for elementary and middle school test scores from SEDA v3.0
#'
#' @param data A dataframe containing lat and long columns
#' @param lat_col Name of latitude column (default: "lat")
#' @param lon_col Name of longitude column (default: "long")
#' @return Original dataframe with four additional columns:
#'   - elementary_school_score: SEDA v3 test score (grades 4-5.5 average)
#'   - middle_school_score: SEDA v3 test score (grades 6.5-7.5 average)
#'   - elementary_school_id: NCES school ID(s) used for elementary score
#'   - middle_school_id: NCES school ID(s) used for middle school score
#'   - elementary_school_overlap_count: number of overlapping primary boundaries
#'   - middle_school_overlap_count: number of overlapping middle boundaries
#'
#' @details
#' Uses spatial point-in-polygon matching with SABS 2015-16 school attendance
#' boundaries. For properties falling in multiple school boundaries, scores are
#' averaged across overlapping schools with non-missing SEDA data. Properties
#' outside all boundaries or matched only to schools without SEDA data receive
#' NA.
#'
merge_school_scores <- function(data, lat_col = "lat", lon_col = "long") {

  cat("Loading school boundaries and test scores...\n")

  school_output_cols <- c(
    "elementary_school_id", "elementary_school_score",
    "elementary_school_overlap_count", "elementary_school_scored_overlap_count",
    "middle_school_id", "middle_school_score",
    "middle_school_overlap_count", "middle_school_scored_overlap_count"
  )
  data <- data %>% select(-any_of(school_output_cols))

  # Load SABS 2015-16 school attendance boundaries
  primary <- st_read("Data/Non_HDS_Data/SABS/SABS_1516_SchoolLevels/SABS_1516_Primary.shp",
                     quiet = TRUE)
  middle <- st_read("Data/Non_HDS_Data/SABS/SABS_1516_SchoolLevels/SABS_1516_Middle.shp",
                    quiet = TRUE)

  # Load SEDA v3.0 standardized test scores
  seda <- read_csv("Data/Non_HDS_Data/SEDA_v3/seda_school_pool_cs_v30.csv",
                   show_col_types = FALSE) %>%
    mutate(ncessch = as.character(ncessch))

  # Calculate elementary school scores (average grades 4, 4.5, 5, 5.5)
  elem_scores <- seda %>%
    filter(midgrd %in% c(4, 4.5, 5, 5.5)) %>%
    group_by(ncessch) %>%
    summarise(elementary_school_score = mean(mn_avg_ol, na.rm = TRUE),
              .groups = "drop")

  # Calculate middle school scores (average grades 6.5, 7, 7.5)
  middle_scores <- seda %>%
    filter(midgrd %in% c(6.5, 7, 7.5)) %>%
    group_by(ncessch) %>%
    summarise(middle_school_score = mean(mn_avg_ol, na.rm = TRUE),
              .groups = "drop")

  cat(sprintf("Matching %d properties to schools...\n", nrow(data)))

  # Filter to properties with valid coordinates
  properties <- data %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]])) %>%
    mutate(row_id = row_number())

  # Convert to spatial object
  properties_sf <- st_as_sf(properties,
                            coords = c(lon_col, lat_col),
                            crs = 4326)
  properties_sf <- st_transform(properties_sf, st_crs(primary))

  summarise_school_matches <- function(boundaries, scores, id_col, score_col,
                                       overlap_count_col, scored_overlap_count_col) {
    score_name <- names(scores)[names(scores) != "ncessch"][1]

    st_join(properties_sf, boundaries, join = st_within) %>%
      st_drop_geometry() %>%
      mutate(ncessch = as.character(ncessch)) %>%
      left_join(scores, by = "ncessch") %>%
      distinct(row_id, ncessch, .keep_all = TRUE) %>%
      group_by(row_id) %>%
      summarise(
        "{id_col}" := if (any(!is.na(.data[[score_name]]))) {
          paste(sort(unique(ncessch[!is.na(.data[[score_name]])])), collapse = ";")
        } else {
          NA_character_
        },
        "{score_col}" := if (any(!is.na(.data[[score_name]]))) {
          mean(.data[[score_name]], na.rm = TRUE)
        } else {
          NA_real_
        },
        "{overlap_count_col}" := sum(!is.na(ncessch)),
        "{scored_overlap_count_col}" := sum(!is.na(.data[[score_name]])),
        .groups = "drop"
      )
  }

  # Match to elementary schools
  elem_matches <- summarise_school_matches(
    primary,
    elem_scores,
    id_col = "elementary_school_id",
    score_col = "elementary_school_score",
    overlap_count_col = "elementary_school_overlap_count",
    scored_overlap_count_col = "elementary_school_scored_overlap_count"
  )

  # Match to middle schools
  middle_matches <- summarise_school_matches(
    middle,
    middle_scores,
    id_col = "middle_school_id",
    score_col = "middle_school_score",
    overlap_count_col = "middle_school_overlap_count",
    scored_overlap_count_col = "middle_school_scored_overlap_count"
  )

  # Merge back with original data
  result <- properties %>%
    left_join(elem_matches, by = "row_id") %>%
    left_join(middle_matches, by = "row_id") %>%
    select(-row_id)

  # For properties with missing coordinates, add NA columns
  missing_coords <- data %>%
    filter(is.na(.data[[lat_col]]) | is.na(.data[[lon_col]])) %>%
    mutate(
      elementary_school_id = NA_character_,
      elementary_school_score = NA_real_,
      elementary_school_overlap_count = NA_integer_,
      elementary_school_scored_overlap_count = NA_integer_,
      middle_school_id = NA_character_,
      middle_school_score = NA_real_,
      middle_school_overlap_count = NA_integer_,
      middle_school_scored_overlap_count = NA_integer_
    )

  # Combine
  final <- bind_rows(result, missing_coords)

  # Report statistics
  n_elem <- sum(!is.na(final$elementary_school_score))
  n_middle <- sum(!is.na(final$middle_school_score))

  cat(sprintf("  Elementary schools matched: %d / %d (%.1f%%)\n",
              n_elem, nrow(data), 100*n_elem/nrow(data)))
  cat(sprintf("  Middle schools matched:     %d / %d (%.1f%%)\n",
              n_middle, nrow(data), 100*n_middle/nrow(data)))

  return(final)
}
