# ===================================================================================
# rsei_merging.R
# ===================================================================================
# 
# PURPOSE:
#   Helper functions to merge EPA RSEI (Risk-Screening Environmental Indicators)
#   TRI (Toxics Release Inventory) toxic concentration data onto HDS property data.
#
# OVERVIEW:
#   The RSEI model provides gridded estimates of toxic chemical concentrations
#   across the US based on TRI facility releases. This script:
#     1. Loads and caches RSEI aggregated grid data (toxic concentrations per cell)
#     2. Loads and caches RSEI grid cell centroid coordinates
#     3. Builds a lookup table joining concentration data to geographic coordinates
#     4. Matches input property locations to RSEI grid cells using either:
#        - Nearest-centroid matching (fast, approximate)
#        - Point-in-polygon matching (slower, exact)
#
# DATA SOURCES:
#   - RSEI aggregated micro data: contains toxic concentration values per grid cell
#   - RSEI grid shapefiles: define the 810m x 810m grid cells covering CONUS
#
# KEY OUTPUT VARIABLES:
#   - toxconc:  Total toxic concentration (all chemicals combined)
#   - ctconc:   Carcinogenic toxic concentration
#   - nctconc:  Non-carcinogenic toxic concentration
#   - score:    RSEI risk score (toxicity-weighted concentration × population)
#
# ===================================================================================

suppressPackageStartupMessages({
  library(data.table)   # Fast data manipulation and file reading

library(dplyr)        # Data wrangling with tidyverse syntax
  library(readr)        # Reading CSV files (loaded but fread used instead)
  library(sf)           # Spatial features for geographic operations
  library(RANN)         # Fast nearest neighbor search
  library(foreign)      # Reading DBF files from shapefiles
})

# ===================================================================================
# LOADERS + CACHE BUILDERS
# ===================================================================================
# These functions handle loading raw RSEI data files and building cached versions
# to speed up subsequent runs. The caching pattern is:
#   1. Check if cache exists and refresh=FALSE -> return cached data
#   2. Otherwise, read raw data, process, save cache, return result
# ===================================================================================

#' Load RSEI Aggregated Grid Data
#'
#' Reads the RSEI aggregated micro data file containing toxic concentration
#' values for each grid cell. Results are cached as RDS for faster reloading.
#'
#' @param agg_path    Path to the raw RSEI aggregated CSV file
#' @param cache_path  Path where the processed RDS cache will be saved
#' @param refresh     If TRUE, ignore cache and reload from raw file
#'
#' @return data.table with columns:
#'   - gridcode: Unique identifier for each grid cell
#'   - cellx, celly: Grid cell indices (x and y coordinates in grid space)
#'   - num_facs: Number of TRI facilities affecting this cell
#'   - num_releases: Number of chemical releases affecting this cell
#'   - num_chems: Number of distinct chemicals released
#'   - toxconc: Total toxic concentration (µg/m³ toxicity-weighted)
#'   - score: RSEI risk score
#'   - pop: Population in the grid cell
#'   - ctconc: Carcinogenic toxic concentration
#'   - nctconc: Non-carcinogenic toxic concentration
load_rsei_agg <- function(
  agg_path = "Data/Non_HDS_Data/RSEI/aggmicro2022_2012.csv",
  cache_path = "Data/Non_HDS_Data/RSEI/rsei_agg_2012.rds",
  refresh = FALSE
) {
  # Return cached data if available and not forcing refresh
  if (!refresh && file.exists(cache_path)) {
    cat("Loading cached RSEI aggregated grid data:\n  ", cache_path, "\n")
    return(readRDS(cache_path))
  }

  cat("Reading RSEI aggregated grid data:\n  ", agg_path, "\n")
  
  # Use fread for fast CSV reading; select only needed columns to save memory
  agg <- data.table::fread(
    agg_path,
    select = c("GridCode", "X", "Y", "NumFacs", "NumReleases", "NumChems",
               "ToxConc", "Score", "Pop", "CTConc", "NCTConc"),
    showProgress = TRUE
  )

  # Standardize column names to lowercase for consistency
  data.table::setnames(
    agg,
    old = c("GridCode", "X", "Y", "NumFacs", "NumReleases", "NumChems",
            "ToxConc", "Score", "Pop", "CTConc", "NCTConc"),
    new = c("gridcode", "cellx", "celly", "num_facs", "num_releases",
            "num_chems", "toxconc", "score", "pop", "ctconc", "nctconc")
  )

  # Cache processed data for future runs
saveRDS(agg, cache_path)
  cat("Cached RSEI aggregated data to:\n  ", cache_path, "\n")
  return(agg)
}

#' Load RSEI Grid Cell Centroids
#'
#' Reads the DBF attribute tables from RSEI grid shapefiles to extract
#' the centroid coordinates (lat/long) for each grid cell. The RSEI grid
#' is split into "bottom" (southern) and "top" (northern) portions.
#'
#' @param bottom_dbf  Path to the DBF file for the bottom (south) grid portion
#' @param top_dbf     Path to the DBF file for the top (north) grid portion
#' @param cache_path  Path where the combined centroids RDS cache will be saved
#' @param refresh     If TRUE, ignore cache and reload from raw files
#'
#' @return data.table with columns:
#'   - cellx, celly: Grid cell indices (match those in aggregated data)
#'   - clat, clong: Centroid latitude and longitude (WGS84)
#'   - cx, cy: Centroid coordinates in the grid's native projection
load_rsei_grid_centroids <- function(
  bottom_dbf = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_bottom.dbf",
  top_dbf = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_top.dbf",
  cache_path = "Data/Non_HDS_Data/RSEI/rsei_grid_centroids.rds",
  refresh = FALSE
) {
  # Return cached data if available and not forcing refresh
  if (!refresh && file.exists(cache_path)) {
    cat("Loading cached RSEI grid centroids:\n  ", cache_path, "\n")
    return(readRDS(cache_path))
  }

  cat("Reading RSEI grid centroids from DBF:\n  ", bottom_dbf, "\n  ", top_dbf, "\n")
  
  # Read DBF files (shapefile attribute tables) for both grid portions
  bottom <- foreign::read.dbf(bottom_dbf, as.is = TRUE)
  top <- foreign::read.dbf(top_dbf, as.is = TRUE)

  # Combine north and south portions into single table
  grid <- data.table::rbindlist(list(bottom, top), use.names = TRUE, fill = TRUE)
  
  # Standardize column names to lowercase
  data.table::setnames(
    grid,
    old = c("CELLX", "CELLY", "CLAT", "CLONG", "CX", "CY"),
    new = c("cellx", "celly", "clat", "clong", "cx", "cy")
  )

  # Keep only the columns we need
  grid <- grid[, .(cellx, celly, clat, clong, cx, cy)]

  # Cache for future runs
  saveRDS(grid, cache_path)
  cat("Cached RSEI grid centroids to:\n  ", cache_path, "\n")
  return(grid)
}

#' Build RSEI Grid Lookup Table
#'
#' Creates a complete lookup table by joining the aggregated toxic concentration
#' data with grid cell centroid coordinates. This allows matching property
#' locations to RSEI values via geographic coordinates.
#'
#' @param agg_path      Path to raw RSEI aggregated CSV
#' @param bottom_dbf    Path to bottom grid DBF file
#' @param top_dbf       Path to top grid DBF file
#' @param agg_cache     Cache path for aggregated data
#' @param grid_cache    Cache path for grid centroids
#' @param lookup_cache  Cache path for the final joined lookup table
#' @param refresh       If TRUE, rebuild all caches from raw files
#'
#' @return data.table combining centroid coordinates with toxic concentration values
#'         Keyed by (cellx, celly) for fast lookups
build_rsei_grid_lookup <- function(
  agg_path = "Data/Non_HDS_Data/RSEI/aggmicro2022_2012.csv",
  bottom_dbf = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_bottom.dbf",
  top_dbf = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_top.dbf",
  agg_cache = "Data/Non_HDS_Data/RSEI/rsei_agg_2012.rds",
  grid_cache = "Data/Non_HDS_Data/RSEI/rsei_grid_centroids.rds",
  lookup_cache = "Data/Non_HDS_Data/RSEI/rsei_grid_lookup_2012.rds",
  refresh = FALSE
) {
  # Return cached lookup if available
  if (!refresh && file.exists(lookup_cache)) {
    cat("Loading cached RSEI grid lookup:\n  ", lookup_cache, "\n")
    return(readRDS(lookup_cache))
  }

  # Load the two component datasets (may use their own caches)
  agg <- load_rsei_agg(agg_path = agg_path, cache_path = agg_cache, refresh = refresh)
  grid <- load_rsei_grid_centroids(
    bottom_dbf = bottom_dbf,
    top_dbf = top_dbf,
    cache_path = grid_cache,
    refresh = refresh
  )

  # Set keys for fast data.table join on cell indices
  data.table::setkey(agg, cellx, celly)
  data.table::setkey(grid, cellx, celly)

  cat("Joining RSEI aggregated data to grid centroids...\n")
  
  # Right join: keep all aggregated data rows, add centroid coords where available
  # grid[agg] is data.table syntax for right join (agg rows with grid columns)
  lookup <- grid[agg]

  # Check for any cells in aggregated data that lack centroid coordinates
  # (shouldn't happen if data is complete, but good to verify)
  missing_centroids <- sum(is.na(lookup$clat) | is.na(lookup$clong))
  if (missing_centroids > 0) {
    cat("Warning:", missing_centroids, "RSEI cells missing centroids after join.\n")
  } else {
    cat("All RSEI cells matched to grid centroids.\n")
  }

  # Cache the complete lookup table
  saveRDS(lookup, lookup_cache)
  cat("Cached RSEI grid lookup to:\n  ", lookup_cache, "\n")

  return(lookup)
}

#' Prepare RSEI Grid for Nearest-Neighbor Matching
#'
#' Prepares the RSEI grid lookup table and coordinate matrix for use with
#' the RANN nearest-neighbor algorithm. Optionally projects coordinates
#' to a specified CRS for distance calculations.
#'
#' @param lookup         Pre-built lookup table (if NULL, will build it)
#' @param distance_crs   EPSG code for projection to use in distance calculations
#'                       If NULL, uses unprojected lat/long (approximate distances)
#' @param coords_cache   Path to cache the projected coordinate matrix
#' @param refresh_coords If TRUE, recompute coordinates even if cached
#' @param ...            Additional arguments passed to build_rsei_grid_lookup()
#'
#' @return List containing:
#'   - lookup: The cleaned lookup table (rows with valid coordinates only)
#'   - coords: Matrix of (x, y) coordinates for nearest-neighbor search
#'   - distance_crs: The CRS used for projection (NULL if unprojected)
prepare_rsei_grid <- function(
  lookup = NULL,
  distance_crs = NULL,
  coords_cache = NULL,
  refresh_coords = FALSE,
  ...
) {
  # Build lookup if not provided
  if (is.null(lookup)) {
    lookup <- build_rsei_grid_lookup(...)
  }

  # Remove rows with missing coordinates (can't be used for spatial matching)
  lookup_clean <- lookup[!is.na(lookup$clong) & !is.na(lookup$clat), ]
  dropped <- nrow(lookup) - nrow(lookup_clean)
  if (dropped > 0) {
    cat("Dropping", dropped, "RSEI grid rows with missing centroids.\n")
  }

  # Try to load cached coordinates if available
  if (!is.null(coords_cache) && file.exists(coords_cache) && !refresh_coords) {
    cat("Loading cached RSEI grid coordinates:\n  ", coords_cache, "\n")
    coords <- readRDS(coords_cache)
    # Verify cache matches current lookup size
    if (nrow(coords) == nrow(lookup_clean)) {
      return(list(lookup = lookup_clean, coords = coords, distance_crs = distance_crs))
    }
    cat("Cached coordinates size mismatch. Recomputing...\n")
  }

  # Build coordinate matrix for nearest-neighbor search
  if (is.null(distance_crs)) {
    # Use unprojected coordinates (lon, lat order for consistency with sf)
    # Note: distances in unprojected coords are approximate (degrees, not meters)
    coords <- as.matrix(lookup_clean[, .(clong, clat)])
  } else {
    # Project to specified CRS for accurate distance calculations
    # Common choices: 5070 (CONUS Albers), 2163 (US National Atlas)
    cat("Projecting RSEI grid centroids to", distance_crs, "(this may take a while)...\n")
    grid_sf <- st_as_sf(lookup_clean, coords = c("clong", "clat"), crs = 4326, remove = FALSE)
    coords <- st_coordinates(st_transform(grid_sf, distance_crs))
  }

  # Cache coordinates for future runs
  if (!is.null(coords_cache)) {
    saveRDS(coords, coords_cache)
    cat("Cached RSEI grid coordinates to:\n  ", coords_cache, "\n")
  }

  list(lookup = lookup_clean, coords = coords, distance_crs = distance_crs)
}

# ===================================================================================
# MATCHING HELPERS
# ===================================================================================
# These functions perform the actual spatial matching between input property
# locations and RSEI grid cells. Two methods are available:
#   1. Centroid nearest-neighbor: Fast, uses RANN to find closest cell centroid
#   2. Polygon point-in-polygon: Slower but exact, uses sf spatial joins
# ===================================================================================

#' Match Properties to RSEI Grid via Nearest Centroid
#'
#' Uses fast nearest-neighbor search (RANN) to find the closest RSEI grid
#' cell centroid for each property location. This is an approximation since
#' a property might be closer to one centroid but actually fall within a
#' different cell's polygon boundaries.
#'
#' @param df           Data frame containing property locations
#' @param grid         RSEI grid lookup table (from prepare_rsei_grid)
#' @param coords       Coordinate matrix for grid centroids (from prepare_rsei_grid)
#' @param lat_col      Name of latitude column in df
#' @param lon_col      Name of longitude column in df
#' @param value_cols   RSEI columns to merge onto properties
#' @param prefix       Prefix to add to merged column names (e.g., "rsei_toxconc")
#' @param keep_cell    If TRUE, also include matched cell indices and coordinates
#' @param distance_crs CRS used for coordinate projection (must match coords)
#'
#' @return Input df with additional columns for matched RSEI values
match_rsei_centroid <- function(
  df,
  grid,
  coords,
  lat_col = "lat",
  lon_col = "long",
  value_cols = c("toxconc", "ctconc", "nctconc", "score"),
  prefix = "rsei_",
  keep_cell = FALSE,
  distance_crs = NULL
) {
  # Validate input columns exist
  if (!lat_col %in% names(df)) stop("Latitude column not found: ", lat_col)
  if (!lon_col %in% names(df)) stop("Longitude column not found: ", lon_col)

  # Only keep value columns that actually exist in the grid lookup
  value_cols <- intersect(value_cols, names(grid))
  if (length(value_cols) == 0) stop("No requested RSEI value columns found in grid lookup.")

  # Add row index to preserve original order and enable rejoining
  df_indexed <- df %>% mutate(row_id = row_number())
  
  # Filter to rows with valid coordinates (can't match NA locations)
  df_valid <- df_indexed %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]]))

  # Handle edge case of no valid coordinates
  if (nrow(df_valid) == 0) {
    cat("No valid coordinates to match. Returning input with NA columns.\n")
    df_out <- df
    for (col in value_cols) {
      df_out[[paste0(prefix, col)]] <- NA_real_
    }
    if (keep_cell) {
      df_out[[paste0(prefix, "cellx")]] <- NA_integer_
      df_out[[paste0(prefix, "celly")]] <- NA_integer_
      df_out[[paste0(prefix, "clat")]] <- NA_real_
      df_out[[paste0(prefix, "clong")]] <- NA_real_
    }
    return(df_out)
  }

  # Build query coordinate matrix (must match CRS of grid coords)
  if (is.null(distance_crs)) {
    # Unprojected: use lon, lat order (x, y)
    query_coords <- as.matrix(df_valid[, c(lon_col, lat_col)])
  } else {
    # Project property coordinates to match grid projection
    pts_sf <- st_as_sf(df_valid, coords = c(lon_col, lat_col), crs = 4326)
    query_coords <- st_coordinates(st_transform(pts_sf, distance_crs))
  }

  # Run RANN nearest-neighbor search (k=1 for single closest match)
  cat("Running nearest-centroid match for", nrow(df_valid), "points...\n")
  nn <- RANN::nn2(coords, query_coords, k = 1)
  match_idx <- nn$nn.idx[, 1]  # Extract index of nearest neighbor

  # Look up RSEI values for matched grid cells
  matched <- grid[match_idx]
  for (col in value_cols) {
    df_valid[[paste0(prefix, col)]] <- matched[[col]]
  }

  # Optionally include cell identifiers and coordinates
  if (keep_cell) {
    df_valid[[paste0(prefix, "cellx")]] <- matched$cellx
    df_valid[[paste0(prefix, "celly")]] <- matched$celly
    df_valid[[paste0(prefix, "clat")]] <- matched$clat
    df_valid[[paste0(prefix, "clong")]] <- matched$clong
  }

  # Rejoin matched values back to full dataset (including rows with NA coords)
  df_out <- df_indexed %>%
    left_join(df_valid %>% select(row_id, starts_with(prefix)), by = "row_id") %>%
    select(-row_id)

  # Report match statistics
  matched_count <- sum(!is.na(df_out[[paste0(prefix, value_cols[1])]]))
  cat("RSEI centroid matches:", matched_count, "/", nrow(df_out), "\n")

  return(df_out)
}

#' Match Properties to RSEI Grid via Point-in-Polygon
#'
#' Uses sf spatial join to determine which RSEI grid polygon each property
#' falls within. This is more accurate than centroid matching but significantly
#' slower due to loading large polygon shapefiles.
#'
#' @param df           Data frame containing property locations
#' @param lat_col      Name of latitude column in df
#' @param lon_col      Name of longitude column in df
#' @param value_cols   RSEI columns to merge onto properties
#' @param prefix       Prefix to add to merged column names
#' @param keep_cell    If TRUE, also include matched cell indices
#' @param bottom_shp   Path to shapefile for bottom (south) grid portion
#' @param top_shp      Path to shapefile for top (north) grid portion
#' @param lookup       Pre-built lookup table (if NULL, will build it)
#' @param ...          Additional arguments passed to build_rsei_grid_lookup()
#'
#' @return Input df with additional columns for matched RSEI values
match_rsei_polygon <- function(
  df,
  lat_col = "lat",
  lon_col = "long",
  value_cols = c("toxconc", "ctconc", "nctconc", "score"),
  prefix = "rsei_",
  keep_cell = FALSE,
  bottom_shp = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_bottom.shp",
  top_shp = "Data/Non_HDS_Data/RSEI/poly_gc14_conus_810m_top.shp",
  lookup = NULL,
  ...
) {
  # Validate input columns exist
  if (!lat_col %in% names(df)) stop("Latitude column not found: ", lat_col)
  if (!lon_col %in% names(df)) stop("Longitude column not found: ", lon_col)

  # Filter to known valid RSEI columns
  value_cols <- intersect(value_cols, c("toxconc", "ctconc", "nctconc", "score"))
  if (length(value_cols) == 0) stop("No requested RSEI value columns found in lookup.")

  # Build lookup table if not provided
  if (is.null(lookup)) {
    lookup <- build_rsei_grid_lookup(...)
  }

  # Load RSEI grid polygons (WARNING: these files are large, ~1GB+ combined)
  cat("Reading RSEI grid polygons (this is large and may take a while)...\n")
  bottom <- st_read(bottom_shp, quiet = TRUE)
  top <- st_read(top_shp, quiet = TRUE)
  
  # Combine and keep only cell identifiers (drop other attributes to save memory)
  grid_polys <- rbind(bottom, top) %>%
    select(CELLX, CELLY)

  # Add row index for rejoining results
  df_indexed <- df %>% mutate(row_id = row_number())
  df_valid <- df_indexed %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]]))

  # Handle edge case of no valid coordinates
  if (nrow(df_valid) == 0) {
    cat("No valid coordinates to match. Returning input with NA columns.\n")
    df_out <- df
    for (col in value_cols) {
      df_out[[paste0(prefix, col)]] <- NA_real_
    }
    if (keep_cell) {
      df_out[[paste0(prefix, "cellx")]] <- NA_integer_
      df_out[[paste0(prefix, "celly")]] <- NA_integer_
    }
    return(df_out)
  }

  # Convert property locations to sf points (WGS84 = EPSG:4326)
  pts_sf <- st_as_sf(df_valid, coords = c(lon_col, lat_col), crs = 4326)
  
  # Transform points to match grid CRS if needed
  grid_crs <- st_crs(grid_polys)
  if (!is.na(grid_crs)) {
    pts_sf <- st_transform(pts_sf, grid_crs)
  }

  # Perform spatial join: find which polygon contains each point
  cat("Running point-in-polygon match...\n")
  joined <- st_join(pts_sf, grid_polys, join = st_within, left = TRUE)

  # Drop geometry and handle any duplicate matches (take first)
  joined <- joined %>%
    st_drop_geometry() %>%
    group_by(row_id) %>%
    slice(1) %>%
    ungroup()

  # Join RSEI values from lookup table using cell indices
  joined <- joined %>%
    rename(cellx = CELLX, celly = CELLY) %>%
    left_join(lookup, by = c("cellx", "celly"))

  # Add prefixed value columns
  for (col in value_cols) {
    joined[[paste0(prefix, col)]] <- joined[[col]]
  }

  # Handle cell identifier columns based on keep_cell flag
  if (!keep_cell) {
    joined <- joined %>% select(-cellx, -celly)
  } else {
    joined[[paste0(prefix, "cellx")]] <- joined$cellx
    joined[[paste0(prefix, "celly")]] <- joined$celly
  }

  # Rejoin to full dataset
  df_out <- df_indexed %>%
    left_join(joined %>% select(row_id, starts_with(prefix)), by = "row_id") %>%
    select(-row_id)

  # Report match statistics
  matched_count <- sum(!is.na(df_out[[paste0(prefix, value_cols[1])]]))
  cat("RSEI polygon matches:", matched_count, "/", nrow(df_out), "\n")

  return(df_out)
}

# ===================================================================================
# MAIN MERGE FUNCTION
# ===================================================================================
# This is the primary user-facing function that wraps the matching helpers
# with a simple interface for merging RSEI data onto property datasets.
# ===================================================================================

#' Merge RSEI Toxic Concentration Data onto Properties
#'
#' Main entry point for adding RSEI toxic concentration data to a property
#' dataset. Supports two matching methods:
#'   - "centroid_nn": Fast nearest-neighbor matching to grid cell centroids
#'   - "polygon": Exact point-in-polygon spatial join (slower)
#'
#' @param df           Data frame containing property locations with lat/long
#' @param lat_col      Name of latitude column in df (default: "lat")
#' @param lon_col      Name of longitude column in df (default: "long")
#' @param method       Matching method: "centroid_nn" (default) or "polygon"
#' @param distance_crs Optional EPSG code for projected distance calculations
#'                     (only used with centroid_nn method)
#' @param value_cols   RSEI columns to merge (default: toxconc, ctconc, nctconc, score)
#' @param prefix       Prefix for new column names (default: "rsei_")
#' @param keep_cell    If TRUE, include matched cell indices and coordinates
#' @param ...          Additional arguments passed to data loading functions
#'                     (e.g., custom file paths, cache paths, refresh flags)
#'
#' @return Input df with additional columns:
#'   - rsei_toxconc:  Total toxic concentration
#'   - rsei_ctconc:   Carcinogenic toxic concentration
#'   - rsei_nctconc:  Non-carcinogenic toxic concentration
#'   - rsei_score:    RSEI risk score
#'   - (if keep_cell=TRUE) rsei_cellx, rsei_celly, rsei_clat, rsei_clong
#'
#' @examples
#' # Basic usage with default centroid matching
#' properties_with_rsei <- merge_rsei_toxic_conc(properties_df)
#'
#' # Use polygon matching for exact cell assignment
#' properties_with_rsei <- merge_rsei_toxic_conc(properties_df, method = "polygon")
#'
#' # Custom column names and projected distance calculation
#' properties_with_rsei <- merge_rsei_toxic_conc(
#'   properties_df,
#'   lat_col = "latitude",
#'   lon_col = "longitude",
#'   distance_crs = 5070  # CONUS Albers Equal Area
#' )
merge_rsei_toxic_conc <- function(
  df,
  lat_col = "lat",
  lon_col = "long",
  method = c("centroid_nn", "polygon"),
  distance_crs = NULL,
  value_cols = c("toxconc", "ctconc", "nctconc", "score"),
  prefix = "rsei_",
  keep_cell = FALSE,
  ...
) {
  # Validate and select matching method
  method <- match.arg(method)

  # Route to appropriate matching function
  if (method == "polygon") {
    # Polygon matching: exact but slow
    return(match_rsei_polygon(
      df,
      lat_col = lat_col,
      lon_col = lon_col,
      value_cols = value_cols,
      prefix = prefix,
      keep_cell = keep_cell,
      ...
    ))
  }

  # Centroid matching: fast nearest-neighbor approximation
  # First prepare the grid (loads data, builds coordinate matrix)
  grid_prep <- prepare_rsei_grid(distance_crs = distance_crs, ...)
  
  return(match_rsei_centroid(
    df,
    grid = grid_prep$lookup,
    coords = grid_prep$coords,
    lat_col = lat_col,
    lon_col = lon_col,
    value_cols = value_cols,
    prefix = prefix,
    keep_cell = keep_cell,
    distance_crs = distance_crs
  ))
}
