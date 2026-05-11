# pm25_merging.R
# Functions for merging modeled PM2.5 concentrations (2012) into properties

#' Load modeled PM2.5 grid from HDF5 or NetCDF
#'
#' Supports the V4/V5 NetCDF files (van Donkelaar et al. style) and the
#' legacy Meng et al. (2019) HDF5 file (values stored at 100x scale).
#'
#' @param pm25_path Path to the PM2.5 grid file (.nc or .h5)
#' @param dataset_name HDF5 dataset name for PM2.5 values (default: "CorrectedPM2.5")
#' @param lat_name HDF5 dataset name for latitude vector (default: "latitude")
#' @param lon_name HDF5 dataset name for longitude vector (default: "longitude")
#' @param nc_pm_var NetCDF variable name for PM2.5 values (default: NULL = infer)
#' @param nc_lat_var NetCDF variable name for latitude (default: NULL = infer)
#' @param nc_lon_var NetCDF variable name for longitude (default: NULL = infer)
#' @return List with lat, lon, pm (matrix), and grid metadata
load_pm25_grid <- function(pm25_path,
                           dataset_name = "CorrectedPM2.5",
                           lat_name = "latitude",
                           lon_name = "longitude",
                           nc_pm_var = NULL,
                           nc_lat_var = NULL,
                           nc_lon_var = NULL) {
  if (!file.exists(pm25_path)) {
    stop(paste("PM2.5 file not found:", pm25_path))
  }

  ext <- tolower(tools::file_ext(pm25_path))
  if (ext == "nc") {
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
      stop("Please install the 'ncdf4' package to read NetCDF files.")
    }

    nc <- ncdf4::nc_open(pm25_path)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    var_names <- names(nc$var)
    dim_names <- names(nc$dim)

    find_coord_name <- function(pattern, names_vec) {
      hit <- names_vec[grepl(pattern, names_vec)][1]
      if (length(hit) == 0 || is.na(hit) || hit == "") return(NULL)
      hit
    }

    if (is.null(nc_lat_var)) {
      nc_lat_var <- find_coord_name("^lat$|^latitude$|^LAT$|^Latitude$", var_names)
    }
    if (is.null(nc_lon_var)) {
      nc_lon_var <- find_coord_name("^lon$|^longitude$|^LON$|^Longitude$", var_names)
    }

    get_coord_vals <- function(name) {
      if (is.null(name)) return(NULL)
      if (name %in% var_names) return(ncdf4::ncvar_get(nc, name))
      if (name %in% dim_names) return(nc$dim[[name]]$vals)
      NULL
    }

    lat <- get_coord_vals(nc_lat_var)
    lon <- get_coord_vals(nc_lon_var)

    if (is.null(lat) || is.null(lon)) {
      lat_dim <- find_coord_name("^lat$|^latitude$|^LAT$|^Latitude$", dim_names)
      lon_dim <- find_coord_name("^lon$|^longitude$|^LON$|^Longitude$", dim_names)
      if (is.null(lat)) lat <- get_coord_vals(lat_dim)
      if (is.null(lon)) lon <- get_coord_vals(lon_dim)
    }

    if (is.null(lat) || is.null(lon)) {
      stop("Could not infer latitude/longitude variables in NetCDF file.")
    }

    if (is.null(nc_pm_var)) {
      candidates <- setdiff(var_names, c(nc_lat_var, nc_lon_var))
      if (length(candidates) == 1) {
        nc_pm_var <- candidates[1]
      } else {
        stop("Multiple candidate NetCDF variables found. Please set nc_pm_var explicitly.")
      }
    }

    pm <- ncdf4::ncvar_get(nc, nc_pm_var)

    scale_factor_default <- 1
  } else {
    if (requireNamespace("hdf5r", quietly = TRUE)) {
      file <- hdf5r::H5File$new(pm25_path, mode = "r")
      on.exit(file$close_all(), add = TRUE)
      lat <- file[[lat_name]]$read()
      lon <- file[[lon_name]]$read()
      pm <- file[[dataset_name]]$read()
    } else if (requireNamespace("rhdf5", quietly = TRUE)) {
      lat <- rhdf5::h5read(pm25_path, lat_name)
      lon <- rhdf5::h5read(pm25_path, lon_name)
      pm <- rhdf5::h5read(pm25_path, dataset_name)
    } else {
      stop("Please install either the 'hdf5r' or 'rhdf5' package to read HDF5 files.")
    }

    scale_factor_default <- 100
  }

  lat <- as.numeric(lat)
  lon <- as.numeric(lon)

  if (length(dim(pm)) != 2) {
    stop("PM2.5 dataset is not a 2D matrix.")
  }
  if (length(lat) == dim(pm)[1] && length(lon) == dim(pm)[2]) {
    # ok
  } else if (length(lat) == dim(pm)[2] && length(lon) == dim(pm)[1]) {
    pm <- t(pm)
  } else {
    stop("PM2.5 grid dimensions do not match latitude/longitude vectors.")
  }

  lat_step <- median(diff(lat))
  lon_step <- median(diff(lon))

  if (lat_step < 0) {
    lat <- rev(lat)
    pm <- pm[rev(seq_len(nrow(pm))), ]
    lat_step <- abs(lat_step)
  }
  if (lon_step < 0) {
    lon <- rev(lon)
    pm <- pm[, rev(seq_len(ncol(pm)))]
    lon_step <- abs(lon_step)
  }

  if (!is.finite(lat_step) || !is.finite(lon_step) || lat_step <= 0 || lon_step <= 0) {
    stop("Invalid PM2.5 grid spacing detected.")
  }

  list(
    lat = lat,
    lon = lon,
    pm = pm,
    lat_min = min(lat),
    lat_max = max(lat),
    lon_min = min(lon),
    lon_max = max(lon),
    lat_step = lat_step,
    lon_step = lon_step,
    scale_factor_default = scale_factor_default
  )
}


#' Lookup PM2.5 values at given coordinates
#'
#' @param lat_vals Latitude vector
#' @param lon_vals Longitude vector
#' @param grid List returned by load_pm25_grid()
#' @param method Matching method: "nearest", "floor", "ceiling", "bilinear"
#' @param scale_factor Divide PM2.5 values by this factor (default: grid-specific)
#' @param round_digits Optional rounding digits for final values (default: 1)
#' @param round_mode Rounding mode: "even" (R default) or "half_up"
#' @return Numeric vector of PM2.5 values (scaled and rounded)
pm25_lookup <- function(lat_vals,
                        lon_vals,
                        grid,
                        method = "nearest",
                        scale_factor = NULL,
                        round_digits = 1,
                        round_mode = "even") {
  method <- match.arg(method, c("nearest", "floor", "ceiling", "bilinear"))
  round_mode <- match.arg(round_mode, c("even", "half_up"))
  if (is.null(scale_factor)) {
    scale_factor <- grid$scale_factor_default
  }

  lat_vals <- as.numeric(lat_vals)
  lon_vals <- as.numeric(lon_vals)

  n <- length(lat_vals)
  out <- rep(NA_real_, n)

  valid <- !is.na(lat_vals) & !is.na(lon_vals)
  if (!any(valid)) {
    return(out)
  }

  lat_v <- lat_vals[valid]
  lon_v <- lon_vals[valid]

  lat_pos <- (lat_v - grid$lat_min) / grid$lat_step + 1
  lon_pos <- (lon_v - grid$lon_min) / grid$lon_step + 1

  if (method == "bilinear") {
    i0 <- floor(lat_pos)
    j0 <- floor(lon_pos)
    i1 <- i0 + 1
    j1 <- j0 + 1

    in_bounds <- i0 >= 1 & j0 >= 1 &
      i1 <= length(grid$lat) & j1 <= length(grid$lon)

    if (any(in_bounds)) {
      idx <- which(in_bounds)
      i0b <- i0[idx]
      j0b <- j0[idx]
      i1b <- i1[idx]
      j1b <- j1[idx]

      f_lat <- lat_pos[idx] - i0b
      f_lon <- lon_pos[idx] - j0b

      v00 <- grid$pm[cbind(i0b, j0b)]
      v10 <- grid$pm[cbind(i1b, j0b)]
      v01 <- grid$pm[cbind(i0b, j1b)]
      v11 <- grid$pm[cbind(i1b, j1b)]

      out_sub <- (1 - f_lat) * (1 - f_lon) * v00 +
        f_lat * (1 - f_lon) * v10 +
        (1 - f_lat) * f_lon * v01 +
        f_lat * f_lon * v11

      out[valid][idx] <- out_sub
    }
  } else {
    idx_lat <- switch(
      method,
      nearest = round(lat_pos),
      floor = floor(lat_pos),
      ceiling = ceiling(lat_pos)
    )
    idx_lon <- switch(
      method,
      nearest = round(lon_pos),
      floor = floor(lon_pos),
      ceiling = ceiling(lon_pos)
    )

    in_bounds <- idx_lat >= 1 & idx_lat <= length(grid$lat) &
      idx_lon >= 1 & idx_lon <= length(grid$lon)

    if (any(in_bounds)) {
      idx <- which(in_bounds)
      out_sub <- grid$pm[cbind(idx_lat[idx], idx_lon[idx])]
      out[valid][idx] <- out_sub
    }
  }

  out <- out / scale_factor
  if (!is.null(round_digits)) {
    if (round_mode == "even") {
      out <- round(out, round_digits)
    } else {
      factor <- 10 ^ round_digits
      out <- sign(out) * floor(abs(out) * factor + 0.5) / factor
    }
  }

  out
}


#' Merge PM2.5 values into a property dataframe
#'
#' @param df Data frame containing property coordinates
#' @param lat_col Latitude column name (default: "lat")
#' @param lon_col Longitude column name (default: "long")
#' @param pm25_path Path to PM2.5 grid file (.nc or .h5)
#' @param pm25_col Output column name (default: "PM25_Rec")
#' @param method Matching method for grid lookup
#' @param scale_factor Divide PM2.5 values by this factor (default: 1 for NetCDF, 100 for HDF5)
#' @param round_digits Optional rounding digits for final values (default: 1)
#' @param round_mode Rounding mode: "even" (R default) or "half_up"
#' @param grid Optional pre-loaded PM2.5 grid (from load_pm25_grid)
#' @return Data frame with added PM2.5 column
merge_pm25 <- function(df,
                       lat_col = "lat",
                       lon_col = "long",
                       pm25_path = "Data/Non_HDS_Data/PM2_5/V5.NA.04.02/V5NA04.02.HybridPM25.NorthAmerica.2012001-2012364.nc",
                       pm25_col = "PM25_Rec",
                       method = "nearest",
                       scale_factor = NULL,
                       round_digits = 1,
                       round_mode = "even",
                       grid = NULL) {
  if (!lat_col %in% names(df)) {
    stop(paste("Latitude column", lat_col, "not found in data frame"))
  }
  if (!lon_col %in% names(df)) {
    stop(paste("Longitude column", lon_col, "not found in data frame"))
  }

  if (is.null(grid)) {
    cat("Loading PM2.5 grid...\n")
    grid <- load_pm25_grid(pm25_path)
  }

  if (is.null(scale_factor)) {
    scale_factor <- grid$scale_factor_default
  }

  cat(sprintf("Matching PM2.5 for %d observations...\n", nrow(df)))

  pm25_vals <- pm25_lookup(
    lat_vals = df[[lat_col]],
    lon_vals = df[[lon_col]],
    grid = grid,
    method = method,
    scale_factor = scale_factor,
    round_digits = round_digits,
    round_mode = round_mode
  )

  df[[pm25_col]] <- pm25_vals

  n_matched <- sum(!is.na(pm25_vals))
  match_rate <- if (nrow(df) > 0) 100 * n_matched / nrow(df) else NA_real_

  cat(sprintf("  %s matched: %d / %d (%.1f%%)\n",
              pm25_col, n_matched, nrow(df), match_rate))
  cat(sprintf("Summary of %s:\n", pm25_col))
  print(summary(df[[pm25_col]]))

  df
}
