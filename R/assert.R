assert_crs_euclidean <- function(crs, na_ok = FALSE) {
  if (is.na(crs$input)) {
    if (isTRUE(na_ok)) {
      return(invisible(NULL))
    }
    stop("no crs information available.")
  }
  # check easting / northing
  crs_value <- as.integer(gsub("\\D", "", crs$input))
  if (!(crs_value >= 32600 && crs_value <= 32760)) {
    stop(sprintf(
      "distance_metric euclidean does not match with crs %s. %s",
      crs$input,
      "Choose distance_metric = 'haversine' instead."
    ))
  }
}


assert_crs_haversine <- function(crs, na_ok = FALSE) {
  if (is.na(crs$input)) {
    if (isTRUE(na_ok)) {
      return(invisible(NULL))
    }
    stop("no crs information available.")
  }
  # check easting / northing
  crs_value <- as.integer(gsub("\\D", "", crs$input))
  if (crs_value >= 32600 && crs_value <= 32760) {
    stop(sprintf(
      "distance_metric haversine does not match with crs %s. %s",
      crs$input,
      "Choose distance_metric = 'euclidean' instead."
    ))
  }
}


assert_lonlat <- function(lon, lat, emsg = NULL) {
  if (any(abs(lon) > 180, na.rm = TRUE) || any(abs(lat) > 90, na.rm = TRUE)) {
    default_msg <- "coordinates do not seem to be lon/lat"
    stop(emsg %||% default_msg)
  }
}
