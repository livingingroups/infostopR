aggregate_stop_centers <- function(x, y, ids, stop_ids, coord_cols, stop_id_col = "stop_id") {
  df <- data.frame(
    id = if (is.null(ids)) "" else make_unique_id(ids),
    coord_x = x,
    coord_y = y,
    stop_id = stop_ids
  )
  names(df) <- c("id", coord_cols, stop_id_col)
  df <- df[!is.na(df[[stop_id_col]]), ]

  out <- aggregate(df[, coord_cols], by = df[, c("id", stop_id_col)], FUN = median)
  out <- out[order(match(out$id, unique(df$id)), out[[stop_id_col]]), ]
  rownames(out) <- NULL
  out[, c("id", coord_cols, stop_id_col)]
}

aggregate_site_centers <- function(
  x,
  y,
  ids,
  stop_ids,
  site_ids,
  coord_cols,
  stop_id_col = "stop_id",
  site_id_col = "site_id"
) {
  df <- data.frame(
    id = if (is.null(ids)) "" else make_unique_id(ids),
    coord_x = x,
    coord_y = y,
    stop_id = stop_ids,
    site_id = site_ids
  )
  names(df) <- c("id", coord_cols, stop_id_col, site_id_col)
  df <- df[!is.na(df[[stop_id_col]]) & !is.na(df[[site_id_col]]), ]

  # To reproduce the Python package we have to first get the medians
  # from the stops and than from the stop medians aggregate to get
  # the site medians.
  stop_df <- aggregate(
    df[, coord_cols],
    by = df[, c("id", stop_id_col, site_id_col)],
    FUN = median
  )
  out <- aggregate(stop_df[, coord_cols], by = stop_df[, site_id_col, drop = FALSE], FUN = median)
  out <- out[order(out[[site_id_col]]), ]
  rownames(out) <- NULL
  out[, c(coord_cols, site_id_col)]
}

#' Calculate stop centers
#'
#' Summarise each detected stop by the median coordinate of its points.
#'
#' @param data a `trackframe`, `sftrack`, or `move2` object with stop labels.
#' @param stop_id_col a string giving the name of the column containing stop ids.
#'
#' @return a data frame with track id, center coordinates, and stop id.
#' @export
stop_centers <- function(data, stop_id_col = "stop_id") {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(stop_id_col)
  UseMethod("stop_centers")
}

#' @export
#' @rdname stop_centers
stop_centers.trackframe <- function(data, stop_id_col = "stop_id") {
  coord_cols <- c(easting_col(data), northing_col(data))
  checkmate::assert_names(colnames(data), must.include = c(id_col(data), coord_cols, stop_id_col))
  aggregate_stop_centers(
    x = easting(data),
    y = northing(data),
    ids = id(data),
    stop_ids = data[[stop_id_col]],
    coord_cols = coord_cols,
    stop_id_col = stop_id_col
  )
}

.stop_centers_sf <- function(data, stop_id_col = "stop_id") {
  checkmate::assert_names(colnames(data), must.include = c(get_id_column(data), stop_id_col))
  lat_lon <- st_coordinates_lat_lon(data)
  aggregate_stop_centers(
    x = lat_lon[, 2L],
    y = lat_lon[, 1L],
    ids = get_ids_sf(data),
    stop_ids = data[[stop_id_col]],
    coord_cols = c("lon", "lat"),
    stop_id_col = stop_id_col
  )
}

#' @export
#' @rdname stop_centers
stop_centers.sftrack <- function(data, stop_id_col = "stop_id") {
  .stop_centers_sf(data, stop_id_col = stop_id_col)
}

#' @export
#' @rdname stop_centers
stop_centers.move2 <- function(data, stop_id_col = "stop_id") {
  .stop_centers_sf(data, stop_id_col = stop_id_col)
}

#' Calculate site centers
#'
#' Summarise each detected site by first computing stop centers and then taking
#' the median of those stop centers per site.
#'
#' @param data a `trackframe`, `sftrack`, or `move2` object with stop and site labels.
#' @param site_id_col a string giving the name of the column containing site ids.
#' @param stop_id_col a string giving the name of the column containing stop ids.
#'
#' @return a data frame with center coordinates and site id.
#' @export
site_centers <- function(data, site_id_col = "site_id", stop_id_col = "stop_id") {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(site_id_col)
  checkmate::assert_string(stop_id_col)
  checkmate::assert_names(colnames(data), must.include = c(stop_id_col, site_id_col))
  UseMethod("site_centers")
}

#' @export
#' @rdname site_centers
site_centers.trackframe <- function(data, site_id_col = "site_id", stop_id_col = "stop_id") {
  coord_cols <- c(easting_col(data), northing_col(data))
  required_cols <- c(id_col(data), coord_cols, stop_id_col, site_id_col)
  checkmate::assert_names(colnames(data), must.include = required_cols)
  aggregate_site_centers(
    x = easting(data),
    y = northing(data),
    ids = id(data),
    stop_ids = data[[stop_id_col]],
    site_ids = data[[site_id_col]],
    coord_cols = coord_cols,
    stop_id_col = stop_id_col,
    site_id_col = site_id_col
  )
}

.site_centers_sf <- function(data, site_id_col = "site_id", stop_id_col = "stop_id") {
  required_cols <- c(get_id_column(data), stop_id_col, site_id_col)
  checkmate::assert_names(colnames(data), must.include = required_cols)
  lat_lon <- st_coordinates_lat_lon(data)
  aggregate_site_centers(
    x = lat_lon[, 2L],
    y = lat_lon[, 1L],
    ids = get_ids_sf(data),
    stop_ids = data[[stop_id_col]],
    site_ids = data[[site_id_col]],
    coord_cols = c("lon", "lat"),
    stop_id_col = stop_id_col,
    site_id_col = site_id_col
  )
}

#' @export
#' @rdname site_centers
site_centers.sftrack <- function(data, site_id_col = "site_id", stop_id_col = "stop_id") {
  .site_centers_sf(data, site_id_col = site_id_col, stop_id_col = stop_id_col)
}

#' @export
#' @rdname site_centers
site_centers.move2 <- function(data, site_id_col = "site_id", stop_id_col = "stop_id") {
  .site_centers_sf(data, site_id_col = site_id_col, stop_id_col = stop_id_col)
}
