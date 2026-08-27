get_stationary_events <- function(
  lon, # x
  lat, # y
  time, # t
  r1,
  min_size,
  min_staying_time,
  max_time_between,
  distance_metric = "euclidean"
) {
  vec_len <- length(lon)
  checkmate::assert_numeric(lon, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(lat, len = vec_len, any.missing = FALSE)
  if (length(time) > 0) {
    checkmate::assert_numeric(time, len = vec_len, any.missing = FALSE)
  } else {
    time <- numeric(0)
  }
  # Python Infostop r1 > 0
  checkmate::assert_number(r1, lower = 0, finite = TRUE)
  # Python Infostop min_size > 1
  checkmate::assert_int(min_size, lower = 2)
  # Python Infostop min_staying_time > 0
  checkmate::assert_number(min_staying_time, lower = 0, finite = TRUE)
  # Python Infostop max_time_between > 0
  checkmate::assert_number(max_time_between, lower = 0, finite = TRUE)
  distance_metric <- match.arg(distance_metric, c("haversine", "euclidean"))

  if (distance_metric == "haversine") {
    checkmate::assert_numeric(lon, lower = -180, upper = 180)
    checkmate::assert_numeric(lat, lower = -90, upper = 90)
  }

  .get_stationary_events_cpp(
    lon,
    lat,
    time,
    r1,
    as.integer(min_size),
    min_staying_time,
    max_time_between, # at C++ it is named max_staying_time
    distance_metric
  )
}


identify_stops_internal <- function(
  lon, # x
  lat, # y
  time, # t
  ids = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean")
) {
  distance_metric <- match.arg(distance_metric)
  min_size <- as.integer(min_size)
  stop_ids <- rep.int(NA_integer_, length(time))
  if (is.null(ids)) {
    res <- get_stationary_events(
      lon,
      lat,
      time,
      r1 = r1,
      min_size = min_size,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      distance_metric = distance_metric
    )
    stop_ids <- unname(refine_labels(res$event_map))
  } else {
    names(stop_ids) <- ids
    unique_ids <- unique(ids)
    for (uid in unique_ids) {
      idx <- which(uid == ids)
      res <- get_stationary_events(
        lon[idx],
        lat[idx],
        time[idx],
        r1 = r1,
        min_size = min_size,
        min_staying_time = min_staying_time,
        max_time_between = max_time_between,
        distance_metric = distance_metric
      )
      stop_ids[idx] <- refine_labels(res$event_map)
    }
  }
  stop_ids
}


add_stop_ids <- function(data, event_map, stop_id_col = "stop_id") {
  class_input <- class(data)
  ids <- make_unique_id(data[[get_id_column(data)]])
  uids <- unique(ids)
  data[[stop_id_col]] <- NA_integer_
  for (i in seq_along(event_map)) {
    data[[stop_id_col]][ids %in% uids[i]] <- refine_labels(event_map[[i]])
  }
  class(data) <- class_input
  data
}


#' Identify stationary periods from coordinate vectors
#'
#' Identify stops in time-ordered coordinate data. A stop is a sequence of
#' points that remains within a spatial radius for a minimum duration.
#'
#' @param x a numeric vector of x-coordinates in a Cartesian coordinate system.
#' @param y a numeric vector of y-coordinates in a Cartesian coordinate system.
#' @param longitude a numeric vector of longitude coordinates in decimal degrees.
#' @param latitude a numeric vector of latitude coordinates in decimal degrees.
#' @param t an optional numeric vector, or an object inheriting from `POSIXt`
#'   or `Date`, containing the timestamps corresponding to the coordinates.
#'   The default is `NULL`.
#' @param r1 a numeric giving the maximum distance between time-consecutive
#'   points to label them as stationary. Higher values result in more points
#'   being considered stationary.
#' @param min_size an integer giving the minimum number of points required to
#'   consider a group stationary.
#' @param min_staying_time a numeric giving the minimum duration in seconds
#'   required to constitute a stop. Only relevant if timestamps are provided.
#' @param max_time_between a numeric giving the maximum duration in seconds
#'   between consecutive points to consider them part of the same stop. Only
#'   relevant if timestamps are provided.
#'
#' @return A list with `stop_events`, the median coordinates of detected stops,
#'   and `event_map`, the stop label assigned to each input point. Non-stop
#'   points have label `-1`.
#'
#' @export
#' @rdname identify_stops_xyt
identify_stops_xyt <- function(
  x,
  y,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
) {
  stops <- get_stationary_events(
    x,
    y,
    t,
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = "euclidean"
  )
  stops
}

#' @export
#' @rdname identify_stops_xyt
identify_stops_longlatt <- function(
  longitude,
  latitude,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
) {
  checkmate::assert_numeric(longitude, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(
    latitude,
    len = length(longitude),
    any.missing = FALSE
  )
  checkmate::assert_numeric(
    t,
    len = length(longitude),
    any.missing = FALSE,
    null.ok = TRUE
  )
  trackframe:::assert_latlon(latitude, longitude)
  # infostop python program expects lat long, not long lat
  stops <- get_stationary_events(
    longitude,
    latitude,
    t,
    r1 = r1,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    distance_metric = "haversine"
  )
  colnames(stops[["stop_events"]]) <- c("lon", "lat")
  stops
}


#' Identify stops in a trajectory
#'
#' Identify stationary periods in time-ordered coordinate data. A stop is a
#' sequence of points that remains within a spatial radius for a minimum
#' duration. Stop labels are added to the input data.
#'
#' @param data a `trackframe`, `sf`, `sftrack`, `move2`, or data frame.
#' @param r1 a numeric giving the maximum distance between time-consecutive
#'   points to label them as stationary. Higher values result in more points
#'   being considered stationary.
#' @param min_size an integer giving the minimum number of points required to
#'   consider a group stationary.
#' @param min_staying_time a numeric giving the minimum duration in seconds
#'   required to constitute a stop.
#' @param max_time_between a numeric giving the maximum duration in seconds
#'   between consecutive points to consider them part of the same stop.
#' @param stop_id_col a character string specifying the name of the new
#'   column to which the detected stop labels are assigned. The default is
#'   `"stop_id"`.
#' @param ... additional arguments passed when coercing a data frame to a
#'   `trackframe`.
#'
#' @return `data` with a new column containing the detected stop labels.
#'   `NA` identifies points that are not assigned to a stop.
#'
#' @examples
#' data("path_trackframe", package = "trackframe")
#' stops <- identify_stops(path_trackframe)
#' head(stops[["stop_id"]])
#'
#' data("path_data_frame", package = "trackframe")
#' stops_df <- identify_stops(path_data_frame, crs = NA)
#'
#' if (requireNamespace("sftrack", quietly = TRUE)) {
#' data("path_sftrack", package = "trackframe")
#' stops_sftrack <- identify_stops(path_sftrack)
#' }
#'
#' if (requireNamespace("move2", quietly = TRUE)) {
#' data("path_move2", package = "trackframe")
#' stops_move2 <- identify_stops(path_move2)
#' }
#'
#' @export
#' @rdname identify_stops
identify_stops <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
) {
  UseMethod("identify_stops")
}


#' @param time_col a character string specifying the column name of the time column.
#'   If no column is specified, the `time_col` is tried to be matched by possible names provided
#'   in `tf_options("time_col")`. In case of multiple matches, the first match is chosen.
#' @param easting_col a character string specifying the column name of the easting column.
#'   If no column is specified, the `easting_col` is tried to be matched by possible names
#'   provided in `tf_options("easting_col")`. In case of multiple matches,
#'   the first match is chosen.
#' @param northing_col a character string specifying the column name of the northing column.
#'   If no column is specified, the `northing_col` is tried to be matched by possible names
#'   provided in `tf_options("northing_col")`. In case of multiple matches,
#'   the first match is chosen.
#' @param id_col optional character vector specifying identifier column names.
#'   If no column is specified, the `id_col` is tried to be matched by possible names provided
#'   in `tf_options("id_col")`. In case of multiple matches, the first match is chosen.
#' @param crs required integer or charactor string identifying coordinate reference system.
#'   Use NA for non-georeferenced cartesian coordinate systems.
#' @export
#' @rdname identify_stops
identify_stops.data.frame <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  crs = NULL,
  ...
) {
  is_longlat <- sf::st_is_longlat(crs)
  if (isTRUE(is_longlat)) {
    stop(paste(
      "Geographic coordinates not supported with non-sf data frame.",
      "Recommend doing one of the following: 1) use identify_stops_longlatt",
      "2) provide an sftrack or move2 object",
      "3) project coordinates"
    ))
  }
  tf <- as.trackframe(
    data = data,
    time_col = time_col,
    easting_col = easting_col,
    northing_col = northing_col,
    id_col = id_col,
    crs = crs
  )

  if (!is.null(trackframe::id(tf)) && length(trackframe::unique_ids(tf)) > 1) {
    stop(
      "Multiple tracks are not supported. Please use infostop() instead in case of multiple tracks."
    )
  }
  result <- trackframe::tf_backtransform(identify_stops.trackframe(
    data = tf,
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    stop_id_col = stop_id_col
  ))
  names(result)[is.na(names(result))] <- stop_id_col
  result
}


#' @export
#' @rdname identify_stops
identify_stops.trackframe <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
) {
  ids <- id(data)
  idx <- if (is.null(ids)) order(time(data)) else order(ids, time(data))
  stop_ids <- identify_stops_internal(
    easting(data)[idx],
    northing(data)[idx],
    time(data)[idx],
    if (is.null(ids)) ids else ids[idx],
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = "euclidean"
  )
  data[[stop_id_col]] <- NA_integer_
  data[[stop_id_col]][idx] <- unname(stop_ids)
  data
}


#
#  Used for move2 and sftrack
#
#' @export
#' @rdname identify_stops
identify_stops.sf <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
) {
  is_longlat <- sf::st_is_longlat(data)
  if (isTRUE(is_longlat)) {
    ids <- get_ids_sf(data)
    time <- get_time_sf(data)
    idx <- if (is.null(ids)) order(time) else order(ids, time)

    # use custom coords function to identify lat and long
    coords <- trackframe:::st_coordinates_lat_lon(data)
    stop_ids <- identify_stops_internal(
      lon = coords[idx, 2],
      lat = coords[idx, 1],
      time = as.numeric(time)[idx],
      ids = if (is.null(ids)) ids else ids[idx],
      r1 = r1,
      min_size = min_size,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      distance_metric = 'haversine'
    )
    data[[stop_id_col]][idx] <- unname(stop_ids)
    data
  } else {
    identify_stops.trackframe(
      # NOTE: as.trackframe sort=TRUE by default.
      as.trackframe(data, ...),
      r1 = r1,
      min_size = min_size,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      stop_id_col = stop_id_col
    )
  }
}
