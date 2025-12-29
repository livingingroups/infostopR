identify_stops_internal <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean")
) {
  check_infostop_initialized()
  checkmate::assert_numeric(
    r1,
    lower = 0,
    len = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    min_staying_time,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    max_time_between,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    min_size,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )

  distance_metric <- match.arg(distance_metric)
  min_size <- as.integer(min_size)

  py_identify_stops <- rpy("identify_stops")
  ret <- py_identify_stops(
    data,
    r1,
    min_size,
    min_staying_time,
    max_time_between,
    distance_metric
  )
  names(ret) <- c("stop_events", "event_map")
  ret
}


add_stop_ids <- function(data, event_map, stop_id_col = "stop_id") {
  ids <- make_unique_id(data[[get_id_column(data)]])
  uids <- unique(ids)
  data[[stop_id_col]] <- NA_integer_
  for (i in seq_along(event_map)) {
    data[[stop_id_col]][ids %in% uids[i]] <- refine_labels(event_map[[i]])
  }
  data
}


#' Find based on distance and time threshold
#'
#' @param x a numeric vector of x-coordinates in cartesian coordinate system
#'   (e.g. projected coordinates).
#' @param y a numeric vector of y-coordinates in cartesian coordinate system
#'   (e.g. projected coordinates).
#' @param longitude numeric vector of longitude coordinates
#' @param latitude numeric vector of latitude coordinates
#' @param t a vecor inheriting from \code{numeric} or \code{POSIXt} or \code{Date}
#'        containing the timestamps corresponding to the x and y coordinates.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points to label
#'   them as stationary. Higher values will result in more points being considered stationary.
#' @param min_size An integer giving the minimum number of points required to consider a group
#'   stationary.
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute
#'   a stop. Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive
#'   points to consider them part of the same stop. Only relevant if timestamps are provided.
#' @export
#' @rdname identify_stops
identify_stops_xyt <- function(
  x,
  y,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
) {
  check_infostop_initialized()

  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(
    t,
    len = length(x),
    any.missing = FALSE,
    null.ok = TRUE
  )
  data <- cbind(x = x, y = y, t = t)
  stops <- identify_stops_internal(
    data = data,
    r1 = r1,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    distance_metric = "euclidean"
  )
  stops
}

#' @export
#' @rdname identify_stops
identify_stops_longlatt <- function(
  longitude,
  latitude,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
) {
  check_infostop_initialized()

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
  assert_lonlat(longitude, latitude)
  # infostop python program expects lat long, not long lat
  data <- cbind(x = latitude, y = longitude, t = t)
  stops <- identify_stops_internal(
    data = data,
    r1 = r1,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    distance_metric = "haversine"
  )
  stops
}


#' Find based on distance and time threshold
#'
#' @param data A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial coordinates.
#'   Column 3 is optional and represents time.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points
#'   to label them as stationary. Higher values will result in more points being considered
#'   stationary.
#' @param min_size An integer giving the minimum number of points required to consider a group
#'   stationary.
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute
#'   a stop. Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive
#'   points to consider them part of the same stop. Only relevant if timestamps are provided.
#' @param stop_id_col A character string specifying the name of the column to be used for
#'   the stop identifiers. Default is "stop_id".
#' @param ... other arguments passed to `as.trackframe()`
#' @examples
#' library(trackframe)
#' data("path_trackframe", package = "trackframe")
#' stops <- identify_stops(data = path_trackframe)
#'
#' # data.frame
#' data("path_data_frame", package = "trackframe")
#' tf <- as.trackframe(path_data_frame, crs = NA)
#' stops <- identify_stops(data = tf)
#'
#' # with sftrack
#' data("path_sftrack", package = "trackframe")
#' class(path_sftrack)
#' stops_sftrack <- identify_stops(path_sftrack)
#'
#' # with move2
#' data("path_move2", package = "trackframe")
#' class(path_move2)
#' stops_move2 <- identify_stops(path_move2)
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
  tf <- as.trackframe(data, time_col, easting_col, northing_col, id_col, crs)

  if (!is.null(trackframe::id(tf)) && length(trackframe::unique_ids(tf)) > 1) {
    stop(
      "Multiple tracks are not supported. Please use infostop() instead in case of multiple tracks."
    )
  }
  trackframe::tf_backtransform(identify_stops.trackframe(
    data = tf,
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between
  ))
}


#' @export
#' @importFrom trackframe northing easting id
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
  stops <- identify_stops_internal(
    data = split_mat_by_id(
      cbind(
        easting(data),
        northing(data),
        as.integer(time(data))
      ),
      id(data)
    ),
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = "euclidean"
  )
  add_stop_ids(data, stops$event_map, stop_id_col = stop_id_col)
}

#' @importFrom trackframe id
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
    # use trackframe package to identify id and time columns
    # easting/northing columns of tf are not meaningful
    data_no_crs <- data
    sf::st_crs(data_no_crs) <- sf::st_crs(NA)
    tf <- as.trackframe(data_no_crs, sort = FALSE)
    ids <- if (is.null(id(tf))) '' else id(tf)

    # reorder
    idx <- order(ids, time(tf))
    data <- data[idx, ]
    tf <- tf[idx, ]

    # use custom coords function to identify lat and long
    coords <- st_coordinates_lat_lon(data)

    # put together
    stops <- identify_stops_internal(
      data = split_mat_by_id(
        as.matrix(data.frame(
          # infostop python program expects lat long, not long lat
          x = coords[, 1],
          y = coords[, 2],
          time = as.integer(time(tf))
        )),
        ids = if (is.null(id(tf))) '' else id(tf)
      ),
      r1 = r1,
      min_size = min_size,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      distance_metric = 'haversine'
    )
    add_stop_ids(data, stops$event_map, stop_id_col = stop_id_col)
  } else {
    identify_stops.trackframe(
      as.trackframe(data, ...),
      r1 = r1,
      min_size = min_size,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      stop_id_col = stop_id_col
    )
  }
}
