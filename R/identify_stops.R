

identify_stops_internal <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean")  
) {
  check_infostop_initialized()
  checkmate::assert_numeric(r1, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  checkmate::assert_integerish(min_staying_time, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(max_time_between, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(min_size, lower = 0, len = 1, any.missing = FALSE)

  distance_metric <- match.arg(distance_metric)
  min_size <- as.integer(min_size)

  py_identify_stops <- rpy("identify_stops")
  ret <- py_identify_stops(data, r1, min_size, min_staying_time, max_time_between, distance_metric)
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
#' @param x a numeric vector of x-coordinates (easting/longitude).
#' @param y a numeric vector of y-coordinates (northing/latitude).
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
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates) or
#'   'euclidean' (for Cartesian coordinates).
#' @param ... other arguments passed to `as.trackframe()`
#' @export
#' @rdname identify_stops
identify_stops_xyt <- function(
  x,
  y,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean")
) {
  check_infostop_initialized()

  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(t, len = length(x), any.missing = FALSE, null.ok = TRUE)
  distance_metric <- match.arg(distance_metric)
  if (distance_metric == "haversine") {
    assert_lonlat(x, y)
  }
  data <- cbind(x = x, y = y, t = t)
  stops <- identify_stops_internal(
    data = data,
    r1 = r1,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    distance_metric = distance_metric
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
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates)
#'   or 'euclidean' (for Cartesian coordinates).
#' @param stop_id_col A character string specifying the name of the column to be used for
#'   the stop identifiers. Default is "stop_id".
#' @param ... other arguments passed to `as.trackframe()`
#' @examples
#' library(trackframe)
#' data("path_trackframe", package = "trackframe")
#' stops <- identify_stops(data = path_trackframe,
#'                     distance_metric = "euclidean")
#'
#' # data.frame
#' data("path_data_frame", package = "trackframe")
#' tf <- as.trackframe(path_data_frame)
#' stops <- identify_stops(data = tf,
#'                     distance_metric = "euclidean")
#'
#' # with sftrack
#' data("path_sftrack", package = "trackframe")
#' class(path_sftrack)
#' stops_sftrack <- identify_stops(path_sftrack, distance_metric = "haversine")
#'
#' # with move2
#' data("path_move2", package = "trackframe")
#' class(path_move2)
#' stops_move2 <- identify_stops(path_move2, distance_metric = "haversine")
#'
#' @export
#' @rdname identify_stops
identify_stops <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean"),
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
#' @export
#' @rdname identify_stops
identify_stops.data.frame <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = "euclidean",
  stop_id_col = "stop_id",
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  ...
) {
  #columns guessing
  guesses <- guess_all_cols(
    col_names = colnames(data),
    time_col_candidates = time_col,
    easting_col_candidates = easting_col,
    northing_col_candidates = northing_col,
    id_col_candidates = id_col
  )

  getNamespace("trackframe")$warn_if_guess_ambiguous(data, guesses) #FIXME: export in trackframe

  time_col <- guesses[["time_col"]][1]
  assert_choice(time_col, colnames(data), null.ok = FALSE)
  assert_character(time_col, len = 1, null.ok = FALSE)
  assert_numeric(data[[time_col]])

  easting_col <- guesses[["easting_col"]][1]
  assert_choice(easting_col, colnames(data), null.ok = FALSE)
  assert_character(easting_col, len = 1, null.ok = FALSE)

  northing_col <- guesses[["northing_col"]][1]
  assert_choice(northing_col, colnames(data), null.ok = FALSE)
  assert_character(northing_col, len = 1, null.ok = FALSE)

  id_col <- guesses[["id_col"]][1]
  if (is.na(id_col)) {
    id_col <- NULL
  }
  assert_choice(id_col, colnames(data), null.ok = TRUE)
  assert_character(id_col, len = 1, null.ok = TRUE)

  ids <- if (is.null(id_col)) NULL else data[[id_col]]

  if (!is.null(ids) && length(unique(ids)) > 1) {
    stop(
      "Multiple tracks are not supported. Please use infostop() instead in case of multiple tracks."
    )
  }

  data <- cbind(as.matrix(data[, c(easting_col, northing_col)]), data[[time_col]])

  stops <- identify_stops_internal(
    data = data,
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = distance_metric
  )
  add_stop_ids(data, stops$event_map, stop_id_col = stop_id_col)
}


#' @export
#' @rdname identify_stops
identify_stops.trackframe <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = "euclidean",
  stop_id_col = "stop_id",
  ...
) {
  if (distance_metric != "euclidean") {
    stop(
      "Only distance_metric = 'euclidean' is available for objects of class trackframe"
    )
  }
  stops <- identify_stops_internal(
    data = as_infostop(data),
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = distance_metric
  )
  add_stop_ids(data, stops$event_map, stop_id_col = stop_id_col)
}


#' @export
#' @rdname identify_stops
identify_stops.move2 <- function(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  distance_metric = c("haversine", "euclidean"),
  stop_id_col = "stop_id",
  ...
) {
  stops <- identify_stops_internal(
    data = as_infostop(data),
    r1 = r1,
    min_size = min_size,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    distance_metric = distance_metric
  )
  add_stop_ids(data, stops$event_map, stop_id_col = stop_id_col)
}


#' @export
#' @rdname identify_stops
identify_stops.sftrack <- identify_stops.move2
