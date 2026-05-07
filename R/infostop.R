infostop_internal <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
  check_infostop_initialized()

  if (is.list(data)) {
    for (i in seq_along(data)) {
      checkmate::assert_matrix(
        data[[i]],
        "numeric",
        any.missing = FALSE,
        min.cols = 2,
        max.cols = 3
      )
    }
  } else {
    checkmate::assert_matrix(
      data,
      "numeric",
      any.missing = FALSE,
      min.cols = 2,
      max.cols = 3
    )
  }
  checkmate::assert_numeric(
    r1,
    lower = 0,
    len = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_numeric(
    r2,
    lower = 0,
    len = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_logical(label_singleton, len = 1, any.missing = FALSE)
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
  checkmate::assert_numeric(
    min_spacial_resolution,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_logical(weighted, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(
    weight_exponent,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)

  distance_metric <- match.arg(distance_metric)

  env <- new.env(parent = emptyenv())
  env$distance_metric <- distance_metric
  env$model <- py_infostop$Infostop(
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = as.integer(min_staying_time),
    max_time_between = as.integer(max_time_between),
    min_size = as.integer(min_size),
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )

  env$compute_label_medians <- function(simplify = TRUE) {
    label_medians <- env$model$compute_label_medians()
    if (isTRUE(simplify)) {
      if (length(label_medians) > 0) {
        label_medians <- do.call(rbind, label_medians)
      } else {
        label_medians <- matrix(numeric(), nrow = 0, ncol = 2)
      }
      if (env$distance_metric == "haversine") {
        colnames(label_medians) <- c("longitude", "latitude")
      } else {
        colnames(label_medians) <- c("x", "y")
      }
    }
    label_medians
  }

  if (is.list(data)) {
    makeActiveBinding(
      "labels",
      function() lapply(env$model$labels, refine_labels),
      env
    )
  } else {
    makeActiveBinding(
      "labels",
      function() refine_labels(env$model$labels[[1]]),
      env
    )
  }

  . <- env$model$fit_predict(data) # nolint: object_usage_linter
  class(env) <- "Infostop"

  return(env)
}


#' Detect Stop Locations in Mobility Data
#'
#' This function creates an Infostop model to infer stop-location labels from mobility trace.
#' The algorithm works by first identifying stationary events in the trace, then clustering these
#' events into stop locations using a network-based approach.
#' Dynamic (moving) points are labeled -1.
#'
#' @param x a numeric vector of x-coordinates in cartesian coordinate system
#'   (e.g. projected coordinates).
#' @param y a numeric vector of y-coordinates in cartesian coordinate system
#'   (e.g. projected coordinates).
#' @param longitude numeric vector of longitude coordinates
#' @param latitude numeric vector of latitude coordinates
#' @param time a vector inheriting from \code{numeric} (in seconds) or \code{POSIXt}
#'   or \code{Date} containing the timestamps corresponding to the x and y coordinates.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points to label
#'   them as stationary. Higher values will result in more points being considered stationary.
#' @param r2 A numeric vector giving the maximum distance between stationary points to form an edge
#'   in the network. Higher values will connect more distant stationary points,
#'   potentially merging stop locations.
#' @param label_singleton A logical, if \code{TRUE}, give stationary locations that
#'   were only visited once their own label. If FALSE, label them as non-stationary (-1).
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute a
#'   stop. Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive
#'   points to consider them part of the same stop. Only relevant if timestamps are provided.
#' @param min_size An integer giving the minimum number of points required to consider a group
#'   stationary.
#' @param min_spacial_resolution A numeric giving the minimum difference allowed between points
#'   before they are considered the same points. Useful for dealing with GPS jitter.
#' @param weighted A logical, if \code{TRUE}, weight edges in the network by distance, giving more
#'   importance to closer points.
#' @param weight_exponent A numeric, exponent used when weighting edges in the network.
#'   Higher values give more importance to distance.
#' @param verbose A logical, if \code{TRUE}, print progress information during computation.
#'
#' @return An Infostop model object with the following methods and properties:
#'   \itemize{
#'     \item \code{compute_label_medians()}
#'     \item \code{labels}
#'   }
#'
#' @details
#' The Infostop algorithm works in two main steps:
#'
#' 1. It first identifies stationary events by grouping consecutive points that are close in space
#'    and time.
#' 2. Then it clusters these stationary events into stop locations using a network-based approach.
#'
#' The main parameters that control the algorithm's behavior are:
#'
#' \describe{
#'   \item{\code{r1}}{The critical radius (in the same units as your coordinates) that determines
#'     whether consecutive points are part of the same stationary event. For geographic coordinates,
#'     this is typically in meters. Larger values will identify more points as stationary.}
#'   \item{\code{r2}}{The maximum distance between stationary events to consider them connected
#'     in the network. Larger values will result in more connections and potentially fewer,
#'     larger clusters.}
#'   \item{\code{min_staying_time}}{The minimum time (in seconds) required for a sequence of points
#'     to be considered a stationary event. Increase this value to ignore brief stops.}
#'   \item{\code{max_time_between}}{The maximum time gap (in seconds) between consecutive points to
#'     still consider them part of the same stationary event. Useful for handling missing data.}
#'   \item{\code{min_size}}{The minimum number of points required to form a stationary event.
#'     Increase this to filter out very short stops.}
#' }
#'
#' The returned object provides the following methods and properties:
#'
#'   \itemize{
#'     \item \code{compute_label_medians()} \cr
#'       Compute the median coordinates for each label. Returns a matrix where each row
#'       corresponds to a unique label and contains the median latitude and longitude.
#'     \item \code{labels} \cr
#'       Access the labels from the fitted model. Returns a vector of integer labels where
#'       each element corresponds to a point in the input data. Points labeled -1 are
#'       considered dynamic (not part of any stop location).
#'   }
#'
#' Objects of class \code{sftrack}, \code{move2} and \code{trackframe} are supported
#' in \code{\link{infostop}}.
#'
#' @seealso \code{\link{infostop}}
#'
#' @examples
#' if (is_infostop_initialized()) {
#'   data("path_matrix", package = "trackframe")
#'     model <- infostop_xyt(
#'     x = path_matrix[, "easting"],
#'     y = path_matrix[, "northing"],
#'     time = path_matrix[, "time"]
#'   )
#' }
#' @export
#' @rdname infostop_xyt
infostop_xyt <- function(
  x,
  y,
  time,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
  check_infostop_initialized()

  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(time, len = length(x), any.missing = FALSE)

  data <- cbind(x = x, y = y, t = time)
  infostop_internal(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = "euclidean",
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}

#' @rdname infostop_xyt
#' @export
infostop_lonlatt <- function(
  longitude,
  latitude,
  time,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
  check_infostop_initialized()

  assert_lonlat(longitude, latitude)
  checkmate::assert_numeric(time, len = length(longitude), any.missing = FALSE)

  # infostop python expects latlon
  data <- cbind(x = latitude, y = longitude, t = time)
  infostop_internal(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = "haversine",
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' Detect Stop Locations in Mobility Data
#'
#' This function creates an Infostop model to infer stop-location labels from mobility trace.
#' The algorithm works by first identifying stationary events in the trace, then clustering
#' these events into stop locations using a network-based approach.
#' Dynamic (moving) points are labeled -1.
#'
#' @param data Either an object of class \code{trackframe} or a numeric matrix with 2 or 3 columns.
#'   The first two columns must contain spatial coordinates (latitude, longitude).
#'   The optional third column contains timestamps.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points to label
#'   them as stationary. Higher values will result in more points being considered stationary.
#' @param r2 A numeric vector giving the maximum distance between stationary points to form an edge
#'   in the network. Higher values will connect more distant stationary points, potentially merging
#'   stop locations.
#' @param label_singleton A logical, if \code{TRUE}, give stationary locations that were only
#'   visited once their own label. If FALSE, label them as non-stationary (-1).
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute
#'   a stop. Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive
#'   points to consider them part of the same stop. Only relevant if timestamps are provided.
#' @param min_size An integer giving the minimum number of points required to consider a group
#'   stationary.
#' @param min_spacial_resolution A numeric giving the minimum difference allowed between points
#'   before they are considered the same points. Useful for dealing with GPS jitter.
#' @param weighted A logical, if \code{TRUE}, weight edges in the network by distance,
#'   giving more importance to closer points.
#' @param weight_exponent A numeric, exponent used when weighting edges in the network.
#'   Higher values give more importance to distance.
#' @param verbose A logical, if \code{TRUE}, print progress information during computation.
#' @param ... other arguments passed to `as.trackframe()`
#'
#' @return An Infostop model object with the following methods and properties:
#'   \itemize{
#'     \item \code{compute_label_medians()}
#'     \item \code{labels}
#'   }
#'
#' @details
#' The Infostop algorithm works in two main steps:
#'
#' 1. It first identifies stationary events by grouping consecutive points that are close in space
#'    and time.
#' 2. Then it clusters these stationary events into stop locations using a network-based approach.
#'
#' The main parameters that control the algorithm's behavior are:
#'
#' \describe{
#'   \item{\code{r1}}{The critical radius (in the same units as your coordinates) that determines
#'     whether consecutive points are part of the same stationary event. For geographic coordinates,
#'     this is typically in meters. Larger values will identify more points as stationary.}
#'   \item{\code{r2}}{The maximum distance between stationary events to consider them connected in
#'     the network. Larger values will result in more connections and potentially fewer,
#'     larger clusters.}
#'   \item{\code{min_staying_time}}{The minimum time (in seconds) required for a sequence of points
#'     to be considered a stationary event. Increase this value to ignore brief stops.}
#'   \item{\code{max_time_between}}{The maximum time gap (in seconds) between consecutive points to
#'     still consider them part of the same stationary event. Useful for handling missing data.}
#'   \item{\code{min_size}}{The minimum number of points required to form a stationary event.
#'     Increase this to filter out very short stops.}
#' }
#'
#' The returned object provides the following methods and properties:
#'
#'   \itemize{
#'     \item \code{compute_label_medians()} \cr
#'       Compute the median coordinates for each label. Returns a matrix where each row
#'       corresponds to a unique label and contains the median latitude and longitude.
#'     \item \code{labels} \cr
#'       Access the labels from the fitted model. Returns a vector of integer labels where
#'       each element corresponds to a point in the input data. Points labeled -1 are
#'       considered dynamic (not part of any stop location).
#'   }
#'
#' @examples
#' library(trackframe)
#' # with trackframe
#' if (is_infostop_initialized()) {
#'   data("path_matrix", package = "trackframe")
#'   tf <- as.trackframe(path_matrix, crs = NA)
#'   infostop_tf <- infostop(tf)
#'
#'   # with data.frame
#'
#'   # coerce to trackframe
#'   data("path_data_frame", package = "trackframe")
#'   tf <- as.trackframe(path_data_frame, crs = NA)
#'   infostop_df1 <- infostop(data = tf)
#'
#'   # or use infostop.data.frame method with col specification
#'   infostop_df2 <- infostop(path_data_frame,
#'     time_col = "time",
#'     easting_col = "easting",
#'     northing_col = "northing",
#'     id_col = "id",
#'     crs = NA
#'   )
#'   # or use automated col guessing if applicable
#'   infostop_df3 <- infostop(path_data_frame, crs = NA)
#'
#'   # with sftrack
#'   data("path_sftrack", package = "trackframe")
#'   class(path_sftrack)
#'   infostop_sftrack <- infostop(path_sftrack)
#'
#'   # with move2
#'   data("path_move2", package = "trackframe")
#'   class(path_move2)
#'   infostop_move2 <- infostop(path_move2)
#' }
#' @export
#' @rdname infostop
infostop <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  UseMethod("infostop")
}


#' @param time_col a character string specifying the column name of the time column. If no column is
#'   specified, the `time_col` is tried to be matched by possible names provided in
#'   `tf_options("time_col")`. In case of multiple matches, the first match is chosen.
#' @param easting_col a character string specifying the column name of the easting column. If no
#'   column is specified, the `easting_col` is tried to be matched by possible names provided in
#'   `tf_options("easting_col")`. In case of multiple matches, the first match is chosen.
#' @param northing_col a character string specifying the column name of the northing column. If no
#'   column is specified, the `northing_col` is tried to be matched by possible names provided in
#'   `tf_options("northing_col")`. In case of multiple matches, the first match is chosen.
#' @param id_col optional character vector specifying identifier column names. If no column is
#'   specified, the `id_col` is tried to be matched by possible names provided in
#'   `tf_options("id_col")`. In case of multiple matches, the first match is chosen.
#' @param crs required integer or charactor string identifying coordinate reference system.
#'   Use NA for non-georeferenced cartesian coordinate systems.
#' @export
#' @rdname infostop
infostop.data.frame <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"), #FIXME: more general name, could also be lon
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
  infostop.trackframe(
    data = as.trackframe(
      data,
      time_col,
      easting_col,
      northing_col,
      id_col,
      crs = crs
    ),
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @importFrom trackframe id easting_col northing_col time_col "time<-"
#' @rdname infostop
infostop.trackframe <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  data <- sort(data)
  # create data input of infostop_internal from track.frame
  ids <- id(data)

  time(data) <- as.integer(time(data))
  cols <- c(easting_col(data), northing_col(data), time_col(data))
  data <- as.matrix(data[, cols, with = FALSE])

  infostop_internal(
    data = split_mat_by_id(data, ids),
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = "euclidean",
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}

#' @importFrom stats time
#' @importFrom trackframe id
#' @export
#' @rdname infostop
infostop.sf <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
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
    infostop_internal(
      data = split_mat_by_id(
        as.matrix(data.frame(
          # infostop python program expects lat long, not long lat
          x = coords[, 1],
          y = coords[, 2],
          time = as.integer(time(tf))
        )),
        ids = if (is.null(id(tf))) "" else id(tf)
      ),
      r1 = r1,
      r2 = r2,
      label_singleton = label_singleton,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      min_size = min_size,
      min_spacial_resolution = min_spacial_resolution,
      distance_metric = "haversine",
      weighted = weighted,
      weight_exponent = weight_exponent,
      verbose = verbose
    )
  } else {
    infostop.trackframe(
      as.trackframe(data, ...),
      r1 = r1,
      r2 = r2,
      label_singleton = label_singleton,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      min_size = min_size,
      min_spacial_resolution = min_spacial_resolution,
      weighted = weighted,
      weight_exponent = weight_exponent,
      verbose = verbose
    )
  }
}


#' @export
#' @rdname infostop
infostop.numeric <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  stop(
    "no applicable method for 'infostop' applied to an object of class c('double', 'numeric').
       use infostop_xyt() instead"
  )
}

#' @noRd
#' @export
print.Infostop <- function(x, ...) {
  writeLines("Infostop object")
  writeLines("  - compute_label_medians()")
  writeLines("  - predict(data)")
  writeLines("  - labels")
}
