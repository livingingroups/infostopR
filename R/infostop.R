

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
    checkmate::assert_matrix(data, "numeric", any.missing = FALSE, min.cols = 2, max.cols = 3)
  }
  checkmate::assert_numeric(r1, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(r2, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  checkmate::assert_logical(label_singleton, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(min_staying_time, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(max_time_between, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(min_size, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(min_spacial_resolution, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(weighted, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(weight_exponent, lower = 0, len = 1, any.missing = FALSE)
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
    makeActiveBinding("labels", function() lapply(env$model$labels, refine_labels), env)
  } else {
    makeActiveBinding("labels", function() refine_labels(env$model$labels[[1]]), env)
  }

  . <- env$model$fit_predict(data)  # nolint: object_usage_linter
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
#' @param x a numeric vector of x-coordinates (easting for distance_metric euclidean - latitude for
#'   distance_metric "haversine").
#' @param y a numeric vector of y-coordinates (northing for distance_metric euclidean - longitude for
#'   distance_metric "haversine").
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
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates) or
#'   'euclidean' (for Cartesian coordinates).
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
#' data("path_matrix", package = "trackframe")
#' infostop_xyt <- infostop_xyt(
#'   x = path_matrix[, "latitude"],
#'   y = path_matrix[, "longitude"],
#'   t = path_matrix[, "time"],
#'   distance_metric = "haversine"
#'  )
#' }
#' @export
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
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
  check_infostop_initialized()

  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(time, len = length(x), any.missing = FALSE)
  distance_metric <- match.arg(distance_metric)

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
    distance_metric = distance_metric,
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
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates) or
#'   'euclidean' (for Cartesian coordinates).
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
#'  data("path_matrix", package = "trackframe")
#'  tf <- as.trackframe(path_matrix, crs_input = 4326)
#'  infostop_tf <- infostop(tf, distance_metric = "euclidean")
#'
#'  # with data.frame
#'
#'  # coerce to trackframe
#'  data("path_data_frame", package = "trackframe")
#'  tf <- as.trackframe(path_data_frame)
#'  infostop_df1 <- infostop(data = tf, distance_metric = "euclidean")
#'
#'  # or use infostop.data.frame method with col specification
#'  infostop_df2 <- infostop(path_data_frame,
#'     distance_metric = "euclidean",
#'                          time_col = "time",
#'                          easting_col = "longitude",
#'                          northing_col = "latitude",
#'     id_col = "id"
#'   )
#'  # or use automated col guessing if applicable
#'  infostop_df3 <- infostop(path_data_frame, distance_metric = "euclidean")
#'
#'  # with sftrack
#'  data("path_sftrack", package = "trackframe")
#'  class(path_sftrack)
#'  infostop_sftrack <- infostop(path_sftrack, distance_metric = "haversine")
#'
#'  # with move2
#'  data("path_move2", package = "trackframe")
#'  class(path_move2)
#'  infostop_move2 <- infostop(path_move2, distance_metric = "haversine")
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
  distance_metric = c("haversine", "euclidean"),
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
  distance_metric = "euclidean",
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"), #FIXME: more general name, could also be lon
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
  checkmate::assert_choice(time_col, colnames(data), null.ok = FALSE)
  checkmate::assert_character(time_col, len = 1, null.ok = FALSE)
  checkmate::assert_numeric(data[[time_col]])

  easting_col <- guesses[["easting_col"]][1]
  checkmate::assert_choice(easting_col, colnames(data), null.ok = FALSE)
  checkmate::assert_character(easting_col, len = 1, null.ok = FALSE)

  northing_col <- guesses[["northing_col"]][1]
  checkmate::assert_choice(northing_col, colnames(data), null.ok = FALSE)
  checkmate::assert_character(northing_col, len = 1, null.ok = FALSE)

  id_col <- guesses[["id_col"]][1]
  if (is.na(id_col)) {
    id_col <- NULL
  }
  checkmate::assert_choice(id_col, colnames(data), null.ok = TRUE)
  checkmate::assert_character(id_col, len = 1, null.ok = TRUE)

  ids <- if (is.null(id_col)) NULL else data[[id_col]]

  coord_cols <- if (distance_metric == "euclidean") {
    c(easting_col, northing_col)
  } else {
    c(northing_col, easting_col)
  }
  data <- cbind(as.matrix(data[, coord_cols]), data[[time_col]])

  # split data in case of multiple ids
  if (!is.null(ids) && length(unique(ids)) > 1) {
    data <- lapply(unname(split(seq_along(ids), ids)), function(i) data[i, ])
  }

  infostop_internal(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
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
  distance_metric = "euclidean",
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  if (distance_metric != "euclidean") {
    stop(
      "Only distance_metric = 'euclidean' is available for objects of class trackframe"
    )
  }
  # create data input of infostop_internal from track.frame
  id_col <- attr(data, "id")
  ids <- if (is.null(id_col)) NULL else data[[id_col]]

  data[[attr(data, "time")]] <- as.integer(data[[attr(data, "time")]])
  cols <- c(attr(data, "easting"), attr(data, "northing"), attr(data, "time"))
  data <- as.matrix(data[, cols])

  if (!is.null(ids) && length(unique(ids)) > 1) {
    data <- lapply(unname(split(seq_len(NROW(data)), f = ids)), function(i) data[i, ])
  }

  infostop_internal(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @rdname infostop
infostop.move2 <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  distance_metric = NULL,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  attr(data, "time_col") <- attr(data, "time_column")
  infostop.sftrack(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )

}


guess_distance_metric <- function(crs) {
  if (!grepl("EPSG", crs$input)) {
    return(NULL)
  }
  crs_value <- as.integer(gsub("EPSG:", "", crs$input))
  # check easting / northing
  if (crs_value >= 32600 && crs_value <= 32760) {
    "euclidean"
  } else {
    "haversine"
  }
}


#' @export
#' @rdname infostop
infostop.sftrack <- function(
  data,
  r1 = 10,
  r2 = 10,
  label_singleton = TRUE,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  distance_metric = NULL,
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  id_col <- get_id_column(data)
  if (length(id_col) && !is.atomic(data[[id_col]])) {
    new_id_col <- "sft_group_id"
    data[[new_id_col]] <- make_unique_id(data[[id_col]])
    attr(data, "group_names") <- attr(data[[id_col]], "active_group")
    id_col <- new_id_col
  }

  ids <- if (is.null(id_col)) NULL else data[[id_col]]

  crs <- st_crs(data)
  distance_metric <- distance_metric %||% guess_distance_metric(crs)
  if (is.null(distance_metric)) {
    stop(
      "Please, specify distance_metric ('haversine' or 'euclidean'). No crs provided in sf object."
    )
  }

  # Check if distance_metric fits to crs
  if (distance_metric == "haversine") {
    x_y <- st_coordinates(data[[attr(data, "sf_column")]])
    assert_lonlat(x_y, 1)
    assert_crs_haversine(crs, na_ok = TRUE)
  } else {
    assert_crs_euclidean(crs, na_ok = TRUE)
  }

  # transform from sftrack
  data <- cbind(
    st_coordinates_lat_lon(data[[attr(data, "sf_column")]]),
    as.integer(data[[attr(data, "time_col")]])
  )

  # split data in case of multiple ids
  if (!is.null(ids) && length(unique(ids)) > 1) {
    data <- lapply(unname(split(seq_along(ids), ids)), function(i) data[i, ])
  }

  infostop_internal(
    data = data,
    r1 = r1,
    r2 = r2,
    label_singleton = label_singleton,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
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
  distance_metric = NULL, #"euclidean", #c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  stop("no applicable method for 'infostop' applied to an object of class c('double', 'numeric').
       use infostop_xyt() instead")
}

#' @noRd
#' @export
print.Infostop <- function(x, ...) {
  writeLines("Infostop object")
  writeLines("  - compute_label_medians()")
  writeLines("  - predict(data)")
  writeLines("  - labels")
}
