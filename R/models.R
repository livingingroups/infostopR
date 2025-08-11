# TODO
# - S3 methods for infostop, find_stops, spatial_infomap, plot_map (transform trackframe to lonlat)
#   transform to data and then run .*_internal
# - vignette
# - tests
# - examples
# - multiple IDs?
# - check as.integer in time input
# - create test.data for data.frame, trackframe, move2, sftrack



#' Detect Stop Locations in Mobility Data
#'
#' This function creates an Infostop model to infer stop-location labels from mobility trace.
#' The algorithm works by first identifying stationary events in the trace, then clustering
#' these events into stop locations using a network-based approach. Dynamic (moving) points are labeled -1.
#'
#' @param data Either an object of class \code{trackframe} or a numeric matrix with 2 or 3 columns. The first two columns must contain spatial
#'   coordinates (latitude, longitude). The optional third column contains timestamps.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points to label them as stationary.
#'   Higher values will result in more points being considered stationary.
#' @param r2 A numeric vector giving the maximum distance between stationary points to form an edge in the network.
#'   Higher values will connect more distant stationary points, potentially merging stop locations.
#' @param label_singleton A logical, if \code{TRUE}, give stationary locations that were only visited once their own label.
#'   If FALSE, label them as non-stationary (-1).
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute a stop.
#'   Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive points
#'   to consider them part of the same stop. Only relevant if timestamps are provided.
#' @param min_size An integer giving the minimum number of points required to consider a group stationary.
#' @param min_spacial_resolution A numeric giving the minimum difference allowed between points before they
#'   are considered the same points. Useful for dealing with GPS jitter.
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates) or 'euclidean'
#'   (for Cartesian coordinates).
#' @param weighted A logical, if \code{TRUE}, weight edges in the network by distance, giving more importance
#'   to closer points.
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
#' 1. It first identifies stationary events by grouping consecutive points that are close in space and time.
#' 2. Then it clusters these stationary events into stop locations using a network-based approach.
#' 
#' The main parameters that control the algorithm's behavior are:
#' 
#' \describe{
#'   \item{\code{r1}}{The critical radius (in the same units as your coordinates) that determines
#'     whether consecutive points are part of the same stationary event. For geographic coordinates,
#'     this is typically in meters. Larger values will identify more points as stationary.}
#'   \item{\code{r2}}{The maximum distance between stationary events to consider them connected in the
#'     network. Larger values will result in more connections and potentially fewer, larger clusters.}
#'   \item{\code{min_staying_time}}{The minimum time (in seconds) required for a sequence of points to
#'     be considered a stationary event. Increase this value to ignore brief stops.}
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
#' if (is_infostop_initialized()) {
#' data <- rtravel_path(100, format = "matrix")
#' model <- infostop(data, r1 = 10, r2 = 10)
#' model$labels
#' model$compute_label_medians()
#' }
#' @export
#' @rdname infostop
infostop <- function(data,
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
                     ...) {
  UseMethod("infostop")
}


refine_labels <- function(labels) {
  if (length(labels) == 0) {
    return(labels)
  }
  labels <- as.integer(labels)
  labels[labels == -1L] <- NA_integer_
  return(labels)
}


infostop_internal <- function(data,
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
                     verbose = FALSE) {
  check_infostop_initialized()

  checkmate::assert_matrix(data, "numeric", any.missing = FALSE, min.cols = 2, max.cols = 3)
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
  
  env$compute_label_medians <- function() {
    env$model$compute_label_medians()[[1]]
  }

  makeActiveBinding('labels', function() refine_labels(env$model$labels[[1]]), env)

  . = env$model$fit_predict(data)
  class(env) <- "Infostop"

  return(env)
}


#' @export
#' @rdname infostop
infostop.matrix <- function(data,
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
                            ...) {
  infostop_internal(data = data, r1 = r1, r2 = r2, label_singleton = label_singleton,
                    min_staying_time = min_staying_time, max_time_between = max_time_between,
                    min_size = min_size, min_spacial_resolution = min_spacial_resolution,
                    distance_metric = distance_metric, weighted = weighted,
                    weight_exponent = weight_exponent, verbose = verbose)
}


#' @export
#' @rdname infostop
infostop.data.frame <- function(data,
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
                                ...) {
  infostop(data = as.trackframe(data, ...), r1 = r1, r2 = r2, label_singleton = label_singleton,
           min_staying_time = min_staying_time, max_time_between = max_time_between,
           min_size = min_size, min_spacial_resolution = min_spacial_resolution,
           distance_metric = distance_metric, weighted = weighted,
           weight_exponent = weight_exponent, verbose = verbose)
}


#' @export
#' @rdname infostop
infostop.trackframe <- function(data,
                                r1 = 10,
                                r2 = 10,
                                label_singleton = TRUE,
                                min_staying_time = 300L,
                                max_time_between = 86400L,
                                min_size = 2L,
                                min_spacial_resolution = 0,
                                distance_metric = "euclidean", #c("haversine", "euclidean"),
                                weighted = FALSE,
                                weight_exponent = 1,
                                verbose = FALSE,
                                ...) {

  stopifnot("Only distance_metric = 'euclidean' is available for objects of class trackframe" = distance_metric == "euclidean")
  # transform from track.frame
  data[[attr(data, "time")]] <- as.integer(data[[attr(data, "time")]])
  data <- as.matrix(data[, c(attr(data, "easting"), attr(data, "northing"), attr(data, "time"))])
  infostop_internal(data = data, r1 = r1, r2 = r2, label_singleton = label_singleton,
                    min_staying_time = min_staying_time, max_time_between = max_time_between,
                    min_size = min_size, min_spacial_resolution = min_spacial_resolution,
                    distance_metric = distance_metric, weighted = weighted,
                    weight_exponent = weight_exponent, verbose = verbose)
}


#' @export
#' @rdname infostop
infostop.move2 <- function(data,
                           r1 = 10,
                           r2 = 10,
                           label_singleton = TRUE,
                           min_staying_time = 300L,
                           max_time_between = 86400L,
                           min_size = 2L,
                           min_spacial_resolution = 0,
                           distance_metric = "euclidean", #c("haversine", "euclidean"),
                           weighted = FALSE,
                           weight_exponent = 1,
                           verbose = FALSE,
                           ...) {

  # transform from move2
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                as.integer(data[[attr(data, "time_column")]]))
  infostop_internal(data = data, r1 = r1, r2 = r2, label_singleton = label_singleton,
                    min_staying_time = min_staying_time, max_time_between = max_time_between,
                    min_size = min_size, min_spacial_resolution = min_spacial_resolution,
                    distance_metric = distance_metric, weighted = weighted,
                    weight_exponent = weight_exponent, verbose = verbose)
}


#' @export
#' @rdname infostop
infostop.sftrack <- function(data,
                             r1 = 10,
                             r2 = 10,
                             label_singleton = TRUE,
                             min_staying_time = 300L,
                             max_time_between = 86400L,
                             min_size = 2L,
                             min_spacial_resolution = 0,
                             distance_metric = "euclidean", #c("haversine", "euclidean"),
                             weighted = FALSE,
                             weight_exponent = 1,
                             verbose = FALSE,
                             ...) {
  #FIXME check if distance_metric fits to crs
  # transform from sftrack
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                       as.integer(data[[attr(data, "time_col")]]))
  infostop_internal(data = data, r1 = r1, r2 = r2, label_singleton = label_singleton,
                    min_staying_time = min_staying_time, max_time_between = max_time_between,
                    min_size = min_size, min_spacial_resolution = min_spacial_resolution,
                    distance_metric = distance_metric, weighted = weighted,
                    weight_exponent = weight_exponent, verbose = verbose)
}


#' @noRd
#' @export
print.Infostop <- function(x, ...) {
  writeLines("Infostop object")
  writeLines("  - compute_label_medians()")
  writeLines("  - compute_label_area()")
  writeLines("  - compute_label_counts()")
  writeLines("  - predict(data)")
  writeLines("  - labels")
}

#' Find based on distance and time threshold
#'
#' @param data A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial coordinates.
#'   Column 3 is optional and represents time.
#' @param r1 A numeric vector giving the maximum distance between time-consecutive points to label them as stationary.
#'   Higher values will result in more points being considered stationary.
#' @param min_staying_time An integer giving the minimum duration (in seconds) that can constitute a stop.
#'   Only relevant if timestamps are provided in the data.
#' @param max_time_between An integer giving the maximum duration (in seconds) between consecutive points
#'   to consider them part of the same stop. Only relevant if timestamps are provided.
#' @param min_size An integer giving the minimum number of points required to consider a group stationary.
#' @param distance_metric A character string, either 'haversine' (for geographic coordinates) or 'euclidean'
#'   (for Cartesian coordinates).
#' @param ... other arguments passed to `as.trackframe()`
#' @export
#' @rdname find_stops
find_stops <- function(data,
                       r1 = 10,
                       min_staying_time = 300L,
                       max_time_between = 86400L,
                       min_size = 2L,
                       distance_metric = c("haversine", "euclidean"),
                       ...) {
  UseMethod("find_stops")
}


find_stops_internal <- function(data,
                                r1 = 10,
                                min_staying_time = 300L,
                                max_time_between = 86400L,
                                min_size = 2L,
                                distance_metric = c("haversine", "euclidean")) {
  check_infostop_initialized()
  
  # checkmate::assert_matrix(data, "numeric", any.missing = FALSE, min.cols = 2, max.cols = 3)
  checkmate::assert_numeric(r1, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  checkmate::assert_integerish(min_staying_time, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(max_time_between, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(min_size, lower = 0, len = 1, any.missing = FALSE)

  distance_metric <- match.arg(distance_metric)
  stops <- py_cpputils$get_stationary_events(
    data,
    r1,
    as.integer(min_size),
    min_staying_time,
    max_time_between,
    distance_metric
  )
  stops[[1]] <- as.matrix(do.call(rbind, stops[[1]]))
  names(stops) <- c("stop_events", "event_map")
  
  if(abs(max(data[, 1:2])) <= 180 & sum(stops[["event_map"]] != 0) == 0) {
    warning("Seems that coordinate reference system and distance_metric do not coincide.")
  }

  stops[[2]] <- refine_labels(stops[[2]])
  
  return(stops)
}


#' @export
#' @rdname find_stops
find_stops.matrix <- function(data,
                              r1 = 10,
                              min_staying_time = 300L,
                              max_time_between = 86400L,
                              min_size = 2L,
                              distance_metric = "euclidean",
                              ...) {
  find_stops_internal(data = data,
                      r1 = r1,
                      min_staying_time = min_staying_time,
                      max_time_between = max_time_between,
                      min_size = min_size,
                      distance_metric = distance_metric)
}


#' @export
#' @rdname find_stops
find_stops.data.frame <- function(data,
                                r1 = 10,
                                min_staying_time = 300L,
                                max_time_between = 86400L,
                                min_size = 2L,
                                distance_metric = "euclidean",
                                ...) {
  find_stops(data = as.trackframe(data, ...),
                              r1 = r1,
                              min_staying_time = min_staying_time,
                              max_time_between = max_time_between,
                              min_size = min_size,
                              distance_metric = distance_metric)
}


#' @export
#' @rdname find_stops
find_stops.trackframe <- function(data,
                                 r1 = 10,
                                 min_staying_time = 300L,
                                 max_time_between = 86400L,
                                 min_size = 2L,
                                 distance_metric = "euclidean",
                                 ...) {
  stopifnot("Only distance_metric = 'euclidean' is available for objects of class trackframe" = distance_metric == "euclidean")
  # transform from track.frame
  data[[attr(data, "time")]] <- as.integer(data[[attr(data, "time")]])
  data <- as.matrix(data[, c(attr(data, "easting"), attr(data, "northing"), attr(data, "time"))])
  find_stops_internal(data = data,
                      r1 = r1,
                      min_staying_time = min_staying_time,
                      max_time_between = max_time_between,
                      min_size = min_size,
                      distance_metric = distance_metric)
}


#' @export
#' @rdname find_stops
find_stops.move2 <- function(data,
                           r1 = 10,
                           min_staying_time = 300L,
                           max_time_between = 86400L,
                           min_size = 2L,
                           distance_metric = c("haversine", "euclidean"),
                           ...) {
  
  # transform from move2
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                as.integer(data[[attr(data, "time_column")]]))
  find_stops_internal(data = data,
                      r1 = r1,
                      min_staying_time = min_staying_time,
                      max_time_between = max_time_between,
                      min_size = min_size,
                      distance_metric = distance_metric)
}


#' @export
#' @rdname find_stops
find_stops.sftrack <- function(data,
                             r1 = 10,
                             min_staying_time = 300L,
                             max_time_between = 86400L,
                             min_size = 2L,
                             distance_metric = c("haversine", "euclidean"),
                             ...) {
  #FIXME check if distance_metric fits to crs
  # transform from sftrack
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                as.integer(data[[attr(data, "time_col")]]))
  find_stops_internal(data = data,
                      r1 = r1,
                      min_staying_time = min_staying_time,
                      max_time_between = max_time_between,
                      min_size = min_size,
                      distance_metric = distance_metric)
}


#' Revert the downsampling of \code{find\_stops}
#'
#' For performance reasons, \code{find\_stops} returns a downsampled version of the
#' stop events. This function helps to revert that downsampling by matching the
#' labels obtained from \code{spatial_infomap} to the original data points.
#'
#' @param labels Either a numeric vector of cluster labels or a SpatialInfomap object.
#'   If a SpatialInfomap object is provided, the labels will be extracted automatically.
#' @param event_map A numeric vector mapping each data point to its corresponding
#'   stationary event. Typically obtained from \code{find_stops()$event_map}.
#'
#' @return A numeric vector of the same length as nuber of rows in the data
#' given to \code{find_stops()}.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- infostop:::example_data()
#' stops <- find_stops(data, r1 = 100, min_staying_time = 300,
#'                     max_time_between = 86400, min_size = 2,
#'                     distance_metric = "haversine")
#' clusters <- spatial_infomap(stops$stop_events, r2 = 50)
#' two_step_labels <- match_labels(clusters, stops$event_map)
#'
#' one_step_labels <- infostop(data, r1 = 100, r2 = 50, min_staying_time = 300,
#'                             max_time_between = 86400, min_size = 2,
#'                             distance_metric = "haversine")
#'
#' cbind(one_step_labels$labels, two_step_labels)
#' }
#'
#' @seealso \code{\link{find_stops}}, \code{\link{spatial_infomap}}
#' @export
match_labels <- function(labels, event_map) {
    if (class(labels) == "SpatialInfomap") {
        labels <- labels$labels
    }
  labels[event_map + 1L]
}


#' Spatial Infomap Cluster a Collection of Points Using Infomap
#'
#' This function applies the SpatialInfomap algorithm to cluster a collection of points.
#' It directly returns the cluster labels rather than a model object.
#'
#' @param data A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial coordinates.
#'   Column 3 is optional and represents time.
#' @param r2 Numeric. Max distance between stationary points to form an edge.
#' @param label_singleton Logical. If TRUE, give stationary locations that were only visited once their own label.
#'   If FALSE, label them as non-stationary (-1).
#' @param min_spacial_resolution Numeric. The minimal difference allowed between points before they are considered the same points.
#' @param distance_metric Character. Either 'haversine' (for geo data) or 'euclidean'.
#' @param weighted Logical. Weight edges in the network representation by distance.
#' @param weight_exponent Numeric. Exponent used when weighting edges in the network.
#' @param verbose Logical. Print output during the fitting procedure.
#' @param ... other arguments passed to `as.trackframe()`
#'
#' @return A numeric vector of cluster labels for each input point. Points labeled -1 are considered non-stationary.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- infostop:::example_data()
#' stops <- find_stops(data, r1 = 100, min_staying_time = 300,
#'                     max_time_between = 86400, min_size = 2,
#'                     distance_metric = "haversine")
#' clusters <- spatial_infomap(stops$stop_events, r2 = 50)
#' labels <- match_labels(clusters, stops$event_map)
#' }
#' @export
#' @rdname spatial_infomap
spatial_infomap <- function(data,
                            r2 = 10,
                            label_singleton = TRUE,
                            min_spacial_resolution = 0,
                            distance_metric = c("haversine", "euclidean"),
                            weighted = FALSE,
                            weight_exponent = 1,
                            verbose = FALSE,
                            ...) {
  UseMethod("spatial_infomap")
}


spatial_infomap_internal <- function(data,
                                     r2 = 10,
                                     label_singleton = TRUE,
                                     min_spacial_resolution = 0,
                                     distance_metric = c("haversine", "euclidean"),
                                     weighted = FALSE,
                                     weight_exponent = 1,
                                     verbose = FALSE) {
  check_infostop_initialized()
  
  checkmate::assert_matrix(data, "numeric", any.missing = FALSE, min.cols = 2, max.cols = 2)
  checkmate::assert_numeric(r2, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(label_singleton, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(min_spacial_resolution, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(weighted, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(weight_exponent, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  
  distance_metric <- match.arg(distance_metric)
  
  env <- new.env(parent = emptyenv())
  env$model <- py_infostop$SpatialInfomap(
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )

  env$labels <- refine_labels(env$model$fit_predict(data))
  
  class(env) <- "SpatialInfomap"
  return(env)
}


#' @export
#' @rdname spatial_infomap
spatial_infomap.matrix <- function(data,
                                   r2 = 10,
                                   label_singleton = TRUE,
                                   min_spacial_resolution = 0,
                                   distance_metric = c("haversine", "euclidean"),
                                   weighted = FALSE,
                                   weight_exponent = 1,
                                   verbose = FALSE,
                                   ...) {
  spatial_infomap_internal(data,
                           r2 = r2,
                           label_singleton = label_singleton,
                           min_spacial_resolution = min_spacial_resolution,
                           distance_metric = distance_metric,
                           weighted = weighted,
                           weight_exponent = weight_exponent,
                           verbose = verbose)
}


#' @export
#' @rdname spatial_infomap
spatial_infomap.data.frame <- function(data,
                                       r2 = 10,
                                       label_singleton = TRUE,
                                       min_spacial_resolution = 0,
                                       distance_metric = c("haversine", "euclidean"),
                                       weighted = FALSE,
                                       weight_exponent = 1,
                                       verbose = FALSE,
                                       ...) {
  spatial_infomap(as.trackframe(data, ...),
                  r2 = r2,
                  label_singleton = label_singleton,
                  min_spacial_resolution = min_spacial_resolution,
                  distance_metric = distance_metric,
                  weighted = weighted,
                  weight_exponent = weight_exponent,
                  verbose = verbose)
}


#' @export
#' @rdname spatial_infomap
spatial_infomap.trackframe <- function(data,
                                        r2 = 10,
                                        label_singleton = TRUE,
                                        min_spacial_resolution = 0,
                                        distance_metric = "euclidean",
                                        weighted = FALSE,
                                        weight_exponent = 1,
                                        verbose = FALSE,
                                        ...) {
  stopifnot("Only distance_metric = 'euclidean' is available for objects of class trackframe" = distance_metric == "euclidean")
  # transform from track.frame
  data[[attr(data, "time")]] <- as.integer(data[[attr(data, "time")]])
  data <- as.matrix(data[, c(attr(data, "easting"), attr(data, "northing"), attr(data, "time"))])
  spatial_infomap_internal(data,
                           r2 = r2,
                           label_singleton = label_singleton,
                           min_spacial_resolution = min_spacial_resolution,
                           distance_metric = distance_metric,
                           weighted = weighted,
                           weight_exponent = weight_exponent,
                           verbose = verbose)
}


#' @export
#' @rdname spatial_infomap
spatial_infomap.move2 <- function(data,
                                  r2 = 10,
                                  label_singleton = TRUE,
                                  min_spacial_resolution = 0,
                                  distance_metric = c("haversine", "euclidean"),
                                  weighted = FALSE,
                                  weight_exponent = 1,
                                  verbose = FALSE,
                                  ...) {
  
  # transform from move2
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                as.integer(data[[attr(data, "time_column")]]))
  spatial_infomap_internal(data,
                           r2 = r2,
                           label_singleton = label_singleton,
                           min_spacial_resolution = min_spacial_resolution,
                           distance_metric = distance_metric,
                           weighted = weighted,
                           weight_exponent = weight_exponent,
                           verbose = verbose)
}


#' @export
#' @rdname spatial_infomap
spatial_infomap.sftrack <- function(data,
                                    r2 = 10,
                                    label_singleton = TRUE,
                                    min_spacial_resolution = 0,
                                    distance_metric = c("haversine", "euclidean"),
                                    weighted = FALSE,
                                    weight_exponent = 1,
                                    verbose = FALSE,
                                    ...) {
  #FIXME check if distance_metric fits to crs
  # transform from sftrack
  data <- cbind(st_coordinates(data[[attr(data, "sf_column")]]),
                as.integer(data[[attr(data, "time_col")]]))
  spatial_infomap_internal(data,
                           r2 = r2,
                           label_singleton = label_singleton,
                           min_spacial_resolution = min_spacial_resolution,
                           distance_metric = distance_metric,
                           weighted = weighted,
                           weight_exponent = weight_exponent,
                           verbose = verbose)
}


#' @noRd
#' @export
print.SpatialInfomap <- function(x, ...) {
  writeLines("SpatialInfomap object")
  writeLines("  - labels")
}

