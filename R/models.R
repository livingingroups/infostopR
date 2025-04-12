#' @importFrom checkmate assert check_list check_character check_logical
#' @importFrom reticulate import conda_create conda_install conda_list miniconda_path py_eval virtualenv_list virtualenv_create virtualenv_install
#' @importFrom stats runif
NULL


#' Detect Stop Locations in Mobility Data
#'
#' This function creates an Infostop model to infer stop-location labels from mobility trace.
#' The algorithm works by first identifying stationary events in the trace, then clustering
#' these events into stop locations using a network-based approach. Dynamic (moving) points are labeled -1.
#'
#' @param data A numeric matrix with 2 or 3 columns. The first two columns must contain spatial
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

  makeActiveBinding('labels', function() as.integer(env$model$labels[[1]]), env)

  . = env$model$fit_predict(data)
  class(env) <- "Infostop"

  return(env)
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
#'
#' @return A numeric vector of cluster labels for each input point. Points labeled -1 are considered non-stationary.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- rtravel_path(100, format = "matrix")
#' labels <- spatial_infomap(data, r2 = 15)
#' unique(labels)
#' }
#' @export
spatial_infomap <- function(data,
                            r2 = 10,
                            label_singleton = TRUE,
                            min_spacial_resolution = 0,
                            distance_metric = c("haversine", "euclidean"),
                            weighted = FALSE,
                            weight_exponent = 1,
                            verbose = FALSE) {
  check_infostop_initialized()

  checkmate::assert_matrix(data, "numeric", any.missing = FALSE, min.cols = 2, max.cols = 3)
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

  env$labels <- as.integer(env$model$fit_predict(data))
  
  class(env) <- "SpatialInfomap"
  return(env)
}


#' @noRd
#' @export
print.SpatialInfomap <- function(x, ...) {
  writeLines("SpatialInfomap object")
  writeLines("  - labels")
}

