#' Spatial Infomap Cluster a Collection of Points Using Infomap
#'
#' This function applies the SpatialInfomap algorithm to cluster a collection of points.
#' It directly returns the cluster labels rather than a model object.
#'
#' @param easting a numeric vector of x-coordinates (easting).
#' @param northing a numeric vector of y-coordinates (northing).
#' @param r2 Numeric. Max distance between stationary points to form an edge.
#' @param label_singleton Logical. If TRUE, give stationary locations that were only visited once
#'   their own label. If FALSE, label them as non-stationary (-1).
#' @param min_spacial_resolution Numeric. The minimal difference allowed between points before they
#'   are considered the same points.
#' @param distance_metric Character. Either 'haversine' (for geo data) or 'euclidean'.
#' @param weighted Logical. Weight edges in the network representation by distance.
#' @param weight_exponent Numeric. Exponent used when weighting edges in the network.
#' @param verbose Logical. Print output during the fitting procedure.
#' @param ... other arguments passed to `as.trackframe()`
#'
#' @return A numeric vector of cluster labels for each input point. Points labeled -1 are
#'   considered non-stationary.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- infostop:::example_data()
#' stops <- identify_stops(data, r1 = 100, min_staying_time = 300,
#'                     max_time_between = 86400, min_size = 2,
#'                     distance_metric = "haversine")
#' clusters <- identify_sites(stops$stop_events, r2 = 50)
#' labels <- match_labels(clusters, stops$event_map)
#' }
#' @export
#' @rdname identify_sites
identify_sites_xyt <- function(
  easting,
  northing,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
  check_infostop_initialized()

  checkmate::assert_numeric(easting, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(northing, len = length(easting), any.missing = FALSE)
  distance_metric <- match.arg(distance_metric)
  data <- cbind(x = easting, y = northing)
  identify_sites_internal(
    data,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' Spatial Infomap Cluster a Collection of Points Using Infomap
#'
#' This function applies the SpatialInfomap algorithm to cluster a collection of points.
#' It directly returns the cluster labels rather than a model object.
#'
#' @param data A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial coordinates.
#'   Column 3 is optional and represents time.
#' @param r2 Numeric. Max distance between stationary points to form an edge.
#' @param label_singleton Logical. If TRUE, give stationary locations that were only visited
#'   once their own label. If FALSE, label them as non-stationary (-1).
#' @param min_spacial_resolution Numeric. The minimal difference allowed between points before
#'   they are considered the same points.
#' @param distance_metric Character. Either 'haversine' (for geo data) or 'euclidean'.
#' @param weighted Logical. Weight edges in the network representation by distance.
#' @param weight_exponent Numeric. Exponent used when weighting edges in the network.
#' @param verbose Logical. Print output during the fitting procedure.
#' @param ... other arguments passed to `as.trackframe()`
#'
#' @return A numeric vector of cluster labels for each input point. Points labeled -1 are
#'   considered non-stationary.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- infostop:::example_data()
#' stops <- identify_stops(data, r1 = 100, min_staying_time = 300,
#'                     max_time_between = 86400, min_size = 2,
#'                     distance_metric = "haversine")
#' clusters <- identify_sites(stops$stop_events, r2 = 50)
#' labels <- match_labels(clusters, stops$event_map)
#' }
#' @export
#' @rdname identify_sites
identify_sites <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  UseMethod("identify_sites")
}


identify_sites_internal <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE
) {
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
#' @rdname identify_sites
identify_sites.matrix <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  identify_sites_internal(
    data,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @rdname identify_sites
identify_sites.data.frame <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  identify_sites(
    as.trackframe(data, ...),
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @rdname identify_sites
identify_sites.trackframe <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
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
  # transform from track.frame
  data[[attr(data, "time")]] <- as.integer(data[[attr(data, "time")]])
  data <- as.matrix(data[, c(attr(data, "easting"), attr(data, "northing"), attr(data, "time"))])
  identify_sites_internal(
    data,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @rdname identify_sites
identify_sites.move2 <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  # transform from move2
  data <- cbind(
    st_coordinates(data[[attr(data, "sf_column")]]),
    as.integer(data[[attr(data, "time_column")]])
  )
  identify_sites_internal(
    data,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @export
#' @rdname identify_sites
identify_sites.sftrack <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  verbose = FALSE,
  ...
) {
  #FIXME check if distance_metric fits to crs
  # transform from sftrack
  data <- cbind(
    st_coordinates(data[[attr(data, "sf_column")]]),
    as.integer(data[[attr(data, "time_col")]])
  )
  identify_sites_internal(
    data,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    verbose = verbose
  )
}


#' @noRd
#' @export
print.SpatialInfomap <- function(x, ...) {
  writeLines("SpatialInfomap object")
  writeLines("  - labels")
}


#' Revert the downsampling of \code{find\_stops}
#'
#' For performance reasons, \code{find\_stops} returns a downsampled version of the
#' stop events. This function helps to revert that downsampling by matching the
#' labels obtained from \code{identify_sites} to the original data points.
#'
#' @param labels Either a numeric vector of cluster labels or a SpatialInfomap object.
#'   If a SpatialInfomap object is provided, the labels will be extracted automatically.
#' @param event_map A numeric vector mapping each data point to its corresponding
#'   stationary event. Typically obtained from \code{identify_stops()$event_map}.
#'
#' @return A numeric vector of the same length as nuber of rows in the data
#' given to \code{identify_stops()}.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data <- infostop:::example_data()
#' stops <- identify_stops(data, r1 = 100, min_staying_time = 300,
#'                     max_time_between = 86400, min_size = 2,
#'                     distance_metric = "haversine")
#' clusters <- identify_sites(stops$stop_events, r2 = 50)
#' two_step_labels <- match_labels(clusters, stops$event_map)
#'
#' one_step_labels <- infostop(data, r1 = 100, r2 = 50, min_staying_time = 300,
#'                             max_time_between = 86400, min_size = 2,
#'                             distance_metric = "haversine")
#'
#' all.equal(one_step_labels$labels, two_step_labels)
#' }
#'
#' @seealso \code{\link{identify_stops}}, \code{\link{identify_sites}}
#' @export
match_labels <- function(labels, event_map) {
  if (inherits(labels, "SpatialInfomap")) {
    labels <- labels$labels
  }
  labels[event_map + 1L]
}
