
identify_sites_internal <- function(
  stop_events,
  event_maps,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1
) {
  check_infostop_initialized()

  checkmate::assert_numeric(r2, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(label_singleton, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(min_spacial_resolution, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(weighted, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(weight_exponent, lower = 0, len = 1, any.missing = FALSE)

  distance_metric <- match.arg(distance_metric)

  pyfun <- rpy("identify_sites")
  ret <- pyfun(
    stop_events,
    event_maps,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent
  )
  ret
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
#' @param stop_id_col A character string specifying the name of the column to be used for
#'   the stop identifiers. Default is "stop_id".
#' @param site_id_col A character string specifying the name of the column to be used for
#'   the site identifiers. Default is "site_id".
#' @param ... other arguments passed to `as.trackframe()`
#'
#' @return A numeric vector of cluster labels for each input point. Points labeled -1 are
#'   considered non-stationary.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' dat <- infostop:::example_data_move2()
#' stops <- identify_stops(dat, r1 = 100, min_staying_time = 300,
#'                         max_time_between = 86400, min_size = 2,
#'                         distance_metric = "haversine")
#' head(stops[["stop_id"]], 30)
#' clusters <- identify_sites(stops, r2 = 50)
#' head(clusters[["site_id"]], 30)
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
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  UseMethod("identify_sites")
}


add_site_ids <- function(data, site_map, site_id_col = "site_id") {
  ids <- make_unique_id(data[[get_id_column(data)]])
  uids <- unique(ids)
  data[[site_id_col]] <- NA_integer_
  for (i in seq_along(site_map)) {
    data[[site_id_col]][ids %in% uids[i]] <- refine_labels(site_map[[i]])
  }
  data
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
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  if (distance_metric != "euclidean") {
    stop(
      "Only distance_metric = 'euclidean' is available for objects of class trackframe"
    )
  }
  stops <- as_stop_locations(data, stop_id_col = stop_id_col)
  site_map <- identify_sites_internal(
    stops$stop_events,
    stops$event_maps,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent
  )
  add_site_ids(data, site_map = site_map, site_id_col = site_id_col)
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
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  stops <- as_stop_locations(data, stop_id_col = stop_id_col)
  site_map <- identify_sites_internal(
    stops$stop_events,
    stops$event_maps,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent
  )
  add_site_ids(data, site_map = site_map, site_id_col = site_id_col)
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
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  stops <- as_stop_locations(data, stop_id_col = stop_id_col)
  site_map <- identify_sites_internal(
    stops$stop_events,
    stops$event_maps,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent
  )
  add_site_ids(data, site_map = site_map, site_id_col = site_id_col)
}
