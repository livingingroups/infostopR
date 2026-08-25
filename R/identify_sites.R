identify_sites_internal <- function(
  stop_events,
  event_maps,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L
) {
  checkmate::assert_numeric(r2, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_logical(label_singleton, len = 1, any.missing = FALSE)
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

  distance_metric <- match.arg(distance_metric)

  coordinate_order <- if (distance_metric == "haversine") c(2L, 1L) else c(1L, 2L)
  coords <- lapply(stop_events, function(event) {
    event <- if (is.list(event)) do.call(rbind, event) else as.matrix(event)
    if (is.null(event) || nrow(event) == 0L) {
      return(matrix(numeric(0), nrow = 0L, ncol = 2L))
    }
    event[, coordinate_order, drop = FALSE]
  })
  n_per_id <- vapply(coords, nrow, integer(1))

  if (!any(n_per_id > 0L)) {
    site_map <- lapply(event_maps, function(event_map) {
      rep.int(-1L, length(event_map))
    })
  } else {
    site_labels <- cluster_centroids(
      do.call(rbind, coords),
      r2,
      distance_metric = distance_metric,
      weighted = weighted,
      weight_exponent = weight_exponent,
      label_singleton = label_singleton,
      min_spacial_resolution = min_spacial_resolution,
      seed = seed
    )

    offsets <- c(0L, cumsum(n_per_id))
    site_map <- lapply(seq_along(event_maps), function(i) {
      event_map <- as.integer(event_maps[[i]])
      if (n_per_id[i] == 0L) {
        return(rep.int(-1L, length(event_map)))
      }

      site_of_stop <- site_labels[seq.int(offsets[i] + 1L, offsets[i + 1L])]
      site_of_stop <- c(site_of_stop, -1L)
      index <- event_map + 1L
      index[event_map == -1L] <- length(site_of_stop)
      site_of_stop[index]
    })
  }

  all_labels <- refine_labels(unlist(site_map, use.names = FALSE))
  split(all_labels, rep(seq_along(site_map), lengths(site_map)))
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
#' @param weighted Logical. Weight edges in the network representation by distance.
#' @param weight_exponent Numeric. Exponent used when weighting edges in the network.
#' @param seed an integer passed as seed to \code{\link[infomap]{cluster_infomap}}
#'   (default is `123L`).
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
#' if (requireNamespace("trackframe", quietly = TRUE)) {
#'   data("path_trackframe", package = "trackframe")
#'   stops <- identify_stops(path_trackframe, r1 = 100, min_staying_time = 300,
#'                         max_time_between = 86400, min_size = 2)
#'   head(stops[["stop_id"]], 30)
#'   clusters <- identify_sites(stops, r2 = 50)
#'   head(clusters[["site_id"]], 30)
#' }
#' @export
#' @rdname identify_sites
identify_sites <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
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
    data[[site_id_col]][ids %in% uids[i]] <- site_map[[i]]
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
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  stops <- prep_stops(
    x = easting(data),
    y = northing(data),
    id = id(data),
    stop_id = data[[stop_id_col]]
  )
  site_map <- identify_sites_internal(
    stop_events = stops$stop_events,
    event_maps = stops$event_maps,
    r2 = r2,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution,
    distance_metric = "euclidean",
    weighted = weighted,
    weight_exponent = weight_exponent,
    seed = seed
  )
  add_site_ids(data, site_map = site_map, site_id_col = site_id_col)
}


#' @export
#' @importFrom stats time
#' @importFrom trackframe id
#' @rdname identify_sites
identify_sites.sf <- function(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
) {
  is_longlat <- sf::st_is_longlat(data)
  if (isTRUE(is_longlat)) {
    ids <- get_ids_sf(data)
    time <- get_time_sf(data)
    idx <- if (is.null(ids)) order(time) else order(ids, time)

    # use custom coords function to identify lat and long
    coords <- trackframe:::st_coordinates_lat_lon(data)

    # put together
    stops <- prep_stops(
      # infostop python program expects lat long, not long lat
      x = coords[idx, 1],
      y = coords[idx, 2],
      id = ids[idx],
      stop_id = data[[stop_id_col]][idx]
    )
    site_map <- identify_sites_internal(
      stops$stop_events,
      stops$event_maps,
      r2 = r2,
      label_singleton = label_singleton,
      min_spacial_resolution = min_spacial_resolution,
      distance_metric = "haversine",
      weighted = weighted,
      weight_exponent = weight_exponent,
      seed = seed
    )
    add_site_ids(data, site_map = site_map, site_id_col = site_id_col)
  } else {
    identify_sites(
      as.trackframe(data, ...),
      r2 = r2,
      label_singleton = label_singleton,
      min_spacial_resolution = min_spacial_resolution,
      weighted = weighted,
      weight_exponent = weight_exponent,
      seed = seed,
      stop_id_col = stop_id_col,
      site_id_col = site_id_col
    )
  }
}
