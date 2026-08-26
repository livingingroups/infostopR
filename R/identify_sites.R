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

  coords <- lapply(stop_events, function(event) {
    event <- if (is.list(event)) do.call(rbind, event) else as.matrix(event)
    if (NROW(event) == 0L) {
      return(matrix(numeric(0), nrow = 0L, ncol = 2L))
    }
    event
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


#' Assign site labels to detected stops
#'
#' This is the second step of Infostop. It treats stop centers as nodes in a
#' spatial network, connects nodes within `r2`, and uses Infomap to identify
#' communities. A community is a site shared by the stops assigned to it.
#'
#' @param data a `trackframe`, `sf`, `sftrack`, or `move2` object containing
#'   stop labels.
#' @param r2 a numeric giving the maximum distance between stop centers in the
#'   same network neighborhood. It is measured in the coordinate units for
#'   projected data and in metres for geographic data.
#' @param label_singleton a logical. If `TRUE`, give stationary locations that
#'   were only visited once their own label. If `FALSE`, leave isolated stops as
#'   `NA`.
#' @param min_spacial_resolution a numeric giving the minimum spatial
#'   resolution. Points that round to the same coordinates at this resolution
#'   are considered the same point. The default is `0`.
#' @param weighted a logical. If `TRUE`, weight network edges by distance.
#' @param weight_exponent a numeric giving the exponent used for
#'   distance-based edge weights.
#' @param seed an integer passed as the random seed to
#'   \code{\link[infomap]{cluster_infomap}}. Defaults to `123L`.
#' @param stop_id_col a character string specifying the name of the column
#'   containing stop identifiers. The default is `"stop_id"`.
#' @param site_id_col a character string specifying the name of the new
#'   column to which the detected site labels are assigned. The default is
#'   `"site_id"`.
#' @param ... additional arguments passed when coercing data to a
#'   `trackframe`.
#'
#' @return `data` with a site-label column added. `NA` identifies points that
#'   are not assigned to a site.
#'
#' @references
#' Aslak, U. and Alessandretti, L. (2020). Infostop: Scalable stop-location
#' detection in multi-user mobility data. doi:10.48550/arXiv.2003.14370
#'
#' @examples
#' if (requireNamespace("trackframe", quietly = TRUE)) {
#'   data("path_trackframe", package = "trackframe")
#'   stops <- identify_stops(path_trackframe)
#'   sites <- identify_sites(stops)
#'   head(sites[["site_id"]])
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

    stops <- prep_stops(
      x = coords[idx, 2],
      y = coords[idx, 1],
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
