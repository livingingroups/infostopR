## Stage 2 of the Infostop pipeline: turn stationary-event centroids
## into reusable location ids via the mapequation Infomap R package on a
## radius-neighbour graph.
##
## Mirrors `infostop.utils.{query_neighbors,infomap_communities,label_network}`
## and `Infostop.fit_predict` lines 198-268 in the Python reference.

# Earth radius used by the Python reference and the local C++ haversine.
.EARTH_RADIUS_M <- 6371000


# ----- helpers --------------------------------------------------------------

#' Vectorised pairwise haversine distance
#'
#' Used internally by [query_neighbors_r2()]. The C++ kernel for
#' stage-1 clustering ships its own copy of this formula.
#'
#' @param lon1,lat1,lon2,lat2 numeric vectors of equal length, decimal
#'   degrees.
#' @param radius Earth radius in metres.
#'
#' @return numeric vector of distances in metres.
#'
#' @keywords internal
haversine_pairwise <- function(lon1, lat1, lon2, lat2,
                               radius = .EARTH_RADIUS_M) {
  d_lat <- (lat2 - lat1) * pi / 180
  d_lon <- (lon2 - lon1) * pi / 180
  lat1r <- lat1 * pi / 180
  lat2r <- lat2 * pi / 180
  a <- sin(d_lat / 2)^2 + sin(d_lon / 2)^2 * cos(lat1r) * cos(lat2r)
  2 * radius * asin(pmin(1, sqrt(a)))
}


#' Collapse duplicate event centroids
#'
#' Mirrors `np.unique(stat_coords, return_inverse=True, return_counts=True)`
#' from the Python reference, with an optional grid-snap controlled by
#' `min_spacial_resolution`. Unique centroids are ordered to preserve Python's
#' Infomap node order: Python sorts its centroid matrix lexicographically, which
#' is `(lat, lon)` for `"haversine"` (vs this package's `(lon, lat)`) and
#' `(x, y)` for `"euclidean"`.
#'
#' @param stat_coords a numeric matrix with columns `longitude`, `latitude`
#'   (one row per stage-1 event centroid).
#' @param min_spacial_resolution optional grid step. Coordinates are
#'   rounded to the nearest multiple before deduplication. `0` (default)
#'   disables.
#' @param distance_metric one of `"haversine"`, `"euclidean"`; selects the
#'   column order used to match Python's `np.unique` centroid sort.
#'
#' @return A list with components
#'   * `unique`: matrix of unique centroids (same column order as input).
#'   * `inverse`: integer vector of length `nrow(stat_coords)`, the row
#'     index in `unique` for each input row.
#'   * `counts`: integer vector of length `nrow(unique)`.
#'
#' @keywords internal
dedup_stat_coords <- function(stat_coords, min_spacial_resolution = 0,
                              distance_metric = c("haversine", "euclidean")) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_matrix(stat_coords, mode = "numeric", min.rows = 0L,
                           ncols = 2L)
  checkmate::assert_number(min_spacial_resolution, lower = 0)

  if (min_spacial_resolution > 0) {
    stat_coords <- round(stat_coords / min_spacial_resolution) *
      min_spacial_resolution
  }

  if (nrow(stat_coords) == 0L) {
    return(list(unique = stat_coords,
                inverse = integer(),
                counts = integer()))
  }

  key <- paste(stat_coords[, 1L], stat_coords[, 2L], sep = "\r")
  unique_idx <- which(!duplicated(key))
  # Match Python np.unique(axis = 0) ordering: Python sorts its centroid matrix
  # lexicographically by column. For "haversine" Python's matrix is (lat, lon)
  # while R's stat matrix is (lon, lat), so sort by R columns (2, 1); for
  # "euclidean" both are (x, y), so sort by (1, 2). Centroid order fixes the
  # Infomap node ids, so this is what keeps the partition numbering in step.
  sort_cols <- if (distance_metric == "haversine") c(2L, 1L) else c(1L, 2L)
  unique_idx <- unique_idx[do.call(order, list(
    stat_coords[unique_idx, sort_cols[1L]],
    stat_coords[unique_idx, sort_cols[2L]]
  ))]
  uniq <- stat_coords[unique_idx, , drop = FALSE]
  inverse <- match(key, key[unique_idx])
  counts <- tabulate(inverse, nbins = nrow(uniq))

  list(unique = uniq, inverse = inverse, counts = counts)
}


#' Radius neighbour query on event centroids
#'
#' Mirrors `infostop.utils.query_neighbors`. Brute-force pairwise (O(N^2));
#' fine for the centroid counts the pipeline produces.
#'
#' @param coords matrix `n x 2` with columns `longitude`, `latitude`.
#' @param r2 radius. Metres for `"haversine"`, raw units for `"euclidean"`.
#' @param distance_metric `"haversine"` or `"euclidean"`.
#' @param weighted if `TRUE`, also return per-edge distances (for the
#'   weighted Infomap formula).
#'
#' @return A list with components
#'   * `neighbors`: list of integer vectors (1-based; includes the node
#'     itself).
#'   * `distances`: list of numeric vectors of matching length, or
#'     `NULL` when `weighted = FALSE`.
#'
#' @keywords internal
query_neighbors_r2 <- function(coords, r2,
                               distance_metric = c("haversine", "euclidean"),
                               weighted = FALSE) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2L)
  checkmate::assert_number(r2, lower = 0, finite = TRUE)
  checkmate::assert_flag(weighted)

  n <- nrow(coords)
  neighbors <- vector("list", n)
  distances <- if (weighted) vector("list", n) else NULL

  if (n == 0L) {
    return(list(neighbors = neighbors, distances = distances))
  }

  lon <- coords[, 1L]
  lat <- coords[, 2L]

  for (i in seq_len(n)) {
    if (distance_metric == "haversine") {
      d <- haversine_pairwise(lon[i], lat[i], lon, lat)
    } else {
      d <- sqrt((lon - lon[i])^2 + (lat - lat[i])^2)
    }
    hit <- which(d <= r2)
    neighbors[[i]] <- hit
    if (weighted) distances[[i]] <- d[hit]
  }

  list(neighbors = neighbors, distances = distances)
}


#' Build a weighted edge list for the Infomap network
#'
#' Mirrors the link-construction loop inside
#' `infostop.utils.infomap_communities`. For each undirected edge `(i, j)`
#' with `i < j` and both endpoints having more than one neighbour
#' (themselves + at least one other), emit
#'
#'   * `weight = max(counts[i], counts[j])` if `distances` is `NULL`
#'   * `weight = max(counts[i], counts[j]) * d_ij^(-weight_exponent)`
#'     otherwise.
#'
#' For the haversine metric the queried distances are in metres already
#' (we pass the radius into `query_neighbors_r2`), so no rescaling is
#' needed here — unlike the Python original, which gets radians out of
#' BallTree and multiplies by 6371000.
#'
#' @param neighbors,distances list output from [query_neighbors_r2()].
#' @param counts per-node counts (typically the dedup counts).
#' @param weight_exponent numeric scalar.
#'
#' @return A list with components
#'   * `edges`: data frame with columns `from`, `to`, `weight`.
#'   * `singleton_nodes`: integer vector of nodes whose only neighbour is
#'     themselves (skipped from the network).
#'
#' @keywords internal
build_infomap_edges <- function(neighbors, distances, counts,
                                weight_exponent = 1) {
  checkmate::assert_list(neighbors, types = "integerish")
  checkmate::assert_list(distances, types = "numeric", null.ok = TRUE)
  if (!is.null(distances)) {
    checkmate::assert_true(length(neighbors) == length(distances))
  }
  checkmate::assert_integerish(counts, len = length(neighbors), lower = 0L)
  checkmate::assert_number(weight_exponent)

  n <- length(neighbors)
  if (n == 0L) {
    return(list(
      edges = data.frame(from = integer(), to = integer(),
                         weight = numeric()),
      singleton_nodes = integer()
    ))
  }

  has_neighbours <- vapply(neighbors, length, integer(1)) > 1L
  singleton_nodes <- which(!has_neighbours)

  # Upper bound on the number of undirected edges; preallocate.
  max_edges <- sum(pmax(0L, lengths(neighbors) - 1L))
  from <- integer(max_edges)
  to <- integer(max_edges)
  weight <- numeric(max_edges)
  k <- 0L

  for (node in seq_len(n)) {
    if (!has_neighbours[node]) next
    nb <- neighbors[[node]]
    keep <- nb > node & has_neighbours[nb]
    if (!any(keep)) next
    nb_keep <- nb[keep]
    if (is.null(distances)) {
      w <- pmax(counts[node], counts[nb_keep])
    } else {
      d <- distances[[node]][keep]
      w <- pmax(counts[node], counts[nb_keep]) * d^(-weight_exponent)
    }
    m <- length(nb_keep)
    idx <- (k + 1L):(k + m)
    from[idx] <- node
    to[idx] <- nb_keep
    weight[idx] <- w
    k <- k + m
  }

  if (k < length(from)) {
    from <- from[seq_len(k)]
    to <- to[seq_len(k)]
    weight <- weight[seq_len(k)]
  }

  list(
    edges = data.frame(from = from, to = to, weight = weight),
    singleton_nodes = singleton_nodes
  )
}


#' Two-level Infomap community detection on the centroid network
#'
#' Mirrors `infostop.utils.infomap_communities` + `label_network`. Returns
#' a per-node integer label. Singletons (nodes with no neighbours within
#' `r2`) are either given a fresh label per node (`label_singleton = TRUE`)
#' or marked as `-1` (`label_singleton = FALSE`).
#'
#' Uses the mapequation `infomap` R package. For Python-reference
#' compatibility, the network is passed through Infomap's link-list parser
#' with compact 0-based node ids, a fixed seed, and stable weight formatting.
#'
#' @param neighbors,distances list output from [query_neighbors_r2()].
#'   Pass `distances = NULL` for unweighted edges.
#' @param counts per-node counts.
#' @param weight_exponent numeric scalar.
#' @param label_singleton logical; see description.
#' @param nb_trials Infomap `--num-trials` value. The default `1` matches
#'   the Python reference.
#'
#' @return Integer vector of labels of length `length(neighbors)`.
#'
#' @keywords internal
infomap_communities_r <- function(neighbors,
                                  distances = NULL,
                                  counts,
                                  weight_exponent = 1,
                                  label_singleton = TRUE,
                                  nb_trials = 1L) {
  built <- build_infomap_edges(neighbors, distances, counts,
                               weight_exponent = weight_exponent)
  edges <- built$edges
  singletons <- built$singleton_nodes
  n <- length(neighbors)

  labels <- rep(-1L, n)
  if (nrow(edges) > 0L) {
    # Only non-singleton vertices participate in the network. Including
    # singletons here would cause Infomap to assign each its own community,
    # racing the explicit policy below.
    # Use Infomap's link-list parser rather than the R wrapper's add_links()
    # path. The parser path matches the Python binding's results exactly on
    # weighted borderline graphs, whereas add_links() can pick a different
    # equal-codelength optimum.
    non_singletons <- setdiff(seq_len(n), singletons)
    compact_id <- seq_along(non_singletons) - 1L
    names(compact_id) <- as.character(non_singletons)
    compact_edges <- data.frame(
      from = unname(compact_id[as.character(edges$from)]),
      to = unname(compact_id[as.character(edges$to)]),
      weight = edges$weight
    )

    link_file <- tempfile(fileext = ".txt")
    on.exit(unlink(link_file), add = TRUE)
    writeLines(
      paste(
        compact_edges$from,
        compact_edges$to,
        format(compact_edges$weight, digits = 14, scientific = TRUE)
      ),
      link_file
    )

    # Python Infomap's default API path is reproduced by the C++ parser with
    # seed 14 for these deterministic reference graphs. Set it explicitly;
    # the R package default seed is different.
    args <- "--two-level --silent --seed 14"
    if (!identical(as.integer(nb_trials), 1L)) {
      args <- paste(args, "--num-trials", as.integer(nb_trials))
    }
    im <- infomap::Infomap(args)
    im$read_file(link_file)
    im$run()

    membership <- as.integer(im$modules)
    compact_nodes <- as.integer(names(im$modules))
    node_ids <- non_singletons[compact_nodes + 1L]
    labels[node_ids] <- membership
  }

  # Singleton policy: fresh sequential ids, or stay at -1.
  if (label_singleton && length(singletons) > 0L) {
    next_label <- max(c(0L, labels)) + 1L
    labels[singletons] <- seq.int(from = next_label,
                                  length.out = length(singletons))
  }

  # Infomap numbers modules from 1; shift the non-noise ids down so they are
  # 0-based, matching the Python reference convention that the copied upstream
  # layer (refine_labels, add_labels) assumes. Without this the +1 those helpers
  # apply double-shifts native ids, making public labels/site_ids start at 2.
  nz <- labels != -1L
  if (any(nz)) {
    labels[nz] <- labels[nz] - min(labels[nz])
  }
  labels
}


#' Canonicalise a label vector by first occurrence
#'
#' Maps the first non-`-1` label encountered to `0`, the next new label to
#' `1`, and so on. `-1` is preserved. Useful for comparing partitions
#' between implementations.
#'
#' @param labels integer vector. `NA` values are treated as `-1`.
#'
#' @return integer vector of the same length.
#'
#' @keywords internal
canonicalise_labels <- function(labels) {
  labels <- as.integer(labels)
  labels[is.na(labels)] <- -1L
  out <- rep(-1L, length(labels))
  next_id <- 0L
  remap <- integer()
  for (i in seq_along(labels)) {
    lab <- labels[i]
    if (lab == -1L) next
    key <- as.character(lab)
    if (is.na(remap[key])) {
      remap[key] <- next_id
      next_id <- next_id + 1L
    }
    out[i] <- remap[key]
  }
  out
}


#' Cluster a set of event centroids into location/site ids (Stage 2)
#'
#' Shared Stage-2 routine: deduplicate centroids, build the radius-`r2`
#' neighbour graph, and run two-level Infomap. Used by [find_locations_multi()]
#' (pooled across users) and by the native `identify_sites` backend.
#'
#' @param coords matrix `n x 2` with columns `longitude`, `latitude`.
#' @param r2 stage-2 radius (metres for `"haversine"`, raw units otherwise).
#' @param distance_metric one of `"haversine"`, `"euclidean"`.
#' @param weighted,weight_exponent,label_singleton,min_spacial_resolution
#'   same meaning as in [find_locations()].
#'
#' @return Integer vector of length `nrow(coords)`: the location id per input
#'   centroid (raw Infomap ids; `-1` for unlabelled when
#'   `label_singleton = FALSE`).
#'
#' @keywords internal
cluster_centroids <- function(coords, r2,
                              distance_metric = c("haversine", "euclidean"),
                              weighted = FALSE,
                              weight_exponent = 1,
                              label_singleton = TRUE,
                              min_spacial_resolution = 0) {
  distance_metric <- match.arg(distance_metric)
  d <- dedup_stat_coords(coords, min_spacial_resolution,
                         distance_metric = distance_metric)
  nb <- query_neighbors_r2(d$unique, r2,
                           distance_metric = distance_metric,
                           weighted = weighted)
  labels_unique <- infomap_communities_r(
    neighbors       = nb$neighbors,
    distances       = nb$distances,
    counts          = d$counts,
    weight_exponent = weight_exponent,
    label_singleton = label_singleton
  )
  labels_unique[d$inverse]
}


#' Detect stop locations in a trajectory
#'
#' Two-stage Infostop pipeline: (1) cluster temporally adjacent points
#' that remain within `r1` into stationary events, (2) cluster the event
#' centroids into reusable location ids via two-level Infomap on a graph
#' of centroids within `r2`.
#'
#' @param x,y longitude and latitude in decimal degrees (`x = lon`,
#'   `y = lat`).
#' @param time numeric vector of timestamps (seconds). `numeric()` skips
#'   the temporal gating.
#' @param r1 stage-1 radius. Metres for `haversine`, raw units for
#'   `euclidean`.
#' @param r2 stage-2 radius (same units as `r1`).
#' @param min_staying_time,max_time_between,min_size stage-1 gating
#'   parameters.
#' @param min_spacial_resolution optional grid snap on event centroids
#'   before deduplication. `0` (default) disables.
#' @param distance_metric one of `"haversine"`, `"euclidean"`.
#' @param weighted if `TRUE`, weight Infomap edges by inverse distance.
#' @param weight_exponent exponent in the inverse-distance weight.
#' @param label_singleton if `TRUE`, isolated centroids each get their
#'   own location id; if `FALSE`, they are unlabelled (`NA`).
#' @param asis if `TRUE`, return raw `-1` labels for unassigned points
#'   and event centroids; if `FALSE` (default), rewrite `-1` to
#'   `NA_integer_`.
#'
#' @return A list of class `"stoplocations"` with components
#'   * `event_map`: integer vector, per input point, of stage-1 event id.
#'   * `location_map`: integer vector, per input point, of location id.
#'   * `stat_coords`: matrix of stage-1 event centroids.
#'   * `stat_labels`: integer vector, per centroid, of location id.
#'   * `distance_metric`: character scalar (`"haversine"` or `"euclidean"`),
#'     stored for use by [compute_label_area()].
#'
#' @export
find_locations <- function(x, y,
                           time = numeric(),
                           r1 = 10,
                           r2 = 10,
                           min_staying_time = 300L,
                           max_time_between = 86400L,
                           min_size = 2L,
                           min_spacial_resolution = 0,
                           distance_metric = c("haversine", "euclidean"),
                           weighted = FALSE,
                           weight_exponent = 1,
                           label_singleton = TRUE,
                           asis = FALSE) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_number(r2, lower = 0, finite = TRUE)
  checkmate::assert_number(min_spacial_resolution, lower = 0)
  checkmate::assert_flag(weighted)
  checkmate::assert_number(weight_exponent)
  checkmate::assert_flag(label_singleton)
  checkmate::assert_flag(asis)

  events <- find_stops_xyt(
    x = x, y = y, time = time,
    r1 = r1,
    min_staying_time = min_staying_time,
    max_time_between = max_time_between,
    min_size = min_size,
    distance_metric = distance_metric,
    asis = TRUE
  )

  stat_coords <- events$stat_coords
  event_map_raw <- as.integer(events$event_map)   # 0-based, -1 for noise

  if (NROW(stat_coords) == 0L) {
    location_map <- rep(-1L, length(event_map_raw))
    out <- list(
      event_map = if (asis) event_map_raw else na_noise_labels(event_map_raw),
      location_map = if (asis) location_map else na_noise_labels(location_map),
      stat_coords = stat_coords,
      stat_labels = integer(),
      distance_metric = distance_metric
    )
    class(out) <- c("stoplocations", "list")
    return(out)
  }

  d <- dedup_stat_coords(stat_coords, min_spacial_resolution,
                         distance_metric = distance_metric)
  nb <- query_neighbors_r2(d$unique, r2,
                           distance_metric = distance_metric,
                           weighted = weighted)
  stat_labels_unique <- infomap_communities_r(
    neighbors = nb$neighbors,
    distances = nb$distances,
    counts = d$counts,
    weight_exponent = weight_exponent,
    label_singleton = label_singleton
  )

  # reverse dedup (per stage-1 event)
  stat_labels <- stat_labels_unique[d$inverse]

  # reverse stage-1 mapping: event_map_raw is 0-based with -1 for noise.
  # Append -1 to stat_labels and redirect event_map_raw == -1 to that
  # appended slot so the lookup stays vectorised. Cannot rely on
  # appended[0] because R drops zero indices silently rather than
  # returning NA.
  appended <- c(stat_labels, -1L)
  idx <- event_map_raw + 1L
  idx[event_map_raw == -1L] <- length(appended)
  location_map <- appended[idx]

  out <- list(
    event_map = if (asis) event_map_raw else na_noise_labels(event_map_raw),
    location_map = if (asis) location_map else na_noise_labels(location_map),
    stat_coords = stat_coords,
    stat_labels = if (asis) stat_labels else na_noise_labels(stat_labels),
    distance_metric = distance_metric
  )
  class(out) <- c("stoplocations", "list")
  out
}


#' Count unique stop events per location
#'
#' After running [find_locations()] or [find_locations_multi()], count how
#' many stage-1 stationary events were assigned to each location id. Each
#' event corresponds to one contiguous stationary episode ("visit"), so the
#' result is a visit-count per location.
#'
#' The computation is `table(x$stat_labels)` restricted to non-`NA`
#' entries. One row in `stat_coords` / one element in `stat_labels`
#' corresponds to one stage-1 event (deduplication is reversed in the
#' returned object).
#'
#' @param x a `"stoplocations"` object returned by [find_locations()] or
#'   [find_locations_multi()].
#'
#' @return A named integer vector. Names are location ids coerced to character
#'   (e.g. `"0"`, `"1"`, ...). Values are the number of stationary
#'   events assigned to each location. Noise (`NA`) is excluded.
#'
#' @export
compute_label_counts <- function(x) {
  checkmate::assert_class(x, "stoplocations")
  labs <- x$stat_labels
  if (length(labs) == 0L) return(stats::setNames(integer(), character()))
  labs_clean <- labs[!is.na(labs) & labs != -1L]
  if (length(labs_clean) == 0L) return(stats::setNames(integer(), character()))
  tbl <- table(labs_clean)
  out <- as.integer(tbl)
  names(out) <- names(tbl)
  out
}


# Internal helper: signed shoelace area from hull vertices in metres.
.shoelace_area_m2 <- function(x_m, y_m) {
  n <- length(x_m)
  if (n < 3L) return(0)
  i <- c(seq_len(n), 1L)
  abs(sum(x_m[i[-length(i)]] * y_m[i[-1L]] -
          x_m[i[-1L]]        * y_m[i[-length(i)]])) / 2
}


#' Compute the geographic area of each stop location
#'
#' For each location id, collect the stage-1 event centroids assigned to it,
#' compute their convex hull, and return the hull area. The area is computed
#' via an equirectangular projection centred on each cluster (for
#' `"haversine"`) or via the raw-unit shoelace formula
#' (for `"euclidean"`).
#'
#' For single-point locations (only one unique centroid) the area is 0.
#' For two-point locations the convex hull degenerates to a line; area is 0.
#'
#' @param x a `"stoplocations"` object returned by [find_locations()] or
#'   [find_locations_multi()].
#' @param distance_metric one of `"haversine"` (default: reads from
#'   `x$distance_metric` if present, else `"haversine"`) or
#'   `"euclidean"`. Override only when `x` was produced without the
#'   stored `distance_metric` field.
#'
#' @return A named numeric vector of areas. For `"haversine"`, values are
#'   in square metres (m^2). For `"euclidean"`, values are in the squared
#'   raw coordinate units. Names are location ids coerced to character.
#'   Noise (`NA`) is excluded.
#'
#' @export
compute_label_area <- function(x, distance_metric = NULL) {
  checkmate::assert_class(x, "stoplocations")

  if (is.null(distance_metric)) {
    distance_metric <- if (!is.null(x$distance_metric)) {
      x$distance_metric
    } else {
      "haversine"
    }
  }
  distance_metric <- match.arg(distance_metric, c("haversine", "euclidean"))

  labs <- x$stat_labels
  coords <- x$stat_coords

  if (length(labs) == 0L) {
    return(stats::setNames(numeric(), character()))
  }

  noise <- is.na(labs) | labs == -1L
  valid_labs <- unique(labs[!noise])

  if (length(valid_labs) == 0L) {
    return(stats::setNames(numeric(), character()))
  }

  areas <- vapply(valid_labs, function(lab) {
    idx <- which((!noise) & labs == lab)
    pts <- coords[idx, , drop = FALSE]
    n <- nrow(pts)
    if (n < 2L) return(0)

    lon <- pts[, 1L]
    lat <- pts[, 2L]

    if (distance_metric == "haversine") {
      lat0_rad <- mean(lat) * pi / 180
      lon_delta_rad <- (lon - mean(lon)) * pi / 180
      lat_delta_rad <- (lat - mean(lat)) * pi / 180
      x_m <- lon_delta_rad * cos(lat0_rad) * .EARTH_RADIUS_M
      y_m <- lat_delta_rad * .EARTH_RADIUS_M
    } else {
      x_m <- lon
      y_m <- lat
    }

    hull_idx <- grDevices::chull(x_m, y_m)
    if (length(hull_idx) < 3L) return(0)

    .shoelace_area_m2(x_m[hull_idx], y_m[hull_idx])
  }, numeric(1))

  names(areas) <- as.character(valid_labs)
  areas
}


#' Compute the median coordinate of each stop location
#'
#' After running [find_locations()], [find_locations_multi()] or the
#' [infostop()] family, compute the median coordinate of the stage-1 event
#' centroids assigned to each location id. Mirrors
#' `Infostop.compute_label_medians()` in the upstream package, and matches the
#' family of [compute_label_counts()] / [compute_label_area()].
#'
#' @param x a `"stoplocations"` object.
#'
#' @return A numeric matrix with one row per location id (ordered by id) and two
#'   columns: `longitude`, `latitude` for `"haversine"`, or `x`, `y` for
#'   `"euclidean"`. Noise (`NA`) is excluded.
#'
#' @export
compute_label_medians <- function(x) {
  checkmate::assert_class(x, "stoplocations")

  distance_metric <- if (!is.null(x$distance_metric)) {
    x$distance_metric
  } else {
    "haversine"
  }
  cols <- if (distance_metric == "haversine") {
    c("longitude", "latitude")
  } else {
    c("x", "y")
  }

  coords <- x$stat_coords
  labs <- x$stat_labels
  keep <- !is.na(labs)

  if (length(labs) == 0L || !any(keep)) {
    out <- matrix(numeric(0), nrow = 0L, ncol = 2L)
    colnames(out) <- cols
    return(out)
  }

  ulab <- sort(unique(labs[keep]))
  out <- t(vapply(ulab, function(lab) {
    pts <- coords[keep & labs == lab, , drop = FALSE]
    c(stats::median(pts[, 1L]), stats::median(pts[, 2L]))
  }, numeric(2)))
  colnames(out) <- cols
  rownames(out) <- NULL
  out
}


#' Detect stop locations across multiple users
#'
#' Multi-user variant of [find_locations()]. Stage 1 (stationary-event
#' detection) runs independently per user. Stage 2 (deduplication,
#' neighbour graph, Infomap) runs once on all users' event centroids
#' pooled together, so location ids are shared across users.
#'
#' @param trajectories a list where each element represents one user's
#'   trajectory. Each element must be a named list (or anything with
#'   `$x`, `$y`, and optionally `$time` components) with
#'   * `x`: numeric vector of longitudes (or raw coordinates for
#'     `"euclidean"`).
#'   * `y`: numeric vector of latitudes.
#'   * `time`: numeric vector of timestamps in seconds, or
#'     `numeric()` to skip temporal gating.
#' @param r1,r2,min_staying_time,max_time_between,min_size,min_spacial_resolution,distance_metric,weighted,weight_exponent,label_singleton,asis
#'   same as [find_locations()], applied uniformly to all users.
#'
#' @return A list of `"stoplocations"` objects, one per user, in the
#'   same order as `trajectories`. Each element has the same structure
#'   as the return value of [find_locations()]. Users with zero stationary
#'   events receive a result with empty `stat_coords` and
#'   `stat_labels`.
#'
#' @export
find_locations_multi <- function(
  trajectories,
  r1 = 10,
  r2 = 10,
  min_staying_time = 300L,
  max_time_between = 86400L,
  min_size = 2L,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  label_singleton = TRUE,
  asis = FALSE
) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_list(trajectories, min.len = 1L)
  checkmate::assert_number(r2, lower = 0, finite = TRUE)
  checkmate::assert_number(min_spacial_resolution, lower = 0)
  checkmate::assert_flag(weighted)
  checkmate::assert_number(weight_exponent)
  checkmate::assert_flag(label_singleton)
  checkmate::assert_flag(asis)

  n_users <- length(trajectories)

  # Stage 1: run independently per user
  stage1_results <- vector("list", n_users)
  for (u in seq_len(n_users)) {
    traj <- trajectories[[u]]
    stage1_results[[u]] <- find_stops_xyt(
      x    = traj$x,
      y    = traj$y,
      time = if (is.null(traj$time)) numeric() else traj$time,
      r1   = r1,
      min_staying_time = min_staying_time,
      max_time_between = max_time_between,
      min_size         = min_size,
      distance_metric  = distance_metric,
      asis = TRUE
    )
  }

  # Pool centroids
  n_events_u <- vapply(stage1_results, function(s) NROW(s$stat_coords), integer(1))

  has_events <- n_events_u > 0L

  if (!any(has_events)) {
    # All users have zero events
    empty_result <- list(
      event_map    = integer(),
      location_map = integer(),
      stat_coords  = matrix(numeric(0), nrow = 0L, ncol = 2L,
                            dimnames = list(NULL, c("longitude", "latitude"))),
      stat_labels  = integer(),
      distance_metric = distance_metric
    )
    class(empty_result) <- c("stoplocations", "list")
    return(lapply(seq_len(n_users), function(u) {
      r <- empty_result
      r$event_map    <- if (asis) rep(-1L, length(stage1_results[[u]]$event_map))
                        else rep(NA_integer_, length(stage1_results[[u]]$event_map))
      r$location_map <- r$event_map
      r
    }))
  }

  pooled_stat_coords <- do.call(rbind,
    lapply(stage1_results[has_events], `[[`, "stat_coords")
  )

  cumulative <- cumsum(n_events_u)
  offsets_start <- c(0L, cumulative[-n_users]) + 1L
  offsets_end   <- cumulative

  # Stage 2: dedup + neighbour graph + Infomap on pooled centroids
  stat_labels_pooled <- cluster_centroids(
    pooled_stat_coords, r2,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution
  )

  # Split results back per user
  results <- vector("list", n_users)

  for (u in seq_len(n_users)) {
    event_map_raw <- as.integer(stage1_results[[u]]$event_map)

    if (n_events_u[u] == 0L) {
      location_map_u <- rep(-1L, length(event_map_raw))
      out_u <- list(
        event_map    = if (asis) event_map_raw else na_noise_labels(event_map_raw),
        location_map = if (asis) location_map_u else na_noise_labels(location_map_u),
        stat_coords  = stage1_results[[u]]$stat_coords,
        stat_labels  = integer(),
        distance_metric = distance_metric
      )
      class(out_u) <- c("stoplocations", "list")
      results[[u]] <- out_u
      next
    }

    i0 <- offsets_start[u]
    i1 <- offsets_end[u]
    stat_labels_u <- stat_labels_pooled[i0:i1]

    appended <- c(stat_labels_u, -1L)
    idx <- event_map_raw + 1L
    idx[event_map_raw == -1L] <- length(appended)
    location_map_u <- appended[idx]

    out_u <- list(
      event_map    = if (asis) event_map_raw else na_noise_labels(event_map_raw),
      location_map = if (asis) location_map_u else na_noise_labels(location_map_u),
      stat_coords  = stage1_results[[u]]$stat_coords,
      stat_labels  = if (asis) stat_labels_u else na_noise_labels(stat_labels_u),
      distance_metric = distance_metric
    )
    class(out_u) <- c("stoplocations", "list")
    results[[u]] <- out_u
  }

  results
}
