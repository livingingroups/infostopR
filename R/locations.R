# Earth radius used by the Python reference.
.EARTH_RADIUS_M <- 6371000


# Collapse duplicate event centroids
#
# Mirrors `np.unique(stat_coords, return_inverse=True, return_counts=True)`
# from the Python reference implementation.
#
# @param stat_coords a numeric matrix with columns `longitude`, `latitude`
#   or `x`, `y`.
# @param min_spacial_resolution optional grid step.
# @param distance_metric one of `"haversine"`, `"euclidean"`.
#
# @return A list with elements
#   * `unique`: matrix of unique centroids (same column order as input).
#   * `inverse`: integer vector of length `nrow(stat_coords)`, the row
#     index in `unique` for each input row.
#   * `counts`: integer vector of length `nrow(unique)`.
#
# @examples
# unique_coords <- matrix(round(rnorm(20, 50, 20), 2), nrow = 10, ncol = 2)
# stat_coords <- unique_coords[sample.int(10, 30, TRUE), ]
# colnames(stat_coords) <- c("x", "y")
# dedup_stat_coords(stat_coords, distance_metric = "euclidean")
dedup_stat_coords <- function(
  stat_coords,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean")
) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_matrix(stat_coords,
    mode = "numeric", min.rows = 0L,
    ncols = 2L
  )
  checkmate::assert_number(min_spacial_resolution, lower = 0)

  if (min_spacial_resolution > 0) {
    stat_coords <- round(stat_coords / min_spacial_resolution) *
      min_spacial_resolution
  }

  if (nrow(stat_coords) == 0L) {
    return(list(
      unique = stat_coords,
      inverse = integer(),
      counts = integer()
    ))
  }

  uniq <- mgcv::uniquecombs(stat_coords)
  inverse <- attr(uniq, "index")
  attr(uniq, "index") <- NULL
  uniq <- matrix(uniq, ncol = ncol(stat_coords))

  sort_cols <- if (distance_metric == "haversine") c(2L, 1L) else c(1L, 2L)
  sort_idx <- do.call(order, list(
    uniq[, sort_cols[1L]],
    uniq[, sort_cols[2L]]
  ))
  inverse <- match(inverse, sort_idx)
  uniq <- uniq[sort_idx, , drop = FALSE]
  counts <- tabulate(inverse, nbins = nrow(uniq))

  list(unique = uniq, inverse = inverse, counts = counts)
}


# Radius neighbour query on event centroids
#
# R reimplementation of `infostop.utils.query_neighbors`
#
# @param coords matrix `n x 2` with columns `longitude`, `latitude` or `x`, `y`.
# @param r2 radius in metres for `"haversine"` and raw units for `"euclidean"`.
# @param distance_metric `"haversine"` or `"euclidean"`.
# @param weighted if `TRUE`, also return per-edge distances (for the
#   weighted Infomap formula).
#
# @return A list with components
#   * `neighbors`: list of integer vectors.
#   * `distances`: list of numeric vectors of matching length, or
#     `NULL` when `weighted = FALSE`.
#
# @examples
# coords <- matrix(round(rnorm(20, 50, 2), 2), nrow = 10, ncol = 2)
# str(query_neighbors_r2(coords, 2, "euclidean"))
query_neighbors_r2 <- function(
  coords,
  r2,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE
) {
  distance_metric <- match.arg(distance_metric)
  checkmate::assert_matrix(coords, mode = "numeric", ncols = 2L)
  checkmate::assert_number(r2, lower = 0, finite = TRUE)
  checkmate::assert_flag(weighted)

  n <- nrow(coords)
  neighbors <- vector("list", n)
  distances <- if (weighted) vector("list", n) else NULL

  lon <- coords[, 1L]
  lat <- coords[, 2L]

  for (i in seq_len(n)) {
    if (distance_metric == "haversine") {
      d <- dist_haversine_cpp(
        rep_len(lon[i], n),
        rep_len(lat[i], n),
        lon,
        lat,
        radius = .EARTH_RADIUS_M
      )
    } else {
      d <- sqrt((lon - lon[i])^2 + (lat - lat[i])^2)
    }
    hit <- which(d <= r2)
    neighbors[[i]] <- hit
    if (weighted) {
      distances[[i]] <- d[hit]
    }
  }

  list(neighbors = neighbors, distances = distances)
}


# Build a weighted edge list for the Infomap network
#
# Mirrors the link-construction loop inside
# `infostop.utils.infomap_communities`. For each undirected edge `(i, j)`
# with `i < j` and both endpoints having more than one neighbour
# (themselves + at least one other), emit
#
#   * `weight = max(counts[i], counts[j])` if `distances` is `NULL`
#   * `weight = max(counts[i], counts[j]) * d_ij^(-weight_exponent)`
#     otherwise.
#
# For the haversine metric the queried distances are in metres already
# (we pass the radius into `query_neighbors_r2`), so no rescaling is
# needed here, unlike the Python original, which gets radians out of
# BallTree and multiplies by 6371000.
#
# @param neighbors list output from `query_neighbors_r2()`.
# @param distances list output from `query_neighbors_r2()`.
# @param counts per-node counts (typically the dedup counts).
# @param weight_exponent numeric scalar.
#
# @return A list with components
#   * `edges`: data frame with columns `from`, `to`, `weight`.
#   * `singleton_nodes`: integer vector of nodes whose only neighbour is themselves.
build_infomap_edges <- function(neighbors, distances, counts, weight_exponent = 1) {
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
      edges = data.frame(from = integer(), to = integer(), weight = numeric()),
      singleton_nodes = integer()
    ))
  }

  has_neighbours <- vapply(neighbors, length, integer(1)) > 1L
  singleton_nodes <- which(!has_neighbours)

  # Upper bound on the number of undirected edges.
  max_edges <- sum(pmax(0L, lengths(neighbors) - 1L))
  from <- integer(max_edges)
  to <- integer(max_edges)
  weight <- numeric(max_edges)
  k <- 0L

  for (node in seq_len(n)) {
    if (!has_neighbours[node]) {
      next
    }
    nb <- neighbors[[node]]
    keep <- nb > node & has_neighbours[nb]
    if (!any(keep)) {
      next
    }
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

    from <- from[seq_len(k)]
    to <- to[seq_len(k)]
    weight <- weight[seq_len(k)]

  list(
    edges = data.frame(from = from, to = to, weight = weight),
    singleton_nodes = singleton_nodes
  )
}


# Two-level Infomap community detection on the centroid network
#
# Mirrors `infostop.utils.infomap_communities` + `label_network`. Returns
# a per-node integer label. Singletons (nodes with no neighbours within
# `r2`) are either given a fresh label per node (`label_singleton = TRUE`)
# or marked as `-1` (`label_singleton = FALSE`).
#
# Uses the mapequation `infomap` R package. For Python-reference
# compatibility, the network is passed with compact 0-based node ids and an
# explicit seed.
#
# @param neighbors,distances list output from [query_neighbors_r2()].
#   Pass `distances = NULL` for unweighted edges.
# @param counts per-node counts.
# @param weight_exponent numeric scalar.
# @param label_singleton logical; see description.
# @param nb_trials Infomap `--num-trials` value. The default `1` matches
#   the Python reference.
# @param seed Infomap random seed. The bundled Python-reference fixtures were
#   generated with infomap-py 1.0.6; with the current R binding, seed 1 matches
#   the haversine fixture and seed 29 matches the euclidean fixture.
#
# @return Integer vector of labels of length `length(neighbors)`.
infomap_communities_r <- function(
  neighbors,
  distances = NULL,
  counts,
  weight_exponent = 1,
  label_singleton = TRUE,
  nb_trials = 1L,
  seed = 1L
) {
  built <- build_infomap_edges(neighbors, distances, counts, weight_exponent = weight_exponent)
  edges <- built$edges
  singletons <- built$singleton_nodes
  n <- length(neighbors)

  labels <- rep(-1L, n)
  if (nrow(edges) > 0L) {
    non_singletons <- setdiff(seq_len(n), singletons)
    compact_edges <- data.frame(
      from = match(edges$from, non_singletons) - 1L,
      to = match(edges$to, non_singletons) - 1L,
      weight = edges$weight
    )

    control <- infomap::infomap_options(
      two_level = TRUE, silent = TRUE,
      seed = as.integer(seed),
      num_trials = as.integer(nb_trials)
    )
    im <- infomap::cluster_infomap(compact_edges, opts = control)

    membership <- as.integer(im$modules)
    compact_nodes <- as.integer(names(im$modules))
    node_ids <- non_singletons[compact_nodes + 1L]
    labels[node_ids] <- membership
  }

  # Singleton policy: fresh sequential ids, or stay at -1.
  if (label_singleton && length(singletons) > 0L) {
    next_label <- max(c(0L, labels)) + 1L
    labels[singletons] <- seq.int(from = next_label, length.out = length(singletons))
  }

  labels
}


# Cluster a set of event centroids into location/site ids
#
# @param coords matrix `n x 2` with columns `longitude`, `latitude`.
# @param r2 stage-2 radius (metres for `"haversine"`, raw units otherwise).
# @param distance_metric one of `"haversine"`, `"euclidean"`.
# @param weighted,weight_exponent,label_singleton,min_spacial_resolution
#   same meaning as in [find_locations()].
#
# @return Integer vector of length `nrow(coords)`, giving the location id per input
#   centroid.
#
# @examples
# coords <- matrix(round(rnorm(20, 50, 2), 2), nrow = 10, ncol = 2)
# coords <- coords[sample.int(nrow(coords), 2 * nrow(coords), TRUE), ]
# cluster_centroids(coords, 1, "euclidean")
cluster_centroids <- function(
  coords,
  r2,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  seed = 123L
) {
  distance_metric <- match.arg(distance_metric)
  d <- dedup_stat_coords(
    coords,
    min_spacial_resolution,
    distance_metric = distance_metric
  )
  nb <- query_neighbors_r2(
    d$unique,
    r2,
    distance_metric = distance_metric,
    weighted = weighted
  )
  labels_unique <- infomap_communities_r(
    neighbors = nb$neighbors,
    distances = nb$distances,
    counts = d$counts,
    weight_exponent = weight_exponent,
    label_singleton = label_singleton,
    seed = seed
  )
  labels_unique[d$inverse]
}
