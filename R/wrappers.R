native_identify_sites_backend <- function(
  stop_events,
  event_maps,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  distance_metric = c("haversine", "euclidean"),
  weighted = FALSE,
  weight_exponent = 1
) {
  distance_metric <- match.arg(distance_metric)
  n_id <- length(event_maps)

  coords_list <- lapply(stop_events, function(m) {
    # Accept either a centroid matrix or a list of point vectors (the latter is
    # how the copied identify_sites_internal / reticulate passed them).
    m <- if (is.list(m)) do.call(rbind, m) else as.matrix(m)
    if (is.null(m) || nrow(m) == 0L) {
      return(matrix(numeric(0), nrow = 0L, ncol = 2L))
    }
    if (distance_metric == "haversine") {
      m[, c(2L, 1L), drop = FALSE]
    } else {
      m[, c(1L, 2L), drop = FALSE]
    }
  })
  n_per_id <- vapply(coords_list, nrow, integer(1))

  if (sum(n_per_id) == 0L) {
    return(lapply(event_maps, function(em) rep(-1L, length(em))))
  }

  pooled <- do.call(rbind, coords_list)
  site_per_centroid <- cluster_centroids(
    pooled,
    r2,
    distance_metric = distance_metric,
    weighted = weighted,
    weight_exponent = weight_exponent,
    label_singleton = label_singleton,
    min_spacial_resolution = min_spacial_resolution
  )

  ends <- cumsum(n_per_id)
  starts <- c(0L, ends[-n_id]) + 1L

  out <- vector("list", n_id)
  for (i in seq_len(n_id)) {
    em <- as.integer(event_maps[[i]])
    if (n_per_id[i] == 0L) {
      out[[i]] <- rep(-1L, length(em))
      next
    }
    site_of_stop <- site_per_centroid[starts[i]:ends[i]]
    # Append a -1 slot and redirect noise points to it (R drops index 0).
    appended <- c(site_of_stop, -1L)
    idx <- em + 1L
    idx[em == -1L] <- length(appended)
    out[[i]] <- appended[idx]
  }
  out
}


# Port of `infostop-py`'s `postprocess.compute_intervals`
#
# Groups consecutive equal labels into intervals, starting a new interval
# whenever the label changes or the gap to the previous point reaches
# `max_time_between`.
#
# @param labels integer vector (`NA` or `-1` denote noise).
# @param times integer vector, same length as `labels`.
# @param max_time_between numeric gap threshold in seconds.
#
# @return data.frame with columns `label`, `start_time`, `end_time`.
native_compute_intervals <- function(labels, times, max_time_between = 86400) {
  labels <- as.integer(labels)
  times <- as.integer(times)
  n <- length(labels)

  empty <- data.frame(
    label = integer(0),
    start_time = integer(0),
    end_time = integer(0)
  )
  if (n == 0L) {
    return(empty)
  }

  same_loc <- function(a, b) {
    (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && a == b)
  }

  rows <- list()
  loc_prev <- labels[1L]
  t_start <- times[1L]
  t_end <- t_start

  if (n >= 2L) {
    for (i in 2:n) {
      loc <- labels[i]
      tm <- times[i]
      if (same_loc(loc, loc_prev) && (tm - t_end) < max_time_between) {
        t_end <- tm
      } else {
        rows[[length(rows) + 1L]] <- c(loc_prev, t_start, t_end)
        t_start <- tm
        t_end <- tm
      }
      loc_prev <- loc
    }
  }

  # The trailing interval is appended only when the last label is noise.
  if (is.na(loc_prev) || loc_prev == -1L) {
    rows[[length(rows) + 1L]] <- c(loc_prev, t_start, t_end)
  }

  if (length(rows) == 0L) {
    return(empty)
  }
  m <- do.call(rbind, rows)
  data.frame(
    label = as.integer(m[, 1L]),
    start_time = as.integer(m[, 2L]),
    end_time = as.integer(m[, 3L])
  )
}
