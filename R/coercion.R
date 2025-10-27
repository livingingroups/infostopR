# A version that enforces the order
st_coordinates_lat_lon <- function(x) {
  trackframe:::st_coordinates_lat_lon(x)
}

split_mat_by_id <- function(x, ids) if (!is.null(ids) && length(unique(ids)) > 1) lapply(
  unname(split(seq_len(NROW(x)), f = ids)),
  function(i) unname(x[i, ])
) else x

convert_labels_to_python <- function(event_maps) {
  for (i in seq_along(event_maps)) {
    event_maps[[i]] <- as.integer(event_maps[[i]] - 1L)
    event_maps[[i]][is.na(event_maps[[i]])] <- -1L
  }
  event_maps
}

prep_stops <- function(x, y, id, stop_id) {
  checkmate::assert_false(is.null(id))
  event_maps <- split(stop_id, id)
  stop_events <- aggregate(as.formula("cbind(x, y) ~ stop_id + id"), data = data.frame(
    x = x,
    y = y,
    id = id,
    stop_id = stop_id

  ), FUN = median)
  uids <- names(event_maps)

  idx <- order(match(stop_events[["id"]], uids), stop_events[["stop_id"]])
  stop_events <- split_mat_by_id(as.matrix(
    stop_events[idx, c("x", "y")]
  ), stop_events[idx, ][["id"]])

  # In the case of only one id
  if (!is.list(stop_events)) stop_events <- list(stop_events)
  names(stop_events) <- NULL
  names(event_maps) <- NULL
  list(stop_events = stop_events, event_maps = convert_labels_to_python(event_maps))
}