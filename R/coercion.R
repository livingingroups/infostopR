


crs_lat_lon_order <- function(crs, value = FALSE) {
  wtk <- unlist(strsplit(crs[["wkt"]], "\\s+"))
  idx <- c(grep("latitude", wtk, fixed = TRUE), grep("longitude", wtk, fixed = TRUE))
  if (length(idx) != 2L) {
    return(NULL)
  }
  if (isTRUE(value)) {
    c("latitude", "longitude")[order(idx)]
  } else {
    order(idx)
  }
}


# A version that enforces the order
st_coordinates_lat_lon <- function(x) {
  coordinates <- st_coordinates(x)
  col_order <- crs_lat_lon_order(st_crs(x))
  if (is.null(col_order) || all(col_order == c(1, 2))) {
    coordinates
  } else {
    coordinates[, col_order]
  }
}


as_infostop <- function(x) {
  UseMethod("as_infostop")
}


as_infostop.trackframe <- function(x) {
  id_col <- attr(x, "id")
  ids <- if (is.null(id_col)) NULL else x[[id_col]]

  x[[attr(x, "time")]] <- as.integer(x[[attr(x, "time")]])
  cols <- c(attr(x, "easting"), attr(x, "northing"), attr(x, "time"))
  x <- as.matrix(x[, cols])

  if (!is.null(ids) && length(unique(ids)) > 1) {
    x <- lapply(unname(split(seq_len(NROW(x)), f = ids)), function(i) unname(x[i, ]))
  }
  x
}


as_infostop_sf <- function(x, sf_col, group_col, time_col) {
  id_col <- attr(x, group_col)
  if (length(id_col) > 0 && !is.atomic(x[[id_col]])) {
    new_id_col <- "sft_group_id"
    x[[new_id_col]] <- make_unique_id(x[[id_col]])
    attr(x, "group_names") <- attr(x[[id_col]], "active_group")
    id_col <- new_id_col
  }
  ids <- if (is.null(id_col)) NULL else x[[id_col]]

  x <- cbind(
    st_coordinates_lat_lon(x[[attr(x, sf_col)]]),
    as.numeric(x[[attr(x, time_col)]])
  )

  if (!is.null(ids) && length(unique(ids)) > 1) {
    x <- lapply(unname(split(seq_along(ids), ids)), function(i) unname(x[i, ]))
  }
  x
}


as_infostop.sftrack <- function(x) {
  as_infostop_sf(x, "sf_column", "group_col", "time_col")
}


as_infostop.move2 <- function(x) {
  as_infostop_sf(x, "sf_column", "track_id_column", "time_col")
}


convert_labels_to_python <- function(event_maps) {
  for (i in seq_along(event_maps)) {
    event_maps[[i]] <- as.integer(event_maps[[i]] - 1L)
    event_maps[[i]][is.na(event_maps[[i]])] <- -1L
  }
  event_maps
}


as_stop_locations <- function(x, stop_id_col = "stop_id") {
  UseMethod("as_stop_locations")
}


as_stop_locations.trackframe <- function(x, stop_id_col = "stop_id") {
  checkmate::assert_choice(stop_id_col, colnames(x))
  id_col <- attr(x, "id")
  if (is.null(id_col)) {
    event_maps <- list(x[[stop_id_col]])
    fomu <- sprintf(
      "cbind(%s, %s) ~ %s",
      attr(x, "easting"), attr(x, "northing"), stop_id_col
    )
  } else {
    event_maps <- split(x[[stop_id_col]], x[[id_col]])
    fomu <- sprintf(
      "cbind(%s, %s) ~ %s + %s",
      attr(x, "easting"), attr(x, "northing"), stop_id_col, id_col
    )
  }
  stop_events <- aggregate(as.formula(fomu), data = x, FUN = median)
  uids <- names(event_maps)
  if (is.null(uids)) {
    cols <- c(attr(x, "easting"), attr(x, "northing"))
    stop_events <- stop_events[, cols]
  } else {
    idx <- order(match(stop_events[[id_col]], uids), stop_events[[stop_id_col]])
    cols <- c(attr(x, "easting"), attr(x, "northing"), id_col)
    stop_events <- stop_events[idx, cols]
    stop_events <- split(stop_events, stop_events[[id_col]])
    stop_events <- lapply(stop_events, function(x) unname(as.matrix(x[, c(1, 2)])))
    names(stop_events) <- NULL
  }
  names(event_maps) <- NULL
  list(stop_events = stop_events, event_maps = convert_labels_to_python(event_maps))
}


# stop_id_col = "stop_id"
# sf_col = "sf_column"
# group_col = "track_id_column"
# time_col = "time_col"
as_stop_locations_sf <- function(x, stop_id_col, sf_col, group_col, time_col) {
  checkmate::assert_choice(stop_id_col, colnames(x))
  id_col <- attr(x, group_col)
  if (length(id_col) > 0 && !is.atomic(x[[id_col]])) {
    new_id_col <- "sft_group_id"
    x[[new_id_col]] <- make_unique_id(x[[id_col]])
    attr(x, "group_names") <- attr(x[[id_col]], "active_group")
    id_col <- new_id_col
  }
  ids <- if (is.null(id_col)) NULL else x[[id_col]]

  coords <- st_coordinates_lat_lon(x[[attr(x, sf_col)]])

  d <- data.frame(x = coords[, 1], y = coords[, 2], stop_id = x[[stop_id_col]])
  d[["id"]] <- ids

  if (is.null(ids)) {
    event_maps <- list(d[["stop_id"]])
    fomu <- "cbind(x, y) ~ stop_id"
  } else {
    event_maps <- split(d[["stop_id"]], d[["id"]])
    fomu <- "cbind(x, y) ~ stop_id + id"
  }
  stop_events <- aggregate(as.formula(fomu), data = d, FUN = median)
  uids <- names(event_maps)
  if (is.null(uids)) {
    stop_events <- stop_events[, c("x", "y")]
  } else {
    idx <- order(match(stop_events[["id"]], uids), stop_events[["stop_id"]])
    stop_events <- stop_events[idx, c("x", "y", "id")]
    stop_events <- split(stop_events, stop_events[["id"]])
    stop_events <- lapply(stop_events, function(mat) unname(as.matrix(mat[, c(1, 2)])))
    names(stop_events) <- NULL
  }
  names(event_maps) <- NULL  # Named lists are converted to Pyton dict we need a list.
  list(stop_events = stop_events, event_maps = convert_labels_to_python(event_maps))
}


as_stop_locations.sftrack <- function(x, stop_id_col = "stop_id") {
  as_stop_locations_sf(x, stop_id_col, "sf_column", "group_col", "time_col")
}


as_stop_locations.move2 <- function(x, stop_id_col = "stop_id") {
  as_stop_locations_sf(x, stop_id_col, "sf_column", "track_id_column", "time_col")
}


# as_ellipsoidal_frame  Longitude/Latitude
# as_cartesian_frame    Eastern/Northing
# as_geo_frame <- function(x) {
#   UseMethod("as_geo_frame")
# }
