refine_labels <- function(labels) {
  if (length(labels) == 0) {
    return(labels)
  }
  labels <- as.integer(labels)
  labels[which(labels == -1L)] <- NA_integer_

  if (all(is.na(labels))) {
    return(labels)
  }

  labels - min(labels, na.rm = TRUE) + 1L
}

make_unique_id <- function(id_col) {
  if (is.null(id_col) || is.atomic(id_col)) {
    return(id_col)
  }
  unique_id <- sapply(id_col, paste, collapse = "<;>")
  attr(unique_id, "group_names") <- attr(id_col, "active_group")
  unique_id
}


get_id_column <- function(x) {
  if (inherits(x, "trackframe")) {
    id_col(x)
  } else if (inherits(x, "move2")) {
    attr(x, "track_id_column")
  } else if (inherits(x, "sftrack")) {
    attr(x, "group_col")
  } else {
    stop("x is expected to inherit from 'trackframe', 'move2' or 'sftrack'")
  }
}


get_ids_sf <- function(data) {
  if (inherits(data, "sftrack")) {
    make_unique_id(data[[attr(data, "group_col")]])
  } else if (inherits(data, "move2")) {
    data[[attr(data, "track_id_column")]]
  } else {
    NULL
  }
}


# This is not the trackframe case since we know if it is an object inheriting from sf
# the most likely time column is "time_column" as used in move2 and sftrack.
get_time_sf <- function(data) {
  if (inherits(data, "sftrack")) {
    time_col <- attr(data, "time_col")
  } else if (inherits(data, "move2")) {
    time_col <- attr(data, "time_column")
  } else {
    time_col_candidates <- tf_options("time_col")
    time_col <- intersect(colnames(data), time_col_candidates)
    if (length(time_col) == 0L) {
      stop("Could not find a valid time column in the data.", call. = FALSE)
    }
    if (length(time_col) > 1L) {
      stop(
        sprintf(
          "Multiple time columns found: %s. Please provide exactly one.",
          deparse(time_col)
        ),
        call. = FALSE
      )
    }
  }
  data[[time_col]]
}
