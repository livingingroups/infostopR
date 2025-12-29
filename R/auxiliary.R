refine_labels <- function(labels) {
  if (length(labels) == 0) {
    return(labels)
  }
  labels <- as.integer(labels)
  labels[labels == -1L] <- NA_integer_
  labels + 1L
}


make_unique_id <- function(id_col) {
  if (is.null(id_col) || is.atomic(id_col)) {
    return(id_col)
  }
  unique_id <- sapply(id_col, paste, collapse = "<;>")
  attr(unique_id, "group_names") <- attr(id_col, "active_group")
  unique_id
}

#' @importFrom trackframe id_col
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
