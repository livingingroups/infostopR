#' Compute Intervals
#'
#' This function computes time intervals for each unique label in the mobility data.
#' It identifies continuous periods when a user was at the same location (same label).
#'
#' @param labels a vector of integer labels for each point in the mobility
#'   trace. `NA` identifies noise points.
#' @param times a vector of integer timestamps corresponding to each point in
#'   the mobility trace.
#' @param max_time_between a numeric giving the maximum time in seconds between
#'   consecutive points to consider them part of the same interval. If the
#'   time between points is equal to or greater than this value, a new interval
#'   is created.
#'
#' @return A data frame with columns `label`, `start_time`, and `end_time`.
#'   The trailing interval is returned only when its label identifies noise
#'   (`NA` or `-1L`).
#'
#' @examples
#' labels <- c(NA, 1L, 1L, NA, 2L, 2L, 2L, NA, 1L, NA)
#' times <- seq(0, 90, by = 10)
#' compute_intervals(labels, times, max_time_between = 15)
#' @export
compute_intervals <- function(labels, times, max_time_between = 86400) {
  checkmate::assert_integerish(labels, any.missing = TRUE)
  checkmate::assert_integerish(times, len = length(labels), any.missing = FALSE)
  checkmate::assert_numeric(
    max_time_between,
    lower = 0,
    len = 1,
    any.missing = FALSE
  )

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

  same_label <- ifelse(
    is.na(labels[-1L]) | is.na(labels[-n]),
    is.na(labels[-1L]) & is.na(labels[-n]),
    labels[-1L] == labels[-n]
  )
  new_interval <- !same_label | diff(times) >= max_time_between
  starts <- c(1L, which(new_interval) + 1L)
  ends <- c(starts[-1L] - 1L, n)

  intervals <- data.frame(
    label = labels[starts],
    start_time = times[starts],
    end_time = times[ends]
  )

  # The trailing interval is returned only when its label is noise.
  last <- nrow(intervals)
  keep <- seq_len(last) < last |
    is.na(intervals$label) |
    intervals$label == -1L
  intervals[keep, , drop = FALSE]
}
