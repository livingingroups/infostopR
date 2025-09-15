#' Compute Intervals
#'
#' This function computes time intervals for each unique label in the mobility data.
#' It identifies continuous periods when a user was at the same location (same label).
#'
#' @param labels A vector of integer labels for each point in the mobility trace.
#' @param times A vector of integer timestamps corresponding to each point in the mobility trace.
#' @param max_time_between Maximum time (in seconds) between consecutive points to consider
#'   them part of the same interval. If the time between points exceeds this value,
#'   a new interval is created.
#'
#' @return A data frame with columns for label, start time, end time, and duration of each interval.
#'
#' @examples
#' if (is_infostop_initialized()) {
#' data("path_data_frame", package = "trackframe")
#' model <- infostop(path_data_frame, r1 = 10, r2 = 10, distance_metric = "haversine")
#' times <- as.integer(path_data_frame[["time"]])
#' intervals <- compute_intervals(model$labels, times)
#' }
#' @export
compute_intervals <- function(labels, times, max_time_between = 86400) {
  check_infostop_initialized()

  checkmate::assert_integerish(labels, any.missing = TRUE) #FIXME: are NAs ok?
  checkmate::assert_integerish(times, len = length(labels), any.missing = FALSE)
  checkmate::assert_numeric(max_time_between, lower = 0, len = 1, any.missing = FALSE)

  result <- py_infostop$compute_intervals(
    as.matrix(as.integer(labels)),
    as.matrix(as.integer(times)),
    max_time_between
  )
  return(result)
}
