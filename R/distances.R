#' Haversine distance
#'
#' Computes the great-circle distance between pairs of points on a sphere
#' using the haversine formula.
#'
#' @param lon1 a numeric vector giving the longitude of the first points in decimal degrees.
#' @param lat1 a numeric vector giving the latitudes of the first points in decimal degrees.
#' @param lon2 a numeric vector giving the longitudes of the second points in decimal degrees.
#' @param lat2 a numeric vector giving the latitudes of the second points in decimal degrees.
#'   All four coordinate vectors must have the same length.
#' @param radius numeric scalar, radius of the sphere. Defaults to `6378137` metres
#'   (WGS84 equatorial radius).
#'
#' @return A numeric vector of distances between `(lon1[i], lat1[i])` and
#'   `(lon2[i], lat2[i])`, in the same unit as `radius` (metres by default).
#'
#' @examples
#' dist_haversine(16.37, 48.21, 2.35, 48.86) # Vienna -> Paris
#'
#' @export
dist_haversine <- function(lon1, lat1, lon2, lat2, radius = 6378137) {
  n <- length(lon1)
  checkmate::assert_numeric(lon1, lower = -180, upper = 180, any.missing = FALSE)
  checkmate::assert_numeric(lat1, lower = -90, upper = 90, len = n, any.missing = FALSE)
  checkmate::assert_numeric(lon2, lower = -180, upper = 180, len = n, any.missing = FALSE)
  checkmate::assert_numeric(lat2, lower = -90, upper = 90, len = n, any.missing = FALSE)
  checkmate::assert_number(radius, lower = 0, finite = TRUE)

  dist_haversine_cpp(lon1, lat1, lon2, lat2, radius)
}
