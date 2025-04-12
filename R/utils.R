
#' Query Neighbors for Spatial Points
#'
#' This function finds neighboring points within a specified distance for each point in the input coordinates.
#' It's useful for creating spatial networks based on proximity.
#'
#' @param coords A numeric matrix with 2 columns representing latitude and longitude coordinates.
#' @param r2 Numeric. Maximum distance between points to consider them neighbors.
#' @param distance_metric Character. The distance metric to use. Options are "haversine" (for geographic coordinates) 
#'   or "euclidean" (for Cartesian coordinates). Default is "haversine".
#' @param weighted Logical. If TRUE, edge weights are calculated based on the distance between points.
#'   If FALSE, all edges have equal weight. Default is FALSE.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{sources}: Integer vector of source point indices.
#'     \item \code{targets}: Integer vector of target point indices.
#'     \item \code{weights}: Numeric vector of edge weights (if weighted=TRUE).
#'   }
#'   Each pair of corresponding indices in sources and targets represents an edge in the proximity network.
#'
#' @examples
#' \dontrun{
#' # Create sample spatial data
#' coords <- matrix(c(
#'   55.75259295, 12.34353885,
#'   55.7525908, 12.34353145,
#'   63.40379175, 10.40477095
#' ), ncol = 2, byrow = TRUE)
#' 
#' # Find neighbors within 10 distance units
#' neighbors <- query_neighbors(coords, r2 = 10)
#' 
#' # Access the network edges
#' edges <- data.frame(source = neighbors$sources, target = neighbors$targets)
#' }
#' @export
query_neighbors <- function(coords,
                            r2,
                            distance_metric="haversine", 
                            weighted=FALSE) {
  check_infostop_initialized()

  checkmate::assert_matrix(coords, "numeric", any.missing = FALSE, ncol = 2)
  checkmate::assert_numeric(r2, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_character(distance_metric, len = 1, any.missing = FALSE)
  checkmate::assert_logical(weighted, len = 1, any.missing = FALSE)

  result <- py_infostop$query_neighbors(coords, r2, distance_metric, weighted)
  return(result)
}


# max_pdist: just use the geosphere package
# convex_hull: just use geometry::convhulln both are wrappers around the C++ Qhull library.
