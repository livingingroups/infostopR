

#' @export
#' @noRd
print.folium_map <- function(x, ...) {
  writeLines("FoliumMap object")
  writeLines("  - render_polygons(color, opacity)")
  writeLines("  - render_points(color, opacity, subsampling)")
  writeLines("  - render_heatmap(radius, subsampling)")
  writeLines("  - points")
  writeLines("  - labels")
}


#' Plot Map of Stop Locations
#'
#' This function creates an interactive map visualization of the stop locations
#' detected by an Infostop model. It's a convenient wrapper around the folium_map
#' function that works directly with an Infostop model object.
#'
#' @param model An Infostop model object created by the infostop() function.
#' @param display_data A character string specifying what data to display. Default is "unique_stationary".
#' @param polygons A logical indicating whether to display polygon areas for clusters. Default is TRUE.
#' @param scatter A logical indicating whether to scatter points on the map. Default is FALSE.
#' @param heatmap A logical indicating whether to add a heatmap layer to the map. Default is TRUE.
#' @param polygons_color A character string giving the color for polygons in hex format or named color. Default is "#ee9999".
#' @param polygons_opacity A numeric between 0 and 1 specifying the transparency of polygons. Default is 0.3.
#' @param scatter_color A character string giving the color for scattered points. Default is "k".
#' @param scatter_opacity A numeric between 0 and 1 specifying the transparency of points. Default is 0.3.
#' @param scatter_subsampling A numeric between 0 and 1 specifying the fraction of points to display. Default is 1.
#' @param heatmap_radius A numeric specifying the radius of influence for each point in the heatmap. Default is 8.
#' @param heatmap_subsampling A numeric between 0 and 1 specifying the fraction of points to include in the heatmap. Default is 1.
#' @param zoom_start A numeric giving the initial zoom level for the map. Default is 12.
#' @param tiles A character string giving the map tile provider. Default is "OpenStreetMap".
#' @param API_key A character string giving the API key for tile providers that require authentication. Default is NULL.
#'
#' @return A FoliumMap object with the visualization of stop locations.
#'
#' @examples
#' if (is_infostop_initialized()) {
#'  data("path_data_frame", package = "trackframe")
#'  model <- infostop(path_data_frame, r1 = 10, r2 = 10, distance_metric = "haversine")
#'   map <- plot_map(model, 
#'                 scatter = TRUE, 
#'                 polygons_color = "#ff0000", 
#'                 zoom_start = 10)
#'  \dontrun{
#'   map$show_in_browser()
#'   map$save("map.html")
#'  }
#' }
#' @export
plot_map <- function(model,
                     display_data="unique_stationary",
                     polygons=TRUE,
                     scatter=FALSE,
                     heatmap=TRUE,
                     polygons_color="#ee9999",
                     polygons_opacity=0.3,
                     scatter_color="k",
                     scatter_opacity=0.3,
                     scatter_subsampling=1,
                     heatmap_radius=8,
                     heatmap_subsampling=1,
                     zoom_start=12,
                     tiles="OpenStreetMap",
                     API_key=NULL
) {
  check_infostop_initialized()
  
  checkmate::assert_multi_class(model, c("Infostop", "SpatialInfomap"))
  checkmate::assert_character(display_data, len = 1, any.missing = FALSE)
  checkmate::assert_logical(polygons, len = 1, any.missing = FALSE)
  checkmate::assert_logical(scatter, len = 1, any.missing = FALSE)
  checkmate::assert_logical(heatmap, len = 1, any.missing = FALSE)
  checkmate::assert_character(polygons_color, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(polygons_opacity, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_character(scatter_color, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(scatter_opacity, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(scatter_subsampling, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(heatmap_radius, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(heatmap_subsampling, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(zoom_start, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_character(tiles, len = 1, any.missing = FALSE)
  checkmate::assert_character(API_key, len = 1, any.missing = FALSE, null.ok = TRUE)

  map <- py_infostop$plot_map(model$model,
                              display_data = display_data,
                              polygons = polygons,
                              scatter = scatter,
                              heatmap = heatmap,
                              polygons_color = polygons_color,
                              polygons_opacity = polygons_opacity,
                              scatter_color = scatter_color,
                              scatter_opacity = scatter_opacity,
                              scatter_subsampling = scatter_subsampling,
                              heatmap_radius = heatmap_radius,
                              heatmap_subsampling = heatmap_subsampling,
                              zoom_start = zoom_start,
                              tiles = tiles,
                              API_key = API_key)
  return(folium_map(map$m))
}


#' Create a Folium Map Object
#'
#' This function creates an R wrapper around a Python Folium Map object,
#' providing access to its methods for interactive map visualization.
#'
#' @param pointer A pointer to a Python Folium Map object
#'
#' @return A FoliumMap R object with the following methods:
#'   \itemize{
#'     \item \code{show_in_browser()}
#'     \item \code{fit_bounds(bounds, padding_top_left = NULL,
#'                            padding_bottom_right = NULL, 
#'                            padding = NULL, max_zoom = NULL)}
#'     \item \code{save(outfile, close_file = TRUE, ...)}
#'   }
#' 
#' @details
#'   \itemize{
#'     \item \code{show_in_browser()}: \cr
#'       Display the map in the default web browser
#'     \item \code{fit_bounds(bounds,
#'                            padding_top_left = NULL,
#'                            padding_bottom_right = NULL, 
#'                            padding = NULL,
#'                            max_zoom = NULL)}: \cr
#' 
#'       Fit the map to contain a bounding box with the maximum zoom level possible.
#'       
#'       Arguments:
#'       \describe{
#'         \item{bounds}{A matrix of two points \code{rbind(c(lat1, lng1), c(lat2, lng2))} specifying the southwest and northeast corners of the bounding box.}
#'         \item{padding_top_left}{A numeric vector of length 2 specifying padding in the top left corner. Useful if controls might obscure objects you're zooming to.}
#'         \item{padding_bottom_right}{A numeric vector of length 2 specifying padding in the bottom right corner.}
#'         \item{padding}{A numeric vector of length 2 specifying padding for both corners.}
#'         \item{max_zoom}{An integer specifying the maximum zoom level to use.}
#'       }
#'     \item \code{save(outfile, close_file = TRUE, ...)}: \cr
#'       Save the map to an HTML file
#'       
#'       Arguments:
#'       \describe{
#'         \item{outfile}{A character string giving the path to the output HTML file.}
#'         \item{close_file}{A logical indicating whether to close the file after writing. Default is TRUE.}
#'         \item{...}{Additional arguments passed to the underlying save method.}
#'       }
#'   }
#' 
#' @examples
#' if (is_infostop_initialized()) {
#'  data("path_data_frame", package = "trackframe")
#'  model <- infostop(path_data_frame, r1 = 10, r2 = 10, distance_metric = "haversine")
#'  map <- plot_map(model, scatter = TRUE)
#'  \dontrun{
#'     map$save("my_map.html")
#'     map$show_in_browser()
#'  }
#' }
#' @export
folium_map <- function(pointer) {
  env <- new.env(parent = emptyenv())
  env$map <- pointer
  
  env$show_in_browser <- function() {
    env$map$show_in_browser()
  }
  
  env$fit_bounds <- function(bounds, 
                             padding_top_left = NULL, 
                             padding_bottom_right = NULL,
                             padding = NULL, 
                             max_zoom = NULL) {
    checkmate::assert_matrix(bounds, "numeric", ncol = 2, any.missing = FALSE)
    bounds <- apply(bounds, 1, as.list)
    env$map$fit_bounds(
      bounds,
      padding_top_left = padding_top_left,
      padding_bottom_right = padding_bottom_right,
      padding = padding,
      max_zoom = max_zoom
    )
  }
  
  env$save <- function(outfile, close_file = TRUE, ...) {
    checkmate::assert_character(outfile, len = 1, any.missing = FALSE)
    checkmate::assert_logical(close_file, len = 1, any.missing = FALSE)
    env$map$save(outfile, close_file, ...)
  }
  
  class(env) <- "FoliumMap"
  return(env)
}


#' @export
#' @noRd
print.FoliumMap <- function(x, ...) {
  writeLines("FoliumMap object")
  writeLines("  - show_in_browser()")
  writeLines("  - fit_bounds(bounds, padding_top_left = NULL, padding_bottom_right = NULL, padding = NULL, max_zoom = NULL)")
  writeLines("  - save(outfile, close_file = TRUE, ...)")
}
