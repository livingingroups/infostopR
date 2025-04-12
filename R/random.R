#' Generate Random Travel Path
#'
#' This function generates a random travel path with longitude, latitude, and datetime values.
#' The path can include stationary periods and movements with configurable parameters.
#'
#' @param size An integer giving the number of points to generate in the path.
#' @param max_step A numeric giving the maximum step size in degrees for each movement. Default is 0.001.
#' @param time_increment A numeric giving the time between consecutive points in seconds. Default is 60 (1 minute).
#' @param start_location A numeric vector giving the starting location as c(latitude, longitude). Default is Vienna (48.2083537, 16.3725042).
#' @param start_time A POSIXct giving the starting time for the path. Default is current time.
#' @param stay_prob A numeric giving the probability of staying at the same location (0-1). Default is 0.2.
#' @param format A character string, either "data.frame" or "matrix". Default is "data.frame".
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item lat - Latitude values
#'     \item lon - Longitude values
#'     \item datetime - POSIXct datetime values
#'   }
#'
#' @examples
#' data <- rtravel_path(100, format = "matrix")
#' @export
rtravel_path <- function(size,
                         max_step = 0.001,
                         time_increment = 60, # in seconds
                         start_location = c(48.2083537, 16.3725042),
                         start_time = Sys.time(),
                         stay_prob = 0.2,
                         format = c("data.frame", "matrix")) {
  checkmate::assert_integerish(size, lower = 2, any.missing = FALSE)
  checkmate::assert_numeric(max_step, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(start_location, len = 2, any.missing = FALSE)
  checkmate::assert_posixct(start_time, any.missing = FALSE)
  checkmate::assert_numeric(stay_prob, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  format <- match.arg(format)
  
  path <- data.frame(
    lat = numeric(size),
    lon = numeric(size),
    datetime = as.POSIXct(rep(NA, size))
  )
  
  path$lat[1] <- start_location[1]
  path$lon[1] <- start_location[2]
  path$datetime[1] <- start_time
  
  for (i in 2:size) {
    if (runif(1) < stay_prob) {
      path$lat[i] <- path$lat[i-1]
      path$lon[i] <- path$lon[i-1]
      path$datetime[i] <- path$datetime[i-1] + round(runif(1, 1, 30)) *  time_increment
    } else {
      dlat <- runif(1, -max_step, max_step)
      dlon <- runif(1, -max_step, max_step)
      
      new_lat <- path$lat[i-1] + dlat
      new_lon <- path$lon[i-1] + dlon
      
      path$lat[i] <- new_lat
      path$lon[i] <- new_lon
      path$datetime[i] <- path$datetime[i-1] + time_increment
    }    
  }
  
  if (format == "matrix") {
    path$datetime <- as.integer(path$datetime)
    return(as.matrix(path))
  } else {
    return(path)
  }
}

