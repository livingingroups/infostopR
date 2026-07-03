library("sf")
library("trackframe")


jitter_around <- function(n, lat, lon, s = 1e-5) {
  cbind(rnorm(n, lat, s), rnorm(n, lon, s))
}



to_infostop <- function(x, time_col, lon_col, lat_col, id_col = NULL) {
  cols <- c(lat_col, lon_col, time_col)
  x[[time_col]] <- as.numeric(x[[time_col]])
  ids <- if (!is.null(id_col)) x[[id_col]] else NULL
  data <- as.matrix(x[, cols])
  if (!is.null(ids) && length(unique(ids)) > 1) {
    idx <- unname(split(seq_len(NROW(data)), f = ids))
    data <- lapply(idx, function(i) data[i, ])
  }
  data
}


data_set_01 <- function() {
  n <- 40
  d <- rbind(jitter_around(n, 48.2082, 16.3725), jitter_around(n, 48.2150, 16.4250))
  data.frame(
    lat = d[, 1],
    lon = d[, 2],
    time = seq_len(nrow(d)) * 60 - 60,
    id = "track_1"
  )
}


# 3 stops of decreasing size plus transit outliers between.
data_set_02 <- function() {
  stop_a <- jitter_around(50, 55.7500, 12.3400)
  stop_b <- jitter_around(30, 55.7600, 12.3500)
  stop_c <- jitter_around(8,  55.7700, 12.3600)
  transit <- cbind(runif(5, 55.74, 55.78), runif(5, 12.33, 12.37))
  d <- rbind(
    stop_a,
    transit[1:2, ],
    stop_b,
    transit[3:4, ],
    stop_c,
    transit[5, , drop = FALSE]
  )
  data.frame(
    lat = d[, 1],
    lon = d[, 2],
    time = seq_len(nrow(d)) * 60 - 60,
    id = "track_1"
  )
}


data_set_03 <- function() {
  data("paths_data_frame", package = "trackframe")
  d <- paths_data_frame[, c("northing", "easting", "time", "id")]
  colnames(d)[1:2] <- c("lat", "lon")
  d
}


data_dir <- "../data"
dir.create(data_dir, showWarnings = FALSE)


write_csv <- function(x, file) {
  write.csv(x, file = file, row.names = FALSE)
}


# Latitude / Longitude
write_csv(data_set_01(), file = file.path(data_dir, "lon-lat_01.csv"))
write_csv(data_set_02(), file = file.path(data_dir, "lon-lat_02.csv"))
write_csv(data_set_03(), file = file.path(data_dir, "lon-lat_03.csv"))

to_xy <- function(x) {
  lola <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  xy <- st_transform(lola, crs = 27700)
  coords <- st_coordinates(xy)
  data.frame(x = coords[, 1], y = coords[, 2], time = xy$time, id = xy$id)
}

# X / Y (Easting / Northing)
write_csv(to_xy(data_set_01()), file = file.path(data_dir, "easting-northing_01.csv"))
write_csv(to_xy(data_set_02()), file = file.path(data_dir, "easting-northing_02.csv"))
write_csv(to_xy(data_set_03()), file = file.path(data_dir, "easting-northing_03.csv"))
