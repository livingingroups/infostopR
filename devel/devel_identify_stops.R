library("infostop")

infostop_initialize()
attach(getNamespace("infostop"))

# data(package = "travelpaths")
data(travel_paths_trackframe, package = "travelpaths")
data(travel_paths_sftrack, package = "travelpaths")
data(travel_paths_move2, package = "travelpaths")


as_infostop <- function(x) {
  UseMethod("as_infostop")
}

grep("json", ls("package:sf"), value = TRUE, ignore.case = TRUE)


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


library("wk")
crs <- st_crs(x)
wkt
wk::wkt(x)


writeLines(wkt)
head(st_as_text(st_geometry(x)))

str(wk_crs(x))
wkt <- wk_crs(x)
parse_wkt()

x
as_lat_lon


as_lon_lat


as_infostop.trackframe <- function(data) {
  # create data input of infostop_internal from track.frame
  id_col <- id_col(data)
  ids <- if (is.null(id_col)) NULL else data[[id_col]]

  data[[time_col(data)]] <- as.integer(data[[time_col(data)]])
  cols <- c(easting_col(data), northing_col(data), time_col(data))
  data <- as.matrix(data[, cols])

  if (!is.null(ids) && length(unique(ids)) > 1) {
    data <- lapply(unname(split(seq_len(NROW(data)), f = ids)), function(i) data[i, ])
  }
  data
}


as_infostop.sftrack <- function(x) {
  id_col <- attr(x, "group_col")
  if (length(id_col) && !is.atomic(x[[id_col]])) {
    new_id_col <- "sft_group_id"
    x[[new_id_col]] <- make_unique_id(x[[id_col]])
    attr(x, "group_names") <- attr(x[[id_col]], "active_group")
    id_col <- new_id_col
  }
  ids <- if (is.null(id_col)) NULL else x[[id_col]]

  x <- cbind(
    st_coordinates_lat_lon(x[[attr(x, "sf_column")]]),
    as.numeric(x[[attr(x, "time_col")]])
  )

  if (!is.null(ids) && length(unique(ids)) > 1) {
    x <- lapply(unname(split(seq_along(ids), ids)), function(i) unname(x[i, ]))
  }
  x
}


str(as_infostop(travel_paths_trackframe))

x <- travel_paths_trackframe

x <- travel_paths_sftrack
head(travel_paths_sftrack)
data <- as_infostop(travel_paths_sftrack)
head(data[[1]])
str(data)


z <- unclass(x[[attr(x, "time_col")]])





py_identify_stops(data[[1]], r1, min_size, min_staying_time, max_time_between, distance_metric)


stops <- py_cpputils$get_stationary_events(
  data[[1]], r1, min_size, min_staying_time, max_time_between, distance_metric
)
stops


head(travel_paths_sftrack)
infostop(travel_paths_sftrack)$labels


#
#
#
library("infostop")
infostop_initialize()

data(travel_paths_sftrack, package = "travelpaths")

x <- identify_stops(travel_paths_sftrack)
head(data)




py_identify_sites <- infostop:::rpy("identify_sites")
labels <- py_identify_sites(stops$stop_events, stops$event_map)


dim(stops$stop_events[[1]])
max(stops$event_map[[1]])



x <- travel_paths_sftrack
x <- x[which(x$id == "track_1"), ]
x$stop_id <- stops$event_map[[1]]
head(x)
stop_col <- "stop_id"


y <- st_coordinates(x)
aggr <- aggregate(y, by = list(x$stop_id), FUN = mean)




