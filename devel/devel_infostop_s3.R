library(travelpaths)
library(infostop)

infostop_initialize()

# matrix
data("travel_path_matrix", package = "travelpaths")
travel_path_matrix
as.trackframe(travel_path_matrix)
class(travel_path_matrix)
onestep <- infostop(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
infostop(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "euclidean"
) #FIXME: wrong distance metric: can we have a warning?
infostop(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "euclidean",
  crs_input = 4326
) #FIXME: wrong distance metric: can we have a warning?

find_stops(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
find_stops(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "euclidean"
) #FIXME: wrong distance metric: can we have a warning?

x <- spatial_infomap(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
x$labels

stops_matrix <- find_stops(
  data = travel_path_matrix[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
twostep <- spatial_infomap(data = stops_matrix$coordinates, distance_metric = "haversine")

dim(onestep$labels)
dim(twostep$labels)
onestep$labels
twostep$labels
onestep$compute_label_medians()
twostep$model
onestep$model
# i1_matrix$compute_label_medians()

# data.frame
data("travel_path_data_frame", package = "travelpaths")
travel_path_data_frame
class(travel_path_data_frame)
infostop(
  data = travel_path_data_frame[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
infostop(
  data = travel_path_data_frame[, c("latitude", "longitude", "time")],
  distance_metric = "euclidean"
) #FIXME: wrong distance metric: can we have a warning?

find_stops(
  data = travel_path_data_frame[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)
find_stops(
  data = travel_path_data_frame[, c("latitude", "longitude", "time")],
  distance_metric = "euclidean"
) #FIXME: wrong distance metric: can we have a warning?

spatial_infomap(
  data = travel_path_data_frame[, c("latitude", "longitude", "time")],
  distance_metric = "haversine"
)

# trackframe
data("travel_path_trackframe", package = "travelpaths")
travel_path_trackframe
class(travel_path_trackframe)
infostop(data = travel_path_trackframe, distance_metric = "haversine") # ERROR: wrong distance metric
infostop(data = travel_path_trackframe, distance_metric = "euclidean")

find_stops(data = travel_path_trackframe, distance_metric = "haversine") # ERROR: wrong distance metric
find_stops(data = travel_path_trackframe, distance_metric = "euclidean")

spatial_infomap(data = travel_path_trackframe, distance_metric = "haversine")
spatial_infomap(data = travel_path_trackframe, distance_metric = "euclidean")

# sftrack
data("travel_path_sftrack", package = "travelpaths")
travel_path_sftrack
class(travel_path_sftrack)
infostop(data = travel_path_sftrack, distance_metric = "haversine")
infostop(data = travel_path_sftrack, distance_metric = "euclidean") # FIXME: ERROR: wrong distance metric check with crs

find_stops(data = travel_path_sftrack, distance_metric = "haversine")
find_stops(data = travel_path_sftrack, distance_metric = "euclidean") # FIXME: ERROR: wrong distance metric check with crs

spatial_infomap(data = travel_path_sftrack, distance_metric = "haversine")


# move2
data("travel_path_move2", package = "travelpaths")
travel_path_move2
class(travel_path_move2)
infostop(data = travel_path_move2, distance_metric = "haversine")
infostop(data = travel_path_move2, distance_metric = "euclidean") # FIXME: ERROR: wrong distance metric check with crs

find_stops(data = travel_path_move2, distance_metric = "haversine")
find_stops(data = travel_path_move2, distance_metric = "euclidean") # FIXME: ERROR: wrong distance metric check with crs

spatial_infomap(data = travel_path_move2, distance_metric = "haversine")
