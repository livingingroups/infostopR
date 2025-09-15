library(travelpaths)
library(infostop)

infostop_initialize()

data <- infostop:::example_data()
stops <- find_stops(
  data,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
clusters <- spatial_infomap(stops$stop_events, r2 = 50)
two_step_labels <- match_labels(clusters, stops$event_map)
one_step_labels <- infostop(
  data,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
all.equal(one_step_labels$labels, two_step_labels)


# data_frame
data("travel_path_data_frame", package = "travelpaths")
class(travel_path_data_frame)
# onestep
onestep_labels <- infostop(
  data = travel_path_data_frame,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
# twostep
stops <- find_stops(
  data = travel_path_data_frame,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
clusters <- spatial_infomap(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
# comparison
cbind(onestep_labels$labels, twostep_labels)
all.equal(onestep_labels$labels, twostep_labels)


# trackframe
data("travel_path_trackframe", package = "travelpaths")
class(travel_path_trackframe)
# onestep
onestep_labels <- infostop(
  data = travel_path_trackframe,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)


stops <- find_stops(
  data = travel_path_trackframe,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
clusters <- spatial_infomap(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
# comparison
cbind(onestep_labels$labels, twostep_labels)
all.equal(onestep_labels$labels, twostep_labels)
