library("tinytest")
library("infostop")

infostop_initialize()

data <- infostop:::example_data()
data_m <- as.matrix(data)

expect_error(
  stops <- find_stops(
    data_m,
    r1 = 100,
    min_staying_time = 300,
    max_time_between = 86400,
    min_size = 2,
    distance_metric = "haversine"
  )
)
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
expect_error(
  one_step_labels <- infostop(
    data_m,
    r1 = 100,
    r2 = 50,
    min_staying_time = 300,
    max_time_between = 86400,
    min_size = 2,
    distance_metric = "haversine"
  )
)

one_step_labels <- infostop(
  data,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
expect_equal(one_step_labels$labels, two_step_labels)

# xyt
data <- infostop:::example_data()
stops_xyt <- find_stops_xyt(
  x = data[, 1],
  y = data[, 2],
  t = data[, 3],
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
expect_equal(stops_xyt, stops)
clusters_xyt <- spatial_infomap(stops_xyt$stop_events, r2 = 50)
expect_equal(clusters_xyt, clusters)
two_step_labels_xyt <- match_labels(clusters_xyt, stops_xyt$event_map)
expect_equal(two_step_labels_xyt, two_step_labels)
one_step_labels_xyt <- infostop_xyt(
  x = data[, 1],
  y = data[, 2],
  t = data[, 3],
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
expect_equal(one_step_labels_xyt$labels, two_step_labels)
expect_equal(one_step_labels_xyt$labels, one_step_labels$labels)

# xyt
data <- infostop:::example_data()
stops_xyt <- find_stops_xyt(
  x = data[, 1],
  y = data[, 2],
  t = data[, 3],
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
expect_equal(stops_xyt, stops)
clusters_xyt <- spatial_infomap(stops_xyt$stop_events, r2 = 50)
expect_equal(clusters_xyt, clusters)
two_step_labels_xyt <- match_labels(clusters_xyt, stops_xyt$event_map)
expect_equal(two_step_labels_xyt, two_step_labels)
one_step_labels_xyt <- infostop_xyt(
  x = data[, 1],
  y = data[, 2],
  t = data[, 3],
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
expect_equal(one_step_labels_xyt$labels, two_step_labels)
expect_equal(one_step_labels_xyt$labels, one_step_labels$labels)

# data_frame
data("path_data_frame", package = "trackframe")
# onestep
onestep_labels <- infostop(
  data = path_data_frame,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
# twostep
expect_warning(find_stops(
  data = path_data_frame,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
))
stops <- find_stops(
  data = path_data_frame,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
clusters <- spatial_infomap(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
# comparison
# cbind(onestep_labels$labels, twostep_labels)
expect_equal(onestep_labels$labels, twostep_labels)


# trackframe
data("path_trackframe", package = "trackframe")
# onestep
onestep_labels <- infostop(
  data = path_trackframe,
  r1 = 100,
  r2 = 50,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)


stops <- suppressWarnings(find_stops(
  data = path_trackframe,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
))
clusters <- spatial_infomap(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
# comparison
# cbind(onestep_labels$labels, twostep_labels)
expect_equal(onestep_labels$labels, twostep_labels)
