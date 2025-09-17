library("tinytest")
library("infostop")

infostop_initialize()

#
# check if onestep and twostep approach gives same results
#
data <- infostop:::example_data()
data_m <- as.matrix(data)

expect_error(
  stops <- identify_stops(
    data_m,
    r1 = 100,
    min_staying_time = 300,
    max_time_between = 86400,
    min_size = 2,
    distance_metric = "haversine"
  )
)
stops <- identify_stops(
  data,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "haversine"
)
clusters <- identify_sites(stops$stop_events, r2 = 50)
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

#
# identify_stops_xyt
#
data <- infostop:::example_data()
stops_xyt <- identify_stops_xyt(
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
clusters_xyt <- identify_sites(stops_xyt$stop_events, r2 = 50)
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


#
# infostop.data.frame method
#
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
expect_warning(identify_stops(
  data = path_data_frame,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
))
stops <- identify_stops(
  data = path_data_frame,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
)
clusters <- identify_sites(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
expect_equal(onestep_labels$labels, twostep_labels)


#
# infostop.trackframe
#
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


stops <- suppressWarnings(identify_stops(
  data = path_trackframe,
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2,
  distance_metric = "euclidean"
))
clusters <- identify_sites(data = stops$stop_events, r2 = 50, distance_metric = "euclidean")
twostep_labels <- match_labels(clusters, stops$event_map)
expect_equal(onestep_labels$labels, twostep_labels)
