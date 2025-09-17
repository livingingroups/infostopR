library("tinytest")
library("infostop")

infostop_initialize()

#
#
# single path
#
#

#
# matrix
#
data("path_matrix", package = "trackframe")
expect_inherits(path_matrix, "matrix")
identify_stops_xyt_h <- identify_stops_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
)

clusters <- identify_sites(identify_stops_xyt_h$stop_events)
two_step_labels <- match_labels(clusters, identify_stops_xyt_h$event_map)

infostop_xyt_h <- infostop_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
)

expect_equal(two_step_labels, infostop_xyt_h$labels)

expect_warning(identify_stops_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "euclidean"
))

#
# trackframe
#
data("path_trackframe", package = "trackframe")

expect_error(identify_stops_xyt(
  x = path_trackframe[, "easting"],
  y = path_trackframe[, "northing"],
  t = path_trackframe[, "time"],
  distance_metric = "haversine"
))
expect_error(identify_stops(data = path_trackframe, distance_metric = "haversine"))

identify_stops_tf_xyt <- identify_stops_xyt(
  x = path_trackframe[, "easting"],
  y = path_trackframe[, "northing"],
  t = path_trackframe[, "time"],
  distance_metric = "euclidean"
)
identify_stops_tf <- identify_stops(data = path_trackframe, distance_metric = "euclidean")
expect_equal(identify_stops_tf_xyt$stop_events, identify_stops_tf$stop_events)

clusters_tf_xyt <- identify_sites(identify_stops_tf_xyt$stop_events, distance_metric = "euclidean")
clusters_tf <- identify_sites(identify_stops_tf$stop_events, distance_metric = "euclidean")
expect_equal(clusters_tf_xyt$stop_events, clusters_tf$stop_events)

two_step_labels_tf_xyt <- match_labels(clusters_tf_xyt, identify_stops_tf_xyt$event_map)
two_step_labels_tf <- match_labels(clusters_tf, identify_stops_tf$event_map)
expect_equal(two_step_labels_tf_xyt, two_step_labels_tf)

infostop_tf <- infostop(data = path_trackframe, distance_metric = "euclidean")
expect_equal(two_step_labels_tf, infostop_tf$labels)

#
# sftrack
#
data("path_sftrack", package = "trackframe")

# only warning here
expect_warning(identify_stops(data = path_sftrack, distance_metric = "euclidean"))

identify_stops_sftrack <- identify_stops(data = path_sftrack, distance_metric = "haversine")
clusters_sftrack <- identify_sites(identify_stops_sftrack$stop_events, distance_metric = "haversine")

two_step_labels_sftrack <- match_labels(clusters_sftrack, identify_stops_sftrack$event_map)

infostop_sftrack <- infostop(data = path_sftrack, distance_metric = "haversine")
expect_equal(two_step_labels_sftrack, infostop_sftrack$labels)

# multiple paths
data("paths_data_frame", package = "trackframe")
expect_error(identify_stops(data = paths_data_frame, distance_metric = "haversine"))
expect_error(identify_stops(data = paths_data_frame, distance_metric = "euclidean"))


#
#trackframe
#
data("paths_trackframe", package = "trackframe")
# wrong distance metric
expect_error(identify_stops(data = paths_trackframe, distance_metric = "haversine"))
# only single tracks are supported in twostep approach
expect_error(identify_stops(data = paths_trackframe, distance_metric = "euclidean"))
