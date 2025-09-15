if (TRUE) {
  library("tinytest")
}

library("infostop")

infostop_initialize()

#FIXME add function with arg coerce_to

# single path
# matrix #FIXME
#single path
#matrix
data("path_matrix", package = "trackframe")
expect_inherits(path_matrix, "matrix")
find_stops_xyt_h <- find_stops_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
)

clusters <- spatial_infomap(find_stops_xyt_h$stop_events)
two_step_labels <- match_labels(clusters, find_stops_xyt_h$event_map)

infostop_xyt_h <- infostop_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
)

# cbind(as.numeric(two_step_labels), infostop_xyt_h$labels)
expect_equal(two_step_labels, infostop_xyt_h$labels)

expect_warning(find_stops_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "euclidean"
))

# trackframe
data("path_trackframe", package = "trackframe")

expect_error(find_stops_xyt(
  x = path_trackframe[, "easting"],
  y = path_trackframe[, "northing"],
  t = path_trackframe[, "time"],
  distance_metric = "haversine"
))
expect_error(find_stops(data = path_trackframe, distance_metric = "haversine"))

find_stops_tf_xyt <- find_stops_xyt(
  x = path_trackframe[, "easting"],
  y = path_trackframe[, "northing"],
  t = path_trackframe[, "time"],
  distance_metric = "euclidean"
)
find_stops_tf <- find_stops(data = path_trackframe, distance_metric = "euclidean")
expect_equal(find_stops_tf_xyt$stop_events, find_stops_tf$stop_events)

clusters_tf_xyt <- spatial_infomap(find_stops_tf_xyt$stop_events, distance_metric = "euclidean")
clusters_tf <- spatial_infomap(find_stops_tf$stop_events, distance_metric = "euclidean")
expect_equal(clusters_tf_xyt$stop_events, clusters_tf$stop_events)

two_step_labels_tf_xyt <- match_labels(clusters_tf_xyt, find_stops_tf_xyt$event_map)
two_step_labels_tf <- match_labels(clusters_tf, find_stops_tf$event_map)
expect_equal(two_step_labels_tf_xyt, two_step_labels_tf)

infostop_tf <- infostop(data = path_trackframe, distance_metric = "euclidean")
expect_equal(two_step_labels_tf, infostop_tf$labels)

#sftrack
data("path_sftrack", package = "trackframe")

#only warning here
expect_warning(find_stops(data = path_sftrack, distance_metric = "euclidean"))

find_stops_sftrack <- find_stops(data = path_sftrack, distance_metric = "haversine")
clusters_sftrack <- spatial_infomap(find_stops_sftrack$stop_events, distance_metric = "haversine")

two_step_labels_sftrack <- match_labels(clusters_sftrack, find_stops_sftrack$event_map)

infostop_sftrack <- infostop(data = path_sftrack, distance_metric = "haversine")
expect_equal(two_step_labels_sftrack, infostop_sftrack$labels)

# multiple paths
#multiple paths
#data.frame #FIXME: should this be supported?
data("paths_data_frame", package = "trackframe")
expect_error(find_stops(data = paths_data_frame, distance_metric = "haversine"))

expect_error(find_stops(data = paths_data_frame, distance_metric = "euclidean"))

# find_stops_df <- find_stops(data = paths_data_frame,
#                         distance_metric = "euclidean")
#
# clusters_df <- spatial_infomap(find_stops_df$stop_events, distance_metric = "euclidean")
# clusters_df$labels
#
# two_step_labels_df <- match_labels(clusters_df, find_stops_df$event_map)
#
# expect_equal(length(find_stops_df$labels), length(unique(paths_data_frame$id)))
#
# infostop_df <- infostop(data = paths_data_frame,
#                             distance_metric = "euclidean")
#
# infostop_df$labels

#trackframe
data("paths_trackframe", package = "trackframe")
expect_error(find_stops(data = paths_trackframe, distance_metric = "haversine"))

expect_error(find_stops(data = paths_trackframe, distance_metric = "euclidean"))
