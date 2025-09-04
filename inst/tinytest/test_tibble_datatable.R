library(tinytest)
library(trackframe)


# onestep - single track
data("path_data_frame", package = "trackframe")
head(path_data_frame)
tf <- as.trackframe(path_data_frame, crs_input = 4326)
infostop_tf <- infostop(data = tf, distance_metric = "euclidean")

tf_tibble <- as.trackframe(path_data_frame, crs_input = 4326, coerce_to = "tibble")
infostop_tibble <- infostop(data = tf, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_tibble$labels)

tf_dt <- as.trackframe(path_data_frame, crs_input = 4326, coerce_to = "data.table")
infostop_dt <- infostop(data = tf_dt, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_dt$labels)


# twostep - single track
find_stops_tf <- find_stops(data = tf,
                            distance_metric = "euclidean")
clusters_tf <- spatial_infomap(find_stops_tf$stop_events, distance_metric = "euclidean")
two_step_labels_tf <- match_labels(clusters_tf, find_stops_tf$event_map)
expect_equal(two_step_labels_tf, infostop_tf$labels)

find_stops_tibble <- find_stops(data = tf_tibble,
                            distance_metric = "euclidean")
clusters_tibble <- spatial_infomap(find_stops_tibble$stop_events, distance_metric = "euclidean")
two_step_labels_tibble <- match_labels(clusters_tibble, find_stops_tibble$event_map)
expect_equal(two_step_labels_tibble, infostop_tf$labels)

find_stops_dt <- find_stops(data = tf_dt,
                                distance_metric = "euclidean")
clusters_dt <- spatial_infomap(find_stops_dt$stop_events, distance_metric = "euclidean")
two_step_labels_dt <- match_labels(clusters_dt, find_stops_dt$event_map)
expect_equal(two_step_labels_dt, infostop_tf$labels)


# onestep - multipe tracks
data("paths_data_frame", package = "trackframe")
head(paths_data_frame)
tf <- as.trackframe(paths_data_frame, crs_input = 4326)
infostop_tf <- infostop(data = tf, distance_metric = "euclidean")

tf_tibble <- as.trackframe(paths_data_frame, crs_input = 4326, coerce_to = "tibble")
infostop_tibble <- infostop(data = tf, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_tibble$labels)

tf_dt <- as.trackframe(paths_data_frame, crs_input = 4326, coerce_to = "data.table")
infostop_dt <- infostop(data = tf_dt, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_dt$labels)
