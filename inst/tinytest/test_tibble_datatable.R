library("infostop")
library("tinytest")
library("trackframe")

projected_crs <- "EPSG:32632"

#
# onestep - single track
#
path_df <- as.data.frame(path_trackframe[, c(
  'easting',
  'northing',
  'time',
  'id'
)])
tf <- as.trackframe(path_df, crs = projected_crs)
infostop_tf <- infostop(data = tf, distance_metric = "euclidean")

tf_tibble <- as.trackframe(path_df, crs = projected_crs, coerce_to = "tibble")
infostop_tibble <- infostop(data = tf, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_tibble$labels)

tf_dt <- as.trackframe(path_df, crs = projected_crs, coerce_to = "data.table")
infostop_dt <- infostop(data = tf_dt, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_dt$labels)

#
# twostep - single track
#
identify_stops_tf <- identify_stops(data = tf, distance_metric = "euclidean")
clusters_tf <- identify_sites(identify_stops_tf, distance_metric = "euclidean")
expect_equal(clusters_tf[["site_id"]], infostop_tf[["labels"]])

identify_stops_tibble <- identify_stops(
  data = tf_tibble,
  distance_metric = "euclidean"
)
clusters_tibble <- identify_sites(
  identify_stops_tibble,
  distance_metric = "euclidean"
)
expect_equal(clusters_tibble[["site_id"]], infostop_tf[["labels"]])

identify_stops_dt <- identify_stops(data = tf_dt, distance_metric = "euclidean")
clusters_dt <- identify_sites(identify_stops_dt, distance_metric = "euclidean")
expect_equal(clusters_dt[["site_id"]], infostop_tf[["labels"]])


#
# onestep - multipe tracks
#
paths_df <- as.data.frame(paths_trackframe[, c(
  'easting',
  'northing',
  'id',
  'time'
)])
tf <- as.trackframe(paths_df, crs = projected_crs)
infostop_tf <- infostop(data = tf, distance_metric = "euclidean")

tf_tibble <- as.trackframe(paths_df, crs = projected_crs, coerce_to = "tibble")
infostop_tibble <- infostop(data = tf, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_tibble$labels)

tf_dt <- as.trackframe(paths_df, crs = projected_crs, coerce_to = "data.table")
infostop_dt <- infostop(data = tf_dt, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_dt$labels)
