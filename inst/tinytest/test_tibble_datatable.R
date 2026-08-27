library("infostop")
library("tinytest")
library("trackframe")

projected_crs <- "EPSG:32632"

#
# Single track
#
path_df <- as.data.frame(path_trackframe[, c('easting', 'northing', 'time', 'id')])
tf <- as.trackframe(path_df, crs = projected_crs)
tf_dt <- as.trackframe(path_df, crs = projected_crs, coerce_to = "data.table")
tf_tbl <- as.trackframe(path_df, crs = projected_crs, coerce_to = "tibble")


stops_tf <- identify_stops(data = tf, distance_metric = "euclidean")
stops_dt <- identify_stops(data = tf_dt, distance_metric = "euclidean")
stops_tbl <- identify_stops(data = tf_tbl, distance_metric = "euclidean")
expect_equal(stops_tf[["stop_id"]], stops_dt[["stop_id"]])
expect_equal(stops_tf[["stop_id"]], stops_tbl[["stop_id"]])


clusters_tf <- identify_sites(stops_tf, distance_metric = "euclidean")
clusters_dt <- identify_sites(stops_dt, distance_metric = "euclidean")
clusters_tbl <- identify_sites(stops_tbl, distance_metric = "euclidean")
expect_equal(clusters_tf[["site_id"]], clusters_dt[["site_id"]])
expect_equal(clusters_tf[["site_id"]], clusters_tbl[["site_id"]])


#
# Multipe tracks
#
paths_df <- as.data.frame(paths_trackframe[, c('easting', 'northing', 'id', 'time')])
tf <- as.trackframe(paths_df, crs = projected_crs)
tf_dt <- as.trackframe(paths_df, crs = projected_crs, coerce_to = "data.table")
tf_tbl <- as.trackframe(paths_df, crs = projected_crs, coerce_to = "tibble")


mstops_tf <- identify_stops(data = tf, distance_metric = "euclidean")
mstops_dt <- identify_stops(data = tf_dt, distance_metric = "euclidean")
mstops_tbl <- identify_stops(data = tf_tbl, distance_metric = "euclidean")
expect_equal(mstops_tf[["stop_id"]], mstops_dt[["stop_id"]])
expect_equal(mstops_tf[["stop_id"]], mstops_tbl[["stop_id"]])


clusters_tf <- identify_sites(mstops_tf, distance_metric = "euclidean")
clusters_dt <- identify_sites(mstops_dt, distance_metric = "euclidean")
clusters_tbl <- identify_sites(mstops_tbl, distance_metric = "euclidean")
expect_equal(clusters_tf[["site_id"]], clusters_dt[["site_id"]])
expect_equal(clusters_tf[["site_id"]], clusters_tbl[["site_id"]])
