library("tinytest")
library("infostop")
library("trackframe")
library("data.table")
library("tibble")

projected_crs <- "EPSG:32632"
df <- as.data.frame(path_trackframe[, c("easting", "northing", "time", "id")])
dt <- data.table::as.data.table(df)
tbl <- tibble::as_tibble(df)

tf_df <- as.trackframe(df, crs = projected_crs)
tf_dt <- as.trackframe(dt, crs = projected_crs, coerce_to = "data.table")
tf_tbl <- as.trackframe(tbl, crs = projected_crs, coerce_to = "tibble")

expect_true(inherits(tf_dt, "trackframe"))
expect_true(inherits(tf_tbl, "trackframe"))

stops_df <- identify_stops(tf_df, distance_metric = "euclidean")
stops_dt <- identify_stops(tf_dt, distance_metric = "euclidean")
stops_tbl <- identify_stops(tf_tbl, distance_metric = "euclidean")
expect_equal(stops_df[["stop_id"]], stops_dt[["stop_id"]])
expect_equal(stops_df[["stop_id"]], stops_tbl[["stop_id"]])

sites_df <- identify_sites(stops_df, distance_metric = "euclidean")
sites_dt <- identify_sites(stops_dt, distance_metric = "euclidean")
sites_tbl <- identify_sites(stops_tbl, distance_metric = "euclidean")
expect_equal(sites_df[["site_id"]], sites_dt[["site_id"]])
expect_equal(sites_df[["site_id"]], sites_tbl[["site_id"]])

expect_equal(stop_centers(stops_df), stop_centers(stops_dt))
expect_equal(stop_centers(stops_df), stop_centers(stops_tbl))
expect_equal(site_centers(sites_df), site_centers(sites_dt))
expect_equal(site_centers(sites_df), site_centers(sites_tbl))
