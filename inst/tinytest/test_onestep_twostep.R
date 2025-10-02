library("tinytest")
library("infostop")

infostop_initialize()

load('data/tf_paths.RData')
expected <- jsonlite::read_json(system.file("tinytest/extdata/exp_onestep_twostep.json", package = "infostop"), simplifyVector = TRUE)


#
# trackframe
#
stops_tf <- identify_stops(paths_trackframe, distance_metric = "euclidean")
sites_tf <- identify_sites(stops_tf, r2 = 50, distance_metric = "euclidean")
sites_tf_onestep <- unlist(infostop(paths_trackframe, r2 = 50)$labels)
expect_equal(stops_tf[["stop_id"]], expected[["tf_pkg_r2_50"]][["expected_stops"]])
expect_equal(sites_tf[["site_id"]], expected[["tf_pkg_r2_50"]][["expected_sites"]])
expect_equal(sites_tf_onestep, expected[["tf_pkg_r2_50"]][["expected_sites"]])

#
# move2
#
stops_mo <- identify_stops(paths_move2, distance_metric = "haversine")
sites_mo <- identify_sites(stops_mo, r2 = 10, distance_metric = "haversine")
sites_mo_onestep <- unlist(infostop(paths_move2, r2 = 10)$labels)
expect_equal(stops_mo[["stop_id"]], expected[["tf_default_h"]][["expected_stops"]])
expect_equal(sites_mo[["site_id"]], expected[["tf_default_h"]][["expected_sites"]])
expect_equal(sites_mo_onestep, expected[["tf_default_h"]][["expected_sites"]])


#
# sftrack
#
stops_sft <- identify_stops(paths_sftrack, distance_metric = "haversine")
sites_sft <- identify_sites(stops_sft, r2 = 10, distance_metric = "haversine")
sites_sft_onestep <- unlist(infostop(paths_sftrack, r2 = 10)$labels)
expect_equal(stops_sft[["stop_id"]], expected[["tf_default_h"]][["expected_stops"]])
expect_equal(sites_sft[["site_id"]], expected[["tf_default_h"]][["expected_sites"]])
expect_equal(sites_sft_onestep, expected[["tf_default_h"]][["expected_sites"]])


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
py_labels <- stops_xyt$event_map[[1]] + 1
py_labels[py_labels == 0] <- NA
expect_equal(py_labels, expected[["infostop_pkg_example"]][["expected_stops"]])


sites_xyt <- infostop_xyt(
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

# check that stops and sites are identical?
expect_equal(sites_xyt$labels, py_labels)
expect_equal(sites_xyt$labels, expected[["infostop_pkg_example"]][["expected_sites"]])
