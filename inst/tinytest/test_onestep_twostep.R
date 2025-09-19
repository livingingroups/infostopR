library("tinytest")
library("infostop")

infostop_initialize()

data(package = "trackframe")
data(paths_trackframe, package = "trackframe")
data(paths_move2, package = "trackframe")
data(paths_sftrack, package = "trackframe")


#
# trackframe
#
stops_tf <- identify_stops(paths_trackframe, distance_metric = "euclidean")
sites_tf <- identify_sites(stops_tf, r2 = 50, distance_metric = "euclidean")
sites_ref <- unlist(infostop(paths_trackframe, r2 = 50)$labels)
expect_equal(sites_ref, sites_tf[["site_id"]])


#
# move2
#
stops_mo <- identify_stops(paths_move2, distance_metric = "haversine")
sites_mo <- identify_sites(stops_mo, r2 = 10, distance_metric = "haversine")
sites_ref <- unlist(infostop(paths_move2, r2 = 10)$labels)
expect_equal(sites_ref, sites_mo[["site_id"]])


#
# sftrack
#
stops <- identify_stops(paths_sftrack, distance_metric = "haversine")
sites <- identify_sites(stops, r2 = 10, distance_metric = "haversine")
ref_sites <- unlist(infostop(paths_sftrack, r2 = 10)$labels)
expect_equal(ref_sites, sites[["site_id"]])


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

py_labels <- stops_xyt$event_map[[1]] + 1
py_labels[py_labels == 0] <- NA
expect_equal(sites_xyt$labels, py_labels)
