library("tinytest")
library("infostop")


load_data <- function(envir = parent.frame(), package = "base") {
  file <- system.file("tinytest/data/tf_paths.RData", package = "infostop")
  load(file, envir = envir)
}

read_expected <- function() {
  file <- system.file("tinytest/extdata/exp_onestep_twostep.json", package = "infostop")
  jsonlite::read_json(file, simplifyVector = TRUE)
}


load_data()
expected <- read_expected()


#
# trackframe
#
stops_tf <- identify_stops(paths_trackframe)
sites_tf <- identify_sites(stops_tf, r2 = 50)
expect_equal(min(stops_tf[["stop_id"]], na.rm = TRUE), 1)
expect_equal(
  stops_tf[["stop_id"]],
  expected[["tf_pkg_r2_50"]][["expected_stops"]]
)
expect_equal(
  sites_tf[["site_id"]],
  # The expected starts with 2, which is not correct.
  expected[["tf_pkg_r2_50"]][["expected_sites"]] - 1L
)


#
# move2
#
stops_mo <- identify_stops(paths_move2)
sites_mo <- identify_sites(stops_mo, r2 = 10)
expect_equal(
  stops_mo[["stop_id"]],
  expected[["tf_default_h"]][["expected_stops"]]
)
expect_equal(
  sites_mo[["site_id"]],
  expected[["tf_default_h"]][["expected_sites"]] - 1L
)


#
# sftrack
#
stops_sft <- identify_stops(paths_sftrack)
sites_sft <- identify_sites(stops_sft, r2 = 10)
expect_equal(
  stops_sft[["stop_id"]],
  expected[["tf_default_h"]][["expected_stops"]]
)
expect_equal(
  sites_sft[["site_id"]],
  expected[["tf_default_h"]][["expected_sites"]] - 1L
)


#
# identify_stops_xyt
#
data <- infostop:::example_data()
stops_xyt <- identify_stops_longlatt(
  latitude = data[, 1],
  longitude = data[, 2],
  t = data[, 3],
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2
)
py_labels <- stops_xyt$event_map + 1
py_labels[py_labels == 0] <- NA
expect_equal(py_labels, expected[["infostop_pkg_example"]][["expected_stops"]])
