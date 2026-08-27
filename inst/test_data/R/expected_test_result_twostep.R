# Updates the reference (expected) outputs for test_onestep_twostep.R
withr::with_dir(
  'inst/tinytest',
  source("test_onestep_twostep.R")
)
jsonlite::write_json(
  list(
    tf_pkg_r2_50 = list(
      params = list(
        data = "trackframe::paths_trackframe",
        distance_metric = "euclidean",
        r2 = 50
      ),
      expected_stops = stops_tf$stop_id,
      expected_sites = sites_tf$site_id
    ),
    tf_default_h = list(
      params = list(
        data = "trackframe::paths_trackframe",
        distance_metric = "haversine"
      ),
      expected_stops = stops_mo$stop_id,
      expected_sites = sites_mo$site_id
    ),
    infostop_pkg_example = list(
      params = list(
        data = "infostop::example_data()",
        distance_metric = "haversine",
        r1 = 100,
        r2 = 50,
        min_staying_time = 300,
        max_time_between = 86400,
        min_size = 2
      ),
      expected_stops = sites_xyt$labels,
      expected_sites = sites_xyt$labels
    )
  ),
  "inst/tinytest/extdata/exp_onestep_twostep.json",
  digits = options('digits')[[1]],
  pretty = TRUE,
  auto_unbox = TRUE
)
