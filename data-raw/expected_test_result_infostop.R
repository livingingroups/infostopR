# Updates the reference (expected) outputs for test_infostop.R
withr::with_dir(
  'inst/tinytest',
  source("test_infostop.R")
)
jsonlite::write_json(
  list(
    tf_default_e = list(
      params = list(
        data = "trackframe::paths_trackframe",
        distance_metric = "euclidean"
      ),
      expected_sites = unlist(infostop_mtf$labels),
      expected_label_medians = infostop_mtf$compute_label_medians()
    ),
    tf_mat_default_e = list(
      params = list(
        data = "trackframe::path_matrix",
        distance_metric = "euclidean"
      ),
      expected_sites = infostop_xyt_e$labels,
      expected_label_medians = infostop_xyt_e$compute_label_medians()
    ),
    tf_mat_default_h = list(
      params = list(
        data = "trackframe::path_matrix",
        distance_metric = "haversine"
      ),
      expected_sites = infostop_xyt_h$labels,
      expected_label_medians = infostop_xyt_h$compute_label_medians()
    )
  ),
  "inst/tinytest/extdata/exp_infostop.json",
  digits = options('digits')[[1]],
  pretty = TRUE,
  auto_unbox = TRUE
)
