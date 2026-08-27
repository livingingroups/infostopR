library("tinytest")
library("infostop")

load_data <- function(envir = parent.frame()) {
  file <- system.file("tinytest/data/tf_paths.RData", package = "infostop")
  load(file, envir = envir)
}

load_data()

check_order_invariance <- function(data) {
  stops <- identify_stops(data)
  sites <- identify_sites(stops, r2 = 50, seed = 14L)

  permutation <- sample.int(nrow(data))
  data_unsorted <- data[permutation, ]
  stops_unsorted <- identify_stops(data_unsorted)
  sites_unsorted <- identify_sites(stops_unsorted, r2 = 50, seed = 14L)

  expect_equal(
    stops_unsorted[["stop_id"]][order(permutation)],
    stops[["stop_id"]]
  )
  expect_equal(
    sites_unsorted[["site_id"]][order(permutation)],
    sites[["site_id"]]
  )
}

set.seed(as.integer(Sys.time()))
check_order_invariance(paths_trackframe)

if (requireNamespace("move2", quietly = TRUE)) {
  check_order_invariance(paths_move2)
}
if (requireNamespace("sftrack", quietly = TRUE)) {
  check_order_invariance(paths_sftrack)
}
