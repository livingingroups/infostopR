# Test Infostop
#
library("tinytest")
library("checkmate")
using("checkmate")

library("infostop")
load("data/tf_paths.RData")

# Note:
# On debian in the terminal the automatic init works as expected.
# However using Positiron
infostop_initialize()
expected <- jsonlite::read_json(system.file("tinytest/extdata/exp_infostop.json", package = "infostop"), simplifyVector = TRUE)
expected_sites_e <- expected[["tf_mat_default_e"]][["expected_sites"]]
expected_sites_h <- expected[["tf_mat_default_h"]][["expected_sites"]]
expected_label_medians_e <- expected[["tf_mat_default_e"]][["expected_label_medians"]]
expected_label_medians_h <- expected[["tf_mat_default_h"]][["expected_label_medians"]]

# FIXME: Add function with arg coerce_to?

#
#
# Test a single path
#
#

#
# Vector input haversine
#
expect_inherits(path_matrix, "matrix")
infostop_xyt_h <- infostop_xyt(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
)
expect_equal(
  NROW(infostop_xyt_h$compute_label_medians()),
  145L
)

expect_equal(
  infostop_xyt_h$labels,
  expected_sites_h
)


# Check that mistakingly running infostop instead of infostop_xyt produces an error.
expect_error(infostop(
  x = path_matrix[, "longitude"],
  y = path_matrix[, "latitude"],
  t = path_matrix[, "time"],
  distance_metric = "haversine"
))

#
# Vector input euclidean
#
coords <- c("longitude", "latitude")
data_sf <- sf::st_as_sf(x = as.data.frame(path_matrix), crs = 4326, coords = coords)
new_data_sf <- sf::st_transform(data_sf, 32633)
x_y <- st_coordinates(new_data_sf[[attr(new_data_sf, "sf_column")]])
colnames(x_y) <- c("easting", "northing")
path_matrix2 <- cbind(path_matrix, x_y)

# check that reconfiguration worked
expect_equal(path_trackframe[, "longitude"], path_matrix2[, "longitude"])
expect_equal(path_trackframe[, "latitude"], path_matrix2[, "latitude"])

expect_equal(path_trackframe[, "easting"], path_matrix2[, "easting"])
expect_equal(path_trackframe[, "northing"], path_matrix2[, "northing"])

infostop_xyt_e <- infostop_xyt(
  x = path_matrix2[, "easting"],
  y = path_matrix2[, "northing"],
  t = path_matrix2[, "time"],
  distance_metric = "euclidean"
)
expect_equal(
  NROW(infostop_xyt_e$compute_label_medians()),
  length(infostop_xyt_e$model$compute_label_medians())
)

expect_equal(
  infostop_xyt_e$labels,
  expected_sites_e
)



#
# data.frame
#

expect_error(infostop_xyt(data = path_data_frame, distance_metric = "euclidean"))
expect_silent(infostop(data = path_data_frame, distance_metric = "euclidean"))

infostop_df_h <- infostop(data = path_data_frame, distance_metric = "haversine")
expect_equal(path_data_frame[, "longitude"], path_matrix[, "longitude"])
expect_equal(path_data_frame[, "latitude"], path_matrix[, "latitude"])
expect_equal(infostop_df_h$labels, expected_sites_h)
expect_equivalent(infostop_df_h$compute_label_medians(), expected_label_medians_h)

#
# trackframe
#
expect_error(infostop(data = path_trackframe, distance_metric = "haversine"))

infostop_tf <- infostop(data = path_trackframe, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_xyt_e$labels)
expect_equivalent(infostop_tf$compute_label_medians(), expected_label_medians_e)

#
# sftrack
#
infostop_sftrack_h <- infostop(data = path_sftrack, distance_metric = "haversine")
expect_error(infostop(data = path_sftrack, distance_metric = "euclidean"))

expect_equal(infostop_sftrack_h$labels, expected_sites_h)
expect_equivalent(infostop_sftrack_h$compute_label_medians(), expected_label_medians_h)

path_sftrack_e <- sf::st_transform(path_sftrack, 32633)
infostop_sftrack_e <- infostop(data = path_sftrack_e, distance_metric = "euclidean")
expect_equal(infostop_sftrack_e$labels, expected_sites_e)
expect_equivalent(infostop_sftrack_e$compute_label_medians(), expected_label_medians_e)
expect_error(infostop(data = path_sftrack_e, distance_metric = "haversine"))

path_sftrack_n <- path_sftrack_e
path_sftrack_n <- st_set_crs(path_sftrack_n, NA_crs_)
expect_error(infostop(data = path_sftrack_n))
expect_error(infostop(data = path_sftrack_n, distance_metric = "haversine"))
infostop_sftrack_n <- infostop(data = path_sftrack_n, distance_metric = "euclidean")
expect_equal(infostop_sftrack_n$labels, expected_sites_e)

#
# move2
#
expect_error(infostop(data = path_move2, distance_metric = "euclidean"))

infostop_move2_h <- infostop(data = path_move2, distance_metric = "haversine")
infostop_move2_a <- infostop(data = path_move2)

expect_equal(infostop_move2_h$labels, expected_sites_h)
expect_equivalent(infostop_move2_h$compute_label_medians(), expected_label_medians_h)
expect_equal(infostop_move2_a$labels, expected_sites_h)


#
#
# Test multiple paths
#
#
expected_sites_e_multi <- expected[["tf_default_e"]][["expected_sites"]]
expected_label_medians_e_multi <- expected[["tf_default_e"]][["expected_label_medians"]]

#
# data.frame
#
expect_silent(infostop(data = paths_data_frame, distance_metric = "euclidean"))

infostop_df <- infostop(data = paths_data_frame, distance_metric = "haversine")
expect_equal(length(infostop_df$labels), length(unique(paths_data_frame$id)))

#
# trackframe
#
expect_error(infostop(data = paths_trackframe, distance_metric = "haversine"))

infostop_mtf <- infostop(data = paths_trackframe, distance_metric = "euclidean")
expect_equal(unlist(infostop_mtf$labels), expected_sites_e_multi)

tf1 <- trackframe::split_by_id(paths_trackframe)[[1]]
infostop_mtf1 <- infostop(data = tf1, distance_metric = "euclidean")



# Due to label switching we cannot compare directly, but can only
# compare the relabeled.
relabel <- function(x) {
  idx <- unique(na.omit(x))
  match(x, idx)
}

# Compare labels
labels_multi <- relabel(infostop_mtf$labels[[1]])
labels_single <- relabel(infostop_mtf1$labels)
labels_ref <- relabel(expected_sites_e)
expect_equal(labels_single, labels_ref)
expect_equal(labels_multi, labels_ref)

#
# sftrack
#
expect_error(infostop(data = paths_sftrack, distance_metric = "euclidean"))

infostop_msftrack <- infostop(data = paths_sftrack, distance_metric = "haversine")

paths_sftrack_e <- sf::st_transform(paths_sftrack, 32633)
infostop_msftrack_e <- infostop(data = paths_sftrack_e, distance_metric = "euclidean")
expect_equal(unlist(infostop_msftrack_e$labels), expected_sites_e_multi) 
expect_equivalent(infostop_msftrack_e$compute_label_medians(), expected_label_medians_e_multi)
# correct number of tracks
expect_equal(length(infostop_msftrack$labels), length(unique(paths_sftrack$id)))
