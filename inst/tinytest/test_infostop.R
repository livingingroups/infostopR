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
expected <- jsonlite::read_json(
  system.file("tinytest/extdata/exp_infostop.json", package = "infostop"),
  simplifyVector = TRUE
)
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
infostop_xyt_h <- infostop_lonlatt(
  path_matrix[, "longitude"],
  path_matrix[, "latitude"],
  t = path_matrix[, "time"]
)
expect_equal(
  NROW(infostop_xyt_h$compute_label_medians()),
  143L
)

expect_equal(
  infostop_xyt_h$labels,
  expected_sites_h
)


# Check that mistakingly running infostop instead of infostop_xyt produces an error.
expect_error(infostop(
  path_matrix[, "longitude"],
  path_matrix[, "latitude"],
  path_matrix[, "time"]
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
  t = path_matrix2[, "time"]
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

path_data_frame <- data.frame(
  x = easting(path_trackframe),
  y = northing(path_trackframe),
  t = time(path_trackframe),
  id = "track_1"
)
expect_error(infostop_xyt(data = path_data_frame))
expect_silent(infostop(data = path_data_frame, crs = NA))

infostop_df_e <- infostop(data = path_data_frame, crs = NA)
expect_equal(infostop_df_e$labels, expected_sites_e)
expect_equivalent(infostop_df_e$compute_label_medians(), expected_label_medians_e)

#
# trackframe
#

infostop_tf <- infostop(data = path_trackframe)

expect_equal(infostop_tf$labels, infostop_xyt_e$labels)
expect_equivalent(infostop_tf$compute_label_medians(), expected_label_medians_e)

#
# sftrack
#
infostop_sftrack_h <- infostop(data = path_sftrack)

expect_equal(infostop_sftrack_h$labels, expected_sites_h)
expect_equivalent(infostop_sftrack_h$compute_label_medians(), expected_label_medians_h)

path_sftrack_e <- sf::st_transform(path_sftrack, 32633)
infostop_sftrack_e <- infostop(data = path_sftrack_e)
expect_equal(infostop_sftrack_e$labels, expected_sites_e)
expect_equivalent(infostop_sftrack_e$compute_label_medians(), expected_label_medians_e)

path_sftrack_n <- path_sftrack_e
path_sftrack_n <- st_set_crs(path_sftrack_n, NA_crs_)
infostop_sftrack_n <- infostop(data = path_sftrack_n)
expect_equal(infostop_sftrack_n$labels, expected_sites_e)

#
# move2
#

infostop_move2_h <- infostop(data = path_move2)

expect_equal(infostop_move2_h$labels, expected_sites_h)
expect_equivalent(infostop_move2_h$compute_label_medians(), expected_label_medians_h)


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

# data frame + no crs = error
df <- as.data.frame(paths_trackframe[,c('easting', 'northing', 'id', 'time')])
expect_error(infostop(data = df))

infostop_df <- infostop(data = df, crs = NA)
expect_equal(length(infostop_df$labels), length(unique(df$id)))

#
# trackframe
#

infostop_mtf <- infostop(data = paths_trackframe)
expect_equal(unlist(infostop_mtf$labels), expected_sites_e_multi)

tf1 <- trackframe::split_by_id(paths_trackframe)[[1]]
infostop_mtf1 <- infostop(data = tf1)


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

infostop_msftrack <- infostop(data = paths_sftrack)

paths_sftrack_e <- sf::st_transform(paths_sftrack, 32633)
infostop_msftrack_e <- infostop(data = paths_sftrack_e)
expect_equal(unlist(infostop_msftrack_e$labels), expected_sites_e_multi)
expect_equivalent(infostop_msftrack_e$compute_label_medians(), expected_label_medians_e_multi)
# correct number of tracks
expect_equal(length(infostop_msftrack$labels), length(unique(paths_sftrack$id)))
