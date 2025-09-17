# Test Inofstop
#
library("tinytest")
library("checkmate")
using("checkmate")

library("infostop")
# Note:
# On debian in the terminal the automatic init works as expected.
# However using Positiron
infostop_initialize()

# FIXME: Add function with arg coerce_to?

#
#
# Test a single path
#
#

#
# Vector input haversine
#
data("path_matrix", package = "trackframe")
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

data("path_trackframe", package = "trackframe")
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


#
# data.frame
#
data("path_data_frame", package = "trackframe")

expect_error(infostop_xyt(data = path_data_frame, distance_metric = "euclidean"))
expect_silent(infostop(data = path_data_frame, distance_metric = "euclidean"))

infostop_df_h <- infostop(data = path_data_frame, distance_metric = "haversine")
expect_equal(path_data_frame[, "longitude"], path_matrix[, "longitude"])
expect_equal(path_data_frame[, "latitude"], path_matrix[, "latitude"])
expect_equal(infostop_df_h$labels, infostop_xyt_h$labels)
expect_equal(infostop_df_h$compute_label_medians(), infostop_xyt_h$compute_label_medians())

#
# trackframe
#
expect_error(infostop(data = path_trackframe, distance_metric = "haversine"))

infostop_tf <- infostop(data = path_trackframe, distance_metric = "euclidean")

expect_equal(infostop_tf$labels, infostop_xyt_e$labels)
expect_equal(infostop_tf$compute_label_medians(), infostop_xyt_e$compute_label_medians())

#
# sftrack
#
data("path_sftrack", package = "trackframe")
path_sftrack <- path_sftrack
infostop_sftrack_h <- infostop(data = path_sftrack, distance_metric = "haversine")
expect_error(infostop(data = path_sftrack, distance_metric = "euclidean"))

expect_equal(infostop_sftrack_h$labels, infostop_xyt_h$labels)
expect_equal(infostop_sftrack_h$compute_label_medians(), infostop_xyt_h$compute_label_medians())

path_sftrack_e <- sf::st_transform(path_sftrack, 32633)
infostop_sftrack_e <- infostop(data = path_sftrack_e, distance_metric = "euclidean")
expect_equal(infostop_sftrack_e$labels, infostop_xyt_e$labels)
expect_equal(infostop_sftrack_e$compute_label_medians(), infostop_xyt_e$compute_label_medians())
expect_error(infostop(data = path_sftrack_e, distance_metric = "haversine"))

path_sftrack_n <- path_sftrack_e
path_sftrack_n <- st_set_crs(path_sftrack_n, NA_crs_)
expect_error(infostop(data = path_sftrack_n))
expect_error(infostop(data = path_sftrack_n, distance_metric = "haversine"))
infostop_sftrack_n <- infostop(data = path_sftrack_n, distance_metric = "euclidean")
expect_equal(infostop_sftrack_n$labels, infostop_sftrack_e$labels)

#
# move2
#
data("path_move2", package = "trackframe")
expect_error(infostop(data = path_move2, distance_metric = "euclidean"))

infostop_move2_h <- infostop(data = path_move2, distance_metric = "haversine")
infostop_move2_a <- infostop(data = path_move2)

expect_equal(infostop_move2_h$labels, infostop_xyt_h$labels)
expect_equal(infostop_move2_h$compute_label_medians(), infostop_xyt_h$compute_label_medians())
expect_equal(infostop_move2_a$labels, infostop_move2_h$labels)


#
#
# Test multiple paths
#
#

#
# data.frame
#
data("paths_data_frame", package = "trackframe")
expect_silent(infostop(data = paths_data_frame, distance_metric = "euclidean"))

infostop_df <- infostop(data = paths_data_frame, distance_metric = "haversine")
expect_equal(length(infostop_df$labels), length(unique(paths_data_frame$id)))

#
# trackframe
#
data("paths_trackframe", package = "trackframe")
expect_error(infostop(data = paths_trackframe, distance_metric = "haversine"))

infostop_mtf <- infostop(data = paths_trackframe, distance_metric = "euclidean")

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
expect_equal(labels_multi, labels_single)


# Per definition the medians are different, since the multi id median contains
# also the locations from the other individuals assigned to the same stop location.
# But they should not be to far appart.
medians_multi <- infostop_mtf$compute_label_medians()[infostop_mtf$labels[[1]], ]
medians_single <- infostop_mtf1$compute_label_medians()[infostop_mtf1$labels, ]
max_abs_err_1 <- max(abs(medians_multi[, 1] - medians_single[, 1]), na.rm = TRUE)
max_abs_err_2 <- max(abs(medians_multi[, 2] - medians_single[, 2]), na.rm = TRUE)

# If we say the error should be less than 1 promille we should be fine, since even
# they don't have to be the same they have to be similar.
expect_true(
  max_abs_err_1 / max(abs(c(medians_multi[, 1], medians_single[, 1])), na.rm = TRUE) <= 1e-3
)
expect_true(
  max_abs_err_2 / max(abs(c(medians_multi[, 2], medians_single[, 2])), na.rm = TRUE) <= 1e-3
)

#
# sftrack
#
data("paths_sftrack", package = "trackframe")
expect_error(infostop(data = paths_sftrack, distance_metric = "euclidean"))

infostop_msftrack <- infostop(data = paths_sftrack, distance_metric = "haversine")

paths_sftrack_e <- sf::st_transform(paths_sftrack, 32633)
infostop_msftrack_e <- infostop(data = paths_sftrack_e, distance_metric = "euclidean")
expect_equal(infostop_msftrack_e$labels, infostop_mtf$labels)
expect_equal(infostop_msftrack_e$compute_label_medians(), infostop_mtf$compute_label_medians())
expect_equal(length(infostop_msftrack$labels), length(unique(paths_sftrack$id)))
