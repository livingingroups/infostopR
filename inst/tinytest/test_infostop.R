if (FALSE) {
  library("tinytest")
}


library("travelpaths")
library("infostop")

infostop_initialize()

# matrix
data("travel_path_matrix", package = "travelpaths")
expect_inherits(travel_path_matrix, "matrix")
onestep_matrix <- infostop(data = travel_path_matrix[, c("latitude", "longitude", "time")], distance_metric = "haversine")
expect_equal(onestep_matrix$compute_label_medians(), c(16.37759, 48.20582), tolerance = 1e-06)
expect_warning(infostop(data = travel_path_matrix[, c("latitude", "longitude", "time")], distance_metric = "euclidean"))

stops_matrix <- find_stops(data = travel_path_matrix[, c("latitude", "longitude", "time")], distance_metric = "haversine")
expect_warning(find_stops(data = travel_path_matrix[, c("latitude", "longitude", "time")], distance_metric = "euclidean"))

twostep_matrix <- spatial_infomap(data = stops_matrix$coordinates, distance_metric = "haversine")

expect_equal(length(onestep_matrix$labels), length(twostep_matrix$labels))
expect_equal(onestep_matrix$labels, twostep_matrix$labels)


# data.frame
data("travel_path_data_frame", package = "travelpaths")

expect_inherits(travel_path_data_frame, "data.frame")
onestep_data_frame <- infostop(data = travel_path_data_frame[, c("latitude", "longitude", "time")], distance_metric = "haversine")
expect_equal(onestep_data_frame$compute_label_medians(), c(16.37759, 48.20582), tolerance = 1e-06)
expect_warning(infostop(data = travel_path_data_frame[, c("latitude", "longitude", "time")], distance_metric = "euclidean"))

stops_data_frame <- find_stops(data = travel_path_data_frame[, c("latitude", "longitude", "time")], distance_metric = "haversine")
expect_warning(find_stops(data = travel_path_data_frame[, c("latitude", "longitude", "time")], distance_metric = "euclidean"))

twostep_data_frame <- spatial_infomap(data = stops_data_frame$coordinates, distance_metric = "haversine")

expect_equal(length(onestep_data_frame$labels), length(twostep_data_frame$labels))
expect_equal(onestep_data_frame$labels, twostep_data_frame$labels)

expect_equal(onestep_data_frame$labels, onestep_matrix$labels)
expect_equal(stops_data_frame$labels, stops_matrix$labels)
expect_equal(twostep_data_frame$labels, twostep_matrix$labels)

# trackframe
data("travel_path_trackframe", package = "travelpaths")

expect_inherits(travel_path_trackframe, "trackframe")
expect_error(infostop(data = travel_path_trackframe, distance_metric = "haversine"))
onestep_trackframe <- infostop(data = travel_path_trackframe, distance_metric = "euclidean")

expect_equal(onestep_trackframe$compute_label_medians(), c(602351.3, 5340094.6), tolerance = 1e-06)

stops_trackframe <- find_stops(data = travel_path_trackframe, distance_metric = "euclidean")
expect_error(find_stops(data = travel_path_trackframe, distance_metric = "haversine"))

twostep_trackframe <- spatial_infomap(data = stops_trackframe$coordinates, distance_metric = "euclidean")
expect_error(spatial_infomap(data = stops_trackframe$coordinates, distance_metric = "haversine"))

expect_equal(length(onestep_trackframe$labels), length(twostep_trackframe$labels))
expect_equal(onestep_trackframe$labels, twostep_trackframe$labels)

# expect_equal(onestep_trackframe$labels, onestep_matrix$labels)
# expect_equal(stops_trackframe$labels, stops_matrix$labels)
# expect_equal(twostep_trackframe$labels, twostep_matrix$labels)

# sftrack
data("travel_path_sftrack", package = "travelpaths")

onestep_sftrack <- infostop(data = travel_path_sftrack, distance_metric = "haversine")
expect_equal(onestep_sftrack$compute_label_medians(), c(16.37759, 48.20582), tolerance = 1e-06)
expect_warning(infostop(data = travel_path_sftrack, distance_metric = "euclidean"))

stops_sftrack <- find_stops(data = travel_path_sftrack, distance_metric = "haversine") 
expect_warning(find_stops(data = travel_path_sftrack, distance_metric = "euclidean"))

twostep_sftrack <- spatial_infomap(data = travel_path_sftrack, distance_metric = "haversine")
expect_warning(spatial_infomap(data = travel_path_sftrack, distance_metric = "euclidean"))

expect_equal(length(onestep_sftrack$labels), length(twostep_sftrack$labels))
expect_equal(onestep_sftrack$labels, twostep_sftrack$labels)

expect_equal(onestep_sftrack$labels, onestep_matrix$labels)
expect_equal(stops_sftrack$labels, stops_matrix$labels)
expect_equal(twostep_sftrack$labels, twostep_matrix$labels)

# move2
data("travel_path_move2", package = "travelpaths")

onestep_move2 <- infostop(data = travel_path_move2, distance_metric = "haversine")
expect_equal(onestep_move2$compute_label_medians(), c(16.37759, 48.20582), tolerance = 1e-06)
expect_warning(infostop(data = travel_path_move2, distance_metric = "euclidean"))

stops_move2 <- find_stops(data = travel_path_move2, distance_metric = "haversine") 
expect_warning(find_stops(data = travel_path_move2, distance_metric = "euclidean"))

twostep_move2 <- spatial_infomap(data = travel_path_move2, distance_metric = "haversine")
expect_warning(spatial_infomap(data = travel_path_move2, distance_metric = "euclidean"))

expect_equal(length(onestep_move2$labels), length(twostep_move2$labels))
expect_equal(onestep_move2$labels, twostep_move2$labels)

expect_equal(onestep_move2$labels, onestep_matrix$labels)
expect_equal(stops_move2$labels, stops_matrix$labels)
expect_equal(twostep_move2$labels, twostep_matrix$labels)

