library("jsonlite")
library("travelpaths")
data("travel_paths_data_frame", package = "travelpaths")

x <- travel_paths_data_frame[, c("latitude", "longitude", "time", "id")]
x$time <- as.numeric(as.integer(x$time))
x <- split(x, x$id)
x <- lapply(x, function(x) as.matrix(x[, 1:3]))
names(x) <- NULL
write_json(x, "trackdata.py", pretty = TRUE)

