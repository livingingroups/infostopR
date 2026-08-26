#
# Compare with pre-calculated results from the infostop Python implementation.
#
library("tinytest")
library("jsonlite")
library("trackframe")
library("infostop")
source("utils.R")


is_move2_avail <- require("move2", quietly = TRUE)
is_sftrack_avail <- require("sftrack", quietly = TRUE)


# nolint start: object_usage_linter
arg_names <- arg_names
load_parameters <- load_parameters
load_test_data <- load_test_data
load_reference <- load_reference
# nolint end

#
# Test identify_stops_longlatt
#
test__identify_stops_longlatt <- function(name) {
  d <- load_test_data(name)
  params <- load_parameters(name)
  params <- params[intersect(names(params), arg_names(identify_stops_longlatt))]
  args <- list(longitude = d$lon, latitude = d$lat, t = d$time)
  stops <- do.call(identify_stops_longlatt, c(args, params))
  ref <- load_reference(name, "stops")
  expect_equal(stops$stop_events, as.matrix(ref$stop_events[, c("lon", "lat")]))
  expect_equal(stops$event_map, ref$event_map[["stop_id"]])
}

test__identify_stops_longlatt(name = "lon-lat_01")
test__identify_stops_longlatt(name = "lon-lat_02")
# "lon-lat_03" does not work since identify_stops_longlatt is single track

#
# Test identify_stops_xyt
#
test__identify_stops_xyt <- function(name) {
  d <- load_test_data(name)
  params <- load_parameters(name)
  params <- params[intersect(names(params), arg_names(identify_stops_xyt))]
  args <- list(x = d$x, y = d$y, t = d$time)
  stops <- do.call(identify_stops_xyt, c(args, params))
  ref <- load_reference(name, "stops")
  expect_equal(stops$stop_events, as.matrix(ref$stop_events[, c("x", "y")]))
  expect_equal(stops$event_map, ref$event_map[["stop_id"]])
}

test__identify_stops_xyt(name = "easting-northing_01")
test__identify_stops_xyt(name = "easting-northing_02")
# "lon-lat_03" does not work since identify_stops_xyt is single track

#
# Test identify stops
#
test__identify_stops <- function(name, use = "trackframe") {
  df <- load_test_data(name)
  if (startsWith(name, "lon-lat")) {
    if (use == "move2") {
      if (!is_move2_avail) {
        message("package move2 not available skip test")
        return(NULL)
      }
      d <- move2::mt_as_move2(
        df,
        coords = c("lon", "lat"),
        time_column = "time",
        track_id_column = "id",
        crs = "OGC:CRS84"
      )
    } else if (use == "sftrack") {
      if (!is_sftrack_avail) {
        return(NULL)
      }
      d <- sftrack::as_sftrack(df, coords = c("lat", "lon"), group = "id", crs = 4326)
    } else {
      # We can not test the lat-lon stuff but we could convert.
      return(NULL)
    }
  } else if (startsWith(name, "easting-northing")) {
    d <- as.trackframe(df, crs = NA)
  } else {
    stop("variable name has to start with lon-lat or easting-northing!")
  }
  params <- load_parameters(name)
  params <- params[intersect(names(params), arg_names(identify_stops))]
  args <- list(data = d)
  stops <- do.call(identify_stops, c(args, params))
  ref <- load_reference(name, "stops")
  event_map <- stops$stop_id
  stop_events <- stop_centers(stops)
  if (startsWith(name, "lon-lat")) {
    ref_stop_events <- ref$stop_events[, c("id", "lon", "lat")]
    ref_event_map <- infostop:::refine_labels(ref$event_map[["stop_id"]])
  } else {
    ref_stop_events <- ref$stop_events[, c("id", "x", "y")]
    ref_event_map <- infostop:::refine_labels(ref$event_map[["stop_id"]])
  }
  expect_equal(stop_events[, 1:3], ref_stop_events)
  expect_equal(event_map, ref_event_map)
}

test__identify_stops(name = "lon-lat_01", use = "sftrack")
test__identify_stops(name = "lon-lat_02", use = "sftrack")
test__identify_stops(name = "lon-lat_03", use = "sftrack")
test__identify_stops(name = "lon-lat_01", use = "move2")
test__identify_stops(name = "lon-lat_02", use = "move2")
test__identify_stops(name = "lon-lat_03", use = "move2")
test__identify_stops(name = "easting-northing_01")
test__identify_stops(name = "easting-northing_02")
test__identify_stops(name = "easting-northing_03")
