#
# Compare site detection with pre-calculated results from the infostop Python implementation.
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
# Test identify sites
#
test__identify_sites <- function(name, use = "trackframe", seed = 123L) {
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
  params_stops <- params[intersect(names(params), arg_names(identify_stops))]
  stops <- do.call(identify_stops, c(list(data = d), params_stops))
  params_sites <- params[intersect(names(params), arg_names(identify_sites))]
  sites <- do.call(identify_sites, c(list(data = stops), params_sites, list(seed = seed)))

  ref <- load_reference(name, "sites")
  event_map <- sites$site_id
  site_events <- site_centers(sites)
  if (startsWith(name, "lon-lat")) {
    ref_site_events <- ref$site_events[, c("lon", "lat")]
  } else {
    ref_site_events <- ref$site_events[, c("x", "y")]
  }
  ref_event_map <- infostop:::refine_labels(ref$event_map[["site_id"]])

  expect_equal(site_events[, 1:2], ref_site_events)
  expect_equal(event_map, ref_event_map)
}


test__identify_sites(name = "lon-lat_01", use = "sftrack", seed = 123L)
test__identify_sites(name = "lon-lat_02", use = "sftrack", seed = 123L)
test__identify_sites(name = "lon-lat_03", use = "sftrack", seed = 1L)
test__identify_sites(name = "lon-lat_01", use = "move2", seed = 123L)
test__identify_sites(name = "lon-lat_02", use = "move2", seed = 123L)
test__identify_sites(name = "lon-lat_03", use = "move2", seed = 1L)
test__identify_sites(name = "easting-northing_01", seed = 123L)
test__identify_sites(name = "easting-northing_02", seed = 123L)
test__identify_sites(name = "easting-northing_03", seed = 29L)
