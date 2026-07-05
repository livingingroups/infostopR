library("jsonlite")
library("trackframe")
library("infostop")
library("infostop")


load_test_data <- function(name) {
  folder <- system.file("test_data/data", package = "infostop")
  if (missing(name)) {
    return(dir(folder))
  }
  if (!endsWith(name, ".csv")) {
    name <- sprintf("%s.csv", name)
  }
  read.csv(file.path(folder, name))
}


load_parameters <- function(name) {
  folder <- system.file("test_data/reference", package = "infostop")
  if (missing(name)) {
    return(dir(folder, pattern = "*.json$"))
  }
  if (!endsWith(name, ".json")) {
    name <- sprintf("%s_params.json", name)
  }
  jsonlite::fromJSON(file.path(folder, name))
}


load_reference <- function(name, type = c("stops", "sites")) {
  type <- match.arg(type)
  folder <- system.file("test_data/reference", package = "infostop")
  if (missing(name)) {
    return(dir(folder, pattern = "*.csv$"))
  }
  if (type == "stops") {
    file_stops <- sprintf("%s_stops.csv", name)
    stop_ids <- read.csv(file.path(folder, file_stops))
    file_stop_centers <- sprintf("%s_stop_centers.csv", name)
    stop_centers <- read.csv(file.path(folder, file_stop_centers))
    list(
      stop_events = as.matrix(stop_centers),
      event_map = stop_ids[[1]]
    )
  } else {
    file_sites <- sprintf("%s_sites.csv", name)
    site_ids <- read.csv(file.path(folder, file_sites))
    file_site_centers <- sprintf("%s_site_centers.csv", name)
    site_centers <- read.csv(file.path(folder, file_site_centers))
    list(
      site_events = as.matrix(site_centers),
      event_map = site_ids[[1]]
    )
  }
}


# lat lon
name <- "lon-lat_01"
name <- "lon-lat_02"
name <- "lon-lat_03"
d <- load_test_data(name)
head(d)
infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 20, min_size = 10L, min_staying_time = 1
)
# returns x: lon, y: lat
infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 20, min_size = 10L, min_staying_time = 1
)
load_reference(name)


str(load_parameters(name))


#
# stops 2
#
name <- "lon-lat_02"
d <- load_test_data(name)
stops1 <- infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 20, min_size = 10L, min_staying_time = 1
)
# returns x: lon, y: lat
stops2 <- infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 20, min_size = 10L, min_staying_time = 1
)
stops_ref <- load_reference(name)
tinytest::expect_equivalent(stops2$event_map, stops_ref$event_map)


#
# stops 3
#
name <- "lon-lat_03"
d <- load_test_data(name)
d$time <- as.POSIXct(d$time)
# is.numeric(d$time)
stops1 <- infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 100, min_size = 2L, min_staying_time = 1
)
# returns x: lon, y: lat
stops2 <- infostop::identify_stops_longlatt(
  d$lon, d$lat, d$time, r1 = 100, min_size = 2L, min_staying_time = 1
)
stops_ref <- load_reference(name)
tinytest::expect_equivalent(stops2$event_map, stops_ref$event_map)
tinytest::expect_equivalent(stops2$stop_events, stops_ref$stop_events[, c(2, 1)])



?infostop::identify_stops_longlatt


name <- "easting-northing_01"


 [3] "easting-northing_01_stop_centers.csv"
 [4] "easting-northing_01_stops.csv"

 [1] "easting-northing_01_site_centers.csv"
 [2] "easting-northing_01_sites.csv"


xy1 <- load_test_data("easting-northing_01")
params <- load_parameters("easting-northing_01_params.json")
infostop::identify_stops(xy1)


identify_stops_longlatt()

infostop::identify_stops_xyt(xy1$x, xy1$y, xy1$t)
infostop::identify_stops_xyt(xy1$x, xy1$y, xy1$t)


dat <- load_test_data("easting-northing_01")
data <- as.trackframe(dat, crs = NA)
stops1 <- infostop::identify_stops(data)
stops2 <- infostop::identify_stops(data)
all(stops2 == stops1)


sites_ref <- load_reference("easting-northing_01", "site")
sites <- infostop::identify_sites(stops1)
infostop::identify_sites(stops1)
tinytest::expect_equivalent(sites$site_id - 1L, sites_ref$event_map)


## Calculate Median
str(native_identify_sites_backend)
native_identify_sites_backend()

head(stops1)

stops_list <- infostop:::prep_stops(
  stops1$x,
  stops1$y,
  stops1$id,
  stops1$stop_id
)

site_ids <- infostop:::identify_sites_internal(stops_list$stop_events, stops_list$event_maps)
all(sites$site_id == site_ids[[1]] + 1L)




dat <- load_test_data("easting-northing_01")
data <- as.trackframe(dat, crs = NA)
stops <- infostop::identify_stops(data)


sites1 <- infostop::identify_sites(stops)
sites2 <- infostop::identify_sites(stops)

head(sites1, 60)
head(sites2, 60)
str(attributes(sites1))
str(attributes(sites2))
all(sites1 == sites2)
sites2$site_id == (sites_ref$event_map + 1L)
