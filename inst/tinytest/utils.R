

arg_names <- function(fun) {
  x <- names(as.list(args(fun)))
  x[nchar(x) > 0]
}


load_test_data <- function(name) {
  folder <- system.file("test_data/data", package = "infostop")
  if (missing(name)) {
    return(dir(folder))
  }
  if (!endsWith(name, ".csv")) {
    name <- sprintf("%s.csv", name)
  }
  d <- read.csv(file.path(folder, name))
  if (is.character(d$time)) {
    d$time <- as.POSIXct(d$time)
  }
  d
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
      stop_events = stop_centers,
      event_map = stop_ids
    )
  } else {
    file_sites <- sprintf("%s_sites.csv", name)
    site_ids <- read.csv(file.path(folder, file_sites))
    file_site_centers <- sprintf("%s_site_centers.csv", name)
    site_centers <- read.csv(file.path(folder, file_site_centers))
    list(
      site_events = site_centers,
      event_map = site_ids
    )
  }
}
