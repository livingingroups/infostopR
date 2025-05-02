devtools::load_all('pkgs/infostop'); infostop_initialize()

library(sf)
library(reticulate)

abby_4652 <- read.csv("./data/FFT.csv") |>
  dplyr::filter(
    individual.local.identifier == "Abby",
    tag.local.identifier == 4652
  ) |>
  dplyr::mutate(timestamp = as.POSIXct(timestamp)) |>
  sf::st_as_sf(crs = 4326, coords = c("location.long", "location.lat"))

data_arr <- array(cbind(
  # TODO: doublecheck this
  sf::st_coordinates(abby_4652)[, 2:1],
  abby_4652$timestamp
), c(dim(abby_4652)[1], 3))

# sort by timestamp
data_arr <- data_arr[order(data_arr[,3]),]


# onestep
onestep <- infostop(data_arr, r1 = 10, r2 = 15, min_staying_time = 12*60, max_time_between = 60*60)

# twostep
stops <- find_stops(data_arr, r1 = 10)
twostep <- spatial_infomap(stops[['coordinates']], r2 = 15)

all.equal(twostep$`_stat_labels`, onestep$`_stat_labels`)
all.equal(twostep$`_stat_coords`, onestep$`_stat_coords`)