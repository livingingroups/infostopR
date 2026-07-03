library("tinytest")
library("infostop")
library("trackframe")

has_move2 <- requireNamespace("move2", quietly = TRUE)
has_sftrack <- requireNamespace("sftrack", quietly = TRUE)

unique_non_na <- function(v) {
  length(unique(v[!is.na(v)]))
}

expect_n_stops <- function(df, n) {
  expect_equal(unique_non_na(df$stop_id), n)
}

expect_n_sites <- function(df, n) {
  expect_equal(unique_non_na(df$site_id), n)
}

expect_n_labels <- function(is, n) {
  expect_equal(unique_non_na(is$labels), n)
}

#
# set up data
#
m <- 0.4
n <- 8
lat <- 1 +
  c(
    rep(0, 2),
    seq(0, m, length.out = n),
    rep(m, 2),
    seq(m, 2 * m, length.out = n),
    rep(2 * m, 2)
  )
long <- 89.99
df <- data.frame(
  lat = lat,
  long = rep(long, length(lat)),
  time = seq(1, length.out = length(lat), by = 301L)
)

# Correct lat / lon results in 3 stops
expected_stops <- list(
  c(1, long),
  c(1 + m, long),
  c(1 + 2 * m, long)
)

matrix(
  c(long, long, long, 1, 1 + m, 1 + 2 * m), ncol = 2
)

# Flipped lat long results in 1 stop
stops_incorrect_axis_order <- list(
  c(long, 1 + m)
)

expect_equal(
  infostop:::identify_stops_internal(
    data = list(unname(as.matrix(df)))
  )$stop_events[[1]],
  expected_stops
)

expect_equal(
  infostop:::identify_stops_internal(
    data = list(unname(as.matrix(df[, c(2, 1, 3)])))
  )$stop_events[[1]],
  stops_incorrect_axis_order
)

df$id <- 'track_1'

#
# move2
#
m2 <- move2::mt_as_move2(
  df,
  coords = c("lat", "long"),
  time_column = "time",
  track_id_column = "id",
  crs = 4326
)
expect_n_stops(
  identify_stops(m2),
  3
)


#
# sftrack
#
sft <- sftrack::as_sftrack(df, coords = c("lat", "long"), crs = 4326)
expect_n_stops(
  identify_stops(sft),
  3
)


#
# identify_stops_xyt
#

stops_xyt <- identify_stops_longlatt(
  df[, "long"],
  df[, "lat"],
  t = df[, "time"],
  r1 = 100,
  min_staying_time = 300,
  max_time_between = 86400,
  min_size = 2
)

expect_equal(
  stops_xyt$stop_events[[1]],
  expected_stops
)

#
# set up data for site check
#

# input stops with axis order reversed
rev_ax_stops <- lapply(
  expected_stops,
  rev
)

event_map <- c(
  0L,
  0L,
  0L,
  -1L,
  -1L,
  -1L,
  -1L,
  -1L,
  -1L,
  1L,
  1L,
  1L,
  1L,
  -1L,
  -1L,
  -1L,
  -1L,
  -1L,
  -1L,
  2L,
  2L,
  2L
)

# expected result
expected_sites <- event_map

# result for flipped axis
sites_incorrect_axis_order <- event_map
sites_incorrect_axis_order[event_map != -1] <- 1

# check that above data is accurate
expect_equivalent(
  infostop:::identify_sites_internal(
    list(expected_stops),
    list(event_map)
  )[[1]],
  as.array(expected_sites)
)
expect_equivalent(
  infostop:::identify_sites_internal(
    list(rev_ax_stops),
    list(event_map)
  )[[1]],
  as.array(sites_incorrect_axis_order)
)

df$id <- 'track_1'
df$stop_id <- NA
df$stop_id[event_map != -1] <- event_map[event_map != -1] + 1

#
# move2
#
m2_lat_lon <- move2::mt_as_move2(
  df,
  coords = c("lat", "long"),
  time_column = "time",
  track_id_column = "id",
  crs = 4326
)
expect_n_stops(
  identify_sites(m2_lat_lon),
  3
)

m2_lon_lat <- move2::mt_as_move2(
  df,
  coords = c("long", "lat"),
  time_column = "time",
  track_id_column = "id",
  crs = "OGC:CRS84"
)
sites_m2_lat_lon <- identify_sites(m2_lat_lon)
sites_m2_lon_lat <- identify_sites(m2_lon_lat)

expect_equal(sites_m2_lat_lon$stop_id, sites_m2_lon_lat$stop_id)
expect_equal(sites_m2_lat_lon$site_id, sites_m2_lon_lat$site_id)

#
# sftrack
#
sft_lat_lon <- sftrack::as_sftrack(df, coords = c("lat", "long"), crs = 4326)
expect_n_stops(
  identify_sites(sft_lat_lon),
  3
)

sft_lon_lat <- sftrack::as_sftrack(
  df,
  coords = c("long", "lat"),
  crs = "OGC:CRS84"
)
sites_sft_lat_lon <- identify_sites(sft_lat_lon)
sites_sft_lon_lat <- identify_sites(sft_lon_lat)

expect_equal(sites_sft_lat_lon$stop_id, sites_sft_lon_lat$stop_id)
expect_equal(sites_sft_lat_lon$site_id, sites_sft_lon_lat$site_id)

#
# full infostop
#

df$stop_id <- NULL
df$site_id <- NULL

#
# move2
#
m2_lat_lon <- move2::mt_as_move2(
  df,
  coords = c("lat", "long"),
  time_column = "time",
  track_id_column = "id",
  crs = 4326
)
expect_n_labels(
  infostop(m2_lat_lon),
  3
)

m2_lon_lat <- move2::mt_as_move2(
  df,
  coords = c("long", "lat"),
  time_column = "time",
  track_id_column = "id",
  crs = "OGC:CRS84"
)
infostop_m2_lat_lon <- infostop(m2_lat_lon)
infostop_m2_lon_lat <- infostop(m2_lon_lat)

expect_equal(
  infostop_m2_lat_lon$compute_label_medians(),
  infostop_m2_lon_lat$compute_label_medians()
)
expect_equal(infostop_m2_lat_lon$labels, infostop_m2_lon_lat$labels)

#
# sftrack
#
sft_lat_lon <- sftrack::as_sftrack(df, coords = c("lat", "long"), crs = 4326)
expect_n_labels(
  infostop(sft_lat_lon),
  3
)


expect_n_labels(
  infostop_lonlatt(
    df[, "long"],
    df[, "lat"],
    t = df[, "time"],
  ),
  3
)

sft_lon_lat <- sftrack::as_sftrack(
  df,
  coords = c("long", "lat"),
  crs = "OGC:CRS84"
)
infostop_sft_lat_lon <- infostop(sft_lat_lon)
infostop_sft_lon_lat <- infostop(sft_lon_lat)

expect_equal(
  infostop_sft_lat_lon$compute_label_medians(),
  infostop_sft_lon_lat$compute_label_medians()
)
expect_equal(infostop_sft_lat_lon$labels, infostop_sft_lon_lat$labels)
