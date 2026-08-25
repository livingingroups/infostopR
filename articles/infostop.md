# Infostop

## Infostop

This vignette demonstrates the two-stage Infostop workflow in R. The
first stage identifies stationary events
([`identify_stops()`](https://livingingroups.github.io/infostop/reference/identify_stops.md)),
and the second stage groups nearby events into recurring sites
([`identify_sites()`](https://livingingroups.github.io/infostop/reference/identify_sites.md)).

### Setup

``` r

library("trackframe")
library("infostop")
```

    ## Loading required package: sf

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

``` r

data("path_trackframe", package = "trackframe")
head(path_trackframe)
```

    ##                  time     northing       easting      id
    ## 1 2025-10-14 13:48:34 0.000000e+00  0.000000e+00 track_1
    ## 2 2025-10-14 13:49:34 2.843171e-05 -4.847714e-05 track_1
    ## 3 2025-10-14 13:50:34 3.693603e-05  5.120919e-04 track_1
    ## 4 2025-10-14 13:51:34 3.262802e-04 -2.322027e-04 track_1
    ## 5 2025-10-14 13:52:34 1.230933e-03 -3.615696e-04 track_1
    ## 6 2025-10-14 13:53:34 1.167179e-03 -1.313898e-03 track_1

### Identify stops and sites

``` r

stops <- identify_stops(path_trackframe)
sites <- identify_sites(stops)

head(stops$stop_id)
```

    ## [1] 1 1 1 1 1 1

``` r

head(sites$site_id)
```

    ## [1] 1 1 1 1 1 1

`NA` labels represent points that are not assigned to a stop or site.

The main parameters can be changed independently for the two stages:

``` r

stops <- identify_stops(
  path_trackframe,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300
)

sites <- identify_sites(
  stops,
  r2 = 10,
  label_singleton = TRUE,
  weighted = FALSE
)
```

### Centers

Stop centers are median coordinates of the points belonging to each
stop. Site centers are computed from the stop centers belonging to each
site.

``` r

stop_centers(stops)
```

    ##        id      easting   northing stop_id
    ## 1 track_1 -0.002353418 0.01089631       1

``` r

site_centers(sites)
```

    ##        easting   northing site_id
    ## 1 -0.002353418 0.01089631       1

### Intervals

[`compute_intervals()`](https://livingingroups.github.io/infostop/reference/compute_intervals.md)
summarizes consecutive periods with the same site label. The time column
is converted to integer seconds for this example.

``` r

intervals <- compute_intervals(
  labels = sites$site_id,
  times = as.integer(sites[["time"]]),
  max_time_between = 86400
)

head(intervals)
```

    ## [1] label      start_time end_time  
    ## <0 rows> (or 0-length row.names)

### Multiple tracks

The same workflow can process multiple track IDs. Each track is
clustered independently and the resulting labels are returned in the
original track order.

``` r

data("paths_trackframe", package = "trackframe")

stops_multiple <- identify_stops(paths_trackframe)
sites_multiple <- identify_sites(stops_multiple)

length(sites_multiple$site_id)
```

    ## [1] 3000

``` r

head(sites_multiple$site_id)
```

    ## [1] 1 1 1 1 1 1
