# Identify stationary periods from coordinate vectors

Identify stops in time-ordered coordinate data. A stop is a sequence of
points that remains within a spatial radius for a minimum duration.

## Usage

``` r
identify_stops_xyt(
  x,
  y,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
)

identify_stops_longlatt(
  longitude,
  latitude,
  t = NULL,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L
)
```

## Arguments

- x:

  a numeric vector of x-coordinates in a Cartesian coordinate system.

- y:

  a numeric vector of y-coordinates in a Cartesian coordinate system.

- t:

  an optional numeric vector, or an object inheriting from \`POSIXt\` or
  \`Date\`, containing the timestamps corresponding to the coordinates.
  The default is \`NULL\`.

- r1:

  a numeric giving the maximum distance between time-consecutive points
  to label them as stationary. Higher values result in more points being
  considered stationary.

- min_size:

  an integer giving the minimum number of points required to consider a
  group stationary.

- min_staying_time:

  a numeric giving the minimum duration in seconds required to
  constitute a stop. Only relevant if timestamps are provided.

- max_time_between:

  a numeric giving the maximum duration in seconds between consecutive
  points to consider them part of the same stop. Only relevant if
  timestamps are provided.

- longitude:

  a numeric vector of longitude coordinates in decimal degrees.

- latitude:

  a numeric vector of latitude coordinates in decimal degrees.

## Value

A list with \`stop_events\`, the median coordinates of detected stops,
and \`event_map\`, the stop label assigned to each input point. Non-stop
points have label \`-1\`.
