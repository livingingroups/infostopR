# Find based on distance and time threshold

Find based on distance and time threshold

Find based on distance and time threshold

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

identify_stops(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
)

# S3 method for class 'data.frame'
identify_stops(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  time_col = tf_options("time_col"),
  easting_col = tf_options("easting_col"),
  northing_col = tf_options("northing_col"),
  id_col = tf_options("id_col"),
  crs = NULL,
  ...
)

# S3 method for class 'trackframe'
identify_stops(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
)

# S3 method for class 'sf'
identify_stops(
  data,
  r1 = 10,
  min_size = 2L,
  min_staying_time = 300L,
  max_time_between = 86400L,
  stop_id_col = "stop_id",
  ...
)
```

## Arguments

- x:

  a numeric vector of x-coordinates in cartesian coordinate system (e.g.
  projected coordinates).

- y:

  a numeric vector of y-coordinates in cartesian coordinate system (e.g.
  projected coordinates).

- t:

  a vecor inheriting from `numeric` or `POSIXt` or `Date` containing the
  timestamps corresponding to the x and y coordinates.

- r1:

  A numeric vector giving the maximum distance between time-consecutive
  points to label them as stationary. Higher values will result in more
  points being considered stationary.

- min_size:

  An integer giving the minimum number of points required to consider a
  group stationary.

- min_staying_time:

  An integer giving the minimum duration (in seconds) that can
  constitute a stop. Only relevant if timestamps are provided in the
  data.

- max_time_between:

  An integer giving the maximum duration (in seconds) between
  consecutive points to consider them part of the same stop. Only
  relevant if timestamps are provided.

- longitude:

  numeric vector of longitude coordinates

- latitude:

  numeric vector of latitude coordinates

- data:

  A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial
  coordinates. Column 3 is optional and represents time.

- stop_id_col:

  A character string specifying the name of the column to be used for
  the stop identifiers. Default is "stop_id".

- ...:

  other arguments passed to \`as.trackframe()\`

- time_col:

  a character string specifying the column name of the time column. If
  no column is specified, the \`time_col\` is tried to be matched by
  possible names provided in \`tf_options("time_col")\`. In case of
  multiple matches, the first match is chosen.

- easting_col:

  a character string specifying the column name of the easting column.
  If no column is specified, the \`easting_col\` is tried to be matched
  by possible names provided in \`tf_options("easting_col")\`. In case
  of multiple matches, the first match is chosen.

- northing_col:

  a character string specifying the column name of the northing column.
  If no column is specified, the \`northing_col\` is tried to be matched
  by possible names provided in \`tf_options("northing_col")\`. In case
  of multiple matches, the first match is chosen.

- id_col:

  optional character vector specifying identifier column names. If no
  column is specified, the \`id_col\` is tried to be matched by possible
  names provided in \`tf_options("id_col")\`. In case of multiple
  matches, the first match is chosen.

- crs:

  required integer or charactor string identifying coordinate reference
  system. Use NA for non-georeferenced cartesian coordinate systems.

## Examples

``` r
if (requireNamespace("trackframe", quietly = TRUE)) {
library(trackframe)
data("path_trackframe", package = "trackframe")
stops <- identify_stops(data = path_trackframe)

# data.frame
data("path_data_frame", package = "trackframe")
tf <- as.trackframe(path_data_frame, crs = NA)
stops <- identify_stops(data = tf)
}

# with sftrack
data("path_sftrack", package = "trackframe")
class(path_sftrack)
#> [1] "sftrack"    "sf"         "data.frame"
stops_sftrack <- identify_stops(path_sftrack)

# with move2
data("path_move2", package = "trackframe")
class(path_move2)
#> [1] "move2"      "sf"         "data.frame"
stops_move2 <- identify_stops(path_move2)
```
