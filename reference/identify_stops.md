# Identify stops in a trajectory

Identify stationary periods in time-ordered coordinate data. A stop is a
sequence of points that remains within a spatial radius for a minimum
duration. Stop labels are added to the input data.

## Usage

``` r
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

- data:

  a \`trackframe\`, \`sf\`, \`sftrack\`, \`move2\`, or data frame.

- r1:

  a numeric giving the maximum distance between time-consecutive points
  to label them as stationary. Higher values result in more points being
  considered stationary.

- min_size:

  an integer giving the minimum number of points required to consider a
  group stationary.

- min_staying_time:

  a numeric giving the minimum duration in seconds required to
  constitute a stop.

- max_time_between:

  a numeric giving the maximum duration in seconds between consecutive
  points to consider them part of the same stop.

- stop_id_col:

  a character string specifying the name of the new column to which the
  detected stop labels are assigned. The default is \`"stop_id"\`.

- ...:

  additional arguments passed when coercing a data frame to a
  \`trackframe\`.

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

## Value

\`data\` with a new column containing the detected stop labels. \`NA\`
identifies points that are not assigned to a stop.

## Examples

``` r
data("path_trackframe", package = "trackframe")
stops <- identify_stops(path_trackframe)
head(stops[["stop_id"]])
#> [1] 1 1 1 1 1 1

data("path_data_frame", package = "trackframe")
stops_df <- identify_stops(path_data_frame, crs = NA)

if (requireNamespace("sftrack", quietly = TRUE)) {
data("path_sftrack", package = "trackframe")
stops_sftrack <- identify_stops(path_sftrack)
}

if (requireNamespace("move2", quietly = TRUE)) {
data("path_move2", package = "trackframe")
stops_move2 <- identify_stops(path_move2)
}
```
