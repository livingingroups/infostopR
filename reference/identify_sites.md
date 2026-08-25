# Spatial Infomap Cluster a Collection of Points Using Infomap

This function applies the SpatialInfomap algorithm to cluster a
collection of points. It directly returns the cluster labels rather than
a model object.

## Usage

``` r
identify_sites(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
)

# S3 method for class 'trackframe'
identify_sites(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
)

# S3 method for class 'sf'
identify_sites(
  data,
  r2 = 10,
  label_singleton = TRUE,
  min_spacial_resolution = 0,
  weighted = FALSE,
  weight_exponent = 1,
  seed = 123L,
  stop_id_col = "stop_id",
  site_id_col = "site_id",
  ...
)
```

## Arguments

- data:

  A numeric matrix with 2 or 3 columns. Columns 1 and 2 are spatial
  coordinates. Column 3 is optional and represents time.

- r2:

  Numeric. Max distance between stationary points to form an edge.

- label_singleton:

  Logical. If TRUE, give stationary locations that were only visited
  once their own label. If FALSE, label them as non-stationary (-1).

- min_spacial_resolution:

  Numeric. The minimal difference allowed between points before they are
  considered the same points.

- weighted:

  Logical. Weight edges in the network representation by distance.

- weight_exponent:

  Numeric. Exponent used when weighting edges in the network.

- seed:

  an integer passed as seed to
  [`cluster_infomap`](https://mapequation.r-universe.dev/infomap/reference/cluster_infomap.html)
  (default is \`123L\`).

- stop_id_col:

  A character string specifying the name of the column to be used for
  the stop identifiers. Default is "stop_id".

- site_id_col:

  A character string specifying the name of the column to be used for
  the site identifiers. Default is "site_id".

- ...:

  other arguments passed to \`as.trackframe()\`

## Value

A numeric vector of cluster labels for each input point. Points labeled
-1 are considered non-stationary.

## Examples

``` r
if (requireNamespace("trackframe", quietly = TRUE)) {
  data("path_trackframe", package = "trackframe")
  stops <- identify_stops(path_trackframe, r1 = 100, min_staying_time = 300,
                        max_time_between = 86400, min_size = 2)
  head(stops[["stop_id"]], 30)
  clusters <- identify_sites(stops, r2 = 50)
  head(clusters[["site_id"]], 30)
}
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```
