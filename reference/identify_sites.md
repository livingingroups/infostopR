# Assign site labels to detected stops

This is the second step of Infostop. It treats stop centers as nodes in
a spatial network, connects nodes within \`r2\`, and uses Infomap to
identify communities. A community is a site shared by the stops assigned
to it.

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

  a \`trackframe\`, \`sf\`, \`sftrack\`, or \`move2\` object containing
  stop labels.

- r2:

  a numeric giving the maximum distance between stop centers in the same
  network neighborhood. It is measured in the coordinate units for
  projected data and in metres for geographic data.

- label_singleton:

  a logical. If \`TRUE\`, give stationary locations that were only
  visited once their own label. If \`FALSE\`, leave isolated stops as
  \`NA\`.

- min_spacial_resolution:

  a numeric giving the minimum spatial resolution. Points that round to
  the same coordinates at this resolution are considered the same point.
  The default is \`0\`.

- weighted:

  a logical. If \`TRUE\`, weight network edges by distance.

- weight_exponent:

  a numeric giving the exponent used for distance-based edge weights.

- seed:

  an integer passed as the random seed to
  [`cluster_infomap`](https://mapequation.r-universe.dev/infomap/reference/cluster_infomap.html).
  Defaults to \`123L\`.

- stop_id_col:

  a character string specifying the name of the column containing stop
  identifiers. The default is \`"stop_id"\`.

- site_id_col:

  a character string specifying the name of the new column to which the
  detected site labels are assigned. The default is \`"site_id"\`.

- ...:

  additional arguments passed when coercing data to a \`trackframe\`.

## Value

\`data\` with a site-label column added. \`NA\` identifies points that
are not assigned to a site.

## References

Aslak, U. and Alessandretti, L. (2020). Infostop: Scalable stop-location
detection in multi-user mobility data. doi:10.48550/arXiv.2003.14370

## Examples

``` r
if (requireNamespace("trackframe", quietly = TRUE)) {
  data("path_trackframe", package = "trackframe")
  stops <- identify_stops(path_trackframe)
  sites <- identify_sites(stops)
  head(sites[["site_id"]])
}
#> [1] 1 1 1 1 1 1
```
