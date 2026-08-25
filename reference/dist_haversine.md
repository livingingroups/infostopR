# Haversine distance

Computes the great-circle distance between pairs of points on a sphere
using the haversine formula.

## Usage

``` r
dist_haversine(lon1, lat1, lon2, lat2, radius = 6378137)
```

## Arguments

- lon1:

  a numeric vector giving the longitude of the first points in decimal
  degrees.

- lat1:

  a numeric vector giving the latitudes of the first points in decimal
  degrees.

- lon2:

  a numeric vector giving the longitudes of the second points in decimal
  degrees.

- lat2:

  a numeric vector giving the latitudes of the second points in decimal
  degrees. All four coordinate vectors must have the same length.

- radius:

  numeric scalar, radius of the sphere. Defaults to \`6378137\` metres
  (WGS84 equatorial radius).

## Value

A numeric vector of distances between \`(lon1\[i\], lat1\[i\])\` and
\`(lon2\[i\], lat2\[i\])\`, in the same unit as \`radius\` (metres by
default).

## Examples

``` r
dist_haversine(16.37, 48.21, 2.35, 48.86) # Vienna -> Paris
#> [1] 1034492
```
