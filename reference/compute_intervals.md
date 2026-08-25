# Compute Intervals

This function computes time intervals for each unique label in the
mobility data. It identifies continuous periods when a user was at the
same location (same label).

## Usage

``` r
compute_intervals(labels, times, max_time_between = 86400)
```

## Arguments

- labels:

  A vector of integer labels for each point in the mobility trace.

- times:

  A vector of integer timestamps corresponding to each point in the
  mobility trace.

- max_time_between:

  Maximum time (in seconds) between consecutive points to consider them
  part of the same interval. If the time between points exceeds this
  value, a new interval is created.

## Value

A data frame with columns for label, start time, end time, and duration
of each interval.

## Examples

``` r
labels <- c(NA, 1L, 1L, NA, 2L, 2L, 2L, NA, 1L, NA)
times <- seq(0, 90, by = 10)
compute_intervals(labels, times, max_time_between = 15)
#>   label start_time end_time
#> 1    NA          0        0
#> 2     1         10       20
#> 3    NA         30       30
#> 4     2         40       60
#> 5    NA         70       70
#> 6     1         80       80
#> 7    NA         90       90
```
