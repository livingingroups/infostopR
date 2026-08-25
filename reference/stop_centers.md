# Calculate stop centers

Summarise each detected stop by the median coordinate of its points.

## Usage

``` r
stop_centers(data, stop_id_col = "stop_id")

# S3 method for class 'trackframe'
stop_centers(data, stop_id_col = "stop_id")

# S3 method for class 'sftrack'
stop_centers(data, stop_id_col = "stop_id")

# S3 method for class 'move2'
stop_centers(data, stop_id_col = "stop_id")
```

## Arguments

- data:

  a \`trackframe\`, \`sftrack\`, or \`move2\` object with stop labels.

- stop_id_col:

  a string giving the name of the column containing stop ids.

## Value

a data frame with track id, center coordinates, and stop id.
