# Calculate site centers

Summarise each detected site by first computing stop centers and then
taking the median of those stop centers per site.

## Usage

``` r
site_centers(data, site_id_col = "site_id", stop_id_col = "stop_id")

# S3 method for class 'trackframe'
site_centers(data, site_id_col = "site_id", stop_id_col = "stop_id")

# S3 method for class 'sftrack'
site_centers(data, site_id_col = "site_id", stop_id_col = "stop_id")

# S3 method for class 'move2'
site_centers(data, site_id_col = "site_id", stop_id_col = "stop_id")
```

## Arguments

- data:

  a \`trackframe\`, \`sftrack\`, or \`move2\` object with stop and site
  labels.

- site_id_col:

  a string giving the name of the column containing site ids.

- stop_id_col:

  a string giving the name of the column containing stop ids.

## Value

a data frame with center coordinates and site id.
