if (FALSE) {
  library('tinytest')
  library('infostop')
}


data <- rtravel_path(100, format = "matrix")
model <- infostop(data, r1 = 10, r2 = 10)

