# q("no")
# R

library("reticulate")
library("infostop")

path <- system.file("python", package = "infostop")

py_dir <- py_eval("dir")

infostop <- reticulate::import_from_path("infostop_helper", path = path)
infomap <- reticulate::import_from_path("infomap_helper", path = path)
py_dir(infostop)
py_dir(infomap)


#
#
#
funs <- c("identify_stops", "downsample", "find_neighbors", "infomap_network", "backtransform")

funs %in% ls("package:infostop")

ls(infostop:::foo())
