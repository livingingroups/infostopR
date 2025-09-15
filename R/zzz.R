# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...) {
  base::`[.data.frame`(x, i, j, drop = drop)
}

rpy_db <- new.env(parent = emptyenv())


rpy <- function(key, value) {
  if (missing(value)) {
    rpy_db[[key]]
  } else {
    rpy_db[[key]] <- value
  }
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


.onLoad <- function(libname, pkgname) {
  # infostop <- reticulate::import("infostop", delay_load = TRUE)
  rpy("initialized", NULL)
  env <- getNamespace("infostop")
  makeActiveBinding("py_infostop", function() rpy("infostop"), env)
  makeActiveBinding("py_cpputils", function() rpy("cpputils"), env)
  try(infostop_initialize(), silent = TRUE)
}
