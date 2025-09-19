#' Check if the Infostop is loaded
#'
#' @return Logical indicating if the Python module is imported.
#'
#' @examples
#' is_infostop_initialized()
#' @export
is_infostop_initialized <- function() {
  isTRUE(rpy("initialized"))
}


is_python_initialized <- function() {
  tryCatch(getNamespace("reticulate")$is_python_initialized(), error = function(e) FALSE)
}


infostop_find_python <- function() {
  spy <- Sys.getenv("INFOSTOP_PYTHON")
  if (nchar(spy) > 0L) {
    if (file.exists(spy)) {
      return(spy)
    } else {
      msg <- "System variable 'INFOSTOP_PYTHON' set, but file '%s' does not exist."
      stop(sprintf(msg, spy))
    }
  }

  rpy <- Sys.getenv("RETICULATE_PYTHON")
  if (nchar(rpy) > 0L) {
    if (file.exists(rpy)) {
      return(rpy)
    } else {
      msg <- "System variable 'RETICULATE_PYTHON' set, but file '%s' does not exist."
      stop(sprintf(msg, rpy))
    }
  }

  config <- reticulate::py_discover_config("infostop", "infostop")
  if (!is.null(config)) {
    python_path <- unlist(strsplit(config$pythonpath, ":", fixed = TRUE))
    files <- unlist(lapply(python_path, dir, include.dirs = TRUE))
    if (isTRUE("infostop" %in% files)) {
      return(config$python)
    }
  }

  venvpy <- file.path(reticulate::virtualenv_root(), "infostop", "bin", "python")
  if (file.exists(venvpy)) {
    return(venvpy)
  }

  condapy <- file.path(reticulate::miniconda_path(), "envs", "infostop", "bin", "python")
  if (file.exists(condapy)) {
    return(condapy)
  }

  msg <- "could not find module 'infostop', please set the environment variable 'INFOSTOP_PYTHON'!"
  stop(msg)
}


set_python_version <- function(python = NULL, virtualenv = NULL, condaenv = NULL) {
  if (!is.null(python)) {
    reticulate::use_python(python, required = TRUE)
  } else if (!is.null(virtualenv)) {
    reticulate::use_virtualenv(virtualenv, required = TRUE)
  } else if (!is.null(condaenv)) {
    reticulate::use_condaenv(condaenv, required = TRUE)
  } else {
    python <- infostop_find_python()
    reticulate::use_python(python, required = TRUE)
  }
  reticulate::py_config()
}


#' Initialize Infostop
#'
#' Initialize the \verb{Python} binding to infostop.
#'
#' @param python a character string giving the path to the \verb{Python}
#'               binary (executeable) to be used.
#'               The variable \code{python} is passed to \code{reticulate::use_python}.
#' @param virtualenv a character string giving the name of the virtual environment,
#'               or the path to the virtual environment, to be used.
#'               The variable \code{virtualenv} is passed to \code{reticulate::use_virtualenv}.
#' @param condaenv a character string giving the name of the \verb{Conda} environment to be used.
#'               The variable \code{condaenv} is passed to \code{reticulate::use_condaenv}.
#' @examples
#' if (nchar(Sys.getenv("INFOSTOP_TESTING", "")) > 0L) {
#' infostop_initialize()
#' }
#' @export
infostop_initialize <- function(python = NULL, virtualenv = NULL, condaenv = NULL) {
  assert(
    check_character(python, len = 1L, any.missing = FALSE, null.ok = TRUE),
    check_character(virtualenv, len = 1L, any.missing = FALSE, null.ok = TRUE),
    check_character(condaenv, len = 1L, any.missing = FALSE, null.ok = TRUE),
    combine = "and"
  )

  if (is_infostop_initialized()) {
    writeLines("Info: Package infostop is already initialized!")
    return(invisible(NULL))
  }

  python_config <- set_python_version(python, virtualenv, condaenv)
  reticulate::py_run_string("import sys", convert = FALSE)
  py_depends <- c("infostop", "cpputils")
  reticulate::py_require(py_depends)
  for (mod in py_depends) {
    state <- try(rpy(mod, reticulate::import(mod)), silent = TRUE)
    if (inherits(state, "try-error")) {
      msg <- c(
        sprintf("could not import module '%s', with ", mod),
        sprintf("python '%s'. ", python_config$python),
        if (mod == "infostop") {
          "Please install infostop and/or set the environment variable 'INFOSTOP_PYTHON'."
        } else {
          ""
        }
      )
      stop(msg)
    }
  }
  path <- system.file("python", package = "infostop")
  infostop <- reticulate::import_from_path("infostop_helper", path = path)
  funs <- c("identify_stops", "identify_sites", "downsample", "find_neighbors",
    "infomap_network", "backtransform")
  for (fun in funs) {
    rpy(fun, infostop[[fun]])
  }
  infomap <- reticulate::import_from_path("infomap_helper", path = path)
  funs <- c("neighbors_to_network", "run_network", "assign_labels_to_nodes")
  for (fun in funs) {
    rpy(fun, infomap[[fun]])
  }
  rpy("initialized", TRUE)
}


check_infostop_initialized <- function() {
  if (!is_infostop_initialized()) {
    # any number bigger than 500 triggers a warning and the default is used.
    max_allowed_cutoff <- 500L
    caller_name <- deparse(sys.calls()[[sys.nframe() - 1]], width.cutoff = max_allowed_cutoff)
    msg <- sprintf(
      "in '%s' infostop is not initialized, %s ",
      caller_name,
      "use 'infostop_initialize' to initialize infostop!"
    )
    stop(msg, call. = FALSE)
  }
}
