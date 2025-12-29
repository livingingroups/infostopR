#' Conda Install Infostop
#'
#' @param envname a character string giving the name or path of the conda environment
#'  to be used or created for the installation.
#' @param packages a character vector giving the packages to be installed.
#' @param forge a logical giving if conda forge should be used for the installation.
#' @param channel a character vector giving the conda channels to be used.
#' @param conda a character string giving the path to the conda executable.
#' @param ... additional arguments passed to \code{conda_install}.
#' @examples
#' \dontrun{
#' conda_install_infostop()
#' }
#' @export
conda_install_infostop <- function(
  envname = "infostop",
  packages = c("python", "infostop"),
  forge = TRUE,
  channel = c("conda-forge"),
  conda = "auto",
  ...
) {
  envs <- conda_list(conda)
  if (!isTRUE(envname %in% envs$name)) {
    conda_create(
      envname = envname,
      packages = packages,
      forge = forge,
      channel = channel,
      conda = conda
    )
  } else {
    packages <- setdiff(packages, "python")
    conda_install(
      envname = envname,
      packages = packages,
      forge = forge,
      channel = channel,
      conda = conda,
      ...
    )
  }
}


#' Install Infostop via Virtual Environment
#'
#' @param envname a character string giving the name or path of the virtual environment
#'  to be used or created for the installation.
#' @param packages a character vector giving the packages to be installed.
#' @param python a string giving the name or path of the python version to be used
#'      (e.g., \code{"python3"}).
#' @param ... additional arguments passed to \code{conda_install}.
#' @examples
#' \dontrun{
#' virtualenv_install_infostop()
#' }
#' @export
virtualenv_install_infostop <- function(
  envname = "infostop",
  packages = "infostop",
  python = NULL,
  ...
) {
  if (!isTRUE(envname %in% virtualenv_list())) {
    virtualenv_create(
      envname = envname,
      python = python,
      packages = packages,
      ...
    )
  } else {
    virtualenv_install(envname = envname, packages = packages, ...)
  }
}
