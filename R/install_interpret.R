#' Install interpret
#'
#' This function will install interpret along with all of its dependencies.
#'
#' @param envname Name of or path to a Python virtual environment.
#' @param extra_packages Additional Python packages to install alongside interpret.
#' @param python_version Passed on to `reticulate::virtualenv_starter()`
#' @param restart_session Whether to restart the R session after installing (note this will only occur within RStudio).
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @returns No return value, called for side effects.
#'
#' @export
install_interpret <- function(
    envname = "r-ebm", ...,
    extra_packages = c("plotly>=3.8.1"),
    python_version = ">=3.9,<=3.12",
    restart_session = TRUE)
{

  # Set up virtual environment for installs, etc.
  reticulate::virtualenv_create(
    envname = envname,
    version = python_version,
    force = identical(envname, "r-ebm"),
    packages = NULL
  )

  # Install extra packages
  extra_packages <- unique(extra_packages)
  if (length(extra_packages)) {
    reticulate::py_install(extra_packages, envname = envname)
  }

  # Install interpret
  reticulate::py_install("interpret==0.6.9", envname = envname)

  # Wrap up
  message("Finished installing interpret!")
  if (restart_session && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession")) {
    rstudioapi::restartSession()
  }
  invisible(NULL)
}
