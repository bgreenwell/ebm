# Global reference to interpret (will be initialized in .onLoad)
interpret <- NULL

.onLoad <- function(libname, pkgname) {
  # Use superassignment to update global reference to interpret
  interpret <<- reticulate::import("interpret", delay_load = TRUE)
}
