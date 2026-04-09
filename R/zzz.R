.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  requireNamespace("ntrs", quietly = TRUE)
  S7::methods_register()
}
