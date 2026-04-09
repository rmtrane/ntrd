.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  requireNamespace("ntrs", quietly = FALSE)
  S7::methods_register()
}
