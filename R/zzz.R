.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  loadNamespace("ntrs")
  S7::methods_register()
}
