.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  S7::methods_register()

  # ntrs:::.set_defaults()
}
