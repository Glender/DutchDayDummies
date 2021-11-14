.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    next_holiday()
  )
}
