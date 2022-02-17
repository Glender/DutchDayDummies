.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    show_upcoming_holidays()
  )
}
