.onAttach <- function(libname, pkgname) {
  packageStartupMessage("palaeoverse ", utils::packageVersion("palaeoverse"))
}
