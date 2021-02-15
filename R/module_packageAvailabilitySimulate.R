
#>> Embedded Module: {packageAvailabilitySimulate}
#>> Author: Indranil Gayen
#>> Version: 0.0.1
#>> Files: c("R/module_packageAvailabilitySimulate.R")


pkg_availability_sim <- new.env()

is_available <- function(pkgname, check_loadability = FALSE) {
  if (any(pkgname %in% pkg_availability_sim$na_pkgs)) {
    return(FALSE)
  }

  all(
    sapply(
      pkgname,
      function(x){
        if(check_loadability){
          isTRUE(requireNamespace(x, quietly = TRUE))
        }else{
          dir.exists(system.file(package = x))
        }
      }
    )
  )


}

not_available <- function(pkgs, add = TRUE) {
  if (missing(pkgs)) {
    pkg_availability_sim$na_pkgs <- NULL
  } else {
    if (length(pkgs) >= 1) {
      if (is.character(pkgs)) {
        if (add) {
          old <- pkg_availability_sim$na_pkgs
          pkgs <- unique(c(old, pkgs))
        }
        pkg_availability_sim$na_pkgs <- pkgs
      }
    }
  }
  return(invisible(0))
}
