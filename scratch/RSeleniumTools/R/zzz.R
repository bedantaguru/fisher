

# environment for whole package
.RSeleniumToolsEnv <- new.env()

# environment for selenium_storm
assign("selenium_storm_env", new.env() , envir = .RSeleniumToolsEnv)


.onLoad <- function(libname,pkgname){
  .RSeleniumToolsEnv$version <- utils::packageVersion(pkg = pkgname, lib.loc = libname)
}
