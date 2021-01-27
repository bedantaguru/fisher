


# helper (specifically for windows)
# this will ensure RSelenium::makeFirefoxProfile works
make_cmd_zip_available <- function(){
  if(is_available("pkgbuild")){
    if(pkgbuild::has_rtools()){
      Sys.setenv(R_ZIPCMD= file.path(pkgbuild::rtools_path(),"zip"))
    }
  }
}


wap_browser_config_implementer_firefox <- function(conf_lst){
  # sample
  # rd <- RSelenium::remoteDriver(browserName = "firefox",
  # extraCapabilities  = RSelenium::makeFirefoxProfile(
  #   list(network.proxy.type = 1L, network.proxy.ssl = "localhost")))
  make_cmd_zip_available()
  tryCatch(
    RSelenium::makeFirefoxProfile(conf_lst),
    error = function(e){
      list()
    })
}





