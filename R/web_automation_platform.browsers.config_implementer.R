
# as of now it is tuned for rst
# (or RSelenium)
# RSelenium::remoteDriver(extraCapabilities = <to put the output here>)
wap_browser_config_implementer <- function(conf_lst){
  # early exits
  if(missing(conf_lst)) return(list())
  if(is.null(conf_lst)) return(list())
  if(length(conf_lst)==0) return(list())

}


wap_browser_config_implementer_firefox <- function(conf_lst){
  list(
    `moz:firefoxOptions` =
      list(
        prefs = conf_lst
      )
  )
}


wap_browser_config_implementer_chrome <- function(conf_lst){
  list(
    chromeOptions =
      list(
        prefs = conf_lst
      )
  )
}
