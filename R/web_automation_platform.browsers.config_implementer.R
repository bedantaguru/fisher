
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

wap_browser_config_implementer_edge <- function(conf_lst){
  list(
    `ms:edgeOptions` =
      list(
        prefs = conf_lst
      )
  )
}

wap_browser_config_implementer_opera <- function(conf_lst){
  ecl <- list()
  # this one works
  if(is_available("jsonlite")){
    tpf <- tempfile(pattern = "userDataDirOpera")
    dir.create(tpf, showWarnings = FALSE)
    tpfp <- file.path(tpf, "Preferences")
    try({
      writeLines(jsonlite::toJSON(conf_lst, auto_unbox = TRUE), tpfp)
      ecl <- list(
        `goog:chromeOptions` = list(
          args = list(paste0('--user-data-dir=',normalizePath(tpf),'')))
      )

    }, silent = TRUE)
  }


  extra <- list(
    # this mostly have no impacts
    `goog:chromeOptions` =
      list(
        prefs = conf_lst
      )
  )

  ecl <- merge_list(ecl, extra)
  ecl
}
