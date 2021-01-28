



rd <- rst_remotedriver(browser = "firefox")


rst_remotedriver_config_path_chrome <- function(rd){
  # x <- rd$open()
  x <- rd$sessionInfo

  if(is.null(x$chrome$userDataDir)){
    # but we'll not open it
    # x <- rd$open()
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  pref <- file.path(x$chrome$userDataDir, "Default","Preferences")
  pref <- normalizePath(pref, mustWork = FALSE)
  wap_browser_config_reader_chrome(pref)
}


rst_remotedriver_config_path_firefox <- function(rd){
  # x <- rd$open()
  x <- rd$sessionInfo

  if(is.null(x$`moz:profile`)){
    # but we'll not open it
    # x <- rd$open()
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  pref <- file.path(x$chrome$userDataDir, "Default","Preferences")
  pref <- normalizePath(pref, mustWork = FALSE)
  wap_browser_config_reader_chrome(pref)
}
