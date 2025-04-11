


rst_remotedriver_config_reader <- function(rd){
  x <- rd$sessionInfo
  if(is.null(x$browserName)){
    # it can be opened by rd$open()
    # but here it is not done yet
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  bn <- x$browserName

  if(bn %in% c("msedge", "MicrosoftEdge")) bn <- "edge"

  l <- tryCatch({
    do.call(paste0("rst_remotedriver_config_reader_", bn),
            args = list(rd = rd))
  }, error = function(e){
    list()
  })

  l

}

rst_remotedriver_config_reader_chrome <- function(rd){

  x <- rd$sessionInfo

  if(is.null(x$chrome$userDataDir)){
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  wap_browser_config_reader_chrome(x$chrome$userDataDir)
}


rst_remotedriver_config_reader_firefox <- function(rd){

  x <- rd$sessionInfo

  if(is.null(x$`moz:profile`)){
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  wap_browser_config_reader_firefox(x$`moz:profile`)
}


rst_remotedriver_config_reader_opera <- function(rd){

  x <- rd$sessionInfo

  if(is.null(x$opera$userDataDir)){
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  wap_browser_config_reader_chrome(x$opera$userDataDir)
}


rst_remotedriver_config_reader_edge <- function(rd){

  x <- rd$sessionInfo

  if(is.null(x$msedge$userDataDir)){
    cat("\nThe session has not started yet.",
        "Kindly call <remote driver>$open()\n")
    return(invisible(0))
  }

  wap_browser_config_reader_chrome(x$msedge$userDataDir)
}

