

check_system_os <- function(){
  os <- .Platform$OS.type
  if(os=="unix"){
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  os
}


safe_reg_read_win <- function(key = "SOFTWARE\\Microsoft",
                              hive = c("HCU","HLM", "HCR", "HU", "HCC", "HPD"),
                              maxdepth = 1,
                              view = c("default", "32-bit", "64-bit")){
  hive <- match.arg(hive)
  view <- match.arg(view)

  e <- try(readRegistry(key = key,
                        hive = hive,
                        maxdepth = maxdepth,
                        view = view), silent = TRUE)
  if(inherits(e, "try-error")){
    return(list())
  }
  e
}

check_system_Registry_access_windows <- function(){
  e <- safe_reg_read_win("SOFTWARE\\Microsoft", "HCU")
  if(is.list(e) && length(e)>0){
    TRUE
  }else{
    FALSE
  }
}

check_system_browsers_windows <- function(){
  if(check_system_Registry_access_windows()){
    # check firefox
    ff <- safe_reg_read_win("SOFTWARE\\Mozilla\\Mozilla Firefox", hive = "HLM")[["CurrentVersion"]]
    # check chrome
    # otherwise check chrome://version/
    # alt: safe_reg_read_win("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe", hive = "HLM")[["(Default)"]]
    ch <- safe_reg_read_win("Software\\Google\\Chrome\\BLBeacon", hive = "HCU")[["version"]]
    # check IE
    ie <- safe_reg_read_win()
  }
}






