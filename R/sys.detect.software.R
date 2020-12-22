


sys_check_web_browsers <- function(){
  sys_use_os_specific_method("sys_check_web_browsers")
}



## windows

sys_reg_read_win <- function(key = "SOFTWARE",
                             hive = c("HCU","HLM", "HCR", "HU", "HCC", "HPD"),
                             maxdepth = 1,
                             view = c("default", "32-bit", "64-bit")){
  hive <- match.arg(hive)
  view <- match.arg(view)

  tryCatch(
    utils::readRegistry(
      key = key,
      hive = hive,
      maxdepth = maxdepth,
      view = view),
    error = function(e){
      list()
    }
  )
}

sys_check_Registry_access_win <- function(){
  e <- sys_reg_read_win(key = "SOFTWARE",hive = "HLM")
  if(is.list(e) && length(e)>0){
    TRUE
  }else{
    FALSE
  }
}

sys_check_web_browsers_windows <- function(){
  if(sys_check_Registry_access_win()){

    # GUI method (ref only)
    # https://www.computerhope.com/issues/ch001329.htm

    # installed browsers
    bnames <- unique(
      c(
        # ref : https://docs.microsoft.com/en-us/windows/win32/shell/start-menu-reg
        names(sys_reg_read_win("Software\\Clients\\StartMenuInternet", hive = "HLM")),

        # ref : https://stackoverflow.com/questions/2370732/how-to-find-all-the-browsers-installed-on-a-machine
        names(sys_reg_read_win("SOFTWARE\\WOW6432Node\\Clients\\StartMenuInternet", hive = "HLM"))
      )
    )

    bnames <- tolower(bnames)

    blst <- list()

    if(any(grepl("firefox", bnames))){

      # check firefox version
      ff <- sys_reg_read_win("SOFTWARE\\Mozilla\\Mozilla Firefox", hive = "HLM")[["CurrentVersion"]]
      # alt: sys_reg_read_win("SOFTWARE\\Wow6432Node\\Mozilla\\Mozilla Firefox",hive = "HLM")

      blst$firefox <- list(
        installed_version = ifelse(is.null(ff), NA, ff)
      )
    }


    if(any(grepl("chrome", bnames))){

      # check chrome version
      # otherwise check chrome://version/ in chrome
      ch <- sys_reg_read_win("Software\\Google\\Chrome\\BLBeacon", hive = "HCU")[["version"]]

      blst$chrome <- list(
        installed_version = ifelse(is.null(ch), NA, ch)
      )
    }

    if(any(grepl("opera", bnames))){

      # check opera version
      op <- names(sys_reg_read_win("SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall", hive = "HLM"))
      op <- op[grepl("opera",tolower(op))]
      op <- sys_reg_read_win(paste0("SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\", op), hive = "HLM")[["DisplayVersion"]]

      blst$opera <- list(
        installed_version = ifelse(is.null(op), NA, op)
      )
    }

    blst

  }else{
    invisible(list())
  }
}






