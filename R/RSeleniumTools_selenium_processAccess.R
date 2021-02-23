
# This is for separate access for selenium through {ps}
# This may be helpful for quick terminating unused processes
rst_selenium_process_access <- function(){

  if(is_available("ps")){
    # this is possible only if {ps} is present
    spid <- sys_find_selenium_pid()

    # Early exit if no selenium is running
    if(is.null(spid)) return(invisible(list()))

    sh <- ps::ps_handle(pid = spid)

    lo <- list(selenium_handle = sh)

    kill_browser_driver_raw <- function(browsers){

      # find all children
      all_children <- ps::ps_children(sh, recursive = TRUE)
      names <- sapply(all_children, ps::ps_name)

      ddn <- data.frame(
        brow = c("chrome", "firefox", "opera", "edge"),
        drvn = c("chromedriver","geckodriver","operadriver","edgedriver")
      )

      for(i in seq(nrow(ddn))){
        if(ddn$brow[i] %in% browsers){
          cchk <- grepl(ddn$drvn[i],names)
          if(any(cchk)){
            cph <- all_children[which(cchk)]
            lapply(cph, sys_ps_kill_tree)
          }
        }
      }

      invisible(0)

    }

    lo$kill_browser_driver <- function(browsers = "all"){
      if("all" %in% browsers) browsers <- c("chrome", "firefox","opera", "edge")
      tryCatch(kill_browser_driver_raw(browsers),
               error = function(e) NULL)
      return(invisible(0))
    }

    lo$kill_selenium <- function(){
      invisible(sys_ps_kill_tree(sh))
    }

    return(invisible(lo))

  }

  invisible(list())

}
