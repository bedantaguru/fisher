
# This is for separate access for selenium through {ps}
# This may be helpful for quick terminating unused processes
rst_selenium_process_access <- function(){
  if(is_available("ps")){
    # this is possible only if {ps} is present
    spid <- sys_find_selenium_pid()
    sh <- ps::ps_handle(pid = spid)

    lo <- list(selenium_handle = sh)

    kill_browser_driver <- function(browsers){

      # find all children
      all_children <- ps::ps_children(sh, recursive = TRUE)
      names <- sapply(all_children, ps::ps_name)

      if("chrome" %in% browsers){
        cchk <- grepl("chromedriver",names)
        if(any(cchk)){
          cph <- all_children[[which(cchk)[1]]]
          ps::ps_kill(cph)
          # @Dev
        }
      }
    }



  }else{
    invisible(list())
  }
}
