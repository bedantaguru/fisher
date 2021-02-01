

# operate on port (Listening port) level x PID

sys_get_pid_port_map <- function(){
  sys_use_os_specific_method("sys_get_pid_port_map")
}


sys_get_pid_port_map_windows <- function(){
  dmap <- NULL
  str <- sys_cmd("netstat -ano -p tcp")

  ti <- grepl("pid", tolower(str))

  if(any(ti)){
    str <- str[seq(which(ti)[1]+1, length(ti), by = 1)]
    ds <- read.table(text = gsub(" +",",", str), header = FALSE, sep = ",")
    ds <- ds[-1]
    colnames(ds) <- c(
      "proto",
      "local_address",
      "foreign_address",
      "state",
      "pid"
    )
    ds <- ds[tolower(ds$state)=="listening",]
    ds$port <- sapply(strsplit(ds$local_address, ":"), function(x) rev(x)[1])
    ds$port <- suppressWarnings(as.integer(ds$port))

    dmap <- unique(ds[c("pid","port")])
  }

  dmap

}


sys_is_port_free <- function(port, pmap){
  # try by looking at complete Listening port mapping
  # pmap can be supplied for faster search possibly in loop

  pok <- FALSE

  if(missing(pmap)){
    pmap <- sys_get_pid_port_map()
  }

  if(is.null(pmap)){
    # try socketConnection in this situation

    sc <- tryCatch(
      suppressWarnings(
        socketConnection(host = "localhost", port = port, timeout = 1)
      ),
      error = function(e) TRUE
    )

    if(isTRUE(sc)){
      pok <- TRUE
    }else{
      try(close(sc), silent = TRUE)
    }

  }else{
    if(!(port %in% pmap$port)){
      pok <- TRUE
    }
  }

  pok

}

sys_get_a_port <- function(
  desired_port,
  port_range = c(1028L, 65000L),
  force  = FALSE,
  kill_logic = function(){FALSE}){

  # ref on port range
  # https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_74/cl/addtcpport.htm
  # 1-65535 (1024-65535) we are taking 1028 to 65000

  if(missing(desired_port)){
    desired_port <- as.integer(
      sample(seq(port_range[1], port_range[2]), size = 1))
  }

  pmap <- try(sys_get_pid_port_map(), silent = TRUE)

  if(!is.data.frame(pmap)) pmap <- NULL

  this_port <- desired_port
  repeat{
    if(sys_is_port_free(this_port, pmap = pmap)){
      break()
    }else{
      get_fresh <- TRUE

      # try the logic whether to kill it or not
      if(!force){
        force <- kill_logic()
      }

      if(force){
        # try to kill it
        # {ps} is required for this
        if(is_available("ps") & !is.null(pmap)){
          try({
            early_pid <- pmap$pid[pmap$port==this_port]
            ph <- ps::ps_handle(as.integer(early_pid)[1])
            # simple kill ps::ps_kill(ph)
            # but since we have better alternative :-)
            sys_ps_kill_tree(ph)
            Sys.sleep(0.1)
            if(!ps::ps_is_running(ph)){
              # update pmap
              pmap <- pmap[pmap$port!=this_port,]
              get_fresh <- FALSE
            }

          }, silent = TRUE)

        }
      }
      if(get_fresh){
        this_port <- as.integer(
          sample(seq(port_range[1], port_range[2]), size = 1))
      }
    }

  }

  as.integer(this_port)

}


sys_is_port_running_selenium <- function(port, timeout_sec = 0.1){

  l <- tryCatch({

    if(is_available("httr")){
      # faster detection as time out can be configured here
      str1 <- httr::content(
        httr::GET(
          paste0("http://localhost:",port, "/wd/hub/status"),
          httr::timeout(timeout_sec)
        ),
        as = "text", encoding = "UTF-8")
      str2 <- httr::content(
        httr::GET(
          paste0("http://localhost:",port,"/wd/hub/sessions"),
          httr::timeout(timeout_sec)
        ),
        as = "text", encoding = "UTF-8")
    }else{
      # slow detection (do not know how to configure timeout here)

      suppressWarnings({
        str1 <- readLines(
          paste0("http://localhost:",port,"/wd/hub/status"),
          warn = FALSE)
        str2 <- readLines(
          paste0("http://localhost:",port,"/wd/hub/sessions"),
          warn = FALSE)
      })

    }


    # it can be true mostly if it is a selenium
    any(grepl("server is running",tolower(str1))) &
      any(grepl("status",tolower(str2)))
  },
  error = function(e) FALSE,
  finally = FALSE)

  if(!is.logical(l)) return(FALSE)
  if(is.na(l)) return(FALSE)

  l
}

# This should be able to detect selenium running state which may be initiated by
# following things:-
#
# 1) this R-session
#
# 2) Separate R-session
#
# 3) Not by any R-session
#
sys_find_selenium_pid <- function(full_search = TRUE){

  # quick detection in case already started by {fisher}
  if(exists("s_handle",envir = rst_wdman_selenium_info_env)){
    if(rst_wdman_selenium_info_env$s_handle$process$is_alive()){
      return(rst_wdman_selenium_info_env$s_handle$process$get_pid())
    }
  }

  pmap <- sys_get_pid_port_map()

  # early exit
  if(nrow(pmap)==0) return(NULL)

  # if orphaned by previous run
  # this usually will not happen

  if(is_available("ps")){
    all_childs <- ps::ps_children(recursive = TRUE)
    child_pids <- sapply(all_childs, ps::ps_pid)
    pmap_small <-pmap[pmap$pid %in% child_pids,]
    c0 <- sapply(pmap_small$port, sys_is_port_running_selenium)
    if(any(c0)){
      # set rst_wdman_selenium_info_env to the extent possible
      rst_wdman_selenium_fill_info_env(
        pmap_small$pid[c0],
        pmap_small
      )
      return(pmap_small$pid[c0])
    }
  }


  c1 <- sapply(pmap$port, sys_is_port_running_selenium)

  if(!any(c1) & full_search){
    # try with more timeout on expected pids

    # first check pid names
    # {ps} required

    if(is_available("ps")){
      psh <- ps::ps()
      pshp <- merge(psh, pmap, by = "pid", suffixes = c("","_p"), all = FALSE)
      jschk <- grepl("java|selenium", pshp$name)
      # try on these with more timeout
      if(!any(jschk)){
        # else try all
        jschk <- rep(TRUE, length(jschk))
      }
      c2 <- sapply(pshp$port[jschk],
                   sys_is_port_running_selenium,
                   timeout_sec = 1)
      if(any(c2)){
        # match that to c1
        got_ports <- pshp$port[jschk][c2]
        c1[pmap$port %in% got_ports] <- TRUE
      }
    }
  }

  if(any(c1)){
    # set rst_wdman_selenium_info_env to the extent possible
    rst_wdman_selenium_fill_info_env(
      pmap$pid[c1],
      pmap
    )
    return(pmap$pid[c1])
  }

  return(NULL)


}
