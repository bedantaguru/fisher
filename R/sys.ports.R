

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
  force  = FALSE){

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
      if(force){
        # try to kill it
        # {ps} is required for this
        if(is_available("ps") & !is.null(pmap)){
          try({
            early_pid <- pmap$pid[pmap$port==this_port]
            ph <- ps::ps_handle(as.integer(early_pid)[1])
            ps::ps_kill(ph)
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
