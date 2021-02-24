






# rst: {RSelenium} Tools
# alt names
# wba_server
# web_browser_automation_server
# web_automation_server
# web_automation_platform

# should be used jointly with web_control_client
# and possibly a web_automation_supervisor

# ref
# https://github.com/jlipps/simple-wd-spec#list-of-all-endpoints
# https://github.com/jlipps/simple-wd-spec#new-session

# @Dev has to make somthing like and fuse with alternatives
# web_automation_platform_alt_rst for now just using web_automation_platform
# as there is only one alternative now

rst_wap_env <- new.env()

rst_wap_config <- function(init = FALSE, client_config){
  ws <- wap_config_store()
  if(init){
    opid <- ws$read("wap_rst_master_r_pid")

    if(!is.null(opid)){
      if(!sys_is_pid_active(opid)) opid <- NULL
    }

    if(isTRUE(opid==Sys.getpid()) |
       is.null(opid)){
      ws$write("wap_type","RSeleniumTools")
      if(!is.null(rst_wdman_selenium_info_env$s_port)){
        ws$write("wap_rst_port", rst_wdman_selenium_info_env$s_port)
        ws$write("wap_rst_pid",
                 rst_wdman_selenium_info_env$s_handle$process$get_pid())
      }
      ws$write("wap_rst_master_r_pid", Sys.getpid())
      if(!missing(client_config)){
        ws$write("wap_rst_client_config",client_config, R_object = TRUE)
      }
    }
    return(invisible(0))
  }

  # add few more functions
  # sid : selenium session id

  get_sid <- function(pid){
    ws$read(paste0("rst_pid_",pid))
  }

  set_sid <- function(pid, sid){
    ws$write(paste0("rst_pid_",pid), sid)
  }

  get_pid <- function(sid){
    ws$read(paste0("rst_sid_",sid))
  }

  set_pid <- function(sid, pid){
    ws$write(paste0("rst_sid_",sid), pid)
  }

  # lock mechanism this requires {filelock}

  lock_file_sid <- function(sid){
    ws$write(paste0("rst_slock_",sid), "", get_file_path_only = TRUE)
  }

  lock_to_sid <- function(sid){
    is_locked <- FALSE
    if(is_available("filelock")){
      fl <- NULL
      tryCatch({
        lf <- lock_file_sid(sid)
        # create it if not present
        if(!file.exists(lf)) file.create(lf)

        fl <- filelock::lock(lf, timeout = 10)

        if(!is.null(fl)){
          is_locked <- TRUE
          # we can keep record of locked sids
          # rst_wap_env$filelocks[[sid]] <- fl
          # or we can simply keep a single record so that it is not garbage
          # collected
          assign("rst_wap_sid_file_lock", fl, envir = rst_wap_env)
        }

      }, error = function(e) NULL)
    }else{
      # not a lock at all
      is_locked <- TRUE
    }
    is_locked
  }

  check_pid_sid <- function(pid, sid){
    chk <- TRUE

    psid <- get_sid(pid)
    spid <- get_pid(sid)

    # special case of self bind
    if(pid==Sys.getpid()){
      # check if the target sid can be locked
      chk <- lock_to_sid(sid)
    }

    if(!is.null(spid) & chk){
      # check if pid previously bind to sid is active or not
      if(sys_is_pid_active(spid)){
        # if active and not equal to current pid don't bind
        # this means it (sid) is already used by spid
        if(spid!=pid) chk <- FALSE
      }
    }

    if(chk & !is.null(sid)){
      # check if target sid is active
      chk <- rst_ssm_is_active(sid)
    }

    if(!is.null(psid) & chk){
      # check if sid previously bind to pid is active or not
      if(rst_ssm_is_active(psid)){
        # if it is active that means only that sid is bind-able to this pid
        if(psid!=sid) chk <- FALSE
      }
    }


    chk
  }

  bind_pid_sid <- function(pid, sid){
    # special case of self bind
    if(pid==Sys.getpid()){
      lock_to_sid(sid)
    }
    set_sid(pid, sid)
    set_pid(sid, pid)
  }

  ws$rst <- list(
    check_pid_sid = check_pid_sid,
    bind_pid_sid = bind_pid_sid,
    lock_to_sid = lock_to_sid,
    get_sid = get_sid,
    set_sid = set_sid,
    get_pid = get_pid,
    set_pid = set_pid
  )

  invisible(ws)

}


#' web automation platform
#'
#' @param client_config config
#'
#' @export
web_automation_platform <- function(
  client_config = rst_remotedriver(get_config_list_only = TRUE)
){
  rst_wdman_selenium_launcher(on_exit_cleanup = TRUE)
  rst_wap_config(init = TRUE, client_config = client_config)
}



#' web control client
#'
#' @export
web_control_client <- function(){

  # check prior saved client

  if(exists("web_control_client_cache",envir = rst_wap_env)){
    rd <- rst_wap_env$web_control_client_cache
    if(rst_remotedriver_check(rd)){
      return(invisible(rd))
    }
  }

  # link selenium instance if possible
  sys_find_selenium_pid()

  ws <- rst_wap_config()
  psid <- ws$rst$get_sid(Sys.getpid())
  if(!is.null(psid)){
    if(!rst_ssm_is_active(psid)){
      psid <- NULL
    }
  }

  if(is.null(psid)){
    # one more try to find free sid
    psid <- rst_ssm_find_free_session_id()
  }

  shared_config <- ws$read("wap_rst_client_config", R_object = TRUE)
  if(is.null(shared_config)) shared_config <- list()

  rd <- do.call("rst_remotedriver", args = shared_config)
  if(is.null(psid)){
    # this means fresh sid need to be generated
    info <- rd$open(silent = TRUE)
    ws$rst$bind_pid_sid(Sys.getpid(), info$id)
  }else{
    # this means old sid can be reused
    rst_ssm_attach_to_active_session(rd, psid)
    ws$rst$bind_pid_sid(Sys.getpid(), psid)
  }

  # save this rd for future use
  assign("web_control_client_cache", rd, envir = rst_wap_env)

  invisible(rd)
}




