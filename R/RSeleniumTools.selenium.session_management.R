
# rst_selenium_session_management -->>> rst_ssm
# it needs an empty remote control client (rcc)
# which can be done via rcc <- rst_remotedriver() and not opening it


rst_ssm_quick_access_env <- new.env()

rst_ssm_get_control_client <- function(){
  if(exists("rcc", envir = rst_ssm_quick_access_env)){
    rcc <- rst_ssm_quick_access_env$rcc
  }else{
    rcc <- rst_remotedriver(vanilla = TRUE)
    assign("rcc", envir = rst_ssm_quick_access_env)
  }
  rcc
}

rst_ssm_get_active_sessions <- function(
  close_inactive_sessions = TRUE,
  return_test_function = FALSE
){
  rcc <- rst_ssm_get_control_client()
  rcenv <- environment(rcc$open)

  test_id_raw <- function(id){
    qpath <- sprintf("%s/session/%s/url", rcenv$serverURL, id)
    rcenv$queryRD(qpath)
    !("message" %in% names(rcenv$.self$value))
  }

  test_id <- function(id){
    if(missing(id)){
      id <- rcenv$sessionid
    }
    chk <- tryCatch(
      test_id_raw(id),
      error = function(e) FALSE)
    if(!is.logical(chk)){
      FALSE
    }else{
      chk[1]
    }
  }

  #  in case only test function is required (early exit)
  if(return_test_function){
    return(test_id)
  }

  all_sessions <- rcc$getSessions()

  # early exit without doing anything further as there is no sessions
  if(length(all_sessions)==0) return(all_sessions)

  all_sessions_id <- sapply(all_sessions, "[[","id")

  all_sessions_chk <- suppressMessages(sapply(all_sessions_id, test_id))

  active_sessions <- all_sessions[all_sessions_chk]

  if(close_inactive_sessions & any(!all_sessions_chk)){
    # close inactive (orphaned or killed manually) sessions
    close_id <- function(id){
      qpath <- sprintf("%s/session/%s", rcenv$serverURL, id)
      try(rcenv$queryRD(qpath, "DELETE"), silent = TRUE)
    }
    suppressMessages(lapply(all_sessions_id[!all_sessions_chk], close_id))
  }

  active_sessions

}

rst_ssm_is_active <- function(session_id){

  if(exists("is_active", envir = rst_ssm_quick_access_env)){
    is_active <- rst_ssm_quick_access_env$is_active
  }else{
    is_active <- rst_ssm_get_active_sessions(return_test_function = TRUE)
    assign("is_active", envir = rst_ssm_quick_access_env)
  }
  is_active(session_id)
}

rst_ssm_attach_to_active_session <- function(client, session_id){
  chk <- rst_ssm_is_active(session_id)
}
