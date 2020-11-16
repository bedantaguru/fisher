

open_browser <- function(client){
  er <- suppressMessages(suppressWarnings(try(client$getSession(), silent = T)))
  store <- T

  if(inherits(er,"try-error")){
    # only place for starting a session
    # this won't prevent situations where singular_pid_sid is false
    slock <- get_selenium_storm_session_lock()
    slocked <- slock$lock(wait = 10)
    if(slocked){
      res <- tryCatch(client$open(silent = TRUE), error = function(e) e)
      if (inherits(res, "error")) {
        message("Could not open browser.")
        message("Client error message:\n", res$message)
        message("Check server log for further details.")
        store <- F
      }
    }
  }

  # after open check
  er <- suppressMessages(suppressWarnings(try(client$getSession(), silent = T)))

  if(inherits(er,"try-error")){
    store <- F
  }

  if(store){
    st <- get_selenium_storm_storr()
    e <- environment(client$open)
    st$set(key = e$sessionid, value = Sys.getpid(), namespace = "sessions")
  }
}


get_active_sessions <- function(client, close_inactive = T, return_test_function = F){

  e <- environment(client$open)

  test_id <- function(id){
    qpath <- sprintf("%s/session/%s/url", e$serverURL, id)
    e$queryRD(qpath)
    !("message" %in% names(e$.self$value))
  }
  test_id_safe <- function(id){
    ms <- suppressMessages(try(test_id(id), silent = T))
    if(inherits(ms, "try-error")){
      F
    }else{
      ms
    }
  }

  if(return_test_function){
    f <- function(id){
      if(missing(id)){
        id <- e$sessionid
      }
      test_id_safe(id)
    }
    return(f)
  }else{
    st <- get_selenium_storm_storr()
    sst <- get_selenium_storm_storr(session = T)
    all_sessions <- client$getSessions()
    st$set("all_sessions", all_sessions, "cache")
    sst$set("all_sessions", all_sessions, "cache")
    all_sessions_id <- all_sessions %>% lapply("[[","id")

    all_sessions_info <- all_sessions_id %>% lapply(test_id_safe) %>% unlist()
    active_sessions <- all_sessions[all_sessions_info]
    if(close_inactive & length(all_sessions_info)){
      close_loc <- function(id){
        qpath <- sprintf("%s/session/%s", e$serverURL, id)
        e$queryRD(qpath, "DELETE")
      }
      all_sessions_id[!all_sessions_info] %>% lapply(close_loc)
    }

    st$set("active_sessions", active_sessions, "cache")
    sst$set("active_sessions", active_sessions, "cache")

    return(active_sessions)
  }
  return(NULL)
}

is_active <- function(client, session_id){
  if(missing(session_id)){
    get_active_sessions(client = client, return_test_function = T)()
  }else{
    get_active_sessions(client = client, return_test_function = T)(session_id)
  }
}

lock_to_sid <- function(sid){
  if(missing(sid)){
    stop("no sid supplied")
  }
  sidlock <- get_selenium_storm_session_lock(sid = sid)
  sidlock$lock(wait = 10)
}

attach_to_active_session <- function(client, session_id, fast = F){
  aok <- F
  chk <- F
  if(!fast){
    active_sessions <- get_active_sessions(get_control_client())
    active_sessions_ids <- active_sessions %>% purrr::map_chr("id")
    all_sessions <- active_sessions
    chk <- session_id %in% active_sessions_ids
  }else{
    chk <- is_active(client, session_id)
  }

  e <- environment(client$open)
  if(chk){
    sid_locked <- lock_to_sid(session_id)
    if(sid_locked){

      fresh_get <- F
      nxt_level <- F

      if(fast){

        get_as <- function(stor){
          l <- list(chk = F)
          if(stor$exists("active_sessions","cache")){
            active_sessions <- stor$get("active_sessions","cache")
            active_sessions_id <- active_sessions %>% purrr::map_chr("id")
            if((session_id %in% active_sessions_id)){
              all_sessions <- active_sessions
            }else{
              nxt_level <- T
            }
          }else{
            nxt_level <- T
          }

          if(nxt_level){
            if(stor$exists("all_sessions","cache")){
              all_sessions0 <- stor$get("all_sessions","cache")
              all_sessions0_id <- all_sessions0 %>% purrr::map_chr("id")
              if((session_id %in% all_sessions0_id)){
                all_sessions <- all_sessions0
              }else{
                fresh_get <- T
              }
            }else{
              fresh_get <- T
            }

          }

          if(exists("all_sessions")){
            l$chk <- T
            l$all_sessions <- all_sessions
          }
          l
        }

        # try in session first
        ses_stor <- get_selenium_storm_storr(session = T)
        al_try <- get_as(ses_stor)

        if(!al_try$chk){
          # try in disk (global)
          al_try <- get_as(get_selenium_storm_storr())
          if(al_try$chk){
            # retain in session for fast reterival
            ses_stor$set("active_sessions", al_try$all_sessions,"cache")
          }
        }

        if(al_try$chk){
          all_sessions <- al_try$all_sessions
        }

      }

      if(!exists("all_sessions")){
        fresh_get <- T
      }

      if(fresh_get){
        all_sessions <- client$getSessions()
        ses_stor$set("active_sessions", all_sessions,"cache")
      }

      all_sessions_id <- all_sessions %>% purrr::map_chr("id")

      tar <- which(all_sessions_id == session_id)
      active_session <- all_sessions[[tar]]
      e$sessionInfo <- active_session
      e$sessionInfo$id <- session_id
      e$sessionid <- session_id
      aok <- T
    }
  }
  aok
}
