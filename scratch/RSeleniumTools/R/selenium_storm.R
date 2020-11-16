
#' Start a Selenium Storm instance
#'
#' @param port port to listen for selenium
#' @param check If set to \code{TRUE}, checks the versions of selenium available and the versions of associated drivers (chromever, geckover, phantomver, iedrver).
#' If new versions are available they will be downloaded.
#' @param clean_start If set to \code{TRUE} it discards old config and starts fresh.
#' @param singular_pid_sid If set to \code{TRUE} only one selenium session per process will be allowed. Default is \code{TRUE}.
#' @param num_sessions number of sessions to allow. If not mentioned it will take values = \code{parallel::detectCores()}.
#' @param Browser (optional) The browser to initiate by default.
#' @param headless (optional) If set \code{TRUE} it will start a headless browser.
#' @param fresh_start Restarts Selenium Storm.
#'
#' @return It returns invisibly the selenium handle (as returned by wdman::selenium)
#' @export
#'
#' @examples
#' selenium_storm()
selenium_storm <- function(port = 15318L,
                           Browser,
                           headless,
                           fresh_start = F,
                           check = FALSE,
                           clean_start =  TRUE,
                           singular_pid_sid = TRUE,
                           num_sessions) {

  if(fresh_start){
    selenium_storm_kill_all()
  }

  client_config_set <- F
  if(!missing(Browser) |!missing(headless)){
    if(is.logical(headless)){
      if(missing(Browser)){
        Browser <- valid_Browser()
      }else{
        Browser <- valid_Browser(Browser)
      }
      client_config_set <- T
    }
  }

  selenium_storm_init(port = port,
                      check = check,
                      clean_start = clean_start,
                      singular_pid_sid = singular_pid_sid,
                      num_sessions = num_sessions,
                      Browser = Browser,
                      headless = headless,
                      client_profile_set = client_config_set)

  st <- get_selenium_storm_storr()

  sst <- get_selenium_storm_storr(session = T)

  getnew <- T
  selServ <- NULL

  if(st$exists("last_selenium_pid","config")){
    last_selenium_pid <- st$get("last_selenium_pid", "config")
    if(is_pid_active(last_selenium_pid)){
      if(sst$exists("selenium","handles")){
        selServ <- sst$get("selenium", "handles")
        getnew <- F
      }else{
        stop(paste0("Selenium is running already with pid:", last_selenium_pid," [Also not possible to retrieve process handle]"))
      }
    }
  }

  if(getnew){
    selServ <- wdman::selenium(port = port,
                               verbose = FALSE,
                               check = check)

    st$set("last_selenium_pid", as.integer(selServ$process$c_handle), namespace = "config")

    sst$set("selenium", selServ, "handles")

  }

  invisible(selServ)
}

#' Check for Selenium Storm instance (can be called in different process also)
#'
#' @return Logical value indicating whether the Selenium Storm is up and running or not.
#' @export
#'
#' @examples
#' check_selenium_strom()
check_selenium_strom <- function(){

  st <- get_selenium_storm_storr(session = T)
  if(!st$exists("port","config")){
    sync_session_config()
  }

  chk1 <- F
  chk2 <- F

  if(st$exists("last_pid","config")){
    last_pid <- st$get("last_pid", "config")
    if(is_pid_active(last_pid)){
      chk1 <- T
    }
  }

  if(st$exists("last_selenium_pid","config")){
    last_selenium_pid <- st$get("last_selenium_pid", "config")
    if(is_pid_active(last_selenium_pid)){
      chk2 <- T
    }
  }

  if(!chk1 & !chk2){
    warning("selenium_storm not running")
  }

  if(!chk1 & chk2){
    warning("selenium_storm is not running! but selenium is started and running")
  }

  if(chk1 & !chk2){
    warning("selenium_storm running! but selenium not started")
  }

  chk1 & chk2

}


#' Get a Selenium Storm Client for oprations
#'
#' @param Browser which browser to start
#' @param headless whether headless of not
#' @param wait_time specific wait time (in min)
#' @param final_active_check if set true whether session is active or not will be checked each time.
#' @param ... additional parameter to be passed to remote Driver (RSelenium)
#'
#' @return Returns a client
#' @export
#'
#' @examples
#' selenium_storm_client()
selenium_storm_client <- function(Browser, headless, wait_time = Inf, final_active_check = F, ...){

  if(!check_selenium_strom()){
    stop("selenium_strom not running")
  }

  cl <- NULL

  vl <- get_unified_client_config(Browser, headless)
  Browser <- vl$Browser
  headless <- vl$headless

  t0 <- Sys.time()
  repeat({

    # first try using fast mode

    cl <- try(client_instant_fast(Browser = Browser, headless = headless, ...), silent = T)

    if(final_active_check){
      chk1 <- try(is_active(cl), silent = T)
      if(!is.logical(chk1)) chk1 <- F
    }else{
      chk1 <- T
    }


    if(inherits(cl,"remoteDriver") & chk1){
      break()
    }

    # then regular mode

    cl <- try(client_instant(Browser = Browser, headless = headless, ...), silent = T)

    chk2 <- try(is_active(cl), silent = T)
    if(!is.logical(chk2)) chk2 <- F

    if(inherits(cl,"remoteDriver") & chk2){
      break()
    }

    t1 <- Sys.time()
    te <- difftime(t1, t0, units = "min")

    if(te>wait_time){
      break()
    }

  })

  cl

}

#' Free the Selenium Storm Client that was currently used
#'
#' @param client client to free
#'
#' @export
#'
free_selenium_storm_client <- function(client){

  st <- get_selenium_storm_storr()
  e <- environment(client$open)
  st$del(key = e$sessionid, namespace = "sessions")

}

