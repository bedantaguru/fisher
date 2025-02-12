
selenium_storm_init <- function(port, check, clean_start = F, singular_pid_sid = T, num_sessions,
                                Browser, headless, client_profile_set){

  if(clean_start){
    unlink(get_selenium_storm_storr_base_path(), recursive = T, force = T)
  }

  if(missing(num_sessions)){
    num_sessions <- parallel::detectCores()
  }
  st <- get_selenium_storm_storr()


  if(clean_start){
    st$destroy()
    st <- get_selenium_storm_storr()
  }

  create_if_not_exists <- function(key, default, ns = "config", force = F){
    if(!st$exists(key, namespace = ns)){
      st$set(key, default, namespace = ns)
    }
    if(force){
      st$set(key, default, namespace = ns)
    }
    st$get(key, ns)
  }

  last_pid <- create_if_not_exists("last_pid", Sys.getpid())

  pid_changed <- F

  if(last_pid!=Sys.getpid()){
    if(is_pid_active(last_pid)){
      stop(paste0("selenium_storm is running already with pid:", last_pid))
    }else{
      st$set("last_pid", Sys.getpid(), "config")
      pid_changed <- T
    }
  }

  # clean locks folder
  unlink(get_selenium_storm_lock_path(""), recursive = T)

  create_if_not_exists("port", port, force = pid_changed)
  create_if_not_exists("check", check, force = pid_changed)
  create_if_not_exists("singular_pid_sid", singular_pid_sid, force = pid_changed)
  create_if_not_exists("client_profile_set", client_profile_set, force = pid_changed)

  if(client_profile_set){
    create_if_not_exists("Browser", Browser, ns = "client_config", force = pid_changed)
    create_if_not_exists("headless", headless, ns = "client_config", force = pid_changed)
  }

  create_if_not_exists("num_cores", num_sessions, force = pid_changed)

  version <- create_if_not_exists("version", get_version())
  if(version!=get_version()){
    warning("Version mismatch. (if not running consider clean_start = T)")
  }



}


selenium_storm_kill_all <- function(){
  st <- get_selenium_storm_storr()
  if(st$exists("last_pid", "config")){
    last_pid <- st$get("last_pid", "config")
    if(last_pid!=Sys.getpid()){
      if(is_pid_active(last_pid)){
        warning(paste0("selenium_storm is running already with pid:", last_pid, "\nThis will not be killed but ownership will be taken away from this process (attempt will be made)."))
      }
    }
  }

  if(st$exists("last_selenium_pid", "config")){
    last_selenium_pid <- st$get("last_selenium_pid", "config")
    if(is_pid_active(last_selenium_pid)){
      warning(paste0("Selenium is running already with pid:", last_selenium_pid, " This will be killed (will be tried to kill)."))
      while(is_pid_active(last_selenium_pid)){
        try({
          pp <- ps::ps_handle(pid = as.integer(last_selenium_pid))
          pc <- ps::ps_children(pp, recursive = T)
          purrr::map(pc, ps::ps_kill)
          ps::ps_kill(pp)
        }, silent = T)
      }
    }
  }

  sst <- get_selenium_storm_storr(T)
  sst$destroy()

  st$destroy()
  unlink(get_selenium_storm_storr_base_path(), recursive = T, force = T)

}

stored_client_config <- function(){

  conf <- list(client_profile_set = F)
  st <- get_selenium_storm_storr(session = T)
  if(!st$exists("port","config")){
    sync_session_config()
  }

  if(st$exists("client_profile_set","config")){
    if(st$get("client_profile_set","config")){
      conf$Browser <- st$get("Browser","client_config")
      conf$headless <- st$get("headless","client_config")
      conf$client_profile_set <- T
    }
  }

  conf

}

get_unified_client_config <- function(Browser, headless){
  conf <- list()

  if(missing(Browser)){
    st_conf <- stored_client_config()
    if(st_conf$client_profile_set){
      Browser <- st_conf$Browser
    }else{
      Browser <- valid_Browser()
    }
  }else{
    Browser <- valid_Browser(Browser)
  }

  if(missing(headless)){
    st_conf <- stored_client_config()
    if(st_conf$client_profile_set){
      headless <- st_conf$headless
    }else{
      headless <- F
    }
  }else{
    headless <- as.logical(headless[1])
  }

  conf$Browser <- Browser
  conf$headless <- headless

  conf

}

client_new <- function(Browser, headless, start_browser = T, ...){

  vl <- get_unified_client_config(Browser, headless)
  Browser <- vl$Browser
  headless <- vl$headless

  if (identical(Browser, "internet explorer") &&
      !identical(.Platform[["OS.type"]], "windows")) {
    stop("Internet Explorer is only available on Windows.")
  }

  if(!(Browser %in% c("chrome","phantomjs") ) & headless){
    stop("Yet not implemented headless browsing with this Browser")
  }

  if(!check_selenium_strom()){
    stop("Can't create remoteDriver instance without selenium_strom properly running")
  }

  st <- get_selenium_storm_storr(session = T)
  if(!st$exists("port","config")){
    sync_session_config()
  }

  exnames <- names(list(...))

  if(("extraCapabilities" %in% exnames) & Browser=="chrome" & headless ){
    warning("Browser = 'chrome', headless = TRUE and additionally extraCapabilities supplied. \nHence headless will be disabled (possibly it has to be taken care by extraCapabilities)")
    headless <- F
  }

  if(Browser=="chrome" & headless){
    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800','--incognito','--disable-extensions')
    ))

    remDr <- RSelenium::remoteDriver(browserName = Browser, port = as.integer(st$get("port", "config")), extraCapabilities = eCaps, ...)
  }else{
    remDr <- RSelenium::remoteDriver(browserName = Browser, port = as.integer(st$get("port", "config")), ...)
  }

  count <- 0
  while (
    inherits(res <- tryCatch(remDr$getStatus(), error = function(e) e), "error")
  ) {
    Sys.sleep(1)
    count <- count + 1
    if (count > 10) {
      warning("Could not determine server status. (after 10 secs)")
      break
    }
  }

  if(start_browser){
    open_browser(remDr)
  }

  invisible(remDr)

}


get_control_client <- function(){

  sst <- get_selenium_storm_storr(session = T)
  if(sst$exists("control_client", "handles")){
    cc <- sst$get("control_client", "handles")
  }else{
    cc <- client_new(start_browser = F)
    sst$set("control_client", cc,"handles")
  }
  invisible(cc)
}


sessions_cleanup.sessions <- function(){

  st <- get_selenium_storm_storr()

  stored_sessions <- st$list("sessions")
  active_sessions <- get_active_sessions(get_control_client()) %>% purrr::map_chr("id")

  delinked <- setdiff(stored_sessions, active_sessions)

  if(length(delinked)){
    delinked %>% purrr::map(~st$del(.x, namespace = "sessions"))
  }

  invisible(0)
}


sid_pid_map <- function(){
  st <- get_selenium_storm_storr()

  sst <- get_selenium_storm_storr(session = T)

  dsi <- data.frame(si = st$list("sessions"), stringsAsFactors = F)
  safeget <- function(x){
    gt <- try({st$get(x,namespace = "sessions")}, silent = T)
    if(inherits(gt,"try-error")) return(NA)
    gt
  }
  dsi$pid <- dsi$si %>% purrr::map_chr(safeget) %>%  as.integer()
  dsi$is_dead_pid <- !is_pid_active(dsi$pid)
  sst$set("sid_pid_map", dsi, "cache_data")
  dsi
}

sid_pid_map_fast <- function(){
  sst <- get_selenium_storm_storr(session = T)
  d <- NULL

  if(!sst$exists("sid_pid_map", "cache_data")){
    sid_pid_map()
  }else{
    d <- sst$get("sid_pid_map", "cache_data")
    if(nrow(d)==0){
      sid_pid_map()
      d <- sst$get("sid_pid_map", "cache_data")
    }
  }

  d
}

sessions_cleanup.pids <- function(){

  st <- get_selenium_storm_storr()

  dsi <- sid_pid_map()

  if(any(dsi$is_dead_pid)){
    dsi$si[dsi$is_dead_pid] %>% purrr::map(~st$del(.x, namespace = "sessions"))
  }

  invisible(dsi$si[dsi$is_dead_pid])
}

sessions_cleanup <- function(){
  sessions_cleanup.sessions()
  sessions_cleanup.pids()
}

free_sessions <- function(){

  sessions_cleanup()

  st <- get_selenium_storm_storr()

  stored_sessions <- st$list("sessions")
  active_sessions <- get_active_sessions(get_control_client()) %>% purrr::map_chr("id")

  setdiff(active_sessions, stored_sessions)

}

client_instant_fast <- function(Browser, headless, ...){

  vl <- get_unified_client_config(Browser, headless)
  Browser <- vl$Browser
  headless <- vl$headless

  # only tries to attach
  rt <- NULL
  st <- get_selenium_storm_storr(session = T)
  if(!st$exists("port","config")){
    sync_session_config()
  }


  if(st$get("singular_pid_sid","config")){

    dsi <- sid_pid_map_fast()

    if(any(dsi$pid == Sys.getpid())){
      this_sid <- dsi$si[dsi$pid == Sys.getpid()][1]


      dummy_client <- client_new(Browser = Browser, headless = headless, start_browser = F, ...)

      # fast attach_to_active_session is also required
      attach_ok <- attach_to_active_session(dummy_client, this_sid, fast = T)
      if(attach_ok){
        # diabled as no extra information will be added
        # open_browser(dummy_client)
        rt <- dummy_client
      }

    }
  }
  rt

}

client_instant <- function(Browser, headless, ...){

  vl <- get_unified_client_config(Browser, headless)
  Browser <- vl$Browser
  headless <- vl$headless

  active_sessions <- get_active_sessions(get_control_client())

  st <- get_selenium_storm_storr(session = T)
  if(!st$exists("port","config")){
    sync_session_config()
  }

  num_cores <- st$get("num_cores","config")
  singular_pid_sid <- st$get("singular_pid_sid","config")
  fss <- free_sessions()

  dummy_client <- client_new(Browser = Browser, headless = headless, start_browser = F, ...)

  if(singular_pid_sid){
    dsi <- sid_pid_map()
    any_sid_for_this_pid <- dsi$si[dsi$pid==Sys.getpid()]
    if(length(fss)==0){
      fss <- any_sid_for_this_pid
    }
  }

  if(length(fss)){
    attach_ok <- attach_to_active_session(dummy_client, fss[1])
    if(attach_ok){
      open_browser(dummy_client)
      return(dummy_client)
    }
  }else{
    if(length(active_sessions) < num_cores){
      open_browser(dummy_client)
      return(dummy_client)
    }
  }

  return(NULL)
}
