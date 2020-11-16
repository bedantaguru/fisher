

valid_Browser <- function(Browser = c("chrome", "firefox", "phantomjs", "internet explorer")){
  Browser <- match.arg(Browser)
  Browser
}

get_version <- function(){
  .RSeleniumToolsEnv$version
}

get_selenium_storm_storr_base_path <- function(){
  rappdirs::user_data_dir("selenium_storm", "RSeleniumTools")
}

get_selenium_storm_storr <- function(session = F){
  if(session){
    storr::storr_environment(.RSeleniumToolsEnv$selenium_storm_env)
  }else{
    get_selenium_storm_storr_base_path() %>% file.path("storr") %>%  storr::storr_rds()
  }
}

sync_session_config <- function(){
  sst <- get_selenium_storm_storr(session = T)
  if(!sst$exists("port", "config")){
    st <- get_selenium_storm_storr()
    st$export(dest = sst, namespace = "config")
    st$export(dest = sst, namespace = "client_config")
  }
}

get_selenium_storm_lock_path <- function(name = "sessions"){
  get_selenium_storm_storr_base_path() %>% file.path("locks", name)
}

get_selenium_storm_session_lock <- function(sid){

  if(missing(sid)){
    st <- get_selenium_storm_storr()
    if(st$exists("num_cores", "config")){
      num_cores <- as.integer(st$get("num_cores", "config"))
    }else{
      num_cores <- parallel::detectCores()
    }

    multiLocks::multi_lock(get_selenium_storm_lock_path(), allowed_number_of_jobs = num_cores, id = "overall_session_lock")
  }else{
    sid <- as.character(sid)
    multiLocks::multi_lock(get_selenium_storm_lock_path(name = sid), allowed_number_of_jobs = 1, id = "session_lock")
  }
}

is_pid_active <- function(pid){
  pid %in% ps::ps_pids()
}
