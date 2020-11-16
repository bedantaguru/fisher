
#' Make the selenium_storm ready for future (async)
#'
#' @export
#'
make_selenium_storm_future_ready <- function(n){
  cat("\nNote this is in Alpha stage (not guaranteed to work all times)\n")
  if(rlang::is_installed("furrr") & rlang::is_installed("future")){
    if(check_selenium_strom()){
      rd <- get_control_client()
      attempt(rd$closeall(), wait_time = 1)

      num_cores <-parallel::detectCores()

      if(!missing(n)){
        future::plan(list(future::tweak(future::multiprocess, workers = as.integer(n))))
      }else{
        future::plan(future::multiprocess)
      }




      pids <- num_cores %>% seq %>% furrr::future_map_int(~Sys.getpid()) %>% unique()

      if(length(pids)!=num_cores){
        warning("multiprocess future is not running in full potential. Only ", length(pids), " processes out of ", num_cores, " is active.")
      }

      a1 <- num_cores %>% seq %>% furrr::future_map(~{
        Sys.time() %>% as.numeric() %>% set.seed() %>% c(runif(1)) %>% Sys.sleep()
        rd <- attempt(client_instant(), wait_time = 1)
      })



      a2 <- num_cores %>% seq %>% furrr::future_map(~{
        Sys.time() %>% as.numeric() %>% set.seed() %>% c(runif(1)) %>% Sys.sleep()
        rd <- attempt(client_instant_fast(), wait_time = 1)
      })

      a3 <- num_cores %>% seq %>% furrr::future_map_int(~{
        rd <- attempt(selenium_storm_client(), wait_time = 1)
        Sys.getpid()
      })

      if(length(a3)!=num_cores){
        warning("multiprocess future is not running in full potential (after obtaining selenium_storm_client).\nOnly ", length(a3), " processes out of ", num_cores, " is active.")
      }


    }else{
      stop("Run selenium_strom first")
    }

  }else{
    stop("furrr or future is not installed. These packages are required.")
  }
  invisible(0)
}
