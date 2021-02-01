

rst_remotedriver_specific_config_chrome <- function(
  proxy = FALSE, proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder
){

  cnf <- list()

  if(proxy){
    # @Dev
  }

  if(best_known_settings){
    bks  <- wap_browser_config_implementer_chrome(
      arg_lst = list('--window-size=1280,800',
                     '--no-sandbox',
                     '--disable-blink-features=AutomationControlled'),
      raw_lst = list(excludeSwitches = list("enable-automation"),
                     useAutomationExtension = FALSE))

    cnf <- merge_list(cnf, bks)
  }


}

