

rst_remotedriver_specific_config_chrome <- function(
  # proxy
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder
){

  # ref
  # https://peter.sh/experiments/chromium-command-line-switches/
  cnf <- list()

  if(proxy){
    # proxy is set at argument level
    # Ref
    # https://www.chromium.org/developers/design-documents/network-settings
    pl  <- wap_browser_config_implementer_chrome(
      arg_lst = list(
        paste0('--proxy-server=',proxy_host,':', proxy_port)
      ))

    cnf <- merge_list(cnf, pl)
  }

  if(best_known_settings){
    # Ref
    #
    # https://github.com/ropensci/RSelenium/issues/207
    # https://stackoverflow.com/questions/57298901/
    #
    bks  <- wap_browser_config_implementer_chrome(
      arg_lst = list('--window-size=1280,800',
                     '--no-sandbox',
                     '--ignore-certificate-errors',
                     '--disable-notifications',
                     '--disable-blink-features=AutomationControlled'),
      raw_lst = list(excludeSwitches = list("enable-automation"),
                     useAutomationExtension = FALSE))

    cnf <- merge_list(cnf, bks)
  }

  if(headless){
    # Ref
    # https://www.scrapingbee.com/blog/introduction-to-chrome-headless/
    hl  <- wap_browser_config_implementer_chrome(
      arg_lst = list('--headless',
                     '--disable-gpu',
                     '--disable-extensions',
                     '--disable-dev-shm-usage')
    )

    cnf <- merge_list(cnf, hl)
  }

  if(download_capture){

    dl  <- wap_browser_config_implementer_chrome(
      conf_lst = list(
        savefile =
          list(default_directory = normalizePath(download_folder)),
        download =
          list(default_directory = normalizePath(download_folder),
               prompt_for_download = FALSE))

    )

    cnf <- merge_list(cnf, dl)
  }

  cnf

}


#@Dev
rst_remotedriver_specific_config_firefox <- function(
  # proxy
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder
){

  # ref
  # https://peter.sh/experiments/chromium-command-line-switches/
  cnf <- list()

  if(proxy){
    # recorded and supervised values
    sf <- list(
      network.proxy.backup.ftp = "",
      network.proxy.backup.ftp_port = 0,
      network.proxy.backup.ssl = "",
      network.proxy.backup.ssl_port = 0,
      network.proxy.ftp = proxy_host,
      network.proxy.ftp_port = as.integer(proxy_port),
      network.proxy.http = proxy_host,
      network.proxy.http_port = as.integer(proxy_port),
      network.proxy.share_proxy_settings = TRUE,
      network.proxy.ssl = proxy_host,
      network.proxy.ssl_port = as.integer(proxy_port),
      network.proxy.type = 1
    )



  }

  if(best_known_settings){
    # @Dev
    # Ref
    #
    # https://github.com/ropensci/RSelenium/issues/207
    # https://stackoverflow.com/questions/57298901/
    #
    bks  <- wap_browser_config_implementer_firefox(
      arg_lst = list(),
      raw_lst = list(excludeSwitches = list("enable-automation"),
                     useAutomationExtension = FALSE))

    cnf <- merge_list(cnf, bks)
  }

  if(headless){
    hl  <- wap_browser_config_implementer_firefox(
      arg_lst = list('--headless')
    )

    cnf <- merge_list(cnf, hl)
  }

  if(download_capture){
    # @Dev

    dl  <- wap_browser_config_implementer_firefox()

    cnf <- merge_list(cnf, dl)
  }

  cnf

}
