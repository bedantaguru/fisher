
rst_remotedriver_specific_config <- function(
  browser,
  # proxy
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder = ".",
  # 1.5 No password save options
  no_credential_store_offer = TRUE
){
  do.call(
    paste0("rst_remotedriver_specific_config_",browser),
    args = list(
      proxy = proxy,
      proxy_host = proxy_host,
      proxy_port = proxy_port,
      best_known_settings = best_known_settings,
      headless = headless,
      download_capture = download_capture,
      download_folder = download_folder,
      no_credential_store_offer = no_credential_store_offer))
}


rst_remotedriver_specific_config_chromium <- function(
  # proxy
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  # 1.5 No password save options
  no_credential_store_offer = TRUE,
  chromium_browser = "chrome"
){

  # ref
  # https://peter.sh/experiments/chromium-command-line-switches/
  cnf <- list()

  if(proxy){
    # proxy is set at argument level
    # Ref
    # https://www.chromium.org/developers/design-documents/network-settings
    pl  <- wap_browser_config_implementer(
      browser = chromium_browser,
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
    bks  <- wap_browser_config_implementer(
      browser = chromium_browser,
      arg_lst = list('--window-size=1280,800',
                     '--no-sandbox',
                     '--guest',
                     '--incognito',
                     '--disable-extensions',
                     '--disable-infobars',
                     '--ignore-certificate-errors',
                     '--disable-notifications',
                     '--disable-dev-shm-usage',
                     '--disable-blink-features=AutomationControlled'),
      raw_lst = list(excludeSwitches = list("enable-automation"),
                     useAutomationExtension = FALSE))

    cnf <- merge_list(cnf, bks)
  }

  if(headless){
    # Ref
    # https://www.scrapingbee.com/blog/introduction-to-chrome-headless/
    hl  <- wap_browser_config_implementer(
      browser = chromium_browser,
      arg_lst = list('--headless',
                     '--disable-gpu',
                     '--disable-extensions',
                     '--disable-dev-shm-usage')
    )

    cnf <- merge_list(cnf, hl)
  }

  if(download_capture){

    dl  <- wap_browser_config_implementer(
      browser = chromium_browser,
      conf_lst = list(
        savefile =
          list(default_directory = normalizePath(download_folder)),
        download =
          list(default_directory = normalizePath(download_folder),
               prompt_for_download = FALSE))

    )

    cnf <- merge_list(cnf, dl)
  }

  if(no_credential_store_offer){

    ncs  <- wap_browser_config_implementer(
      browser = chromium_browser,
      conf_lst = list(
        credentials_enable_service =
          FALSE,
        profile =
          list(password_manager_enabled = FALSE,
               default_content_setting_values = list(
                 notifications = 2L
               )))

    )

    cnf <- merge_list(cnf, ncs)

  }

  cnf

}

rst_remotedriver_specific_config_chrome <- function(
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  no_credential_store_offer = TRUE
){
  rst_remotedriver_specific_config_chromium(
    proxy = proxy,
    proxy_host = proxy_host, proxy_port = proxy_port,
    best_known_settings = best_known_settings,
    headless = headless,
    download_capture = download_capture,
    download_folder = download_folder,
    no_credential_store_offer = no_credential_store_offer,
    chromium_browser = "chrome"
  )
}

rst_remotedriver_specific_config_edge <- function(
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  no_credential_store_offer = TRUE
){
  rst_remotedriver_specific_config_chromium(
    proxy = proxy,
    proxy_host = proxy_host, proxy_port = proxy_port,
    best_known_settings = best_known_settings,
    headless = headless,
    download_capture = download_capture,
    download_folder = download_folder,
    no_credential_store_offer = no_credential_store_offer,
    chromium_browser = "edge"
  )
}

rst_remotedriver_specific_config_opera <- function(
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  no_credential_store_offer = TRUE,
  turn_on_inbuilt_VPN = FALSE
){
  l <- rst_remotedriver_specific_config_chromium(
    proxy = proxy,
    proxy_host = proxy_host, proxy_port = proxy_port,
    best_known_settings = best_known_settings,
    headless = headless,
    download_capture = download_capture,
    download_folder = download_folder,
    no_credential_store_offer = no_credential_store_offer,
    chromium_browser = "opera"
  )

  if(turn_on_inbuilt_VPN){

    # recorded and supervised
    vpnl <- wap_browser_config_implementer_opera(
      conf_lst = list(
        settings_page = list(vpn_disclaimer_enabled = FALSE),
        webrtc = list(ip_handling_policy = "disable_non_proxied_udp"),
        freedom = list(proxy_switcher = list(
          automatic_connection = TRUE, enabled = TRUE,
          last_ui_interaction_time = as.integer(Sys.time()),
          ui_visible = TRUE)),
        profile = list(default_content_setting_values = list(
          plugins = 3L))),
      user_data_dir = wap_browser_config_implementer_opera_prior_profile(l))

    l <- merge_list(l, vpnl)
  }

  l
}

rst_remotedriver_specific_config_firefox <- function(
  # proxy
  proxy = FALSE,
  proxy_host = "localhost", proxy_port = 8888L,
  best_known_settings = TRUE,
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  # 1.5 No password save options
  no_credential_store_offer = TRUE
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

    pl  <- wap_browser_config_implementer_firefox(
      conf_lst = sf
    )

    cnf <- merge_list(cnf, pl)

  }

  if(best_known_settings){
    # No known best setting
  }

  if(headless){
    hl  <- wap_browser_config_implementer_firefox(
      arg_lst = list('--headless')
    )

    cnf <- merge_list(cnf, hl)
  }

  if(download_capture){
    # ref
    # https://developer.mozilla.org/en-US/docs/Archive/Mozilla/Download_Manager_preferences

    dl  <- wap_browser_config_implementer_firefox(
      conf_lst = list(
        browser.download.dir = download_folder,
        browser.download.folderList = 2L))

    cnf <- merge_list(cnf, dl)
  }

  if(no_credential_store_offer){

    warning("Not implemented yet")

  }

  cnf

}



