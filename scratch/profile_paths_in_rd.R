
# rst_wdman_selenium_info_env$s_port <- 4567L

# https://peter.sh/experiments/chromium-command-line-switches/

rd <- rst_remotedriver(browser = "opera")


#wap_browser_config_implementer_chrome()
##########################
# edge
# download

list(
  savefile =
    list(default_directory = "C:\\Users\\IG\\Downloads\\wifi"),
  download =
    list(default_directory = "C:\\Users\\IG\\Downloads\\wifi",
         prompt_for_download = FALSE))


##########################
# opera
# download same
# VPN
list(webrtc = list(ip_handling_policy = "disable_non_proxied_udp"),
     profile = list(default_content_setting_values = list(plugins = 3L)),
     freedom = list(proxy_switcher = list(automatic_connection = TRUE,
                                          enabled = TRUE, last_ui_interaction_time = 1611836435.35346,
                                          ui_visible = TRUE)))

# as.numeric(Sys.time()) can convert real time in last_ui_interaction_time

list(webrtc = list(ip_handling_policy = "disable_non_proxied_udp"),
     profile = list(default_content_setting_values = list(plugins = 3L)),
     freedom = list(proxy_switcher = list(automatic_connection = TRUE,
                                          enabled = TRUE, last_ui_interaction_time =  as.numeric(Sys.time()),
                                          ui_visible = TRUE)))


y <- merge_list(list(
  savefile =
    list(default_directory = "C:\\Users\\IG\\Downloads\\wifi"),
  download =
    list(default_directory = "C:\\Users\\IG\\Downloads\\wifi",
         prompt_for_download = FALSE)), list(webrtc = list(ip_handling_policy = "disable_non_proxied_udp"),
                                             profile = list(default_content_setting_values = list(plugins = 3L)),
                                             freedom = list(proxy_switcher = list(automatic_connection = TRUE,
                                                                                  enabled = TRUE, last_ui_interaction_time =  as.numeric(Sys.time()),
                                                                                  ui_visible = TRUE))))



y <- list(savefile = list(default_directory = "C:\\Users\\IG\\Downloads\\wifi"),
          settings_page = list(vpn_disclaimer_enabled = FALSE), webrtc = list(
            ip_handling_policy = "disable_non_proxied_udp"), download = list(
              default_directory = "C:\\Users\\IG\\Downloads\\wifi",
              prompt_for_download = FALSE), freedom = list(proxy_switcher = list(
                automatic_connection = TRUE, enabled = TRUE, last_ui_interaction_time = 1611904835.54258,
                ui_visible = TRUE)), profile = list(default_content_setting_values = list(
                  plugins = 3L)))

# opera
# VPN



rd <- rst_remotedriver("opera",
                       browser_config = wap_browser_config_implementer_opera(y))


fok <-"C:/Users/IG/Downloads/op/Preferences"
ok <- jsonlite::fromJSON(fok)
ft <- file.path(rd$sessionInfo$opera$userDataDir, "Preferences")

writeLines(jsonlite::toJSON(ok, auto_unbox = T), ft)
file.copy(fok, ft, overwrite = T)

rd$open()
