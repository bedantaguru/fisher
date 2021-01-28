
# rst_wdman_selenium_info_env$s_port <- 4567L

# https://peter.sh/experiments/chromium-command-line-switches/

rd <- rst_remotedriver(browser = "opera")

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
