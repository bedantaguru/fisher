

# ref https://www.microfocus.com/documentation/silk-test/195/en/silk4net-195-help-en/GUID-781614F2-54A0-4BE3-95B9-C282121A9B43.html
rd <- RSelenium::remoteDriver(
  port = rst_wdman_selenium_info_env$s_port,
  browserName = "firefox",
  extraCapabilities  =
    list(`moz:firefoxOptions` =
           list(prefs =
                  list(network.proxy.type = 1L,
                       network.proxy.ssl = "182.140.180.150", network.proxy.ssl_port = 4500L))))




rd <- RSelenium::remoteDriver(
  port = rst_wdman_selenium_info_env$s_port, browserName = "firefox",
  extraCapabilities  = RSelenium::makeFirefoxProfile(
    list(network.proxy.type = 1L, network.proxy.ssl = "192.0.0.1", network.proxy.ssl_port = 145L)))
