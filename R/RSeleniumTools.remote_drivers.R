

# Client should be able to change following things in web-browsers
# - Proxy
# - Best known settings for remote access
# - Headless mode (equivalent)
# - Download folder
#  KFL
# - Add-on / plugins (as applicable)
#

rst_remotedriver <- function(
  browser="chrome",
  # Web browser configurations
  # 1.Customized and unified configurations
  # 1.1 Proxy
  proxy = FALSE, proxy_host = "localhost", proxy_port = 8888L,
  # 1.2 Best known settings for remote access
  best_known_settings = TRUE,
  # 1.3 Headless mode (equivalent)
  headless = FALSE,
  # 1.4 Download folder
  download_capture = FALSE, download_folder,
  # 2. Additional configuration if required
  browser_config,
  # 3. Documented Arguments (whichever are configurable)
  remoteServerAddr = "localhost",
  version = "", platform = "ANY", javascript = TRUE,
  # 4. Further argument
  ...
){
  # As per the documentation of remoteDriver-class {RSelenium} Documented
  # objects are remoteDriver is a generator object. To define a new remoteDriver
  # class method 'new' is called. The slots (default value) that are user
  # defined are: remoteServerAddr(localhost), port(4444), browserName(firefox),
  # version(""), platform(ANY), javascript(TRUE). See examples for more
  # information on use.
  #
  # From the code :
  # remoteServerAddr = "localhost",
  # port = 4444,
  # browserName = "firefox",
  # path = "/wd/hub",
  # version = "",
  # platform = "ANY",
  # javascript = TRUE,
  # nativeEvents = TRUE,
  # extraCapabilities = list()
  #
  # Among these only remoteServerAddr, version, platform and javascript is
  # configurable (if wanted) (rest are set by {fisher} or defined alternatively)
  browser <- wap_valid_browser(browser)

  # @Dev
  # qick tap

  # soo big name! making small
  rst_wsrcbn <-rst_webdriver_selenium_remote_client_browser_names

  ecaps <- list()

  if(!missing(browser_config)){
    ecaps <- browser_config
  }

  RSelenium::remoteDriver(
    remoteServerAddr = remoteServerAddr,
    port = rst_wdman_selenium_info_env$s_port,
    browserName = rst_wsrcbn$browser_name_client[
      rst_wsrcbn$browser_name_short==browser],
    extraCapabilities = ecaps,
    version = version,
    platform = platform,
    javascript = javascript)

}
