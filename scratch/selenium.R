




# all has same wd

# parallel processing

cls <- parallel::makeCluster(parallel::detectCores())

fisher::web_automation_platform()

parallel::clusterApply(cls, seq(parallel::detectCores()), function(x) Sys.getpid())
parallel::clusterApply(cls, seq(parallel::detectCores()), function(x) getwd())

parallel::clusterApply(cls, seq(parallel::detectCores()), function(x) fisher::web_control_client())

parallel::clusterApply(cls, seq(parallel::detectCores()), function(x){
  rd <- fisher::web_control_client()
  rd$navigate(paste0("https://www.google.com/search?q=",x))
})

paste0("https://www.google.com/search?q=")

parallel::stopCluster(cls)

# parallel::clusterApply(cls, seq(parallel::detectCores()),
#                        function(x) {
#                          ws <- fisher:::rst_wap_config()
#                          ws$rst$bind_pid_sid(Sys.getpid(),x)
#                          ws$write(x, value = ws, R_object = T)
#                          saveRDS(ws, file.path(ws$store_path, "robj", x))
#                          file.path(ws$store_path, "robj", x)
#                          file.exists(file.path(ws$store_path, "robj", x))
#                        })




# furrr

future::plan(future::multisession)


furrr::future_map_int(1:10, ~Sys.getpid())

furrr::future_map_chr(1:10, ~getwd())

# future::plan(future::sequential)









# @Dev
# test of killing existing selenium
# remDr <- RSelenium::remoteDriver(browserName = "htmlunit", port = 4567L)
# remDr$closeServer # does not work
#


rst_remotedriver(
  browser = "chrome",
  browser_config = wap_browser_config_implementer(
    "chrome",
    arg_lst = list('--window-size=600,600'))
)$open()


rst_remotedriver(
  browser = "firefox",
  browser_config = list(
    `moz:firefoxOptions` = list(
      #args = list('--window-size=1900,500')
      args = list('--width=600',
                  '--height=600')
    )
  )
)$open()


rst_remotedriver(
  browser = "edge",
  browser_config = list(
    `ms:edgeOptions` = list(
      args = list('--window-size=600,600')
    )
  )
)$open()


rst_remotedriver(
  browser = "opera",
  browser_config = list(
    `goog:chromeOptions` = list(
      args = list('--window-size=600,600')
    )
  )
)$open()


se <- wdman::selenium(check = F, chromever = "86.0.4240.22")

# REf https://github.com/ropensci/RSelenium/issues/207
# https://stackoverflow.com/questions/57298901/
extraCap <- list(
  chromeOptions = list(
    args = c('--window-size=1200,750',
             '--no-sandbox',
             '--disable-blink-features=AutomationControlled'),
    excludeSwitches = list("enable-automation"),
    useAutomationExtension = FALSE
  )
)


extraCap <- list(
  chromeOptions = list(
    args = c('--window-size=1280,800',
             '--no-sandbox',
             '--disable-blink-features=AutomationControlled',
             '--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36'),
    excludeSwitches = list("enable-automation"),
    useAutomationExtension = FALSE
  )
)


# below is working
extraCap <- list(
  chromeOptions = list(
    args = c('--window-size=1280,800',
             '--no-sandbox',
             '--disable-blink-features=AutomationControlled',
             '--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36'),
    excludeSwitches = list("enable-automation"),
    prefs = list(`profile.default_content_settings.popups` = 0,
                 `download.default_directory` = normalizePath("C:/Users/Nil/Downloads/etc/")),
    useAutomationExtension = FALSE
  )
)



# remDr <- RSelenium::remoteDriver(browserName = "internet explorer", port = 4567L)

remDr <- RSelenium::remoteDriver(browserName = "chrome", port = 4567L, extraCapabilities = extraCap)
remDr <- RSelenium::remoteDriver(browserName = "chrome", port = 4567L)
remDr <- RSelenium::remoteDriver(browserName = "firefox", port = 4567L)


# download test

remDr$navigate("http://speedtest.tele2.net/")

remDr$findElement('css', '#http a[href*="1MB"]')$clickElement()


# doanload location set

# not working
remDr$queryRD(
  ipAddr = paste0(remDr$serverURL, "/session/", remDr$sessionid, "/chromium/send_command"),
  method = "POST",
  qdata = list(
    cmd = "Page.setDownloadBehavior",
    params = list(
      behavior = "allow",
      downloadPath = normalizePath("C:/Users/Nil/Downloads/etc/"))))

# see it in https://github.com/rgzn/CollarScraper/blob/3c7b6f221eb000c577f961e719589c846e453d8a/R/seleniumScraper.R
# if ( Sys.info()['sysname'] == "Windows" ) {
#   self$download_path <<- gsub("/", "\\\\", self$download_path)
# }


# String downloadFilepath = "/path/to/download";
# HashMap<String, Object> chromePrefs = new HashMap<String, Object>();
# chromePrefs.put("profile.default_content_settings.popups", 0);
# chromePrefs.put("download.default_directory", downloadFilepath);
# ChromeOptions options = new ChromeOptions();
# options.setExperimentalOption("prefs", chromePrefs);
# DesiredCapabilities cap = DesiredCapabilities.chrome();
# cap.setCapability(CapabilityType.ACCEPT_SSL_CERTS, true);
# cap.setCapability(ChromeOptions.CAPABILITY, options);
# WebDriver driver = new ChromeDriver(cap);

se$process$kill_tree()

# selenium opera


# "freedom":{
#   "proxy_switcher":{
#     "automatic_connection":true,
#     "automatic_connection_update_applied":true,
#     "bytes_transferred":"0",
#     "enabled":true,
#     "forbidden":false,
#     "last_ui_interaction_time":1606768493.430966,
#     "stats":{"last_date_stored":"13251234600000000","values":["2210922"]},
#     "ui_visible":true}},
#
#
#


# opera try


rd <- RSelenium::remoteDriver(browserName = "opera", port = 4567L, extraCapabilities = extraCap)

binman::list_versions("chromedriver")

browseURL(binman::app_dir("chromedriver"))

# mask opera driver
#  rename opera driver to chrome driver
# check opera version
se <- wdman::selenium(check = F, retcommand = T)


se <- wdman::selenium(
  check = F,
  jvmargs = list(
    opera =
      paste0(
        "-Dwebdriver.opera.driver=",
        normalizePath(
          "C:/Users/Nil/Downloads/operadriver_win64/operadriver.exe"
        )
      )
  ))

rd <- RSelenium::remoteDriver(browserName = "opera", port = 4567L)

#rd <- RSelenium::remoteDriver(browserName = "opera", port = 4567L, extraCapabilities = extraCap)


ps::ps_system_memory()
ps::ps_cpu_count()

ps::ps_disk_partitions()

ps::ps_children(recursive = T)
ps::ps_disk_usage()

# need a port listner obj


# pref is json
rjson::fromJSON(file = "C:\\Users\\Nil\\Downloads\\Opera Stable VPN\\Preferences")
jsonlite::fromJSON(readLines("C:\\Users\\Nil\\Downloads\\Opera Stable VPN\\Preferences", warn = F))

#
# rjson::toJSON(list( alpha = 1:5, beta = "Bravo",
#                     gamma = list(a=1:3, b=NULL),
#                     delta = c(TRUE, FALSE) ))




