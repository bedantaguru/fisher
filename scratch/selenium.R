
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



