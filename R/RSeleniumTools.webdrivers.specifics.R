
# Supported web-browsers
# Chrome, Firefox, Edge, Opera, Safari, IE
# IE : may not be required ref:
# https://indianexpress.com/article/technology/social/microsoft-to-soon-end-internet-explorer-11-support-6559820/
# Safari : may not be required as it is only Mac specific

rst_webdriver_specific_finalizer <- function(driver_web_info, info){
  driver_web_info$core$for_this_system <-
    driver_web_info$core$for_this_platform &
    driver_web_info$core$for_this_browser
  if(!any(driver_web_info$core$for_this_system)){
    driver_web_info$core$for_this_system <-
      driver_web_info$core$for_this_platform_belowbit &
      driver_web_info$core$for_this_browser
  }
  if(!any(driver_web_info$core$for_this_system)){
    stop(paste0(
      "No compatible webdriver found. Please check:",
      info$compatibility
    ), call. = FALSE)
  }

  # this is the correct driver
  driver_web_info$core$this_one <- FALSE

  # latest driver within multiple matches
  driver_web_info$core$this_one[
    driver_web_info$core$for_this_system
  ][
    which.max(
      driver_web_info$core$time_idx[
        driver_web_info$core$for_this_system
      ]
    )[1]
  ] <- TRUE

  this_d <- driver_web_info$core[driver_web_info$core$this_one,]

  this_d <- this_d[1,]

  list(this_driver_info = this_d,
       info = info,
       all_driver_info = driver_web_info)
}

rst_webdriver_specific_chrome <- function(cver){

  info <- list(
    driver_url = "https://www.googleapis.com/storage/v1/b/chromedriver/o",
    compatibility = "https://chromedriver.chromium.org/downloads"
  )

  cver_n <- numeric_version(
    gsub("[^0-9.]","", cver)
  )
  # thumb rule major version should match

  driver_web_info <- rst_webdriver_url_parser(info$driver_url)

  driver_web_info$core$for_this_browser <-
    as.numeric(driver_web_info$core$version_num[,1]) == as.numeric(cver_n[1,1])

  rst_webdriver_specific_finalizer(driver_web_info, info)


}

rst_webdriver_specific_firefox <- function(fver){

  info <- list(
    driver_url = "https://api.github.com/repos/mozilla/geckodriver/releases",
    compatibility = "https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html"
  )

  fver_n <- numeric_version(
    gsub("[^0-9.]","", fver)
  )

  fver_n_major <- as.numeric(fver_n[1,1])

  # codified (simplified) version of compatibility matrix as mentioned in
  # https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html
  if(fver_n_major<57){
    stop("Firefox is really old. please update!", call. = FALSE)
  }

  driver_web_info <- rst_webdriver_url_parser(info$driver_url)

  driver_web_info$core$for_this_browser <- TRUE

  if(fver_n_major<60){
    driver_web_info$core$for_this_browser[
      driver_web_info$core$version_num > numeric_version("0.25.0")
    ] <- FALSE
  }

  if(fver_n_major>79){
    driver_web_info$core$for_this_browser[
      driver_web_info$core$version_num <= numeric_version("0.24.0")
    ] <- FALSE
  }

  if(fver_n_major>62){
    driver_web_info$core$for_this_browser[
      driver_web_info$core$version_num <= numeric_version("0.20.1")
    ] <- FALSE
  }

  driver_web_info$core$for_this_browser[
    driver_web_info$core$appname!="geckodriver"
  ] <- FALSE


  rst_webdriver_specific_finalizer(driver_web_info, info)


}

rst_webdriver_specific_edge <- function(ever){

  info <- list(
    driver_url = "https://msedgedriver.azureedge.net/",
    compatibility =
      "https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/"
  )

  ever_n <- numeric_version(
    gsub("[^0-9.]","", ever)
  )

  driver_web_info <- rst_webdriver_url_parser(info$driver_url)

  # exact match
  driver_web_info$core$for_this_browser <-
    driver_web_info$core$version_num == ever_n

  if(!any(driver_web_info$core$for_this_browser)){
    # loose match
    # however this may not be correct way so issue warning
    warning("Exact match not found for edge driver!", call. = FALSE)
    driver_web_info$core$for_this_browser <-
      as.numeric(driver_web_info$core$version_num[,1]) == as.numeric(ever_n[1,1])
  }

  rst_webdriver_specific_finalizer(driver_web_info, info)


}

rst_webdriver_specific_opera <- function(over){

  info <- list(
    driver_url =
      "https://api.github.com/repos/operasoftware/operachromiumdriver/releases",
    compatibility =
      "https://github.com/operasoftware/operachromiumdriver/releases"
  )

  over_n <- numeric_version(
    gsub("[^0-9.]","", over)
  )

  if(as.numeric(over_n[1,1])<62){
    stop("Kindly update your Opera browser!", call. = FALSE)
  }

  driver_web_info <- rst_webdriver_url_parser(info$driver_url)

  # as Opera 58 was released on January 23, 2019, based on Chromium 71.
  driver_web_info$core$for_this_browser <-
    as.numeric(driver_web_info$core$version_num[,1]) ==
    as.numeric(over_n[1,1])-58+71

  rst_webdriver_specific_finalizer(driver_web_info, info)


}
