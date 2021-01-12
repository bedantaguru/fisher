
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

  if(!isTRUE(info$offline) &
     !any(driver_web_info$core$for_this_system)){
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

  if(nrow(this_d)>1){
    this_d <- this_d[1,]
  }

  list(this_driver_info = this_d,
       info = info,
       all_driver_info = driver_web_info)
}

rst_webdriver_specific_chrome <- function(cver, offline_info = NULL){

  info <- list(
    driver_url = "https://www.googleapis.com/storage/v1/b/chromedriver/o",
    compatibility = "https://chromedriver.chromium.org/downloads"
  )

  cver_n <- numeric_version(
    gsub("[^0-9.]","", cver)
  )
  # thumb rule major version should match

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      do_offline <- TRUE
      info$offline <- TRUE
    }
  }
  if(do_offline){
    driver_web_info <- rst_webdriver_url_parser(offline_info, offline = TRUE)
  }else{
    driver_web_info <- rst_webdriver_url_parser(info$driver_url)
  }

  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname == "chromedriver",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)

  driver_web_info$core$for_this_browser <-
    as.numeric(driver_web_info$core$version_num[,1]) == as.numeric(cver_n[1,1])

  rst_webdriver_specific_finalizer(driver_web_info, info)


}

rst_webdriver_specific_firefox <- function(fver, offline_info = NULL){

  info <- list(
    driver_url = "https://api.github.com/repos/mozilla/geckodriver/releases",
    compatibility =
      "https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html"
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

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      do_offline <- TRUE
      info$offline <- TRUE
    }
  }
  if(do_offline){
    driver_web_info <- rst_webdriver_url_parser(offline_info, offline = TRUE)
  }else{
    driver_web_info <- rst_webdriver_url_parser(info$driver_url)
  }

  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname == "geckodriver",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)

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

rst_webdriver_specific_edge <- function(ever, offline_info = NULL){

  info <- list(
    driver_url = "https://msedgedriver.azureedge.net/",
    compatibility =
      "https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/"
  )

  ever_n <- numeric_version(
    gsub("[^0-9.]","", ever)
  )

  # direct query mechanism for edge only
  # https://msedgewebdriverstorage.z22.web.core.windows.net/?prefix={ever}/
  # like
  # https://msedgewebdriverstorage.z22.web.core.windows.net/?prefix=84.0.512.0/


  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      do_offline <- TRUE
      info$offline <- TRUE
    }
  }
  if(do_offline){
    driver_web_info <- rst_webdriver_url_parser(offline_info, offline = TRUE)
  }else{
    driver_web_info <- rst_webdriver_url_parser(
      paste0("edgedriver_direct_query_", as.character(ever_n))
    )

    if(nrow(driver_web_info$core)==0){
      # direct query failed
      # try to read whole / available records
      driver_web_info <- rst_webdriver_url_parser(info$driver_url)
    }

  }

  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname == "edgedriver",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)


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

rst_webdriver_specific_opera <- function(over, offline_info = NULL){

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

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      do_offline <- TRUE
      info$offline <- TRUE
    }
  }
  if(do_offline){
    driver_web_info <- rst_webdriver_url_parser(offline_info, offline = TRUE)
  }else{
    driver_web_info <- rst_webdriver_url_parser(info$driver_url)
  }

  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname == "operadriver",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)

  # as Opera 58 was released on January 23, 2019, based on Chromium 71.
  driver_web_info$core$for_this_browser <-
    as.numeric(driver_web_info$core$version_num[,1]) ==
    as.numeric(over_n[1,1])-58+71

  rst_webdriver_specific_finalizer(driver_web_info, info)


}

# technically it is not webdriver (it is selenium itself)
# sver can be stable, dev, both
rst_webdriver_specific_selenium <- function(sver = "both", offline_info = NULL){

  sver <- match.arg(sver, choices = c("both","dev","stable"))

  info <- list(
    driver_url = "https://www.googleapis.com/storage/v1/b/selenium-release/o",
    compatibility = "https://www.selenium.dev/"
  )

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      do_offline <- TRUE
      info$offline <- TRUE
    }
  }
  if(do_offline){
    driver_web_info <- rst_webdriver_url_parser(offline_info, offline = TRUE)
  }else{
    driver_web_info <- rst_webdriver_url_parser(info$driver_url)
  }


  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname == "selenium-server-standalone",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)

  if(!do_offline){
    driver_web_info$core$version <- gsub(
      "selenium-server-standalone-|.jar","",
      driver_web_info$core$file)
  }

  driver_web_info$core$version_num <- numeric_version(
    gsub(
      "[^0-9.]","",
      gsub("alpha|beta","0.",driver_web_info$core$version)
    )
  )

  if(sver == "stable" | sver == "both"){

    # as on 06-01-2021
    stablev_last_known <- numeric_version("3.141.59")
    stablev <- numeric_version("3.141.59")

    if(!do_offline){
      try({
        # try to update
        wc <- readLines("https://www.selenium.dev/downloads/", warn = FALSE)
        wc <- tolower(wc)
        wc <- wc[grepl("latest stable version", wc)]
        # very bad way
        stablev <-
          numeric_version(
            strsplit(
              strsplit(
                strsplit(
                  wc,"latest stable version")[[1]][2],
                "</a>")[[1]][1],
              ">")[[1]][2]
          )
      }, silent = TRUE)
    }

    driver_web_info$core$for_this_browser <-
      driver_web_info$core$version_num == stablev
    if(!any(driver_web_info$core$for_this_browser)){
      # this should match 1
      driver_web_info$core$for_this_browser <-
        driver_web_info$core$version_num == stablev_last_known
    }

    # special case of sver == "both"
    if(sver == "both"){
      st <- rst_webdriver_specific_finalizer(driver_web_info, info)

      driver_web_info$core$for_this_browser <- TRUE
      lat <- rst_webdriver_specific_finalizer(driver_web_info, info)

      comb <- list(
        this_driver_info = unique(
          rbind(
            st$this_driver_info,
            lat$this_driver_info
          )),
        info = st$info,
        all_driver_info = st$all_driver_info
      )
      # early exit
      return(comb)
    }

  }

  if(sver == "dev"){
    # pick latest
    driver_web_info$core$for_this_browser <- TRUE
  }


  rst_webdriver_specific_finalizer(driver_web_info, info)


}
