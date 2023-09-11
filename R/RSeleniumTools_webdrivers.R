


# ref
# https://github.com/bonigarcia/webdrivermanager
# https://github.com/bonigarcia/webdrivermanager/blob/732debf4fd8f43e036b43e1e827e1bf8d42ffbc7/src/main/resources/webdrivermanager.properties



# firefox check
# https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html
# https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/

rst_webdriver_info_env <- new.env()

rst_webdriver_selenium_remote_client_browser_names <- data.frame(
  appname = c("geckodriver","chromedriver", "operadriver", "edgedriver"),
  browser_name_short = c("firefox", "chrome", "opera", "edge"),
  browser_name_client = c("firefox", "chrome", "opera", "MicrosoftEdge")
)

# collects both online and offline info (with minimum possible footprint)
rst_webdriver_info <- function(){
  this_sys_browsers <- sys_check_web_browsers()
  oi <- rst_webdriver_offline_info(this_sys_browsers)
  missing_browsers <- setdiff(names(this_sys_browsers), oi$browser_name_short)
  if(!any(grepl("selenium", oi$appname))){
    # this means no selenium server is installed in this system
    # adding this to donwload required selenium
    missing_browsers <- c(missing_browsers, "selenium")
  }
  ii <- rst_webdriver_online_info(missing_browsers)

  list(online = ii, offline = oi, system_browsers = this_sys_browsers)
}

rst_webdriver_online_offline_info_core <- function(browsers,
                                                   offline_info = NULL,
                                                   this_system_browsers){
  # cached result if present
  if(is.null(offline_info)){
    if(exists("online",envir = rst_webdriver_info_env)){
      return(rst_webdriver_info_env$online)
    }
  }else{
    if(exists("offline",envir = rst_webdriver_info_env)){
      return(rst_webdriver_info_env$offline)
    }
  }


  if(missing(this_system_browsers)){
    this_system_browsers <- sys_check_web_browsers()
  }

  info <- list()

  if("chrome" %in% browsers | "all" %in% browsers){
    if("chrome" %in% names(this_system_browsers)){
      info$chrome <- tryCatch(
        rst_webdriver_specific_chrome(
          cver = this_system_browsers$chrome$installed_version,
          offline_info = offline_info),
        error = function(e) NULL
      )
    }
  }

  if("firefox" %in% browsers | "all" %in% browsers){
    if("firefox" %in% names(this_system_browsers)){
      info$firefox <- tryCatch(
        rst_webdriver_specific_firefox(
          fver = this_system_browsers$firefox$installed_version,
          offline_info = offline_info),
        error = function(e) NULL)
    }
  }

  if("opera" %in% browsers | "all" %in% browsers){
    if("opera" %in% names(this_system_browsers)){
      info$opera <- tryCatch(
        rst_webdriver_specific_opera(
          over = this_system_browsers$opera$installed_version,
          offline_info = offline_info),
        error = function(e) NULL)
    }
  }

  if("edge" %in% browsers | "all" %in% browsers){
    if("edge" %in% names(this_system_browsers)){
      info$edge <- tryCatch(
        rst_webdriver_specific_edge(
        ever = this_system_browsers$edge$installed_version,
        offline_info = offline_info),
        error = function(e) NULL)
    }
  }

  if("selenium" %in% browsers | "all" %in% browsers){
    info$selenium <- rst_webdriver_specific_selenium(
      offline_info = offline_info
    )
  }

  d_inf <- do.call(rbind, lapply(info, `[[`, "this_driver_info"))

  d_inf <- unique(d_inf[c(
    "appname",
    "version",
    "platform_tag",
    "file",
    "url",
    "file_integrity",
    "file_integrity_algo",
    "remarks"
  )])

  if(("all" %in% browsers) &
     length(setdiff(names(this_system_browsers), names(info))) == 0){
    if(is.null(offline_info)){
      assign("online", d_inf, envir = rst_webdriver_info_env)
    }else{
      assign("offline", d_inf, envir = rst_webdriver_info_env)
    }
  }

  d_inf
}

rst_webdriver_online_info <- function(browsers){
  if(missing(browsers)){
    browsers = "all"
  }

  rst_webdriver_online_offline_info_core(browsers)

}

rst_webdriver_offline_info <- function(this_system_browsers){
  # currently only binman manages download and update etc
  existing <- rst_binman_all_apps_details()

  # exit early
  if(is.null(existing)) return(NULL)

  if(missing(this_system_browsers)){
    this_system_browsers <- sys_check_web_browsers()
  }


  # exit early
  # @Dev need to see what is value in case no browser is installed
  if(is.null(this_system_browsers)) return(NULL)

  conv_from_binman <- rst_webdriver_offline_info_feeder_from_binman(existing)

  compatibility <- rst_webdriver_online_offline_info_core(
    browser = "all",
    offline_info = conv_from_binman,
    this_system_browsers = this_system_browsers)

  # binman specific
  compatibility$appname <- ifelse(
    compatibility$appname=="selenium-server-standalone",
    "seleniumserver",
    compatibility$appname)

  compatibility$version_m <- gsub("[^0-9\\.]+","",compatibility$version)
  compatibility$platform_m <- tolower(compatibility$platform_tag)


  existing$appname <- existing$app
  existing$version_m <- gsub("[^0-9\\.]+","",existing$version)
  existing$platform_m <- tolower(existing$platform)

  this_system_browsers_d <- do.call(
    rbind,
    lapply(
      seq_along(this_system_browsers),
      function(i) {
        data.frame(
          browser_name_short = names(this_system_browsers)[i],
          installed_version = this_system_browsers[[i]]$installed_version)
      }
    )
  )



  # selenium remote browser names

  this_system_browsers_d <- merge(this_system_browsers_d,
                                  rst_webdriver_selenium_remote_client_browser_names,
                                  by = "browser_name_short")

  matched <- merge(compatibility, existing,
                   by = c("appname","version_m","platform_m"),
                   suffixes = c("","_b"))


  matched_core <- matched[
    c("appname","version","platform_tag","remarks","bin_file")
  ]

  merge(matched_core, this_system_browsers_d, by = "appname",
        all.x = TRUE, all.y = FALSE)



}

