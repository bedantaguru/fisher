
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

  # add remark column if not present
  if(is.null(driver_web_info$core$remarks)){
    driver_web_info$core$remarks <- ""
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

  # only these columns will be present

  # all these may not be required
  # this_d <- this_d[
  #   c("time_idx", "appname", "appname_from_url", "version", "is_zip",
  #     "is_jar", "platform_tag", "is_valid_platform", "file", "url",
  #     "file_integrity", "file_integrity_algo", "version_num", "for_this_platform",
  #     "for_this_platform_belowbit", "for_this_browser", "for_this_system",
  #     "remarks", "this_one")
  # ]

  this_d <- this_d[
    c("appname",
      "version",
      "platform_tag",
      "file",
      "url",
      "file_integrity",
      "file_integrity_algo",
      "remarks")
  ]

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

  if(!any(driver_web_info$core$for_this_browser) & is.null(offline_info)){
    # loose match
    # however this may not be correct way so issue warning (except in offline
    # check. Where it is simply disabled)
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
rst_webdriver_specific_selenium <- function(sver = "all", offline_info = NULL){

  info <- list()

  #TODO fix it for Selenium 4.8

  # ref : https://testsigma.com/blog/selenium-standalone-server/
  sver <- match.arg(sver, choices = c(
    # all will not be explicitly called but in first go /subsequently it is
    # required for downloading
    "all", # all means all 3 variants
    # standalone server not present
    "4.x+", # or "latest"
    # standalone server
    "4.x_dev","3.x"))


  do_offline_4.x <- FALSE
  do_offline_3.x <- FALSE

  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      offline_info <- offline_info[
        grepl("selenium", tolower(offline_info$app)),]

      # check again
      if(nrow(offline_info)>0){
        offline_info$is_dev <- grepl("alpha|beta", offline_info$version)
        offline_info$version_parse <-
          package_version(gsub("[^0-9\\.]","",offline_info$version))

        offline_info$version_tag <- paste0(
          offline_info$version_parse$major,
          ".x",
          ifelse(offline_info$is_dev, "_dev","")
        )

        offline_info$version_tag[
          offline_info$version_parse$major>=4 &
            !offline_info$is_dev
        ] <- "4.x+"

        if(sver == "all"){
          var_tags <- c("4.x+","4.x_dev","3.x")
        }else{
          var_tags <- sver
        }

        rem_tags <- setdiff(var_tags, offline_info$version_tag)

        if("4.x+" %in% rem_tags){
          do_offline_4.x <- FALSE
        }else{
          # offline is sufficient
          do_offline_4.x <- TRUE
          info$offline <- TRUE
        }

        if(("4.x_dev" %in% rem_tags) | ("3.x" %in% rem_tags)){
          do_offline_3.x <- FALSE
        }else{
          do_offline_3.x <- TRUE
          info$offline <- TRUE
        }

      }
    }
  }


  driver_web_info_List_4.x <- list()

  # merge info from 4.x source (different source)
  if(sver=="all" | sver == "4.x+" ){
    if(do_offline_4.x){
      driver_web_info_List_4.x <- rst_webdriver_specific_selenium_4.x(
        offline_info = offline_info)
    }else{
      driver_web_info_List_4.x <- rst_webdriver_specific_selenium_4.x()
    }
  }


  # merge info from 3.x source (different source)
  driver_web_info_List_3.x <- list()
  if(sver=="all" | sver == "3.x" | sver =="4.x_dev"){
    if(do_offline_3.x){
      driver_web_info_List_3.x <- rst_webdriver_specific_selenium_3.x(
        offline_info = offline_info)
    }else{
      driver_web_info_List_3.x <- rst_webdriver_specific_selenium_3.x()
    }
  }

  # return only 4.x+
  if(sver == "4.x+"){
    driver_web_info_List_4.x$driver_web_info$core$for_this_browser <- TRUE
    return(
      rst_webdriver_specific_finalizer(
        driver_web_info_List_4.x$driver_web_info,
        driver_web_info_List_4.x$info
      )
    )
  }

  # return for 3.x or 4.x_dev
  if(sver == "3.x" | sver =="4.x_dev"){
    # driver_web_info_List_3.x$driver_web_info$core$for_this_browser <- FALSE
    driver_web_info_List_3.x$driver_web_info$core$for_this_browser <-
      driver_web_info_List_3.x$driver_web_info$core$version_tag == sver

    return(
      rst_webdriver_specific_finalizer(
        driver_web_info_List_3.x$driver_web_info,
        driver_web_info_List_3.x$info
      )
    )
  }



  # case for sver == all

  commn_cnames <- intersect(
    colnames(driver_web_info_List_3.x$driver_web_info$core),
    colnames(driver_web_info_List_4.x$driver_web_info$core)
  )

  driver_web_info_merged <- rbind(
    driver_web_info_List_3.x$driver_web_info$core[commn_cnames],
    driver_web_info_List_4.x$driver_web_info$core[commn_cnames]
  )

  driver_web_info_merged$for_this_browser <- TRUE

  # filtering out 2.x and older (if any)

  driver_web_info_merged <- driver_web_info_merged[
    driver_web_info_merged$version_tag %in% c("3.x", "4.x_dev", "4.x+"),]

  # latest info only
  info_merged <- driver_web_info_List_4.x$info

  m_dwi_list <- split(
    driver_web_info_merged,
    driver_web_info_merged$version_tag
  )

  m_final_part <-lapply(
    m_dwi_list,
    function(x){
      rst_webdriver_specific_finalizer(list(core = x), info_merged)
    }
  )

  m_this_driver_info <-
    do.call(rbind, lapply(m_final_part, `[[`,"this_driver_info"))
  m_info <- info_merged

  comb <- list(
    this_driver_info = m_this_driver_info,
    info = m_info,
    all_driver_info = list(
      core = driver_web_info_merged,
      raw = list(
        ver_3.x = driver_web_info_List_3.x,
        ver_4.x = driver_web_info_List_4.x))
  )

  comb


}

# static for selenium 3.x
rst_webdriver_specific_selenium_3.x <- function(offline_info = NULL){
  # this is obsolete / pointing to stale source (no more update on this branch)

  info <- list(
    driver_url = "https://www.googleapis.com/storage/v1/b/selenium-release/o",
    compatibility = "https://www.selenium.dev/"
  )

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      # proper logic for each version is implemented in
      # <rst_webdriver_specific_selenium>
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

  driver_web_info$core$version <- gsub(
    "selenium-server-standalone-|.jar","",
    driver_web_info$core$file)

  driver_web_info$core$version_num <- numeric_version(
    gsub(
      "[^0-9.]","",
      gsub("alpha|beta","0.",driver_web_info$core$version)
    )
  )

  driver_web_info$core$is_dev <- grepl("[^0-9\\.]",driver_web_info$core$version)

  driver_web_info$core$major_version <- unlist(
    lapply(
      driver_web_info$core$version_num,
      function(x) package_version(x)$major
    )
  )

  driver_web_info$core$version_tag <- paste0(
    driver_web_info$core$major_version, ".x",
    ifelse(driver_web_info$core$is_dev,"_dev","")
  )

  driver_web_info$core$remarks <- driver_web_info$core$version_tag

  list(
    driver_web_info = driver_web_info,
    info = info
  )

}

#@Dev
#TODO
# new development for selenium 4.8
rst_webdriver_specific_selenium_4.x <- function(offline_info = NULL){


  info <- list(
    driver_url =
      "https://api.github.com/repos/SeleniumHQ/selenium/releases",
    compatibility =
      "https://www.selenium.dev/"
  )

  do_offline <- FALSE
  if(is.data.frame(offline_info)){
    if(nrow(offline_info)>0){
      # proper logic for each version is implemented in
      # <rst_webdriver_specific_selenium>
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
    driver_web_info$core$appname == "selenium",
  ]

  driver_web_info$core$appname_based_on_filename <-
    sub("-[0-9alphabetarc.-]+.jar$","",driver_web_info$core$file)

  driver_web_info$core <- driver_web_info$core[
    driver_web_info$core$appname_based_on_filename == "selenium-server",
  ]

  # early exit
  if(nrow(driver_web_info$core)==0) return(NULL)

  driver_web_info$core$version_tag <- "4.x+"
  driver_web_info$core$remarks <- driver_web_info$core$version_tag


  list(
    driver_web_info = driver_web_info,
    info = info
  )

}
