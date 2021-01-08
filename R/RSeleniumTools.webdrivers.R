


# ref
# https://github.com/bonigarcia/webdrivermanager
# https://github.com/bonigarcia/webdrivermanager/blob/732debf4fd8f43e036b43e1e827e1bf8d42ffbc7/src/main/resources/webdrivermanager.properties



# firefox check
# https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html
# https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/

rst_webdriver_info <- new.env()

rst_webdriver_online_info <- function(browser){
  if(missing(browser)){
    browser = "all"
  }else{
    if(browser!="selenium"){
      browser <- wap_valid_browser(browser)
    }
  }

  # cached result if present
  if(exists("online",envir = rst_webdriver_info)){
    return(rst_webdriver_info$online)
  }

  this_system_browsers <- sys_check_web_browsers()

  info <- list()

  if(browser=="chrome" | browser=="all"){
    if("chrome" %in% names(this_system_browsers)){
      info$chrome <- rst_webdriver_specific_chrome(
        cver = this_system_browsers$chrome$installed_version)
    }
  }

  if(browser=="firefox" | browser=="all"){
    if("firefox" %in% names(this_system_browsers)){
      info$firefox <- rst_webdriver_specific_firefox(
        fver = this_system_browsers$firefox$installed_version)
    }
  }

  if(browser=="opera" | browser=="all"){
    if("opera" %in% names(this_system_browsers)){
      info$opera <- rst_webdriver_specific_opera(
        over = this_system_browsers$opera$installed_version)
    }
  }

  if(browser=="edge" | browser=="all"){
    if("edge" %in% names(this_system_browsers)){
      info$edge <- rst_webdriver_specific_edge(
        ever = this_system_browsers$edge$installed_version)
    }
  }

  if(browser=="selenium" | browser=="all"){
    info$selenium <- rst_webdriver_specific_selenium()
  }

  d_inf <- do.call(rbind, lapply(info, `[[`, "this_driver_info"))

  d_inf <- unique(d_inf[c(
    "appname",
    "version",
    "platform_tag",
    "file",
    "url",
    "file_integrity",
    "file_integrity_algo"
  )])

  if(browser=="all"){
    assign("online", d_inf, envir = rst_webdriver_info)
  }

  d_inf

}

rst_webdriver_url_parser <- function(src_url){

  raw <- NULL

  if(grepl("www.googleapis.com", src_url)){
    raw <- rst_webdriver_json_url_parser_googleapis(src_url)
  }else{
    if(grepl("api.github.com", src_url)){
      raw <- rst_webdriver_json_url_parser_github(src_url)
    }else{
      if(grepl("msedgedriver.azureedge.net", src_url) |
         grepl("edgedriver_direct_query_", src_url)){
        raw <- rst_webdriver_edge_url_parser(src_url)
      }else{
        stop("Unknwon source", call. = FALSE)
      }
    }
  }

  if(!is.data.frame(raw)){
    stop("Unable to fetch from source URL", call. = FALSE)
  }


  must_cols <- c(
    "time_idx",
    "appname",
    "appname_from_url",
    "version",
    "is_zip",
    "is_jar",
    "platform_tag",
    "is_valid_platform",
    "file",
    "url",
    "file_integrity",
    "file_integrity_algo"
  )

  all_plat <- unique(raw[must_cols])
  all_plat <- all_plat[all_plat$is_zip | all_plat$is_jar ,]
  all_plat <- all_plat[all_plat$is_valid_platform,]

  # version fix
  all_plat$version <- gsub("[^0-9.]","",all_plat$version)
  all_plat$version_num <- numeric_version(all_plat$version)

  all_plat$for_this_platform <- sys_valid_os_string(
    all_plat$platform_tag,
    this_machine = TRUE
  )
  all_plat$for_this_platform[all_plat$platform_tag=="generic"] <- TRUE

  all_plat$for_this_platform_belowbit <- sys_valid_os_string(
    all_plat$platform_tag,
    this_machine = TRUE,
    allow_lowbit = TRUE
  )

  all_plat$for_this_platform_belowbit[all_plat$platform_tag=="generic"] <- TRUE


  invisible(
    list(
      core = all_plat,
      raw = raw
    )
  )

}


# JSON URL parser
# 3 kinds of JSON URL at this moment
# www.googleapis.com api.github.com api.bitbucket.org
# Following are specific urls (bitbucket not implemented)
# https://www.googleapis.com/storage/v1/b/chromedriver/o
# https://www.googleapis.com/storage/v1/b/selenium-release/o
# https://api.github.com/repos/mozilla/geckodriver/releases
# https://api.github.com/repos/operasoftware/operachromiumdriver/releases
# https://api.bitbucket.org/2.0/repositories/ariya/phantomjs/downloads?pagelen=100

rst_webdriver_json_url_parser_github <- function(src_url){

  pre_app_name <- basename(
    dirname(
      rev(
        strsplit(src_url, "api.github.com")[[1]]
      )[1]
    )
  )

  jorig <- jsonlite::fromJSON(src_url)
  al <- sapply(jorig$assets, length)

  jorig <- jorig[al>0,]

  ads <- lapply(
    seq(nrow(jorig)),
    function(i){
      d1 <- jorig[i,]
      d2 <- jorig$assets[[i]]

      d1$assets <- NULL
      d1$jk <-1
      d2$jk <- 1

      d <- merge(d1, d2, by = "jk", all = TRUE, suffixes = c("",".assets"))
      d$jk <- NULL

      # price to pay for base rbind
      # can be solved using bind_rows
      lstcol <- names(which(sapply(d, is.list)))
      d <- d[setdiff(colnames(d), lstcol)]

      d
    }
  )

  ads <- do.call("rbind", ads)

  ads$releases_url <- ads$url
  ads$is_zip <- grepl(".zip$",ads$name.assets)
  ads$is_jar <- grepl(".jar$",ads$name.assets)

  ads$appname <- sapply(
    strsplit(
      gsub(".zip$|.jar$","",ads$name.assets), "_|-"
    ),
    function(x) x[1]
  )
  ads$appname_from_url <-  pre_app_name
  ads$platform_tag <- NA

  if(any(ads$is_zip)){
    platforms <- sapply(
      strsplit(
        gsub(".zip$","",ads$name.assets[ads$is_zip]), "_|-"
      ),
      function(x) rev(x)[1]
    )
    ads$platform_tag[ads$is_zip] <- platforms
  }

  if(any(ads$is_jar)){
    ads$platform_tag[ads$is_jar] <- "generic"
  }

  ads$is_valid_platform <- sys_valid_os_string(ads$platform_tag)
  ads$is_valid_platform <- ads$is_valid_platform | (ads$platform_tag == "generic")

  ads$url <- ads$browser_download_url

  ads$version <- gsub("^[^0-9]+","",ads$tag_name)

  ads$time_idx <- ads$id.assets


  ads$file <- basename(ads$name.assets)

  ads$file_integrity <- ads$size
  ads$file_integrity_algo <- "size"

  ads

}

rst_webdriver_json_url_parser_googleapis <- function(src_url){
  pre_app_name <- basename(
    dirname(
      rev(
        strsplit(src_url, "www.googleapis.com")[[1]]
      )[1]
    )
  )
  jorig <- jsonlite::fromJSON(src_url)
  jitems <- jorig$items
  jitems$is_zip <- grepl(".zip$",jitems$name)
  jitems$is_jar <- grepl(".jar$",jitems$name)

  # appname detection from source
  get_app_name <- function(name_str, get_plat = FALSE){
    bn <- basename(name_str)
    dn <- dirname(name_str)
    dnp <- strsplit(dn, "[^0-9.]")[[1]][1]
    if(grepl(dnp, bn)){
      bnp <- strsplit(bn, dnp)[[1]][1]
      bnp <- strsplit(bnp, "_")[[1]][1]
    }else{
      bnp <- strsplit(bn, "_|.zip")[[1]][1]
    }
    bnp <- gsub("[^0-9a-zA-Z]$","",bnp)
    if(get_plat){
      plt <- rev(strsplit(bn, bnp)[[1]])[1]
      plt <- gsub("^[^a-zA-Z0-9]","",plt)
      plt <- strsplit(plt, "[^a-zA-Z0-9]")[[1]][1]
      return(plt)
    }
    bnp
  }

  jitems$appname <- sapply(jitems$name, get_app_name)
  jitems$appname_from_url <-  pre_app_name
  jitems$platform_tag <- NA

  # platform detection from src
  if(any(jitems$is_zip)){
    platforms <- sapply(jitems$name[jitems$is_zip],
                        get_app_name, get_plat = TRUE)

    jitems$platform_tag[jitems$is_zip] <- platforms
  }

  if(any(jitems$is_jar)){
    jitems$platform_tag[jitems$is_jar] <- "generic"
  }

  jitems$is_valid_platform <- sys_valid_os_string(jitems$platform_tag)
  jitems$is_valid_platform <- jitems$is_valid_platform | (jitems$platform_tag == "generic")

  jitems$url <- jitems$mediaLink

  jitems$version <- dirname(jitems$name)

  jitems$time_idx <- jitems$generation


  jitems$file <- basename(jitems$name)

  jitems$file_integrity <- jitems$md5Hash
  jitems$file_integrity_algo <- "md5"

  jitems

}


# edge drivers
# https://msedgedriver.azureedge.net/

rst_webdriver_edge_url_parser <- function(
  edge_url = "https://msedgedriver.azureedge.net/"
){

  pre_app_name <- "edgedriver"

  # well these are not available if https://msedgedriver.azureedge.net/ is
  # not available. Hence creating a dummy integrity in case required
  f_integrity <- 6*1024*1024 # 6MB
  f_integrity_algo <- "size"

  if(grepl("msedgedriver.azureedge.net", edge_url)){

    # details
    # https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/
    code <- tryCatch({
      u <- xml2::read_xml(edge_url)
      dl_name <- rvest::html_text(rvest::html_nodes(rvest::html_nodes(u, "Blob"), "Name"))
      dl_urls <- rvest::html_text(rvest::html_nodes(rvest::html_nodes(u, "Blob"), "Url"))
      f_integrity <-  rvest::html_text(rvest::html_nodes(rvest::html_nodes(u, "Blob"), "Content-MD5"))
      f_integrity_algo <- "md5"
      # @Dev checked digest is not matching. may be we have to switch to size
      0
    },
    error = function(e){
      1
    })

    # alternative way
    # As on 8th Jan 2021 https://msedgedriver.azureedge.net/ read access is
    # restricted possibly by the server
    if(code!=0){
      wp <- xml2::read_html(
        "https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/"
      )
      dl_urls <- rvest::html_attr(rvest::html_nodes(wp,"a"),"href")
      dl_name <- file.path(
        basename(dirname(dl_urls)),
        basename(dl_urls),
        fsep = "/"
      )

    }

  }else{

    ####### direct query ########
    # e.g. https://msedgewebdriverstorage.z22.web.core.windows.net/?prefix=87.0.664.66/
    # but that is java script enabled
    # so searching directly if download links exists
    # URL pattern {edgedriver_direct_query_<version>}

    dq_url <- "edgedriver_direct_query_"

    ever <- gsub(dq_url, "", edge_url)

    all_plats <- c("mac64", "win32", "win64", "linux64")
    dlinks <- paste0(
      "https://msedgewebdriverstorage.blob.core.windows.net/edgewebdriver/",
      ever,
      "/edgedriver_",
      all_plats[
        sys_valid_os_string(all_plats, this_machine = TRUE)]
      ,".zip"
    )

    if(length(dlinks)>0){
      chks <- sapply(dlinks, is_url_exists)
      dlinks <- dlinks[chks]
    }

    if(length(dlinks)==0){
      # no URL
      dlinks <- ""
    }

    dl_urls <- dlinks
    dl_name <- file.path(
      basename(dirname(dl_urls)),
      basename(dl_urls),
      fsep = "/"
    )

  }

  n <- min(length(dl_name), length(dl_urls))

  dl <- data.frame(Name = dl_name[seq(n)], URL = dl_urls[seq(n)])

  dl$is_zip <- grepl(".zip$",dl$Name)
  dl$is_jar <- grepl(".jar$",dl$Name)

  dl$appname <- sapply(
    strsplit(
      gsub(".zip$|.jar$","",basename(dl$Name)), "_"
    ),
    function(x) x[1]
  )

  dl$appname_from_url <- pre_app_name

  dl$platform_tag <- NA

  if(any(dl$is_zip)){
    platforms <- sapply(
      strsplit(
        gsub(".zip$","",dl$Name[dl$is_zip]), "_"
      ),
      function(x) rev(x)[1]
    )
    dl$platform_tag[dl$is_zip] <- platforms
  }

  dl$is_valid_platform <- sys_valid_os_string(dl$platform_tag)
  dl$url <- dl$URL
  dl$URL <- NULL

  dl$version <- gsub("^[^0-9]+","",dirname(dl$Name))

  dl$time_idx <- seq(dl$Name)


  dl$file <- basename(dl$Name)


  dl$file_integrity <- f_integrity
  dl$file_integrity_algo <- f_integrity_algo

  dl
}
