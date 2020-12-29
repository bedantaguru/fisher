



# @Dev
# firefox check
# https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html
# https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/

# edge drivers
# https://msedgedriver.azureedge.net/

rst_webdriver_edge_url_parser <- function(edge_url = "https://msedgedriver.azureedge.net/"){

  # details
  # https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/
  u <- xml2::read_xml(edge_url)
  dl_name <- rvest::html_text(rvest::html_nodes(rvest::html_nodes(u, "Blob"), "Name"))
  dl_urls <- rvest::html_text(rvest::html_nodes(rvest::html_nodes(u, "Blob"), "Url"))
  n <- min(length(dl_name), length(dl_urls))

  invisible(data.frame(Name = dl_name[seq(n)], URL = dl_urls[seq(n)]))
}

rst_webdriver_json_url_parser <- function(src_url){
  # 3 kinds of URL at this moment
  # www.googleapis.com api.github.com api.bitbucket.org
  # Following are specific urls
  # https://www.googleapis.com/storage/v1/b/chromedriver/o
  # https://www.googleapis.com/storage/v1/b/selenium-release/o
  # https://www.googleapis.com/storage/v1/b/selenium-release/o
  # https://api.github.com/repos/mozilla/geckodriver/releases
  # https://api.github.com/repos/operasoftware/operachromiumdriver/releases
  # https://api.bitbucket.org/2.0/repositories/ariya/phantomjs/downloads?pagelen=100



}


rst_webdriver_json_url_parser_github <- function(src_url){

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

      # @Dev
      # bad hack for duplicate 'row.names' are not allowed
      row.names(d)<-paste0(i,"_",seq(nrow(d)))
      d
    }
  )

  ads <- do.call("rbind", ads)

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
  jitems$appname_match <-  jitems$appname == pre_app_name
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

  jitems

}

rst_webdriver_json_url_parser_bitbucket <- function(src_url){
  # is it required?
  # only PhantomJS (which is almost dead)
  jorig <- jsonlite::fromJSON(src_url)
}
