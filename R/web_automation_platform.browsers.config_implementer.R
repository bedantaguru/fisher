
# as of now it is tuned for rst
# (or RSelenium)
# RSelenium::remoteDriver(extraCapabilities = <to put the output here>)
wap_browser_config_implementer <- function(
  browser,
  # conf_lst: this will be added to preference (which also can be read by
  # `wap_browser_config_reader`)
  conf_lst = NULL,
  # arg_lst: this will be argument to driver (prior knowledge required for this.
  # Not possible to read dynamically.)
  arg_lst = NULL,
  # raw_lst: add directly to driver options (it is like experimental arguments).
  # Ideally both above can be codified inside this. But it is kept for other
  # options.
  raw_lst = NULL
){

  if(length(conf_lst)==0 ) conf_lst <- NULL
  if(length(arg_lst)==0 ) arg_lst <- NULL
  if(length(raw_lst)==0 ) raw_lst <- NULL

  # early exit if all are NULL
  if(is.null(conf_lst) & is.null(arg_lst) & is.null(raw_lst)) return(list())

  browser <- wap_valid_browser(browser)

  do.call(
    what = paste0("wap_browser_config_implementer_",browser),
    args = list(
      conf_lst = conf_lst,
      arg_lst = arg_lst,
      raw_lst = raw_lst))
}


wap_browser_config_implementer_firefox <- function(
  conf_lst = NULL,
  arg_lst = NULL,
  raw_lst = NULL
){
  # ref :
  # https://firefox-source-docs.mozilla.org/main/65.0/testing/geckodriver/geckodriver/Capabilities.html
  l <- list()

  if(!is.null(arg_lst)){
    al <- list(
      `moz:firefoxOptions` =
        list(
          args = arg_lst
        )
    )

    l <- merge_list(l, al)
  }

  if(!is.null(conf_lst)){
    pl <- list(
      `moz:firefoxOptions` =
        list(
          prefs = conf_lst
        )
    )

    l <- merge_list(l, pl)
  }

  if(!is.null(raw_lst)){
    rl <- list(
      `moz:firefoxOptions` =
        raw_lst
    )

    l <- merge_list(l, rl)
  }

  l

}

wap_browser_config_implementer_chrome <- function(
  conf_lst = NULL,
  arg_lst = NULL,
  raw_lst = NULL
){

    l <- list()

  if(!is.null(arg_lst)){
    al <- list(
      chromeOptions =
        list(
          args = arg_lst
        )
    )

    l <- merge_list(l, al)
  }

  if(!is.null(conf_lst)){
    pl <-  list(
      chromeOptions =
        list(
          prefs = conf_lst
        )
    )

    l <- merge_list(l, pl)
  }

  if(!is.null(raw_lst)){
    rl <- list(
      chromeOptions =
        raw_lst
    )

    l <- merge_list(l, rl)
  }

  l
}

wap_browser_config_implementer_edge <- function(
  conf_lst = NULL,
  arg_lst = NULL,
  raw_lst = NULL
){


  l <- list()

  if(!is.null(arg_lst)){
    al <- list(
      `ms:edgeOptions` =
        list(
          args = arg_lst
        )
    )

    l <- merge_list(l, al)
  }

  if(!is.null(conf_lst)){
    pl <-  list(
      `ms:edgeOptions` =
        list(
          prefs = conf_lst
        )
    )

    l <- merge_list(l, pl)
  }

  if(!is.null(raw_lst)){
    rl <- list(
      `ms:edgeOptions` =
        raw_lst
    )

    l <- merge_list(l, rl)
  }

  l
}

wap_browser_config_implementer_opera <- function(
  conf_lst = NULL,
  arg_lst = NULL,
  raw_lst = NULL,
  user_data_dir = NULL
){


  l <- list()

  if(!is.null(arg_lst)){
    al <- list(
      `goog:chromeOptions` =
        list(
          args = arg_lst
        )
    )

    l <- merge_list(l, al)
  }

  if(!is.null(conf_lst)){
    ecl <- list()
    # this one works
    if(is_available("jsonlite")){
      # read back config and add to it
      read_back_config <- FALSE
      if(is.null(user_data_dir)){
        tpf <- tempfile(pattern = "userDataDirOpera")
      }else{
        tpf <- user_data_dir
        if(dir.exists(tpf)){
          read_back_config <- TRUE
        }
      }

      dir.create(tpf, showWarnings = FALSE)
      tpfp <- file.path(tpf, "Preferences")
      try({
        if(read_back_config){
          prior_cnf <- jsonlite::fromJSON(tpfp)
          conf_lst <- merge_list(prior_cnf, conf_lst)
        }
        writeLines(jsonlite::toJSON(conf_lst, auto_unbox = TRUE), tpfp)
        ecl <- list(
          `goog:chromeOptions` = list(
            args = list(paste0('--user-data-dir=',normalizePath(tpf),'')))
        )

      }, silent = TRUE)
    }


    extra <- list(
      # this mostly have no impacts
      `goog:chromeOptions` =
        list(
          prefs = conf_lst
        )
    )

    ecl <- merge_list(ecl, extra)
    ecl

    l <- merge_list(l, ecl)
  }

  if(!is.null(raw_lst)){
    rl <- list(
      `goog:chromeOptions` =
        raw_lst
    )

    l <- merge_list(l, rl)
  }

  l

}


wap_browser_config_implementer_opera_prior_profile <- function(l){

  prior_profile <- NULL

  if(!is.null(l$`goog:chromeOptions`$args)){
    nl <- unlist(l$`goog:chromeOptions`$args)
    if(any(grepl("user-data-dir", nl))){
      pd <- nl[grepl("user-data-dir", nl)]
      pd <- rev(strsplit(pd, "=|= +")[[1]])[1]
      if(dir.exists(pd)){
        prior_profile <- pd
      }
    }
  }

  prior_profile
}


wap_browser_config_appender <- function(browser, c1, c2){
  browser <- wap_valid_browser(browser)
  l <- list()
  if(browser == "opera"){
    # special case for opera only
    # read them separately and then merge back
    p1 <- wap_browser_config_implementer_opera_prior_profile(c1)
    p2 <- wap_browser_config_implementer_opera_prior_profile(c2)

    if(!is.null(p2) & !is.null(p1)){
      # removing --user-data-dir entry from c2
      if(length(c2$`goog:chromeOptions`$args)==1){
        c2$`goog:chromeOptions`$args <-NULL
      }else{
        nds <- unlist(c2$`goog:chromeOptions`$args)
        c2$`goog:chromeOptions`$args <-
          c2$`goog:chromeOptions`$args[-which(grepl("--user-data-dir",nds))]
      }

      c2prior <- wap_browser_config_reader_opera(p2)
      # we can delete it if required
      # but keeping it (as it can be a non temp folder)
      # just writing c2 config also in c1 profile
      wap_browser_config_implementer_opera(
        conf_lst = c2prior,
        user_data_dir = p1)

    }

    l <- merge_list(c1, c2)


  }else{
    l <- merge_list(c1, c2)
  }
  l
}
