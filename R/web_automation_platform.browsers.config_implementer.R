
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
  raw_lst = NULL
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
      tpf <- tempfile(pattern = "userDataDirOpera")
      dir.create(tpf, showWarnings = FALSE)
      tpfp <- file.path(tpf, "Preferences")
      try({
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
