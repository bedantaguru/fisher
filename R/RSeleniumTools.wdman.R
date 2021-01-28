

rst_wdman_selenium_info_env <- new.env()

rst_wdman_selenium_launcher <- function(
  port = NULL,
  selenium_version = c("dev","stable"),
  webdrivers_offline
){

  selenium_version <- match.arg(selenium_version)

  if(missing(webdrivers_offline)){
    webdrivers_offline <- rst_webdriver_offline_info()
  }


  # {wdman} managed browsers + selenium
  # chrome
  # gecko
  # ie
  # phantom
  #
  # Here only chrome and gecko is left to be managed by them rest we are taking
  # care (opera and edge). Also disabling ie and phantom

  # selenium
  sver <- NULL
  if(any(grepl("selenium",webdrivers_offline$appname))){

    sel <- webdrivers_offline[
      grepl("selenium",webdrivers_offline$appname),
    ]

    sver <- sel$version


    if(selenium_version %in% sel$remarks){
      sver <- sver[sel$remarks==selenium_version]
    }

    # default is latest
    sver <- max(sver)

  }

  # chrome
  cver <- NULL
  if("chromedriver" %in% webdrivers_offline$appname){
    cver <- webdrivers_offline$version[
      webdrivers_offline$appname=="chromedriver"
    ]
  }

  # gecko (firefox)
  gver <- NULL
  if("geckodriver" %in% webdrivers_offline$appname){
    gver <- webdrivers_offline$version[
      webdrivers_offline$appname=="geckodriver"
    ]
  }

  extra_jvmargs_lst <- list()

  # opera
  if("operadriver" %in% webdrivers_offline$appname){
    oexe <- webdrivers_offline$bin_file[
      webdrivers_offline$appname=="operadriver"
    ]
    extra_jvmargs_lst$opera <- paste0(
      "-Dwebdriver.opera.driver=",
      paste0('"',oexe,'"')
    )
  }

  # edge
  if("edgedriver" %in% webdrivers_offline$appname){
    eexe <- webdrivers_offline$bin_file[
      webdrivers_offline$appname=="edgedriver"
    ]
    extra_jvmargs_lst$edge <- paste0(
      "-Dwebdriver.edge.driver=",
      paste0('"',eexe,'"')
    )
  }


  # kill previous instance
  if(exists("s_handle",envir = rst_wdman_selenium_info_env)){
    rst_wdman_selenium_info_env$s_handle$process$kill_tree()
  }

  # desired port
  # match with below dport in force_kill_logic
  dport <- 4567L

  # sets arg `force` in sys_get_a_port as TRUE is some condition is met
  # otherwise FALSE
  force_kill_logic <- function(){
    # match this with above dport
    dport <- 4567L

    l <- tryCatch({

      str1 <- readLines(
        paste0("http://localhost:",dport,"/wd/hub/status"),
        warn = FALSE)
      str2 <- readLines(
        paste0("http://localhost:",dport,"/wd/hub/sessions"),
        warn = FALSE)

      # it can be true mostly if it is a selenium
      any(grepl("server is running",tolower(str1))) &
        any(grepl("status",tolower(str2)))

    },
    error = function(e) FALSE,
    finally = FALSE)

    if(!is.logical(l)) return(FALSE)
    if(is.na(l)) return(FALSE)

    l
  }

  sport <- sys_get_a_port(dport, kill_logic = force_kill_logic)

  sel <- wdman::selenium(
    port = sport,
    # selenium, chrome and gecko : these are managed by {wdman} directly
    # However, we can manage directly if required
    # version = ,
    chromever = cver,
    geckover = gver,

    # rest (opera and edge managed by {fisher})
    jvmargs = extra_jvmargs_lst,

    # disabling phantom and ie
    phantomver = NULL, iedrver = NULL,
    # no need to check as it is one time action, and will be taken care by
    # {fisher}
    check = FALSE)

  assign( "s_handle", sel, envir = rst_wdman_selenium_info_env)
  assign( "s_port", sport, envir = rst_wdman_selenium_info_env)

  invisible(sel)

}
