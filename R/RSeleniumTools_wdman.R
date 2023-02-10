

rst_wdman_selenium_info_env <- new.env()

# fill rst_wdman_selenium_info_env in case rst_wdman_selenium_info_env is not
# populated by rst_wdman_selenium_launcher may be useful for later found
# selenium instance.
#
# full_control = TRUE gives kill capabality to selenium (by default it is TRUE)
rst_wdman_selenium_fill_info_env <- function(spid, pmap, full_control = TRUE){

  tryCatch({

    if(!missing(pmap)){

      assign( "s_port", pmap$port[ pmap$pid == spid ],
              envir = rst_wdman_selenium_info_env)

      if(full_control & is_available("ps")){

        # this has to match all use sections in the code
        sel <- list(process = list(
          kill_tree = function(){
            sys_ps_kill_tree(ps::ps_handle(spid))
          },
          as_ps_handle = function(){
            ps::ps_handle(spid)
          },
          get_pid = function(){
            spid
          },
          is_alive = function(){
            ps::ps_is_running(ps::ps_handle(spid))
          })
        )

        assign( "s_handle", sel, envir = rst_wdman_selenium_info_env)
      }

    }

  },
  error = function(e) NULL)

  invisible(0)
}

rst_wdman_selenium_launcher <- function(
    port = NULL,
    selenium_version = c("dev","stable"),
    on_exit_cleanup = FALSE,
    # -browserTimeout
    # <Integer> in seconds : number of seconds a browser session is allowed to
    # hang while a WebDriver command is running (example: driver.get(url)). If
    # the timeout is reached while a WebDriver command is still processing,
    # the session will quit. Minimum value is 60. An unspecified, zero, or
    # negative value means wait indefinitely. If a node does not specify it,
    # the hub value will be used.
    browserTimeout = 300L,
    # -timeout, -sessionTimeout
    # <Integer> in seconds : Specifies the timeout before the server
    # automatically kills a session that hasn't had any activity in the last X
    # seconds. The test slot will then be released for another test to use.
    # This is typically used to take care of client crashes. For grid hub/node
    # roles, cleanUpCycle must also be set. If a node does not specify it, the
    # hub value will be used.
    #
    # ref : https://stackoverflow.com/questions/55438913/is-there-a-way-too-prevent-selenium-automatically-terminating-idle-sessions
    sessionTimeout = 57868143L,

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

  # selenium specific

  selargs_lst <- list()

  selargs_lst$browserTimeout <- paste0("-browserTimeout ",
                                       as.integer(browserTimeout))

  selargs_lst$sessionTimeout  <- paste0("-sessionTimeout  ",
                                        as.integer(sessionTimeout ))

  # this is required for fix in "wdman and LICENSE.chromedriver issue"
  bypass_wdman_detection <- TRUE
  extra_jvmargs_lst <- list()

  # chrome
  cver <- NULL
  if("chromedriver" %in% webdrivers_offline$appname){
    if(bypass_wdman_detection){
      cexe <- webdrivers_offline$bin_file[
        webdrivers_offline$appname=="chromedriver"
      ]
      extra_jvmargs_lst$chrome <- paste0(
        "-Dwebdriver.chrome.driver=",
        paste0('"',cexe,'"')
      )
    }else{
      cver <- webdrivers_offline$version[
        webdrivers_offline$appname=="chromedriver"
      ]
    }
  }

  # gecko (firefox)
  gver <- NULL
  if("geckodriver" %in% webdrivers_offline$appname){
    if(bypass_wdman_detection){

      stop("To Develop")

    }else{
      gver <- webdrivers_offline$version[
        webdrivers_offline$appname=="geckodriver"
      ]
    }
  }


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

    sys_is_port_running_selenium(dport)
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

    # selenium args
    selargs = selargs_lst,

    # disabling phantom and ie
    phantomver = NULL, iedrver = NULL,
    # no need to check as it is one time action, and will be taken care by
    # {fisher}
    check = FALSE)

  assign( "s_handle", sel, envir = rst_wdman_selenium_info_env)
  assign( "s_port", sport, envir = rst_wdman_selenium_info_env)

  if(on_exit_cleanup){
    ws <- wap_config_store()
    # make first caller initiator
    prior_pid <- ws$read("wap_rst_selenium_initiator_pid")
    if(!is.null(prior_pid)){
      if(!sys_is_pid_active(prior_pid)){
        # if somehow old pid which is not active is present
        prior_pid <- NULL
      }
    }

    if(is.null(prior_pid)){
      ws$write("wap_rst_selenium_initiator_pid", Sys.getpid())
    }

    # stop selenium on exit (or on unload)
    # if it is initiator process
    reg.finalizer(
      asNamespace("fisher"),
      function(e){
        ws <- e$wap_config_store()
        prior_pid <- ws$read("wap_rst_selenium_initiator_pid")
        # destroy if the calling process is initiator
        if(isTRUE(as.integer(prior_pid)==Sys.getpid())){
          tryCatch(
            e$rst_wdman_selenium_info_env$s_handle$process$kill_tree(),
            error = function(e) NULL
          )
        }
      },
      onexit = TRUE
    )
  }

  invisible(sel)

}

