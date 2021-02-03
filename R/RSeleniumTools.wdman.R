

rst_wdman_selenium_info_env <- new.env()

# fill rst_wdman_selenium_info_env in case rst_wdman_selenium_info_env is not
# populated by rst_wdman_selenium_launcher may be useful for later found
# selenium instance.
#
# full_control = TRUE gives kill capabality to selenium (by default it is TRUE)
rst_wdman_selenium_fill_info_env <- function(spid, pmap, full_control = TRUE){

  try({

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

  },silent = TRUE)

  invisible(0)
}

rst_wdman_selenium_launcher <- function(
  port = NULL,
  selenium_version = c("dev","stable"),
  on_exit_cleanup = FALSE,
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
        if(isTRUE(prior_pid==Sys.getpid())){
          try(
            e$rst_wdman_selenium_info_env$s_handle$process$kill_tree(),
            silent = TRUE
          )
        }
      },
      onexit = TRUE
    )
  }

  invisible(sel)

}

