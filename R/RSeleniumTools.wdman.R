

rst_wdman_selenium_launcher <- function(port = NULL){
  webdrivers_offline <- rst_webdriver_offline_info()
  # @Dev need to infuse in offline info
  bad <- rst_binman_all_apps_details()
  bad <- bad[c("app", "version","bin_file")]

  # {wdman} managed browsers
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
    sver <- webdrivers_offline$version[
      grepl("selenium",webdrivers_offline$appname)
    ]
    # @Dev
    # this need to work out
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
    oexe <- webdrivers_offline$file[
      webdrivers_offline$appname=="operadriver"
    ]
    extra_jvmargs_lst$opera <- oexe
  }

  # edge
  ever <- NULL
  if("edgedriver" %in% webdrivers_offline$appname){
    ever <- webdrivers_offline$version[
      webdrivers_offline$appname=="edgedriver"
    ]
  }


  sport <- sys_get_a_port(4567)

  sel <- wdman::selenium(
    port = sport,
    # selenium, chrome and gecko : these are managed by {wdman} directly
    # However, we can manage directly if required
    # version = ,
    chromever = cver,
    geckover = gver,

    # rest (opera and edge managed by {fisher})
    # @Dev
    jvmargs = list(
      opera =
        paste0(
          "-Dwebdriver.opera.driver=",
          paste0('"',oexe,'"')
        ),
      edge =
        paste0(
          "-Dwebdriver.edge.driver=",
          paste0('"',eexe,'"')
        )
    ),

    # disabling phantom and ie
    phantomver = NULL, iedrver = NULL,
    # no need to check as it is one time action and will be taken care by
    # {fisher}
    check = FALSE)


  # @Dev
  # client names MicrosoftEdge
  rd <- RSelenium::remoteDriver(browserName = "chrome", port = sport)


}
