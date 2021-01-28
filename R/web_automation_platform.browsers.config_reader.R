

wap_browser_config_reader <- function(browser, file_path){
  browser <- wap_valid_browser(browser)
  l <- tryCatch({
    if(!missing(file_path)){
      do.call(what = paste0("wap_browser_config_reader_",browser),
              args = list(file_path = file_path))
    }else{
      do.call(what = paste0("wap_browser_config_reader_",browser),
              args = list())
    }

  }, error = function(e){
    cat("\nwap_browser_config_reader failed you may",
        "try rst_remotedriver_config_reader\n")
    list()
  })
  l
}

######### Chrome ########

wap_browser_config_reader_chrome <- function(file_path,
                                             chromium_name = "Chrome",
                                             chromium_details = "chrome://version/"){
  # path found in chrome://version/
  if(is_available("jsonlite")){

    if(missing(file_path)){
      cf <- wap_sys_browser_config_path_chrome()
    }else{
      cf <- file_path
    }

    # @Dev
    # mostly in linux it is different
    # Preferences
    pref <- file.path(cf, "Default","Preferences")
    pref <- normalizePath(pref, mustWork = FALSE)

    # Secure Preferences
    pref2 <- file.path(cf, "Default","Secure Preferences")
    pref2 <- normalizePath(pref2, mustWork = FALSE)

    # Preferences (in root) (for opera)
    pref3 <- file.path(cf, "Preferences")
    pref3 <- normalizePath(pref3, mustWork = FALSE)

    # Secure Preferences (in root) (for opera)
    pref4 <- file.path(cf, "Secure Preferences")
    pref4 <- normalizePath(pref4, mustWork = FALSE)



    # early exit
    if(!any(file.exists(c(pref, pref2, pref3, pref4)))){
      cat(paste0(
        "\nUnable to found profile path. Check ",chromium_details,
        " in ",chromium_name,"\n"
      ))
      return(list())
    }

    if(file.exists(pref)){
      l1 <- jsonlite::fromJSON(
        readLines(pref, warn = FALSE)
      )
    }else{
      l1 <- list()
    }


    for(pf in c(pref2, pref3, pref4)){

      if(file.exists(pf)){
        l2 <- jsonlite::fromJSON(
          readLines(pf, warn = FALSE)
        )
        l1 <- merge_list(l1, l2)
      }

    }

    l1

  }else{
    list()
  }
}

# Default location for chrome profile (default)
# https://chromium.googlesource.com/chromium/src/+/master/docs/user_data_dir.md

wap_sys_browser_config_path_chrome <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_chrome")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  if(!any(dir.exists(al))) return(NA)

  al <- al[which(dir.exists(al))[1]]

  al
}

wap_sys_browser_config_path_chrome_windows <- function(){
  # Windows
  # The default location is in the local app data folder:
  #
  # [Chrome] %LOCALAPPDATA%\Google\Chrome\User Data
  # [Chrome Canary] %LOCALAPPDATA%\Google\Chrome SxS\User Data
  # [Chromium] %LOCALAPPDATA%\Chromium\User Data

  lad <- Sys.getenv("LOCALAPPDATA")

  list(
    Chrome = file.path(lad, "Google\\Chrome\\User Data"),
    Chrome_Canary = file.path(lad, "Google\\Chrome SxS\\User Data"),
    Chromium = file.path(lad, "Chromium\\User Data")
  )

}

wap_sys_browser_config_path_chrome_mac <- function(){
  # Mac OS X
  # The default location is in the Application Support folder:
  #
  # [Chrome] ~/Library/Application Support/Google/Chrome
  # [Chrome Canary] ~/Library/Application Support/Google/Chrome Canary
  # [Chromium] ~/Library/Application Support/Chromium

  list(
    Chrome = normalizePath("~/Library/Application Support/Google/Chrome"),
    Chrome_Canary =
      normalizePath("~/Library/Application Support/Google/Chrome Canary"),
    Chromium = normalizePath("~/Library/Application Support/Chromium")
  )

}

wap_sys_browser_config_path_chrome_linux <- function(){
  # Linux
  # The default location is in ~/.config:
  #
  # [Chrome Stable] ~/.config/google-chrome
  # [Chrome Beta] ~/.config/google-chrome-beta
  # [Chrome Dev] ~/.config/google-chrome-unstable
  # [Chromium] ~/.config/chromium

  list(
    Chrome_Stable = normalizePath("~/.config/google-chrome"),
    Chrome_Beta = normalizePath("~/.config/google-chrome-beta"),
    Chrome_Dev = normalizePath("~/.config/google-chrome-unstable"),
    Chromium = normalizePath("~/.config/chromium")
  )

}


######### Firefox ########
# ref :
# https://developer.mozilla.org/en-US/docs/Mozilla/Preferences/A_brief_guide_to_Mozilla_preferences
# A preference is read from a file, and can call up to four methods: pref(),
# user_pref(), sticky_pref() and lockPref(). All preferences files may call
# pref(), user_pref() and sticky_pref(), while the config file in addition may
# call lockPref().
# Ref : https://github.com/Theemim/GeckoAutoconfigIntro

wap_browser_config_reader_firefox <- function(file_path){
  # path found in about:support

  if(missing(file_path)){
    ff <- wap_sys_browser_config_path_firefox()
  }else{
    ff <- file_path
  }

  profs  <- list.files(ff, pattern = "default")
  prefs <- list.files(
    ff,
    pattern = "prefs.js|user.js",
    recursive = TRUE, full.names = TRUE)

  pref <- prefs[grepl(paste0(profs, collapse = "|"), prefs)]

  if(length(pref)==0){
    ff <- NA
  }else{
    pref <- normalizePath(pref, mustWork = FALSE)

    if(all(file.exists(pref))){
      ff <- pref
    }else{
      ff <- NA
    }

  }

  # early exit
  if(any(is.na(ff))){
    cat("\nUnable to found profile path. Check about:support in firefox\n")
    return(list())
  }

  # simulate js and take out pref as list
  senv <- new.env()
  senv$lo <- list()
  senv$true <- TRUE
  senv$false <- FALSE
  senv$false <- FALSE

  senv$user_pref <- function(prefName, value){
    senv$lo[[prefName]]<<-value
  }

  senv$pref <- senv$sticky_pref <- senv$user_pref

  ffls <- unlist(lapply(ff, readLines))
  # valid lines retention
  ffls <- ffls[grepl("pref\\(|pref\\(|;$",ffls)]

  # # alternate approach (but not so easy)
  # # remove linear comments
  # ffls <- ffls[!grepl("^ +//|^//",ffls)]
  # # obfuscation of /* comments
  # # but this is not working as expected
  # ffls <- gsub("^/\\*|^ +/\\*","f <- function()\\{",ffl)
  # ffls <- gsub("\\*/$|\\*/ +$","\\}",ffls)
  # # Other options is to use {v8}

  # fallback option
  lo <- list()

  try({
    source(exprs = parse(text = ffls), local = senv)

    lo <- senv$lo
  }, silent = TRUE)

  lo


}

# Default location for firefox profile (default)
# http://kb.mozillazine.org/Profile_folder_-_Firefox
# https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data
# ... #w_finding-your-profile-without-opening-firefox (sub header)
wap_sys_browser_config_path_firefox <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_firefox")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  if(!any(dir.exists(al))) return(NA)

  al <- al[which(dir.exists(al))[1]]

  al
}


wap_sys_browser_config_path_firefox_windows <- function(){

  ad <- Sys.getenv("APPDATA")

  list(
    firefox = file.path(ad, "Mozilla\\Firefox\\Profiles\\")
  )

}

wap_sys_browser_config_path_firefox_mac <- function(){
  # Mac
  # Profile folders are in one of these locations:
  #
  # ~/Library/Application Support/Firefox/Profiles/<profile folder>
  # ~/Library/Mozilla/Firefox/Profiles/<profile folder>


  list(
    f1 = normalizePath("~/Library/Application Support/Firefox/Profiles/"),
    f2 = normalizePath("~/Library/Mozilla/Firefox/Profiles/")
  )

}

wap_sys_browser_config_path_firefox_linux <- function(){

  # Linux
  # Profile folders are located here:
  #
  #   ~/.mozilla/firefox/<profile folder>

  list(
    firefox = normalizePath("~/.mozilla/firefox/")
  )

}


######### Opera ########

wap_browser_config_reader_opera <- function(file_path){
  # path found in opera://about
  if(missing(file_path)){
    file_path <- wap_sys_browser_config_path_opera()
  }
  wap_browser_config_reader_chrome(file_path,
                                   chromium_name = "Opera",
                                   chromium_details = "opera://about")
}

# Default location for opera profile (default)
# https://techdows.com/2016/08/opera-profile-location.html

wap_sys_browser_config_path_opera <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_opera")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  if(!any(dir.exists(al))) return(NA)

  al <- al[which(dir.exists(al))[1]]

  al
}

wap_sys_browser_config_path_opera_windows <- function(){

  ad <- Sys.getenv("APPDATA")

  list(
    Opera = file.path(ad, "Opera Software\\Opera Stable")
  )

}

######### Edge ########
wap_browser_config_reader_edge <- function(file_path){
  # path found in edge://version/
  if(missing(file_path)){
    file_path <- wap_sys_browser_config_path_edge()
  }
  wap_browser_config_reader_chrome(file_path,
                                   chromium_name = "Edge",
                                   chromium_details = "edge://version/")
}

# Default location for edge profile (default)
# https://www.tenforums.com/tutorials/144642-how-add-profile-microsoft-edge-chromium.html

wap_sys_browser_config_path_edge <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_edge")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  if(!any(dir.exists(al))) return(NA)

  al <- al[which(dir.exists(al))[1]]

  al
}

wap_sys_browser_config_path_edge_windows <- function(){

  lad <- Sys.getenv("LOCALAPPDATA")

  list(
    Edge = file.path(lad, "Microsoft\\Edge\\User Data"),
    Edge_beta = file.path(lad, "Microsoft\\Edge Beta\\User Data"),
    Edge_dev = file.path(lad, "Microsoft\\Edge Dev\\User Data"),
    Edge_canary = file.path(lad, "Microsoft\\Edge SxS\\User Data")
  )

}

