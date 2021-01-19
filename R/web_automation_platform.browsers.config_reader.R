

wap_browser_config_reader <- function(){

}

######### Chrome ########

wap_browser_config_reader_chrome <- function(){
  # path found in chrome://version/
  if(is_available("jsonlite")){
    cf <- wap_sys_browser_config_path_chrome()
    # early exit
    if(is.na(cf)){
      cat("\nUnable to found profile path. Check chrome://version/ in chrome\n")
      return(list())
    }
    jsonlite::fromJSON(
      readLines(cf, warn = FALSE)
    )
  }else{
    list()
  }
}

# Default location for chrome profile (default)
# https://chromium.googlesource.com/chromium/src/+/master/docs/user_data_dir.md
wap_sys_browser_config_path_chrome <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_chrome")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  # @Dev
  # mostly in linux it is different
  pref <- file.path(al[which(dir.exists(al))[1]], "Default","Preferences")
  pref <- normalizePath(pref, mustWork = FALSE)

  if(file.exists(pref)){
    pref
  }else{
    NA
  }
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
# ref https://developer.mozilla.org/en-US/docs/Mozilla/Preferences/A_brief_guide_to_Mozilla_preferences

wap_browser_config_reader_firefox <- function(){
  # path found in about:support
  if(is_available("jsonlite")){
    cf <- wap_sys_browser_config_path_firefox()
    # early exit
    if(is.na(cf)){
      cat("\nUnable to found profile path. Check about:support in firefox\n")
      return(list())
    }
    jsonlite::fromJSON(
      readLines(cf, warn = FALSE)
    )
  }else{
    list()
  }
}

# Default location for firefox profile (default)
# http://kb.mozillazine.org/Profile_folder_-_Firefox
# https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data
# ... #w_finding-your-profile-without-opening-firefox (sub header)
wap_sys_browser_config_path_firefox <- function(){

  al <- sys_use_os_specific_method("wap_sys_browser_config_path_firefox")

  al <- sapply(al, normalizePath, mustWork = FALSE)

  # @Dev
  # mostly in linux it is different
  pref <- file.path(al[which(dir.exists(al))[1]], "Default","Preferences")
  pref <- normalizePath(pref, mustWork = FALSE)

  if(file.exists(pref)){
    pref
  }else{
    NA
  }
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

wap_sys_browser_config_path_chrome_linux <- function(){

  # Linux
  # Profile folders are located here:
  #
  #   ~/.mozilla/firefox/<profile folder>

  list(
    firefox = normalizePath("~/.mozilla/firefox/")
  )

}
