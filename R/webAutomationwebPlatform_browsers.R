


wap_valid_browser <- function(browser){
  browser <- match.arg(
    browser,
    choices = c(
      "chrome",
      "firefox",
      "edge",
      "opera"
    )
  )
  browser
}



