


# https://www.java.com/en/download/
# https://www.opera.com/download

# https://gist.github.com/tyjeon/ebae6a59f11822acdf8fb1a40fb24247
# https://developer.github.com/v3/repos/releases/
# https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#releases


require(rvest)


wp <- "https://b-ok.asia/book/2883645/4d513b?dsource=recommend" %>% read_html()


dl_link <- wp %>% html_node(".addDownloadedBook") %>% html_attr("href") %>% paste0("https://b-ok.asia",.)

dl_link
# [1] "https://b-ok.asia/dl/2883645/919283?dsource=recommend"
# but you want (something like)
# https://swab.zlibcdn.com/dtoken/9cc60846256859eff89eefe3ee3f1d1d



require(RSelenium)


rd <- rsDriver()


rd$client$navigate("https://b-ok.asia/book/2883645/4d513b")

Sys.sleep(0.1)

rd$client$findElement(using = "css", value = ".addDownloadedBook")$clickElement()

view_me <- function(wp){
  wpc <- as.character(wp)
  tf <- tempfile(pattern = "wp_preview_", fileext = ".html")
  writeLines(wpc, tf)
  suppressWarnings(rstudioapi::viewer(tf))
  # unlink(tf)
}

