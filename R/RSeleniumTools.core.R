

############################ may need to migrate to different place ############

# @Dev
# copied /obtained from from tidycells
# latest version is present in tidycells_nightly
# migrate to filetools or similar package

is_txt_file <- function(fn) {
  # it's a directory
  if(dir.exists(fn)) return(FALSE)
  f <- file(fn, "rb", raw = TRUE)
  on.exit(close(f))
  bytes <- readBin(f, "int", 1000, size = 1, signed = FALSE)
  chk <- (max(bytes) <= 128)
  if(!chk){
    embnul_chk <-
      tryCatch(
        readLines(f, n = 100, warn = TRUE),
        warning = function(e) e)
    chk <- TRUE
    if (inherits(embnul_chk, "warning")) {
      if (any(grepl("embedded nul", embnul_chk$message))) {
        chk <- FALSE
      }
    }
  }
  return(chk)
}






# rst: {RSelenium} Tools
# alt names
# wba_server
# web_browser_automation_server
# web_automation_server

# should be used jointly with web_control_client



