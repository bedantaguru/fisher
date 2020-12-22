

############################ may need to migrate to different place ############

# @Dev

# copied from tidycells

is_txt_file <- function(fn) {
  f <- file(fn, "rb", raw = TRUE)
  bytes <- readBin(f, "int", 1000, size = 1, signed = FALSE)
  close(f)
  return(max(bytes) <= 128)
}


####################################


rst_binman_dir <- function(){
  dirname(binman::app_dir("tst", check = FALSE))
}

rst_binman_apps <- function(){
  bn <- basename(
    list.dirs(rst_binman_dir(), recursive = FALSE)
  )
  gsub("binman_","",bn)
}


rst_binman_app_details <- function(app_name){
  lvs <- binman::list_versions(app_name)

}

rst_binman_apps_diag <- function(){
  bna <-rst_binman_apps()
  chk_a_app <- function(an){
    binman::list_versions(an)

  }
  state <- lapply(
    bna,
    function(an){

      !isTRUE(
        tryCatch(
          chk_a_app(an),
          error=function(e){
            TRUE
          }
        )
      )
    }
  )
}

# rst: {RSelenium} Tools
# alt names
# wba_server
# web_browser_automation_server
# web_automation_server

# should be used jointly with web_control_client



