

is_url_exists <- function(url = "https://www.google.com/"){
  e <- tryCatch({
    suppressWarnings(readLines(url, n = 1, warn = FALSE))
    TRUE
  },
  error = function(e){
    FALSE
  })
  e
}

is_internet_active <- function(){
  c1 <- FALSE
  if(is_available("curl")){
    c1 <- try(curl::has_internet(), silent = TRUE)
  }

  if(!isTRUE(c1)){
    is_url_exists("https://www.google.com/")
  }else{
    c1
  }
}


# copied / obtained from from {tidycells}
# latest version is present in tidycells_nightly
# migrate to {filetools} or similar package

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


