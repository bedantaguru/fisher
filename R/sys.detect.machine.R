

# it determines R Architecture
# it maybe possible that one is running 32 bit R in 64bit machine
sys_get_R_arch <- function(){
  indx <- .Machine$sizeof.pointer/4
  if(indx %in% c(1,2)){
    c("32", "64")[indx]
  }else{
    "unknown"
  }
}

sys_get_os_arch <- function(){
  sys_use_os_specific_method("sys_get_os_arch")
}

sys_get_os_arch_win <- function(){
  co <- sys_cmd("wmic os get osarchitecture")
  bit <- "unknown"
  if(any(grepl("64-bit", tolower(co)))){
    bit <- "64"
  }else{
    if(any(grepl("32-bit", tolower(co)))){
      bit <- "32"
    }
  }
  bit
}


sys_get_os_arch_unix <- function(){
  co <- sys_cmd("uname -a")
  bit <- "unknown"
  if(any(grepl("x86_64", tolower(co)))){
    bit <- "64"
  }else{
    if(any(grepl("i686", tolower(co)))){
      bit <- "32"
    }
  }
  bit
}
