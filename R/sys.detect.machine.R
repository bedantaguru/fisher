

# End (Middle) User Function
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

# End (Middle) User Function
sys_get_os_arch <- function(){
  sys_use_os_specific_method("sys_get_os_arch")
}

sys_get_os_arch_win <- function(){

  ro <- sys_reg_read_win(
    "System\\CurrentControlSet\\Control\\Session Manager\\Environment",
    hive = "HLM")

  ro <- ro$PROCESSOR_ARCHITECTURE

  if(!is.null(ro)){
    if(any(grepl(ro, "86"))){
      bit <- "32"
    }else{
      bit <- "64"
    }
  }else{
    # cmd way (slow)
    co <- sys_cmd("wmic os get osarchitecture")
    bit <- "unknown"
    if(any(grepl("64-bit", tolower(co)))){
      bit <- "64"
    }else{
      if(any(grepl("32-bit", tolower(co)))){
        bit <- "32"
      }
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


sys_all_os_valid_names_this_machine <- function(
  with_arch = FALSE,
  allow_lowbit = FALSE
){
  os_names <- unique(
    sys_os_alt_names(sys_get_os())
  )

  if(with_arch){
    if(allow_lowbit & sys_get_os_arch()=="64"){
      arch_names <- unique(
        c(
          sys_os_arch_alt_names("32"),
          sys_os_arch_alt_names("64")
        )
      )
    }else{
      arch_names <- unique(
        sys_os_arch_alt_names(sys_get_os_arch())
      )
    }


    os_names <- unique(
      apply(
        expand.grid(os_names, arch_names),
        1,
        paste0, collapse = "")
    )
  }
  invisible(os_names)
}

# End (Middle) User Function
sys_valid_os_string <- function(
  str,
  this_machine = FALSE,
  allow_lowbit = FALSE
){
  # post process
  pp <- function(x){
    x <- tolower(x)
    x <- gsub("[^a-z0-9]","",x)
    x
  }

  if(this_machine){
    osns <- sys_all_os_valid_names_this_machine()
    osnsf <- sys_all_os_valid_names_this_machine(
      with_arch = TRUE,
      allow_lowbit = allow_lowbit)
  }else{
    osns <- sys_all_os_valid_names()
    osnsf <- sys_all_os_valid_names(with_arch = TRUE)
  }

  strp <- pp(str)
  osnsp <- pp(osns)
  osnsfp <- pp(osnsf)

  strp %in% c(osnsp, osnsfp)

}
