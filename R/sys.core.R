
# End (Middle) User Function
# in 3 known OS (windows, osx, linux)
sys_get_os <- function(){
  os <- .Platform$OS.type
  os <- ifelse(is.null(os),"unknown",os[1])
  if(os=="unix"){
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  os
}

sys_os_alt_names <- function(osn){
  switch (
    osn,
    `windows` = c("windows","win"),
    `osx` = c("osx","mac","macos","darwin", "unix"),
    `linux` = c("linux","unix"),
    osn
  )
}

sys_os_arch_alt_names <- function(arn){
  switch (
    arn,
    `64` = c("64","x64","x86_64","amd64"),
    `32` = c("32","x86","i686","i386"),
    arn
  )
}


sys_all_os_valid_names <- function(with_arch = FALSE){

  os_names <- unique(
    unlist(
      lapply(c("windows","osx","linux"),
             sys_os_alt_names)
    )
  )

  if(with_arch){
    arch_names <- unique(
      unlist(
        lapply(c("32","64"),
               sys_os_arch_alt_names)
      )
    )

    os_names <- unique(apply(expand.grid(os_names, arch_names), 1, paste0, collapse = ""))
  }
  invisible(os_names)
}


# it's like base::UseMethod but for specific OS
# Proto: https://stackoverflow.com/questions/65287106
sys_use_os_specific_method <- function(fname){
  os <- sys_get_os()
  call_f_name <- paste0(fname, "_", os)
  found <- FALSE
  # call direct method if present
  # if not search of alternative names
  if(!exists(call_f_name)){
    alt_names <- sys_os_alt_names(os)
    alt_names <- setdiff(alt_names, os)
    if(length(alt_names)>0){
      call_f_names <- paste0(fname, "_", alt_names)
      call_f_names_chk <-sapply(call_f_names, exists)
      if(any(call_f_names_chk)){
        call_f_name <- call_f_names[which(call_f_names_chk)[1]]
        # definition / method found using alternative string
        found <- TRUE
      }
    }
  }else{
    # definition / method found
    found <- TRUE
  }

  if(found){
    do.call(call_f_name, args = as.list(sys.frame(sys.parent())))
  }else{
    stop(paste0("No method named '", fname,"' found for OS ", os),call. = FALSE)
  }
}


sys_is_pid_active <- function(pid){
  if(is_available("ps")){
    pid %in% ps::ps_pids()
  }else{
    stop("{ps} is required for this", call. = FALSE)
    # alternative can be defined here
    # which is not implemented yet
  }
}

# safe system call
sys_cmd <- function(cmd, ...){
  tryCatch(
    system(cmd, intern = TRUE, ...),
    error = function(e){
      ""
    }
  )
}
