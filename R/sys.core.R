


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


# safe system call
sys_cmd <- function(cmd, ...){
  tryCatch(
    system(cmd, intern = TRUE, ...),
    error = function(e){
      ""
    }
  )
}
