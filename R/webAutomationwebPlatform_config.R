
wap_config_local_env <- new.env()

wap_sync_config <- function(){
 #@Dev
}

wap_config_store <- function(){

  h <- persistent_object_store(appname = "wap_config")

  # make first caller initiator
  prior_pid <- h$read("wap_initiator_pid")
  if(!is.null(prior_pid)){
    if(!sys_is_pid_active(prior_pid)){
      # if somehow old pid which is not active is present
      prior_pid <- NULL
    }
  }

  if(is.null(prior_pid)){
    h$write("wap_initiator_pid", Sys.getpid())
  }


  # remove this folder on exit (or on unload)
  reg.finalizer(
    asNamespace("fisher"),
    function(e){
      h <- e$persistent_object_store(appname = "wap_config")
      prior_pid <- h$read("wap_initiator_pid")
      # destroy if the calling process is initiator
      # @Dev cleanup option is not so good
      cat("\nBye bye\n")
      if(isTRUE(as.integer(prior_pid)==Sys.getpid())) h$destroy()
    },
    onexit = TRUE
  )

  invisible(h)
}



