

# wap_core : for common web automation platform functions

wap_config_local_env <- new.env()

wap_sync_config <- function(){

}

wap_config_store <- function(){
  h <- persistent_object_store(appname = "wap_config")

  # remove this folder on exit (or on unload)
  reg.finalizer(
    asNamespace("fisher"),
    function(e){
      h <- e$persistent_object_store(appname = "wap_config")
      h$destroy()
    },
    onexit = TRUE
  )

  invisible(h)
}



