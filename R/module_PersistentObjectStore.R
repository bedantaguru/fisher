
#>> Embedded Module: {PersistentObjectStore}
#>> Author: Indranil Gayen
#>> Version: 0.0.1
#>> Files: c("R/module_PersistentObjectStore.R")

# Disclaimer: https://cran.r-project.org/web/packages/policies.html

# Note: This is like alternatives for persistent_object_store but without
# alternatives
# CRAN packages: {R.cache}, {rappdirs}, {memoise}, {storr}
# base packages: For R version 4.0 or later tools::R_user_dir()

# TODO: options to edit R Startup options
# TODO: history check for last used case (".Rhistory") (on in other module!)
persistent_object_store <- function(
  # pos : persistent object store
  appname = "pos",
  authname = "rudra",
  scope = c("auto","user","project"),
  # leave this NULL for auto setup
  store_path = NULL
){

  scope <- match.arg(scope)

  store_dir <- NULL
  d_mode <- ifelse(isTRUE(is.null(store_dir)[1]), NA, "user_specified")

  # option 1 running in RStudio (use local file) or someone started a
  # persistent_object_store under RStudio (specifically for non interactive
  # cases)
  if(is.null(store_dir) & scope != "user"){
    if(interactive()){
      if(exists(".rs.getProjectDirectory")){
        pd <- get(".rs.getProjectDirectory")()
        if(!is.null(pd)){
          store_dir <- file.path(pd, paste0(".",appname))
          d_mode <- "project"
        }
      }
    }else{
      pd <- getwd()
      store_dir_chk <- file.path(pd, paste0(".",appname))
      if(dir.exists(store_dir_chk)){
        store_dir <- store_dir_chk
        d_mode <- "project"
      }
    }
  }
  # option 2
  # base: works for R>4.0 as per doc
  if(is.null(store_dir) & (scope != "project") &
     # this is kept mainly for testing but can also be utilized by users
     !isTRUE(getOption("prefer_non_base_pkgs"))){
    nst <- asNamespace("tools")
    if(exists("R_user_dir", envir = nst)){
      store_dir <- tools::R_user_dir(appname, which = "config")
      d_mode <- "user"
    }
  }

  # option 3
  # rappdirs
  if(is.null(store_dir) & (scope != "project")){
    # it is a standalone file hence not using is_available
    if(requireNamespace("rappdirs", quietly = TRUE)){
      store_dir <- rappdirs::user_config_dir(appname, authname)
      d_mode <- "user"
    }
  }



  # finalize
  handle <- list()

  handle$ready <- !is.null(store_dir)
  handle$store_path <- store_dir
  handle$file_mode <- d_mode

  # write method
  handle$write <- function(what, value, lst,
                           R_object = FALSE,
                           get_file_path_only = FALSE){

    if(!missing(lst)){
      what <- names(lst)
      value <- unlist(lst)
    }

    if(R_object){

      if(!dir.exists(file.path(store_dir, "robj"))){
        dir.create(file.path(store_dir, "robj"), recursive = TRUE)
      }

      fn <- file.path(store_dir, "robj", what)
    }else{

      if(!dir.exists(file.path(store_dir, "str"))){
        dir.create(file.path(store_dir, "str"), recursive = TRUE)
      }

      fn <- file.path(store_dir, "str", what)
    }

    # just return get_file_path
    if(get_file_path_only) return(fn)

    lapply(
      seq_along(fn),
      function(i){
        tryCatch({
          if(R_object){
            saveRDS(value[i], fn[i])
          }else{
            writeLines(as.character(value[i]), fn[i])
          }
        }, error = function(e) NULL)
      }
    )
    invisible(0)
  }


  # read method Note: if what is singleton then result will be singleton in all
  # other cases it will be list
  handle$read <- function(
    what,
    all = FALSE,
    R_object = FALSE,
    uniform_output = FALSE
  ){

    v <- NULL

    if(all){
      if(R_object){
        afs <- list.files(file.path(store_dir, "robj"))
      }else{
        afs <- list.files(file.path(store_dir, "str"))
      }

      if(length(afs)>0){
        what <- afs
      }else{
        # early exit
        return(invisible(v))
      }
    }

    if(R_object){
      fn <- file.path(store_dir, "robj", what)
    }else{
      fn <- file.path(store_dir, "str", what)
    }


    if(all | length(fn)>1 | uniform_output){
      v <- lapply(
        seq_along(fn),
        function(i){
          # file will be present for sure
          tryCatch({
            if(R_object){
              readRDS(fn[i])
            }else{
              readLines(fn[i], warn = FALSE)
            }
          }, error = function(e) NULL)

        }
      )
      names(v) <- what
    }else{

      # single value in case only single key is requested
      if(file.exists(fn)){
        if(R_object){
          v <- readRDS(fn)
        }else{
          v <- readLines(fn)
        }
      }
    }


    invisible(v)
  }

  # key exists method
  handle$key_exists <- function(
    what,
    R_object = FALSE){

    if(R_object){
      fn <- file.path(store_dir, "robj", what)
    }else{
      fn <- file.path(store_dir, "str", what)
    }

    file.exists(fn)
  }

  # list key method
  handle$list_key <- function(
    R_object = FALSE){

    if(R_object){
      afs <- list.files(file.path(store_dir, "robj"))
    }else{
      afs <- list.files(file.path(store_dir, "str"))
    }

    afs
  }

  # remove method
  handle$remove <- function(
    what,
    R_object = FALSE){

    if(R_object){
      fn <- file.path(store_dir, "robj", what)
    }else{
      fn <- file.path(store_dir, "str", what)
    }

    lapply(
      fn,
      function(fnn){
        if(file.exists(fnn)){
          unlink(fnn, force = TRUE)
        }
      }
    )

    invisible(0)

  }

  # reset method
  handle$reset <- function(){

    unlink(store_dir, recursive = TRUE, force = TRUE)
    dir.create(store_dir, showWarnings = FALSE, recursive = TRUE)

  }

  # destroy method
  handle$destroy <- function(){
    if(dir.exists(store_dir)){
      unlink(store_dir, recursive = TRUE)
    }
  }

  invisible(handle)
}
