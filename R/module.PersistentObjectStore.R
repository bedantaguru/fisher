
#>> Embedded Module: {PersistentObjectStore}
#>> Author: Indranil Gayen
#>> Version: 0.0.1
#>> Files: c("R/module.PersistentObjectStore.R")

# Disclaimer: https://cran.r-project.org/web/packages/policies.html

# Note: This is like alternatives for persistent_object_store but without
# alternatives
# CRAN packages: {R.cache}, {rappdirs}, {memoise}, {storr}
# base packages: For R version 4.0 or later tools::R_user_dir()

# TODO: options to edit R Startup options
# TODO: history check for last used case (".Rhistory") (on in other module!)
persistent_object_store <- function(
  appname,
  authname = "rudra",
  scope = c("auto","user","project")
){

  scope <- match.arg(scope)

  store_dir <- NULL
  d_mode <- NA

  # option 1
  # running in RStudio (use local file)
  if(scope != "user"){
    if(exists(".rs.getProjectDirectory")){
      pd <- get(".rs.getProjectDirectory")()
      if(!is.null(pd)){
        store_dir <- file.path(pd, paste0(".",appname))
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

  handle$write <- function(what, value){
    if(!dir.exists(store_dir)){
      dir.create(store_dir, recursive = TRUE)
    }
    fn <- file.path(store_dir, what)
    tryCatch(
      writeLines(value, fn),
      error = function(e) NULL)
    invisible(0)
  }

  handle$read <- function(what){

    fn <- file.path(store_dir, what)

    v <- NULL

    if(file.exists(fn)){
      v <- readLines(fn)
    }

    invisible(v)
  }

  handle$destroy <- function(){
    if(dir.exists(store_dir)){
      unlink(store_dir, recursive = TRUE)
    }
  }

  handle$open_location <- function(){
    if(dir.exists(store_dir)){
      ifelse(interactive(), isFALSE(utils::browseURL(store_dir)), TRUE)
    }
  }

  invisible(handle)
}
