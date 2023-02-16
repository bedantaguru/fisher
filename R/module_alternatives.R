

#>> Embedded Module: {alternatives}
#>> Depends on (shallow): {PersistentObjectStore}, {packageAvailabilitySimulate}
#>> Note: Prototype
#>> Author: Indranil Gayen
#>> Version: 0.0.1
#>> Files: c("R/module_alternatives.R")



alternatives <- function(method_name,
                         use,
                         install) {


  if (missing(method_name)) {
    method_name <- alternatives_get_method_name()
  }else{
    method_name <- deparse(substitute(method_name))
  }

  search_info <- tryCatch(
    alternatives_search(method_name),
    error = function(e){
      stop("Alternatives search error..", call. = FALSE)
    }
  )

  if(!isTRUE(nrow(search_info$results)>0)){
    stop("No alternatives found..", call. = FALSE)
  }

  availability_info <- tryCatch(
    alternatives_check_availability(search_info),
    error = function(e){
      stop("Alternatives availability check error..", call. = FALSE)
    }
  )

  if(length(sys.calls()) == 1){
    # direct alternatives call to know alternatives and do operation on it
    # not further processing
    return(alternatives_settings(search_info, availability_info, use, install))

  }

  picked_alt <- alternatives_pick_alt(availability_info)


  do.call(
    what = picked_alt$obj,
    args = as.list(sys.frame(sys.parent())),
    envir = search_info$search_space[[picked_alt$env_name]])

}


alternatives_pick_alt <- function(availability_info){
  cached <- alternatives_env$alternatives_use[[
    availability_info$method_name[1]
  ]]

  if(is.null(cached)){
    ok_alt <- availability_info[availability_info$available,]
    if(nrow(ok_alt)>0){

      ok_alt_first <- ok_alt[1,]
      cat(
        paste0(
          "No alternatives is set to use for method <",
          availability_info$method_name[1],
          ">! Using the first available (which is <",
          ok_alt_first$alt_name[1],">)...\n"
        )
      )

      return(ok_alt_first)

    }else{
      stop(
        paste0(
          "No alternatives is available for method <",
          availability_info$method_name[1],">.."
        ),
        call. = FALSE)
    }
  }else{
    return(cached)
  }
}

#  direct call handler
alternatives_settings <- function(search_info, availability_info, use, install){
  # case 1 > alternatives(method_name) : list all alternatives with
  # availability
  if(missing(use) & missing(install)){
    inf <- availability_info[c("env_name","alt_name","available")]
    colnames(inf) <- c("Environment","Alternatives","Available")
    return(inf)
  }

  # case 2 > want to use an alternative (for this session) unless changed
  if(!missing(use)){
    alternatives_use(availability_info, use)
    return(invisible(0))
  }

  # case 3 > want to install required packages for an alternative
  if(missing(use) & !missing(install)){
    alternatives_install(search_info, availability_info, install)
    return(invisible(0))
  }

  return(invisible(-1))
}

alternatives_use <- function(availability_info, use){
  ok_to_use <- availability_info[availability_info$available,]
  to_use <- intersect(ok_to_use$alt_name, use)
  # multiple name resolution not handled yet
  if(length(to_use)>0){
    to_use <- to_use[1]
    use_d <- ok_to_use[ok_to_use$alt_name==to_use,]
    use_d <- use_d[1,]
    alternatives_env$alternatives_use[[
      availability_info$method_name[1]
    ]] <- use_d
    cat(paste0(
      "Alternatives: Method <",availability_info$method_name[1],
      "> will use alternative <", use_d$alt_name[1],">\n"
    ))
  }else{
    warning(
      paste0(
        "Alternatives: Method <",availability_info$method_name[1],
        "> failed to use alternative <", use[1],">\n"
      ),
      call. = FALSE
    )
  }
  invisible(0)
}

alternatives_install <- function(search_info, availability_info, install){
  com_inst <- intersect(install, availability_info$alt_name)
  if(length(com_inst)>0){
    to_inst <- availability_info[availability_info$alt_name %in% com_inst,]
    pkgs_to_install <- unlist(lapply(to_inst$check_info, `[[`, "packages"))
    ut <- asNamespace("utils")
    ut$install.packages(pkgs_to_install)
    alternatives_check_availability(search_info, fresh = TRUE)
  }
}


alternatives_get_method_name <- function() {
  # called directly alternatives()
  if (length(sys.calls()) <= 2)
    return(NULL)
  # scl: sys call list
  scl <- as.list(sys.call(sys.parent(2L)))
  if (length(scl) > 0) {
    as.character(scl[[1]])
  } else{
    stop("Unable to determine method_name for alternatives.", call. = FALSE)
  }
}

alternatives_dispatch_style_naming <- function(method_name) {
  list(
    # name of the alternative
    alt_name = paste0(method_name, "_alt_"),
    # dependency (on another alternative)
    alt_dep_name = paste0(method_name, "_altDep_"),
    # meta info including availability check (whether it can be used)
    alt_meta_name = paste0(method_name, "_altMeta_")
  )
}

alternatives_search <- function(method_name) {
  # we can get package name from : methods::getPackageName(create = FALSE)
  # .packageName may not be reliable
  # Or simply we may not need the package name (yet)
  search_space <- list()
  # 1. current package
  search_space$native <- parent.env(environment())
  # 2. Global Environment
  search_space$global <- globalenv()
  # 3. attached pkgs if any
  if(!is.null(alternatives_env$extra_pkgs)){
    for(pn in alternatives_env$extra_pkgs){
      search_space[[pn]] <- asNamespace(pn)
    }
  }

  search_results <- lapply(
    search_space,
    ls, all.names = TRUE
  )

  name_styles <- alternatives_dispatch_style_naming(method_name)

  alt_name_hits <- lapply(search_results, grep, pattern = name_styles$alt_name)

  ssl <- lapply(
    names(search_space),
    function(ss){
      dout <- NULL
      alt_name_hits <- grep(
        pattern = name_styles$alt_name,
        x = search_results[[ss]]
      )
      if(length(alt_name_hits)>0){
        dout<-data.frame(
          env_name = ss,
          obj = search_results[[ss]][alt_name_hits]
        )
        dout$alt_name <- gsub(name_styles$alt_name, "", dout$obj)
        dout$dep_name <- paste0(name_styles$alt_dep_name, dout$alt_name)
        dout$meta_name <- paste0(name_styles$alt_meta_name, dout$alt_name)
        # as of now only dep and meta can be picked from same environement
        # i.e filter_alt and filter_altDep, filter_altMeta should belong to
        # same environment
        dout$dep_present <- unlist(
          lapply(
            dout$dep_name,
            function(dn){
              any(
                grepl(
                  pattern = dn,
                  x = search_results[[ss]]
                )
              )
            }
          )
        )

        dout$meta_present <- unlist(
          lapply(
            dout$meta_name,
            function(dn){
              any(
                grepl(
                  pattern = dn,
                  x = search_results[[ss]]
                )
              )
            }
          )
        )
      }
      dout
    }
  )

  ssd <- do.call(rbind, ssl)
  ssd$method_name <- method_name

  list(
    search_space = search_space,
    results = ssd
  )

}

alternatives_check_availability <- function(search_info, fresh = FALSE){
  srd <- search_info$results

  # if fresh is TRUE it will compute availability again
  if(!fresh){
    if(!is.null(
      alternatives_env$availability_info_cache[[
        srd$method_name[1]
      ]]
    )){
      # early return from cache
      return(alternatives_env$availability_info_cache[[
        srd$method_name[1]
      ]])
    }
  }


  # rowwise meta fn call
  srd$check_info <- lapply(
    1:nrow(srd),
    function(i){

      if(srd$meta_present[i]){
        se <- search_info$search_space[[srd$env_name[i]]]
        mfn <- srd$meta_name[i]
        meta_fn <- get(mfn, envir = se)
        mi <- tryCatch(
          meta_fn(),
          error = function(e){
            warning(paste0("Alternatives: Meta function for alternative <",srd$alt_name[i],"> of method <",srd$method_name[i],"> failed to run.."))
            list(available = FALSE)
          }
        )
      }else{
        # if no meta present then it should be available by default
        mi <- list(available = TRUE)
      }
      # in case meta run failed
      if(is.null(mi$available)){
        # dependent package check
        mi$package_installed <- alternatives_check_package(mi$packages)
        # system check (apart from package external dependency or inside
        # package validation of usability of specific functions etc.)
        mi$system_check <- unlist(
          lapply(mi$system, function(f){
            chk <- tryCatch(
              f(),
              error = function(e) FALSE
            )
            if(isTRUE(!is.logical(chk))){
              chk <- FALSE
            }
            chk
          })
        )
        mi$system_ok <- all(mi$system_check)
        mi$available <- mi$package_installed & mi$system_ok
      }
      mi
    }
  )

  srd$available <- unlist(lapply(srd$check_info, `[[`, "available"))


  alternatives_availability_info_cache(srd)

  srd

}

alternatives_availability_info_cache <- function(availability_info){
  alternatives_env$availability_info_cache[[
    availability_info$method_name[1]
  ]] <- availability_info
}

# kept for compatibility and shallow dependency on {packageAvailabilitySimulate}
alternatives_check_package <- function(pkgs){
  this_pkg <- parent.env(environment())
  if(exists("is_available", envir = this_pkg)){
    # this means current package has {packageAvailabilitySimulate}
    # in this case follow {packageAvailabilitySimulate}
    this_pkg$is_available(pkgs)
  }else{
    all(
      sapply(
        pkgname,
        function(x){
          dir.exists(system.file(package = x))
        }
      )
    )
  }
}

# environment kept for storing setting and other meta related to "alternatives"
# mainly for methods which opt for registration/attaching on the fly
alternatives_env <- new.env()

alternatives_attach <- function(pkg_name) {
  if(!isNamespaceLoaded(pkg_name)){
    # this is to avoid extra searches and ensuring that the pkg_name is correct
    stop(paste0("Kindly load the package : {", pkg_name,"}"), call. = FALSE)
  }
  if(is.null(alternatives_env$extra_pkgs)){
    alternatives_env$extra_pkgs <- pkg_name
  }else{
    alternatives_env$extra_pkgs <- c(alternatives_env$extra_pkgs, pkg_name)
  }
  alternatives_env$extra_pkgs <- unique(alternatives_env$extra_pkgs)
  invisible(0)
}
