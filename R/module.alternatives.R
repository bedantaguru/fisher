
#>> Embedded Module: {alternatives}
#>> Depends on (shallow): {PersistentObjectStore}
#>> Note: Prototype
#>> Author: Indranil Gayen
#>> Version: 0.0.1
#>> Files: c("R/module.alternatives.R")



alternatives <- function(
  method_name,
  implement,
  install
){

  if(missing(method_name)){
    method_name <- get_method_name_for_alternatives()
  }
  method_name
}

get_method_name_for_alternatives <- function(){
  # called directly alternatives()
  if(length(sys.calls())<=2) return(NULL)
  # scl: sys call list
  scl <- as.list(sys.call(sys.parent(2L)))
  if(length(scl)>0){
    as.character(scl[[1]])
  }else{
    stop("Unable to determine method_name for alternatives.", call. = FALSE)
  }
}

alternatives_dispatch_style_naming <- function(
  method_name
){
  list(
    alt_name <- paste0(method_name,"_alt_"),
    alt_dep_name <- paste0(method_name,"_altDep_"),
    alt_meta_name <- paste0(method_name,"_altMeta_")
  )
}

search_alternatives <- function(
  method_name
){

}

# environment kept for storing "alternatives" for methods which opt for
# registration on the fly.
alternatives_env <- new.env()

register_alternatives <- function(){

}
