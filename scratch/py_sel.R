# https://www.anaconda.com/products/individual/get-started
# sel and docker env



is_pkg_present <- function(x, loadable = F){
  if(!loadable){
    chk <- tryCatch(find.package(x), error = function(e) e)
    is.character(chk)
  }else{
    isTRUE(requireNamespace(x, quietly = T))
  }
}

is_pkg_present("reticulate")

#
# reticulate::py_available(initialize = T)
#
# reticulate::conda_list()

reticulate::conda_create("r-fisher", packages = c("python", "numpy"))


library(reticulate)
detach("package:reticulate", unload = TRUE)

reticulate::use_condaenv("r-fisher", required = TRUE)

reticulate::py_config()


# clean miniconda
# unlink(reticulate::miniconda_path(),recursive = T)
#
# reticulate::use_condaenv("r-fisher", required = T)
reticulate::conda_install("r-fisher", packages = c("selenium"))
reticulate::conda_install("r-fisher", packages = c("docker","pypiwin32"), pip = TRUE)
#reticulate::py_install(c("docker","pypiwin32"), envname = "r-fisher", pip = TRUE)

