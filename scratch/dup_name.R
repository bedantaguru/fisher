check_duplicate_names <- function(dir){
  files <- list.files(dir)
  for (file in file.path(dir, files)){
    duplicate_test_env <- new.env()
    sys.source(file, envir = duplicate_test_env)
    attach(duplicate_test_env)
  }
  for (i in seq_along(files)){
    detach(duplicate_test_env)
  }
}


# check_duplicate_names("path-to-package/R")
