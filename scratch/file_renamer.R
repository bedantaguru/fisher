


all_r <- list.files("R")

snake_to_camel <- function(x){
  sapply(
    strsplit(x, "_"),
    function(nn){
      paste0(nn[1], tools::toTitleCase(nn[-1]), collapse = "")
    }
  )
}
#
# camel_to_snake <- function(x){
#   caps <- gregexpr("[A-Z]", x)
#   lapply(
#     seq_along(x),
#     function(i){
#       td <- data.frame(xi=seq(nchar(x[i])))
#
#     }
#   )
# }



new_name <- sapply(strsplit(all_r,"\\."), function(x) paste0(paste0(snake_to_camel(x)[-length(x)], collapse = "_"),".R"))

file.path("R", all_r)
file.path("R", new_name)

file.rename(file.path("R", all_r), file.path("R", new_name))
