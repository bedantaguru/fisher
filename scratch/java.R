

# java not present


java_assist <- function(){
  cat("https://java.com/en/download/")

}

terminate_rstudio <- function(){
  if(.Platform$GUI=="RStudio"){
    this_pid <- ps::ps_handle()
    pr <- ps::ps_parent(this_pid)
    if(grepl("rstudio",tolower(ps::ps_name(pr)))){
      # mother is rstudio
      # os sepecific
      ps::ps_kill(pr)
    }
  }
}




