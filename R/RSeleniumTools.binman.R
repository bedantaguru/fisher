


rst_binman_dir <- function(){
  dirname(binman::app_dir("tst", check = FALSE))
}

rst_binman_apps <- function(){
  bn <- basename(
    list.dirs(rst_binman_dir(), recursive = FALSE)
  )
  gsub("binman_","",bn)
}


rst_binman_app_details <- function(app_name){
  # this is not required :
  # lvs <- binman::list_versions(app_name)
  ad <- binman::app_dir(app_name)

  plat_details <- function(plat){
    vers <- list.files(file.path(ad, plat))
    do.call(rbind, lapply(vers, ver_details, plat = plat))
  }

  ver_details <- function(ver, plat){
    fls <- list.files(
      file.path(ad, plat, ver),
      full.names = TRUE,
      recursive = TRUE
    )
    fl_zips <- list.files(
      file.path(ad, plat, ver),
      pattern =  "\\.zip$",
      full.names = TRUE
    )

    dout <- data.frame(app = app_name, platform = plat, version = ver)

    dout$zip_present <- (length(fl_zips) > 0)

    dout$bin_present <- FALSE
    dout$bin_file <- NA
    dout$multi_bin <- FALSE

    fl_non_zips <- setdiff(fls, fl_zips)

    if(length(fl_non_zips)>0){
      fl_non_zips_chk <- sapply(fl_non_zips, is_txt_file)

      fl_non_zips_bin <- fl_non_zips[!fl_non_zips_chk]

      if(length(fl_non_zips_bin)>0){
        dout$bin_present <- TRUE
        dout$bin_file <- normalizePath(fl_non_zips_bin[1], winslash = "/")
        dout$multi_bin <- length(fl_non_zips_bin)>1
      }
    }

    dout
  }

  plats <- list.files(ad)

  do.call(rbind, lapply(plats, plat_details))

}


rst_binman_all_apps_details <- function(){
  bna <-rst_binman_apps()

  do.call(rbind, lapply(bna, rst_binman_app_details))
}

rst_binman_apps_diag <- function(){
  # binman app details : bad
  bad <- rst_binman_all_apps_details()


}





