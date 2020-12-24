


base_url <- "https://api.github.com/repos/operasoftware/operachromiumdriver/releases"

jorig <- jsonlite::fromJSON(base_url)

al <- sapply(jorig[["assets"]], length)

jorig <- jorig[al>0,]

tj <- tempfile(fileext = ".json")

writeLines(jsonlite::toJSON(jorig), tj)


binman::predl_github_assets(base_url, platform = "win32", history = 1L, appname = "test")
binman::predl_github_assets(tj, platform = "win32", history = 1L, appname = "test")







get_os_arch <- function(pre=""){
  # check help: help(.Machine)
  paste0(pre, c("32", "64")[.Machine$sizeof.pointer/4])
}


tamper_yaml<- function(yaml_file){

  # yaml dep
  yml <- asNamespace("yaml")$yaml.load_file(yaml_file)
  platvec <- c("predlfunction",
               names(yml[["predlfunction"]]),
               "platform")
  yml[[platvec]] <- switch(
    Sys.info()["sysname"],
    Linux = grep(get_os_arch("linux"), yml[[platvec]], value = TRUE),
    Windows = grep(get_os_arch("win"), yml[[platvec]], value = TRUE),
    Darwin = grep(get_os_arch("mac"), yml[[platvec]], value = TRUE),
    stop("Unknown OS"))
  tempyml <- tempfile(fileext = ".yml")

  asNamespace("yaml")$write_yaml(yml, tempyml)
  tempyml
}


patch::patch_function(
  binman::predl_github_assets,
  "data.frame\\(file = file",
  if(length(file)*length(url)*length(plat) == 0) return(data.frame()),
  modification_placement = "before",
  move_up_to = 2, auto_assign_patched_function = T)



binman::process_yaml(tamper_yaml("scratch/opera.yml"))




od <- normalizePath(
  file.path(
    binman::app_dir("operadriver"),
    names(binman::list_versions("operadriver")),
    max(binman::list_versions("operadriver")[[1]])
  )
)

oexe <- normalizePath(
  list.files(list.dirs(od)[2], pattern = "operadriver", full.names = TRUE)
)



#wdman::selenium(chromever = NULL, geckover = NULL, iedrver = NULL, phantomver = NULL, retcommand = T)

se <- wdman::selenium(
  check = F,geckover = NULL, phantomver = NULL, iedrver = NULL,
  jvmargs = list(
    opera =
      paste0(
        "-Dwebdriver.opera.driver=",
        paste0('"',oexe,'"')
      )
  ))


rd <- RSelenium::remoteDriver(browserName = "opera", port = 4567L)
rd$open()

rd <- RSelenium::remoteDriver(browserName = "chrome", port = 4567L)

se$process$kill_tree()
# identify browser
# install guidance (in case not installed)
# identify driver
# download and maintain (os specific)


