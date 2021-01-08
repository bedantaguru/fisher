


wp <- xml2::read_html(
  "https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver/"
  )
rvest::html_attr(rvest::html_nodes(wp,"a"),"href")

rvest::html_node(rvest::html_node(wp,"#downloads"),".module")

rvest::html_nodes(rvest::html_node(rvest::html_node(wp,"#downloads"),".module"),"a")


#https://msedgewebdriverstorage.z22.web.core.windows.net/?prefix=84.0.512.0/
