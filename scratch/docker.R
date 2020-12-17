
#  move docker images in WSL 2 mode (windows 10) to different location

# https://www.sitepoint.com/wsl2/
# https://stackoverflow.com/questions/62441307/how-can-i-change-the-location-of-docker-images-when-using-wsl2-with-windows-10-h



install.packages("stevedore")

stevedore::docker_available()

# download docker
# https://www.docker.com/get-started

# proton VPN docker
# https://github.com/tprasadtp/protonvpn-docker
# https://hub.docker.com/r/walt3rl/proton-privoxy
# https://github.com/krey/protonvpn-docker


splashr::install_splash()

splashr::start_splash()


pg <- render_html(url = "https://analytics.usa.gov/")


require(rvest)


splash_local %>%
  splash_go("https://en.wikipedia.org/wiki/Main_Page") %>%
  splash_focus("#searchInput") %>%
  splash_send_text("maine") %>%
  splash_send_keys("<Return>") %>%
  splash_wait() %>%
  splash_png() -> wiki_png




# Hi,
# Just managed to overcome connectivity issues on my Windows 10 Ubuntu (WSL 2).
#
# Altering the following setting:
#   Comodo -> Advanced Settings -> Firewall -> Global Rules -> Block IP in Any To MAC Any Where Protocol is Any
# by excluding the vEthernet (WSL) from this rule (please see attached screenshot),
# allowed sudo apt update to execute successfully.

#
# ht click at the server box, select Add, enter any of following new host listed below, and click OK.
#
# http://cdn.download.comodo.com/
#   http://downloads.comodo.com/
#   http://download-cn.comodo.com/
#
