
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
