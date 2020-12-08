
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


fun <- function(x, y){
  if("test" %in% names(x)){
    y[1] <- x["test"]
  }
  z <- x+y
  if(z[2] == 3){
    u <- this_loc(Sys.time())
    z[2] <- u
  }
  return(z)
}




