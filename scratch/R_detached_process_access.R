

# install.packages(c("Rserve", "RSclient"))


for_pid <- function(pid, port = 6311L, pid_as_port = FALSE){
  if(pid_as_port) port <- as.integer(Sys.getpid())
  if(Sys.getpid()==pid) Rserve::run.Rserve(port = port)
}




run <- function(x, port = 6311L){
  con <- RSclient::RS.connect(port = port)
  on.exit(RSclient::RS.close(con))
  RSclient::RS.eval(con, x)
}

kill <- function(port = 6311L){
  con <- RSclient::RS.connect(port = port)
  on.exit(RSclient::RS.close(con))
  RSclient::RS.eval(con, q(save = "no"))
}
