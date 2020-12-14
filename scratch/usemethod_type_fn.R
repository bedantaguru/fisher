



# this wont work for sure
alt <- function(fn){
  UseMethod(fn)
}

# this works but what's the point
alt <- UseMethod

# very odd implementation
alt <- function(fn){
  fn_args<- formals(sys.function(sys.parent()))
  fn_env <- sys.frame(sys.parent())

  # here is the modification logic
  arg_class <- class(fn_env[[names(fn_args)[1]]])[1]
  mod_fn <- paste0(fn,"_",arg_class)
  if(exists(mod_fn)){
    do.call(mod_fn, args = as.list(fn_env))
  }else{
    stop(paste0("No method '",fn,"' found for class: ", arg_class), call. = FALSE)
  }
}



fmother <- function(x, y){
  alt("fmother")
}

fmother.plus <- function(x, y){
  x+y
}


fmother.mult <- function(x, y){
  x*y
}


fmother_plus <- function(x, y){
  cat("working\n")
  x+y
}


fmother_mult <- function(x, y){
  cat("working\n")
  x*y
}




x1 <- 5
class(x1) <- c("plus",class(x1))

x2 <- 5
class(x2) <- c("mult",class(x2))



fmother(x1, 5)
fmother(x2, 5)
