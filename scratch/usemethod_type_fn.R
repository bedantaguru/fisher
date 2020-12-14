
# protype I

# https://stackoverflow.com/questions/65287106/how-to-reproduce-baseusemethod-in-r-code-only

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


fmother_plus <- function(x, y){
  x+y
}


fmother_mult <- function(x, y){
  x*y
}




x1 <- 5
class(x1) <- c("plus",class(x1))

x2 <- 5
class(x2) <- c("mult",class(x2))




# bench

fmotherB <- function(x, y){
  UseMethod("fmotherB")
}


fmotherB.plus <- function(x, y){
  x+y
}


fmotherB.mult <- function(x, y){
  x*y
}



# fmother(x1, 5)
# fmother(x2, 5)
# fmotherB(x1, 5)
# fmotherB(x2, 5)



microbenchmark::microbenchmark(
  alt = {
    fmother(x1, 5)
    fmother(x2, 5)
  },
  base = {
    fmotherB(x1, 5)
    fmotherB(x2, 5)
  }
)
#> Unit: microseconds
#>  expr  min   lq    mean median    uq     max neval
#>   alt 26.0 26.5 213.059   27.0 27.85 17023.5   100
#>  base  3.8  4.1  27.819    4.3  4.50  1642.7   100


bench::mark(
  alt = {
    fmother(x1, 5)
    fmother(x2, 5)
  },
  base = {
    fmotherB(x1, 5)
    fmotherB(x2, 5)
  }
)
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 alt          27.7us   29.9us    32401.        0B     35.7
#> 2 base          4.2us    4.6us   207984.        0B     41.6
