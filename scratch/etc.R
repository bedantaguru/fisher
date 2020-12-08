

fn0 <- function(x){
  if(!missing(x)){
    x <- match.arg(x, choices = c("test","indra","nakshatra"))
    return(x)
  }
  return(0)
}



# locking environments
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockEnvironment(e)
get("x", envir = e)
assign("x", 2, envir = e)
try(assign("y", 2, envir = e)) # error

# locking bindings
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockBinding("x", e)
try(assign("x", 2, envir = e)) # error
unlockBinding("x", e)
assign("x", 2, envir = e)
get("x", envir = e)

# active bindings
f <- local( {
  x <- 1
  function(v) {
    if (missing(v))
      cat("get\n")
    else {
      cat("set\n")
      x <<- v
    }
    x
  }
})
makeActiveBinding("fred", f, .GlobalEnv)
bindingIsActive("fred", .GlobalEnv)
fred
fred <- 2
fred




#########

en <- new.env()

assign("fn", function(x) x, envir = en)

#lockEnvironment(en, bindings = T)

# en2 <- new.env()
#
# assign("fn2", function(x) x^2, envir = en2)

attach(en)
lockEnvironment(as.environment(2), bindings = T)

assign("fn", function(x) x^2,pos = 2)
unlockBinding("fn",as.environment(2))
assign("fn", function(x) x^2,pos = 2)
lockBinding("fn",as.environment(2))

attach(en2)





fn <- function(x){
  "test"

  "test"

  if(TRUE){
    "test"
  }else{
    if(TRUE){
      "test"

      "test"

      if(FALSE){

        "test"

      }
    }
  }

  "test"

  x
}





locate_section_rec <- function(fbody, search_str, pre = integer(0)) {
  tloc <- list()
  rlocs <- grepl(search_str, as.character(fbody))
  if(any(rlocs)){
    if(length(fbody)>1){
      tl0 <- seq_along(rlocs)[rlocs]
      tl1 <- lapply(tl0, function(x) c(pre, x))
      tl2 <- lapply(
        seq_along(tl0),
        function(i){
          locate_section_rec(fbody[[tl0[i]]], search_str, pre = tl1[[i]])
        })
      tl3 <- Reduce(c, tl2)
      tloc <- tl3
    }else{
      tloc <- list(c(pre, which(rlocs)))
    }
  }
  tloc
}


