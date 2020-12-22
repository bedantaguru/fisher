



f0 <-function(){
  f1<- function(){
    f2<- function(){
      f3<- function(){
        f4 <- purrr::as_mapper(~{
          f5 <- function(){
            suppressWarnings({
              u <- unlist(lapply(seq(1, sys.nframe(), by = 1), function(n) isNamespace(environment(sys.function(n)))))
              if(any(u)){
                nsn <- as.character(getNamespaceName(environment(sys.function(min(which(u))))))
              }else{
                nsn <- NULL
              }
              nsn
            })
          }
          f5()
        })
        f4()
      }
      f3()
    }
    f2()
  }
  f1()
}

