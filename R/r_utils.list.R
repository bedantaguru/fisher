
# # sample
# l1 <- list(a=list(a1=1, a2=F, a3="hi"), b=list(a=1, b=2, c=T))
# l2 <- list(a=list(a1=1, a2=T, a3="hello"), b=list(a=2, b=1, c=F))

# diff objects will be retained in list 1 (l1)
diff_list <- function(l1, l2){
  if(identical(l1, l2)){
    NULL
  }else{

    if(!is.list(l1)){
      lo <- l1
    }else{

      n1 <- names(l1)
      n2 <- names(l2)
      # name in n1 only
      n10 <- setdiff(n1, n2)
      # # name in n2 only
      # n01 <- setdiff(n2, n1)
      # (above is not required)

      lo <- list()

      if(length(n10)>0){
        lo <- c(lo, l1[n10])
      }

      # common name
      cns <- intersect(n1, n2)

      for(cn in cns){
        if(!identical(l1[[cn]], l2[[cn]])){
          lo[[cn]] <- diff_list(l1[[cn]], l2[[cn]])
        }
      }

    }

    lo
  }
}

