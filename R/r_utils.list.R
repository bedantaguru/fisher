
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

# # sample
# l1 <- list(
#   a=list(a1=1, a2=F, a3="hi"), b=list(a=1, b=2, c=T), d= 123, e = list(14))
# l2 <- list(
#   a=list(a1=10, a4=T, a5="hello"), c=list(y=1), b= list(d = "hi"), e = 14)

# following logic is used for merging
# 1) if node name matches, match further
# 2) if same name node present first one is used
# 3) if only one common nodes are not list first one is used
# 4) if both common nodes are not list c is used
# 5) all nodes (including uncommon nodes) are added back

merge_list <- function(l1, l2){

  n1 <- names(l1)
  n2 <- names(l2)
  # name in n1 only
  n10 <- setdiff(n1, n2)
  # name in n2 only
  n01 <- setdiff(n2, n1)

  # common name
  cns <- intersect(n1, n2)

  lo <- list()

  if(length(n10)>0){
    lo <- c(lo, l1[n10])
  }

  if(length(n01)>0){
    lo <- c(lo, l2[n01])
  }

  for(cn in cns){
    if(is_named_list(l1[[cn]]) & is_named_list(l2[[cn]])){
      lo[[cn]] <- merge_list(l1[[cn]], l2[[cn]])
    }else{
      if(!is_named_list(l1[[cn]]) & !is_named_list(l2[[cn]])){
        lo[[cn]] <- c(l1[[cn]], l2[[cn]])
      }else{
        lo[[cn]] <- l1[[cn]]
      }

    }
  }

  # ordering

  lo[unique(c(n1, n2))]


}


is_named_list <- function(l){
  rt <- FALSE
  if(is.list(l)){
    if(!is.null(names(l))) rt <- TRUE
  }
  rt
}
