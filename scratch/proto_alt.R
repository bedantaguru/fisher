
# calling alternatives()
# 1) should give registered alternatives
# 2) can also give "dispatch style" if auto register is on (but need to think
# how it behaves with pkg loading unloading)

# two ways to access alternatives
# its like changing back-ends

#####################################
# 1
# help(Methods_for_S3) dispatch style
# use predefined style for alternatives

# either this
filter <- function(...){
  alternatives("filter")
}

# or this : this is actually non-portable in a sense that if you re-assign
# xyz <- filter alternatives will be detected for xyz instead of filter.
# this is inferred from sys.calls. Not a recommended way. Kept for
# experimentation
filter <- function(...){
  alternatives()
}

# style method_name then _alt_ then alternative name / implementation
filter_alt_stats <- function(...){
  stats::filter(...)
}


filter_alt_dplyr <- function(...){
  dplyr::filter(...)
}

# dependency of an alternative method may be mentioned in following way
# method_name then _altMeta_ then alternative name
# 1) it should have no argument
# 2) it should check and report the requirements (possibly other meta info)
#   this should return a list with requirements
# 3) they should belong to same environment
# # # #
# the return list should be named as (all optional)
#
# - packages : the R packages that are required for this alternative
# - system : a names list with (node of which should be functions to determine
# if the dependency is installed or not. If not installed a possible hint may be
# given how to install the same)

# - these two are alternatives specific
# - capabilities or cap : performing same work, similar work, diverse scope etc.
# - meta : Who wrote it, other meta info
filter_altMeta_dplyr <- function(){
  list(
    packages = c("dplyr"),
    system = list(
      xyz = function() TRUE,
      abc = function() "Please do this to install <abc>",
      rst = function() FALSE
    )
  )
}

# for meta
# filter_altMeta

# complex call test
g <- function(){
  r <- list()
  r$jh <- function(){
    gh <- function(){
      filter()
    }
    gh()
  }
  ff <- function(fc){
    fc()
  }
  ff(r$jh)
}

g()

#####################################
# 2
# register manually


















f <- function(e) dir.create("Clean", showWarnings = F)
g <- function(x){ e <- asNamespace("fisher"); reg.finalizer(e, f, onexit = T) }
g()
invisible(gc()) # trigger cleanup


