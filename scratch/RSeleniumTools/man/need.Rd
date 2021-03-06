% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{need}
\alias{need}
\title{Alias for stopifnot}
\usage{
need(..., exprs, local = TRUE)
}
\description{
Alias for stopifnot
}
\details{
This is to be added in attempt block based on requirements. The \code{TRUE} logical value mentioned under \code{need} will end the attmpt block.
It should be given in the end of attempt block
}
\examples{
set.seed(1)
attempt({
u <- rnorm(1,mean = -2)
need(u>0)})
u
#0.4016178
}
