% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selenium_storm.R
\name{selenium_storm_client}
\alias{selenium_storm_client}
\title{Get a Selenium Storm Client for oprations}
\usage{
selenium_storm_client(
  Browser,
  headless,
  wait_time = Inf,
  final_active_check = F,
  ...
)
}
\arguments{
\item{Browser}{which browser to start}

\item{headless}{whether headless of not}

\item{wait_time}{specific wait time (in min)}

\item{final_active_check}{if set true whether session is active or not will be checked each time.}

\item{...}{additional parameter to be passed to remote Driver (RSelenium)}
}
\value{
Returns a client
}
\description{
Get a Selenium Storm Client for oprations
}
\examples{
selenium_storm_client()
}
