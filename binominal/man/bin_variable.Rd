% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_variable}
\alias{bin_variable}
\title{bin variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
random variable object
}
\description{
binominal random variable
}
\examples{
bin_variable(5, 0.5)
bin_variable(7, 0.3)
}
