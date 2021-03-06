% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_probability}
\alias{bin_probability}
\title{bin probability}
\usage{
bin_probability(success, trials, prob)
}
\arguments{
\item{success}{number of successes}

\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
probability
}
\description{
binominal probability
}
\examples{
# probability of getting 2 successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 2, trials = 5, prob = 0.5)

# probabilities of getting 2 or less successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)

# 55 heads in 100 tosses of a loaded coin with 45\% chance of heads
bin_probability(success = 55, trials = 100, prob = 0.45)
}
