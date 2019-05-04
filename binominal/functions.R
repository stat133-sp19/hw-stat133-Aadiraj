# title: Check Prob
# description: check prob valid
# param: prob: probability
# return: validity
check_prob <- function(prob) {
  if(prob < 0 | prob > 1) {
    stop("Invalid prob value")
  }
  else {return(T)}
}

# title: Check trials
# description: check number of trials valid
# param: trials: number of trials
# return: validity
check_trials <- function(trials) {
  if(trials < 0) {
    stop("invalid trial value")
  }
  else {return(T)}
}

# title: Check Success
# description: check number of successes valid
# param: success: vector of number of successes
# param: trials: number of trials
# return: validity
check_success <- function(success, trials) {
  for(s in success) {
    if(s < 0) {
      stop("Invalid success value")
    }
    else if(s > trials) {
      stop("success cannot be greater than trials")
    }
  }
  return(T)
}

# title: Auxiliary Mean
# description: Gets mean
# param: trials: number of trials
# param: prob: probability
# return: mean
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# title: Auxiliary Variance
# description: Gets variance
# param: trials: number of trials
# param: prob: probability
# return: variance
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# title: Auxiliary Mode
# description: Gets mode
# param: trials: number of trials
# param: prob: probability
# return: mode
aux_mode <- function(trials, prob) {
  return(floor(trials * prob + prob))
}

# title: Auxiliary Skewness
# description: Gets skewness
# param: trials: number of trials
# param: prob: probability
# return: skewness
aux_skewness <- function(trials, prob) {
  return((1- 2*prob)/sqrt(trials * prob*(1 - prob)))
}

# title: Auxiliary Kurtosis
# description: Gets kurtosis
# param: trials: number of trials
# param: prob: probability
# return: kurtosis
aux_kurtosis <- function(trials, prob) {
  return((1- 6*prob*(1 - prob))/(trials * prob*(1 - prob)))
}

aux_mean(10, 0.3)
aux_variance(10, 0.3)
aux_mode(10, 0.3)
aux_skewness(10, 0.3)
aux_kurtosis(10, 0.3)

#' @title bin choose
#' @description combination n choose k
#' @param n number to choose from
#' @param k number selected
#' @return number of combinations
#' @export
#' @examples
#' bin_choose(5,2)
#' bin_choose(5,1:3)
#' bin_choose(5,0)
bin_choose <- function(n, k) {
  if(k > n) {
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k) * factorial(n - k)))
}

#' @title bin probability
#' @description binominal probability
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return probability
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials
#' # (assuming prob of success = 0.5) 
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' # probabilities of getting 2 or less successes in 5 trials 
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' 
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob) {
  if(check_trials(trials) & check_prob(prob) & check_success(success, trials)) {
    return(bin_choose(trials, success)*prob^(success)*(1 - prob)^(trials - success))
  }
}

#' @title bin distribution
#' @description binominal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return distribution
#' @export
#' @examples
#' bin_distribution(5, 0.5)
#' bin_distribution(7, 0.3)
bin_distribution <- function(trials, prob) {
  probs <- rep(0,trials + 1)
  for(k in 0:trials) {
    probs[k+1] = bin_probability(k, trials, prob)
  }
  result = data.frame("success" = 0:trials, "probability" = probs)
  class(result) <- c("bindis", "data.frame")
  return(result)
}

#' @export
plot.bindis <- function(dis) {
  return(barplot(dis$probability, names.arg = dis$success))
}

#' @title bin cumlative
#' @description binominal cumulative
#' @param trials number of trials
#' @param prob probability of success
#' @return cumulative
#' @export
#' @examples
#' bin_cumulative(5, 0.5)
#' bin_cumulative(7, 0.3)
bin_cumulative <- function(trials, prob) {
  result <- bin_distribution(trials, prob)
  culs <- rep(0, trials)
  culs[1] <- result[1,2]
  for(k in 1:trials + 1) {
    culs[k] <- result[k,2] + culs[k-1]
  }
  result$cumulative <- culs
  class(result) <- c("bincum", "data.frame")
  return(result)
}

#' @export
plot.bincums <- function(dis) {
  return(plot(dis$success, dis$cumulative, type="o", xlab = "success", ylab = "probability"))
}

plot.bincums(bin_cumulative(5, 0.5))

#' @title bin variable
#' @description binominal random variable
#' @param trials number of trials
#' @param prob probability of success
#' @return random variable object
#' @export
#' @examples
#' bin_variable(5, 0.5)
#' bin_variable(7, 0.3)
bin_variable <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    result <- list(trials, prob)
    names(result) <- c("trials", "prob")
    class(result) <- c("binvar")
    return(result)
  }
}

#' @export
cat("Binominal Variable\n\nParamaters \n - number of trials:", (bin_variable(5,0.5)[[1]]), "\n - prob of success:", (bin_variable(5,0.5)[[2]]))
bin_variable(5,0.5)[1]
print.binvar <- function(var) {
  cat("Binominal Variable\n\nParamaters \n - number of trials:", var[[1]], "\n - prob of success:", var[[2]])
}
bin_variable(5, 0.5)

#' @export
summary.binvar <- function(var) {
  result = list(var[[1]], var[[2]], aux_mean(var[[1]], var[[2]]), aux_variance(var[[1]], var[[2]]), aux_mode(var[[1]], var[[2]]), aux_skewness(var[[1]], var[[2]]), aux_kurtosis(var[[1]], var[[2]])) 
}

#' @export
print.summary.binvar <- function(var) {
  summary <- summary.binvar(var)
  cat("Summary Binominal\n\nParamaters \n - number of trials:", var[[1]], "\n - prob of success:", var[[2]], "\nMeasures\n - mean:", summary[[1]], "\n - varience:", summary[[2]], "\n - mode:", summary[[3]],"\n - skewness:", summary[[4]],"\n - kurtosis:", summary[[5]] )
}
print.summary.binvar(var1sum)

#' @title bin mean
#' @description binominal mean
#' @param trials number of trials
#' @param prob probability of success
#' @return mean
#' @export
#' @examples
#' bin_mean(5, 0.5)
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    return(aux_mean(trials,prob))
  }
}

#' @title bin variance
#' @description binominal variance
#' @param trials number of trials
#' @param prob probability of success
#' @return variance
#' @export
#' @examples
#' bin_variance(5, 0.5)
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    return(aux_variance(trials,prob))
  }
}

#' @title bin mode
#' @description binominal mode
#' @param trials number of trials
#' @param prob probability of success
#' @return mode
#' @export
#' @examples
#' bin_mode(5, 0.5)
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    return(aux_mode(trials,prob))
  }
}

#' @title bin skewness
#' @description binominal skewness
#' @param trials number of trials
#' @param prob probability of success
#' @return skewness
#' @export
#' @examples
#' bin_skewness(5, 0.5)
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    return(aux_skewness(trials,prob))
  }
}

#' @title bin kurtosis
#' @description binominal kurtosis
#' @param trials number of trials
#' @param prob probability of success
#' @return kurtosis
#' @export
#' @examples
#' bin_kurtosis(5, 0.5)
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  if(check_trials(trials) & check_prob(prob)) {
    return(aux_kurtosis(trials,prob))
  }
}

bin_mean(10, 0.3)
bin_kurtosis(10, 0.3)
