#' This function provides the density function of the gamma distribution based on mean and stddev
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

dgamma_mean_sd <- function(x, mean, sd, log = FALSE) {
  scale <- sd^2 / mean
  shape <- mean / scale
  result <- dgamma(x, shape = shape, scale = scale, log = log)
  return(result)
}


#' This function provides the distribution function of the gamma distribution based on mean and stddev
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

pgamma_mean_sd <- function(x, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  scale <- sd^2 / mean
  shape <- mean / scale
  result <- pgamma(x, shape = shape, scale = scale, lower.tail = lower.tail, log.p = log.p)
  return(result)
}


#' This function provides the quantile function of the gamma distribution based on mean and stddev
#' @param p vector of probabilities
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

qgamma_mean_sd <- function(p, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  scale <- sd^2 / mean
  shape <- mean / scale
  result <- qgamma(p, shape = shape, scale = scale, lower.tail = lower.tail, log.p = log.p)
  return(result)
}


#' This function provides random generation for the gamma distribution based on mean and stddev
#' @param n number of observations
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

rgamma_mean_sd <- function(n, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  scale <- sd^2 / mean
  shape <- mean / scale
  result <- rgamma(n, shape = shape, scale = scale)
  return(result)
}


