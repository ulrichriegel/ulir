#' This function provides the density function of the lognormal distribution based on mean and stddev
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

dlnorm_mean_sd <- function(x, mean, sd, log = FALSE) {
  sdlog <- sqrt(log(sd^2 / mean^2 + 1))
  meanlog <- log(mean) - sdlog^2 / 2
  result <- dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = log)
  return(result)
}


#' This function provides the distribution function of the lognormal distribution based on mean and stddev
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

plnorm_mean_sd <- function(x, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  sdlog <- sqrt(log(sd^2 / mean^2 + 1))
  meanlog <- log(mean) - sdlog^2 / 2
  result <- plnorm(x, meanlog = meanlog, sdlog = sdlog, lower.tail = lower.tail, log.p = log.p)
  return(result)
}


#' This function provides the quantile function of the lognormal distribution based on mean and stddev
#' @param p vector of probabilities
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

qlnorm_mean_sd <- function(p, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  sdlog <- sqrt(log(sd^2 / mean^2 + 1))
  meanlog <- log(mean) - sdlog^2 / 2
  result <- qlnorm(p, meanlog = meanlog, sdlog = sdlog, lower.tail = lower.tail, log.p = log.p)
  return(result)
}


#' This function provides random generation for the lognormal distribution based on mean and stddev
#' @param n number of observations
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @export

rlnorm_mean_sd <- function(n, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  sdlog <- sqrt(log(sd^2 / mean^2 + 1))
  meanlog <- log(mean) - sdlog^2 / 2
  result <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  return(result)
}

