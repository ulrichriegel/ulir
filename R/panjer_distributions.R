#' This function provides the density function of the Panjer distribution class based on mean and dispersion/overdispersion/contagion
#' @param x vector of quantiles
#' @param mean vector of means
#' @param dispersion vector of dispersions
#' @param overdispersion vector of overdispersions
#' @param contagion vector of contagions
#' @export

dpanjer <- function(x, mean, dispersion, overdispersion = dispersion - 1, contagion = overdispersion / mean, log = FALSE) {
  variance <- (contagion * mean + 1) * mean
  dispersion <- variance / mean
  if (dispersion > 1.0001) {
    # NegBin
    size = mean^2 / (variance - mean)
    result <- dnbinom(x, mu = mean, size = size, log = log)
  } else if (dispersion < 0.9999) {
    # Binomial
    prob <- 1 - dispersion
    size <- round(mean / prob, 0)
    prob <- mean / size
    result <- dbinom(x, size = size, prob = prob, log = log)
  } else {
    # Poisson
    result <- dpois(x, lambda = mean, log = log)
  }
  return(result)
}

#' This function provides the distribution function of the Panjer distribution class based on mean and dispersion/overdispersion/contagion
#' @param x vector of quantiles
#' @param mean vector of means
#' @param dispersion vector of dispersions
#' @param overdispersion vector of overdispersions
#' @param contagion vector of contagions
#' @export

ppanjer <- function(x, mean, dispersion, overdispersion = dispersion - 1, contagion = overdispersion / mean, log = FALSE) {
  variance <- (contagion * mean + 1) * mean
  dispersion <- variance / mean
  if (dispersion > 1.0001) {
    # NegBin
    size = mean^2 / (variance - mean)
    result <- pnbinom(x, mu = mean, size = size, lower.tail = TRUE, log.p = FALSE)
  } else if (dispersion < 0.9999) {
    # Binomial
    prob <- 1 - dispersion
    size <- round(mean / prob, 0)
    prob <- mean / size
    result <- pbinom(x, size = size, prob = prob, lower.tail = TRUE, log.p = FALSE)
  } else {
    # Poisson
    result <- ppois(x, lambda = mean, lower.tail = TRUE, log.p = FALSE)
  }
  return(result)
}



#' This function provides the quantile function of the Panjer distribution class based on mean and dispersion/overdispersion/contagion
#' @param p vector of probabilities
#' @param mean vector of means
#' @param dispersion vector of dispersions
#' @param overdispersion vector of overdispersions
#' @param contagion vector of contagions
#' @export

qpanjer <- function(p, mean, dispersion, overdispersion = dispersion - 1, contagion = overdispersion / mean, log = FALSE) {
  variance <- (contagion * mean + 1) * mean
  dispersion <- variance / mean
  if (dispersion > 1.0001) {
    # NegBin
    size = mean^2 / (variance - mean)
    result <- qnbinom(p, mu = mean, size = size, lower.tail = TRUE, log.p = FALSE)
  } else if (dispersion < 0.9999) {
    # Binomial
    prob <- 1 - dispersion
    size <- round(mean / prob, 0)
    prob <- mean / size
    result <- qbinom(p, size = size, prob = prob, lower.tail = TRUE, log.p = FALSE)
  } else {
    # Poisson
    result <- qpois(p, lambda = mean, lower.tail = TRUE, log.p = FALSE)
  }
  return(result)
}




#' This function provides random generation the Panjer distribution class based on mean and dispersion/overdispersion/contagion
#' @param n number of observations
#' @param mean vector of means
#' @param dispersion vector of dispersions
#' @param overdispersion vector of overdispersions
#' @param contagion vector of contagions
#' @export

rpanjer <- function(n, mean, dispersion, overdispersion = dispersion - 1, contagion = overdispersion / mean, log = FALSE) {
  variance <- (contagion * mean + 1) * mean
  dispersion <- variance / mean
  if (dispersion > 1.0001) {
    # NegBin
    size = mean^2 / (variance - mean)
    result <- rnbinom(n, mu = mean, size = size)
  } else if (dispersion < 0.9999) {
    # Binomial
    prob <- 1 - dispersion
    size <- round(mean / prob, 0)
    prob <- mean / size
    result <- rbinom(n, size = size, prob = prob)
  } else {
    # Poisson
    result <- rpois(n, lambda = mean)
  }
  return(result)
}

