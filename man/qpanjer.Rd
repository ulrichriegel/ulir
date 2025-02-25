% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panjer_distributions.R
\name{qpanjer}
\alias{qpanjer}
\title{This function provides the quantile function of the Panjer distribution class based on mean and dispersion/overdispersion/contagion}
\usage{
qpanjer(p, mean, dispersion, overdispersion = dispersion - 1,
  contagion = overdispersion/mean, log = FALSE)
}
\arguments{
\item{p}{vector of probabilities}

\item{mean}{vector of means}

\item{dispersion}{vector of dispersions}

\item{overdispersion}{vector of overdispersions}

\item{contagion}{vector of contagions}
}
\description{
This function provides the quantile function of the Panjer distribution class based on mean and dispersion/overdispersion/contagion
}
