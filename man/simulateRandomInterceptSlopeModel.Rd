% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohortSimulation.R
\name{simulateRandomInterceptSlopeModel}
\alias{simulateRandomInterceptSlopeModel}
\title{Simulate random-slope, random-intercept longitudinal data}
\usage{
simulateRandomInterceptSlopeModel(
  n_subjects,
  n_timepoints,
  intercept_mean,
  intercept_sd,
  slope_mean,
  slope_sd,
  epsilon,
  group = "",
  id_prefix = ""
)
}
\arguments{
\item{n_subjects}{Number of subjects}

\item{n_timepoints}{Number of timepoints}

\item{intercept_mean}{Mean random intercept}

\item{intercept_sd}{Standard deviation of random intercept}

\item{slope_mean}{Mean random slope}

\item{slope_sd}{Standard deviation of random slope}

\item{epsilon}{Observation-level error}

\item{group}{Character string defining group name}

\item{id_prefix}{Character to append to ID variable}
}
\description{
Simulate random-slope, random-intercept longitudinal data
}
\examples{
n_subjects <- 10
n_timepoints <- 4
intercept_mean <- 0
intercept_sd <- 0.01
slope_mean <- 0
slope_sd <- 0.01
epsilon <- 0.01
group <- "testgroup"
id_prefix <- "test"
simulateRandomInterceptSlopeModel(n_subjects, n_timepoints, intercept_mean, intercept_sd, slope_mean, slope_sd, epsilon, group, id_prefix)

}
