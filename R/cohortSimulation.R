#' Simulate fixed-slope, fixed-intercept longitudinal data
#' @param n_subjects Number of subjects
#' @param n_timepoints Number of timepoints
#' @param intercept Intercept
#' @param slope Slope
#' @param epsilon Observation-level error
#' @param group Character string defining group name
#' @param id_prefix Character to append to ID variable

#' @examples n_subjects <- 10
#' n_timepoints <- 4
#' intercept <- 0
#' slope <- -0.5
#' epsilon <- 0.01
#' group <- "testgroup"
#' id_prefix <- "test"
#' simulateSlopeModel(n_subjects, n_timepoints, intercept, slope, epsilon, group, id_prefix)
#'
#' @import dplyr
#' @export

simulateSlopeModel <- function(n_subjects,
                               n_timepoints,
                               intercept,
                               slope,
                               epsilon,
                               group = "",
                               id_prefix = "") {

  cohort_level_data <- expand.grid(time = 1:n_timepoints,
                                   id = 1:n_subjects) %>%
    mutate(intercept = intercept,
           error = rnorm(n_timepoints * n_subjects, 0, epsilon),
           group = group)

  subject_level_data <- data.frame(id = 1:n_subjects,
                                   slope = slope)


  cohort_data <- cohort_level_data  %>%
    merge(y = subject_level_data,
          by = "id")  %>%
    mutate(id = paste0(id_prefix, stringr::str_pad(id, width = 4, side = "left", pad = 0) ),
           time = (time - 1),
           observed_value = intercept + time * slope + error)

  return(cohort_data)

}


#' Simulate fixed-slope, random-intercept longitudinal data
#' @param n_subjects Number of subjects
#' @param n_timepoints Number of timepoints
#' @param intercept_mean Mean random intercept
#' @param intercept_sd Standard deviation of random intercept
#' @param slope Slope
#' @param epsilon Observation-level error
#' @param group Character string defining group name
#' @param id_prefix Character to append to ID variable
#'
#' @examples n_subjects <- 10
#' n_timepoints <- 4
#' intercept_mean <- 0
#' intercept_sd <- 0.01
#' slope <- -0.5
#' epsilon <- 0.01
#' group <- "testgroup"
#' id_prefix <- "test"
#' simulateRandomInterceptModel(n_subjects, n_timepoints, intercept_mean, intercept_sd, slope, epsilon, group, id_prefix)
#'
#' @import dplyr
#' @export

simulateRandomInterceptModel <- function(n_subjects,
                                         n_timepoints,
                                         intercept_mean,
                                         intercept_sd,
                                         slope,
                                         epsilon,
                                         group = "",
                                         id_prefix = "") {

  cohort_level_data <- expand.grid(time = 1:n_timepoints,
                                   id = 1:n_subjects) %>%
    mutate(error = rnorm(n_timepoints * n_subjects, 0, epsilon),
           group = group)

  subject_level_data <- data.frame(id = 1:n_subjects,
                                   intercept = rnorm(n_subjects, intercept_mean, intercept_sd),
                                   slope = slope)


  cohort_data <- cohort_level_data %>%
    merge(y = subject_level_data,
          by = "id") %>%
    mutate(id = paste0(id_prefix, stringr::str_pad(id, width = 4, side = "left", pad = 0) ),
           time = (time - 1),
           observed_value = intercept + time * slope + error)

  return(cohort_data)

}


#' Simulate random-slope, random-intercept longitudinal data
#' @param n_subjects Number of subjects
#' @param n_timepoints Number of timepoints
#' @param intercept_mean Mean random intercept
#' @param intercept_sd Standard deviation of random intercept
#' @param slope_mean Mean random slope
#' @param slope_sd Standard deviation of random slope
#' @param epsilon Observation-level error
#' @param group Character string defining group name
#' @param id_prefix Character to append to ID variable

#' @examples n_subjects <- 10
#' n_timepoints <- 4
#' intercept_mean <- 0
#' intercept_sd <- 0.01
#' slope_mean <- 0
#' slope_sd <- 0.01
#' epsilon <- 0.01
#' group <- "testgroup"
#' id_prefix <- "test"
#' simulateRandomInterceptSlopeModel(n_subjects, n_timepoints, intercept_mean, intercept_sd, slope_mean, slope_sd, epsilon, group, id_prefix)
#'
#' @import dplyr
#' @export


simulateRandomInterceptSlopeModel <- function(n_subjects,
                                              n_timepoints,
                                              intercept_mean,
                                              intercept_sd,
                                              slope_mean,
                                              slope_sd,
                                              epsilon,
                                              group = "",
                                              id_prefix = "") {

  cohort_level_data <- expand.grid(time = 1:n_timepoints,
                                   id = 1:n_subjects) %>%
    mutate(error = rnorm(n_timepoints * n_subjects, 0, epsilon),
           group = group)

  subject_level_data <- data.frame(id = 1:n_subjects,
                                   slope = rnorm(n_subjects, slope_mean, slope_sd),
                                   intercept = rnorm(n_subjects, intercept_mean, intercept_sd))


  cohort_data <- cohort_level_data %>%
    merge(y = subject_level_data,
          by = "id") %>%
    mutate(id = paste0(id_prefix, stringr::str_pad(id, width = 4, side = "left", pad = 0) ),
           time = (time - 1),
           observed_value = intercept + time * slope + error)

  return(cohort_data)

}
