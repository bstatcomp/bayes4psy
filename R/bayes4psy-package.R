#' The 'bayes4psy' package.
#'
#' @description A user-friendly implementation of Bayesian statistical methods commonly used in social sciences. All used models are pre-compiled, meaning that users only need to call appropriate functions using their data.
#'
#' @docType package
#' @name bayes4psy-package
#' @aliases bayes4psy
#' @useDynLib bayes4psy, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom rstan sampling
#'
#' @references
#' Stan Development Team (NA) - the Stan framework and RStan interface.
#' John Kruschke - mcmc_hdi function
#' Rasmus Bååth - Easy Bayesian Bootstrap in R
NULL
