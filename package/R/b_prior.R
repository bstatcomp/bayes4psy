#' @title b_prior
#' @description An S4 class for defining priors for Bayesian models.
#' @slot family Prior family - \"uniform\", \"normal\", \"gamma\" or \"beta\".
#' @slot pars Parameters of the prior - a vector of two numerical values.
#' @export
b_prior <- setClass(
  "b_prior",
  slots = c(
    family  = "character",
    pars = "vector"
  )
)
