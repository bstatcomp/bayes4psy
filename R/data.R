#' Datasets for bayes4psy examples
#' Small datasets for use in \pkg{rstanarm} examples and vignettes.
#'
#' @name rstanarm-datasets
#' @aliases adaptation_level_small
#' @format
#' \describe{
#' \item{\code{adaptation_level_small}}{
#' Data on subjects picking up weights and determining their weights from 1..10.
#'
#' 50 obs. of 5 variables
#' \itemize{
#' \item \code{sequence} sequence index.
#' \item \code{weight} actual weight of the object.
#' \item \code{response} subject's estimation of weight.
#' }
#' }
#' }
#'
#' @examples
#'
#' # Example of Bayesian bootstraping on 'adaptation_level_small' dataset
#' # linear function of seqence vs. response
#' lm_statistic <- function(data) {
#'   lm(sequence ~ response, data)$coef
#' }
#'
#' # load data
#' data <- adaptation_level_small
#'
#' # bootstrap
#' data_bootstrap <- b_bootstrap(data, lm_statistic, n1=1000, n2=1000)
#'
NULL
