#' @title b_prior
#' @description An S4 class for defining priors for Bayesian models.
#' @slot family Prior family - \"uniform\", \"normal\", \"gamma\" or \"beta\".
#' @slot pars Parameters of the prior - a vector of two numerical values.
#' @export b_prior
#' @exportClass b_prior
b_prior <- setClass(
  "b_prior",
  slots = c(
    family  = "character",
    pars = "vector"
  )
)

#' @rdname b_prior-get_prior_id
#' @exportMethod get_prior_id
setGeneric(name="get_prior_id", function(object) standardGeneric("get_prior_id"))

#' @title get_prior_id
#' @description \code{get_prior_id} returns an integer id of prior's family (1 = uniform, 2 = normal, 3 = gamma, 4 = beta).
#' @param object b_prior object.
#' @rdname b_prior-get_prior_id
#' @aliases get_prior_id_b_prior
setMethod(f="get_prior_id", signature(object="b_prior"), definition=function(object) {
  if (object@family == "uniform") {
    return(1)
  } else if (object@family == "normal") {
    return(2)
  } else if (object@family == "gamma") {
    return(3)
  } else if (object@family == "beta") {
    return(4)
  }
  return(0)
})
