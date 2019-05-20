#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param data Numeric vector of values on which the fit will be based.
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu (mean) and sigma (variance) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 9000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 10000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @return An object of class `ttest_class`.
b_ttest <- function(data,
                    priors=NULL,
                    warmup=9000,
                    iter=10000,
                    chains=4,
                    control=NULL) {

  # prepare data
  n <- length(data)

  # prior ids and values
  p_ids <- rep(0, 2)
  p_values <- rep(0, 4)

  # parameter mapping
  df_pars <- data.frame(par=c("mu", "sigma"), index=c(1, 2))

  # priors
  if (!is.null(priors)) {
    for (p in priors) {
      par <- p[[1]]
      prior <- p[[2]]

      # get parameter index
      id <- 0
      par_id <- df_pars[df_pars$par==par,]
      if (nrow(par_id) > 0) {
        id <- par_id$index
      } else {
        wrong_prior <- "Provided an unknown parameter for prior, use \"mu\" or \"sigma\"."
        warning(wrong_prior)
        return()
      }
      # set prior family id
      p_ids[id] <- get_prior_id(prior)
      if (p_ids[id] == 0) {
        wrong_prior <- "Provided an unknown prior family, use \"uniform\", \"normal\", \"gamma\" or \"beta\"."
        warning(wrong_prior)
        return()
      }

      # set parameter values
      if (length(prior@pars) != 2) {
        wrong_pars <- "Incorrect prior parameters, provide a vector of 2 numerical values."
        warning(wrong_pars)
        return()
      }
      p_values[2*id-1] <- prior@pars[1]
      p_values[2*id] <- prior@pars[2]
    }
  }

  stan_data <- list(n = n,
                    y = data,
                    p_ids = p_ids,
                    p_values = p_values)

  fit <- sampling(stanmodels$ttest,
                  data = stan_data,
                  warmup = warmup,
                  iter = iter,
                  chains = chains,
                  control=control)

  extract <- extract(fit)

  # create output class
  out <- ttest_class(extract=extract, fit=fit, data=data)

  # return
  return(out)
}
