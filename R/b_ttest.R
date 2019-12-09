#' @title b_ttest
#' @description Bayesian t-test.
#' @import rstan
#' @export
#' @param data Numeric vector of values on which the fit will be based.
#' @param priors List of parameters and their priors - b_prior objects. You can put a prior on the mu (mean) and sigma (variance) parameters (default = NULL).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 1000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 2000).
#' @param chains Integer specifying the number of parallel chains (default = 4).
#' @param seed Random number generator seed (default = NULL).
#' @param refresh Frequency of output (default = NULL).
#' @param control A named list of parameters to control the sampler's behavior (default = NULL).
#' @param suppress_warnings Suppress warnings returned by Stan (default = TRUE).
#' @return An object of class `ttest_class`.
#'
#' @examples
#' \donttest{
#' # priors
#' mu_prior <- b_prior(family="normal", pars=c(0, 1000))
#' sigma_prior <- b_prior(family="uniform", pars=c(0, 500))
#' nu_prior <- b_prior(family="normal", pars=c(2000, 1000))
#'
#' # attach priors to relevant parameters
#' priors <- list(c("mu", mu_prior),
#'                c("sigma", sigma_prior),
#'                c("nu", nu_prior))
#'
#' # generate some data
#' data  <- rnorm(20, mean=150, sd=20)
#'
#' # fit
#' fit <- b_ttest(data=data, priors=priors, chains=1)
#' }
#'
b_ttest <- function(data,
                    priors=NULL,
                    warmup=1000,
                    iter=2000,
                    chains=4,
                    seed=NULL,
                    refresh=NULL,
                    control=NULL,
                    suppress_warnings=TRUE) {

  # multi core
  if (chains > 1) {
    options(mc.cores = parallel::detectCores())
  }

  # prepare data
  n <- length(data)

  # prior ids and values
  p_ids <- rep(0, 3)
  p_values <- rep(0, 6)

  # parameter mapping
  df_pars <- data.frame(par=c("mu", "sigma", "nu"), index=c(1, 2, 3))

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
        wrong_prior <- "Provided an unknown parameter for prior, use \"nu\", \"mu\" or \"sigma\"."
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

  # put data together
  stan_data <- list(n = n,
                    y = data,
                    p_ids = p_ids,
                    p_values = p_values)

  # set seed
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  # set output frequency
  if (is.null(refresh)) {
    refresh <- max(iter/10, 1)
  }

  # fit
  if (suppress_warnings) {
    fit <- suppressWarnings(sampling(stanmodels$ttest,
                                     data=stan_data,
                                     warmup=warmup,
                                     iter=iter,
                                     chains=chains,
                                     seed=seed,
                                     refresh=refresh,
                                     control=control))
  } else {
    fit <- sampling(stanmodels$ttest,
                    data=stan_data,
                    warmup=warmup,
                    iter=iter,
                    chains=chains,
                    seed=seed,
                    refresh=refresh,
                    control=control)
  }

  # extract and parse into a list
  extract_raw <- extract(fit, permuted=FALSE)
  extract <- NULL
  samples <- iter - warmup
  for (i in 1:samples) {
    extract <- rbind(extract, extract_raw[i, 1,])
  }
  extract <- as.list(data.frame(extract))

  # create output class
  out <- ttest_class(extract=extract, fit=fit, data=data)

  # return
  return(out)
}
