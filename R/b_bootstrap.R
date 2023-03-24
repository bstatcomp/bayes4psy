#' @title b_bootstrap
#' @description Performs a Bayesian bootstrap and returns a sample of size n1 representing the posterior distribution of the statistic. Returns a vector if the statistic is one-dimensional (like for mean(...)) or a data.frame if the statistic is multi-dimensional (like for the coefficients of lm).
#' @author Rasmus Baath
#' @references \url{https://www.sumsar.net/blog/2015/07/easy-bayesian-bootstrap-in-r/}
#' @references Rubin, D. B. (1981). The Bayesian Bootstrap. The annals of statistics, 9(1), 130-134.
#' @export
#' @param data The data as either a vector, matrix or data.frame.
#' @param statistic A function that accepts data as its first argument and if use_weights is TRUE the weights as its second argument. Function should return a numeric vector.
#' @param n1 The size of the bootstrap sample (default = 1000).
#' @param n2 The sample size used to calculate the statistic each bootstrap draw (default = 1000).
#' @param use_weights Whether the statistic function accepts a weight argument or should be calculated using resampled data (default = FALSE).
#' @param weight_arg If the statistic function includes a named argument for the weights this could be specified here (default = NULL).
#' @param ... Further arguments passed on to the statistic function.
#' @return A data frame containing bootstrap samples.
#'
#' @examples
#'
#' # linear function of seqence vs. response
#' lm_statistic <- function(data) {
#'   lm(sequence ~ response, data)$coef
#' }
#'
#' # load data
#' data <- adaptation_level_small
#'
#' # bootstrap
#' data_bootstrap <- b_bootstrap(data, lm_statistic, n1 = 1000, n2 = 1000)
#'
b_bootstrap <- function(data, statistic, n1 = 1000, n2 = 1000, use_weights = FALSE, weight_arg = NULL, ...) {
  # Draw from a uniform Dirichlet dist. with alpha set to rep(1, n_dim).
  # Using the facts that you can transform gamma distributed draws into
  # Dirichlet draws and that rgamma(n, 1) <=> rexp(n, 1)
  dirichlet_weights <- matrix(stats::rexp(NROW(data) * n1, 1), ncol = NROW(data), byrow = TRUE)
  dirichlet_weights <- dirichlet_weights / rowSums(dirichlet_weights)

  if (use_weights) {
    stat_call <- quote(statistic(data, w, ...))
    names(stat_call)[3] <- weight_arg
    boot_sample <- apply(dirichlet_weights, 1, function(w) {
      eval(stat_call)
    })
  } else {
    if (is.null(dim(data)) || length(dim(data)) < 2) { # data is a list type of object
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        data_sample <- sample(data, size = n2, replace = TRUE, prob = w)
        statistic(data_sample, ...)
      })
    } else { # data is a table type of object
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        index_sample <- sample(nrow(data), size = n2, replace = TRUE, prob = w)
        statistic(data[index_sample, , drop = FALSE], ...)
      })
    }
  }
  if (is.null(dim(boot_sample)) || length(dim(boot_sample)) < 2) {
    # If the bootstrap sample is just a simple vector return it.
    boot_sample
  } else {
    # Otherwise it is a matrix. Since apply returns one row per statistic
    # let's transpose it and return it as a data frame.
    as.data.frame(t(boot_sample))
  }
}
