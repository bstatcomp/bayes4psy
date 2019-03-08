shared_difference <- function(y1, y2, rope=NULL) {
  y_diff <- y1 - y2

  n <- length(y_diff)

  if (is.null(rope)) {
    # 1 > 2
    diff_smaller <- y_diff < 0
    y1_smaller <- round(sum(diff_smaller) / n, 2)
    diff_greater <- y_diff > 0
    y1_greater <- round(sum(diff_greater) / n, 2)
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f +/- %.5f",
                y1_smaller, mcmcse::mcse(diff_smaller)$se))
    cat(sprintf("\n  - Group 1 > Group 2: %.2f +/- %.5f",
                y1_greater, mcmcse::mcse(diff_greater)$se))
  } else {
    # 1 > 2
    diff_smaller <- y_diff < rope[1]
    y1_smaller <- round(sum(diff_smaller) / n, 2)
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f +/- %.5f",
                y1_smaller, mcmcse::mcse(diff_smaller)$se))

    # 2 > 1
    diff_greater <- y_diff > rope[2]
    y1_greater <- round(sum(y_diff > diff_greater) / n, 2)
    cat(sprintf("\n  - Group 1 > Group 2: %.2f +/- %.5f",
                y1_greater, mcmcse::mcse(diff_greater)$se))

    # equal
    diff_equal <- y_diff > rope[1] & y_diff < rope[2]
    equal <- 1 - y1_smaller - y1_greater
    cat(sprintf("\n  - Equal: %.2f\n +/- %.5f",
                equal, mcmcse::mcse(diff_equal)$se))
  }

  hdi <- mcmc_hdi(y_diff)
  y_diff_l <- stats::quantile(hdi[1], 0.025)
  y_diff_h <- stats::quantile(hdi[2], 0.975)
  cat(sprintf("\n95%% HDI:\n  - Group 1 - Group 2: [%.2f, %.2f]\n", y_diff_l, y_diff_h))
}

prepare_rope <- function(rope) {
  # rope is NULL
  if (is.null(rope)) {
    return(NULL)
  }

  # validity check for rope
  if (length(rope) > 2) {
    warning("You provided more than two values for the ROPE interval! Rope value was thus set to 0.")
    return(NULL)
  }
  else if (!is.null(rope) && length(rope) == 1 && rope < 0) {
    warning("When a single number is provided for the ROPE interval it should be positive or 0! Rope value was thus set to 0.")
    return(NULL)
  }

  # if rope as as single number cast it to a list with 2 elements
  if (length(rope) == 1) {
    rope[2] <- rope[1]
    rope[1] <- -rope[1]
  }

  # order ascending
  rope <- sort(rope)

  # return
  return(rope)
}
