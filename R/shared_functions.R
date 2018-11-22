#' Shared helper functions used in several models.
#'
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Numeric vector of values for the second group.
#' @param rope Optional parameter for the rope interval.
#' @return Prints the difference and 95% confidence interval into the console.
#'
shared_difference <- function(y1, y2, rope = NULL) {
  y_diff <- y1 - y2

  n <- length(y_diff)

  if (is.null(rope)) {
    # 1 > 2
    y1_smaller <- round(sum(y_diff < 0) / n, 2)
    y1_greater <- round(sum(y_diff > 0) / n, 2)
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f", y1_smaller))
    cat(sprintf("\n  - Group 1 > Group 2: %.2f", y1_greater))
  } else {
    # 1 > 2
    y1_smaller <- round(sum(y_diff < rope[1]) / n, 2)
    cat(sprintf("Probabilities:\n  - Group 1 < Group 2: %.2f", y1_smaller))

    # 2 > 1
    y1_greater <- round(sum(y_diff > rope[2]) / n, 2)
    cat(sprintf("\n  - Group 1 > Group 2: %.2f", y1_greater))

    # equal
    equal <- 1 - y1_smaller - y1_greater
    cat(sprintf("\n  - Equal: %.2f\n", equal))
  }

  y_diff_l <- quantile(y_diff, 0.025)
  y_diff_h <- quantile(y_diff, 0.975)
  cat(sprintf("\n95%% CI:\n  - Group 1 - Group 2: [%.2f, %.2f]\n", y_diff_l, y_diff_h))
}

#' @param rope Rope interval parameter (single number, or an interval).
#' @return Rope as an interval.
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
