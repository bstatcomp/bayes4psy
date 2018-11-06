#' Shared helper functions used in several models.
#'
#' @param y1 Numeric vector of values for the first group.
#' @param y2 Numeric vector of values for the second group.
#' @param rope Optional parameter for the rope interval.
#' @return Prints the difference and 95% confidence interval into the console.
#'
shared_difference <- function(y1, y2, rope = NULL) {
  y_diff <- y1 - y2

  # 1 > 2
  y1_smaller <- sum(y_diff < rope[1]) / n
  cat("Probabilities:\n  - Group 1 < Group 2: ", y1_smaller)

  # 2 > 1
  y1_greater <- sum(y_diff > rope[2]) / n
  cat(sprintf("\n  - Group 1 > Group 2: %.2f", y1_greater))

  # equal
  if (!is.null(rope)) {
    if (rope[1] == 0 && rope[2] == 0) {
      cat("\n  - Equal: NA\n")
    } else {
      equal <- sum((y_diff > rope[1]) & (y_diff < rope[2])) / n
      cat(sprintf("\n  - Equal: %.2f\n", equal))
    }
  }

  y_diff_l <- quantile(y_diff, 0.025)
  y_diff_h <- quantile(y_diff, 0.975)
  cat(sprintf("\n95%% CI:\n  - Group 1 - Group 2: [%.2f, %.2f]", y_diff_l, y_diff_h))
}
