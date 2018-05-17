# class for returning results of ttest

setClass(
  "ttest_results",
  representation(y1_samples = "list",
                 y2_samples = "list",
                 ROPE = "numeric")
)

setMethod("show", "ttest_results", function(object) {
  # draw from samples
  n <- 1000
  y1 <- rnorm(n, mean(object@y1_samples$mu), mean(object@y1_samples$sigma));
  y2 <- rnorm(n, mean(object@y2_samples$mu), mean(object@y2_samples$sigma));

  # 1 > 2
  y1_greater <- sum((y1 - y2) > object@ROPE) / n
  cat("Probabilities:\n  - Group 1 > Group 2: ", y1_greater)

  # 2 > 1
  y2_greater <- sum((y2 - y1) > object@ROPE) / n
  cat("\n  - Group 2 > Group 1: ", y2_greater)

  # equal
  if (object@ROPE == 0)
    cat("\n  - Equal: NA")
  else
  {
    equal <- sum(abs(y1 - y2) < object@ROPE) / n
    cat("\n  - Equal: ", equal)
  }
})
