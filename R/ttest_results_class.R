#' An S4 class for storing results of Bayes t-test
#' @slot y1_samples samples for the first group.
#' @slot y2_samples samples for the second group.
#' @slot ROPE region of practical equivalence.
#' @export
ttest_results <- setClass(
  "ttest_results",

  slots = c(y1_samples = "list",
            y2_samples = "list",
            ROPE = "numeric")
)

#' @export
setMethod(f = "show", definition = function(object) {
  difference_print(object)
})

#' @export
setMethod(f = "summary", signature(object="ttest_results"), definition = function(object) {
  difference_print(object)
})

#' @export
#' @docType methods
#' @rdname ttest_results-methods
setGeneric(name = "difference_plot", function(object) standardGeneric("difference_plot"))

#' Plot difference histograms between two groups.
#' @rdname ttest_results-methods
#' @aliases difference_plot,ANY-method
#' @export
setMethod(f = "difference_plot", signature(object="ttest_results"), definition = function(object) {
  # draw from samples
  n <- 1000
  y1 <- rnorm(n, mean(object@y1_samples$mu), mean(object@y1_samples$sigma));
  y2 <- rnorm(n, mean(object@y2_samples$mu), mean(object@y2_samples$sigma));

  # difference
  diff <- data.frame(value = y1 - y2)

  # show difference in percentage?
  #if (percentage) {
  #  diff$value <- (diff$value / mean(object@y1_samples$mu))
  #}

  # get 95% hdi
  hdi <- mcmc_hdi(diff$value)

  # mean difference
  mean_diff <- mean(diff$value)

  ggplot() +
    geom_histogram(data=diff, aes(x=value), fill="#3182bd", binwidth = 0.01, alpha=0.3)
})


### Helper functions
difference_print <- function(object) {
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

  # geom_segment(aes(x=MPS,xend=MPS,y=0,yend=200),size=1,color="#3182bd") +
  #   geom_segment(aes(x=-ROPE,xend=ROPE,y=3,yend=3),size=2,color="grey50") +
  #   geom_segment(aes(x=HDIPS[1],xend=HDIPS[2],y=-3,yend=-3),size=2,color="black") +
  #   theme_minimal() +
  #   geom_text(aes(label = sprintf("%.2f%%",MPS * 100), x = MPS, y = 210), size = 5, family="Source Sans Pro") +
  #   geom_text(aes(label = sprintf("%.2f%%",HDIPS[1] * 100), x = HDIPS[1], y = -20), size = 5, family="Source Sans Pro") +
  #   geom_text(aes(label = sprintf("%.2f%%",HDIPS[2] * 100), x = HDIPS[2], y = -20), size = 5, family="Source Sans Pro") +
  #   theme(text = element_text(family="Source Sans Pro", size=20)) +
  #   labs(title = "", x = expression(paste("Area difference")), y = "") +
  #   ylim(-20, 220) +
  #   theme(axis.title.x=element_text(margin=margin(10,0,0,0)))  +
  #   scale_x_continuous(labels = scales::percent)
}
