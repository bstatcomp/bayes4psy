# validity check for this class
ttest_results_check <- function(object) {
  errors <- character()

  # ROPE < 0
  if (object@ROPE < 0) {
    msg <- "ROPE interval should have a positive value!"
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

#' An S4 class for storing results of Bayesian t-test results.
#' @slot y1_samples samples for the first group.
#' @slot y2_samples samples for the second group.
#' @slot ROPE region of practical equivalence.
#' @examples
#' ttest_results: prints difference/equality of two tested groups.
#'
#' summary(ttest_resuolts): prints difference/equality of two tested groups.
#'
#' difference_plot(ttest_results): visaualizes difference between two groups.
#' @exportClass ttest_results
ttest_results <- setClass(
  "ttest_results",

  slots = c(y1_samples = "list",
            y2_samples = "list",
            ROPE = "numeric"),

  validity = ttest_results_check
)

#' @exportMethod show
setMethod(f = "show", definition = function(object) {
  difference_print(object)
})

#' @exportMethod summary
setMethod(f = "summary", signature(object="ttest_results"), definition = function(object) {
  difference_print(object)
})


#' @rdname ttest_results-difference_plot
#' @exportMethod difference_plot
setGeneric(name = "difference_plot", function(object) standardGeneric("difference_plot"))

#' @title difference_plot
#' @description \code{difference_plot} visualizes difference/equality of two tested groups.
#' @rdname ttest_results-difference_plot
#' @aliases difference_plot,ANY-method
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

  # basic histogram chart
  graph <- ggplot() +
    geom_histogram(data=diff, aes(x=value), fill="#3182bd", alpha=0.5) +
    theme_minimal() +
    labs(title = "Difference between two groups", x = "Value", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

  # add mean
  y_max <- max(ggplot_build(graph)$data[[1]]$count)
  graph <- graph +
    geom_segment(aes(x=mean_diff, xend=mean_diff, y=0, yend=y_max * 1.05), size=1.5, color="#3182bd") +
    geom_text(aes(label = sprintf("%.2f", mean_diff), x = mean_diff, y = y_max * 1.08), size = 4)

  # add CI
  graph <- graph +
    geom_segment(aes(x=hdi[1], xend=hdi[2], y=-(y_max * 0.01), yend=-(y_max * 0.01)), size=3, color="black") +
    geom_text(aes(label = sprintf("%.2f", hdi[1]), x = hdi[1], y = -(y_max * 0.04)), size = 4) +
    geom_text(aes(label = sprintf("%.2f", hdi[2]), x = hdi[2], y = -(y_max * 0.04)), size = 4)

  # add ROPE?
  if (object@ROPE != 0) {
    graph <- graph +
      geom_segment(aes(x=-object@ROPE, xend=object@ROPE, y=y_max * 0.01, yend=y_max * 0.01), size=3, color="grey50")
  }

  graph

  #   geom_segment(aes(x=-ROPE,xend=ROPE,y=3,yend=3),size=2,color="grey50") +
  #   +
  #   ylim(-20, 220) +
  #   theme(axis.title.x=element_text(margin=margin(10,0,0,0)))  +
  #   scale_x_continuous(labels = scales::percent)
})


### Helper functions
# print difference between two groups
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
    cat("\n  - Equal: ", equal, "\n")
  }
}
