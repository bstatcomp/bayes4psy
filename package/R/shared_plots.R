#' @import ggplot2

shared_plot_difference <- function(y1, y2, rope = NULL, bins = 30) {
  # init local varibales for CRAN check
  value <- NULL

  # difference
  diff <- data.frame(value = y1 - y2)

  # get 95% hdi
  hdi <- mcmc_hdi(diff$value)

  # mean difference
  mean_diff <- mean(diff$value)

  # get x range
  x_min <- min(diff)
  x_max <- max(diff)
  if (!is.null(rope)) {
    x_min <- min(x_min, rope[1])
    x_max <- max(x_max, rope[2])
  }

  # basic histogram chart
  graph <- ggplot() +
    geom_histogram(data = diff, aes(x = value), fill = "#3182bd", alpha = 0.4, bins = bins, na.rm=T) +
    xlim(x_min, x_max)

  # add mean
  y_max <- max(ggplot_build(graph)$data[[1]]$count)
  graph <- graph +
    geom_segment(aes(x = mean_diff, xend = mean_diff, y = 0, yend = y_max * 1.05), size = 1.5, color = "#3182bd", na.rm=T) +
    geom_text(aes(label = sprintf("%.2f", mean_diff), x = mean_diff, y = y_max * 1.08), size = 4)

  # add HDI
  graph <- graph +
    geom_segment(aes(x = hdi[1], xend = hdi[2], y = -(y_max * 0.01), yend = -(y_max * 0.01)), size = 3, color = "black", na.rm=T) +
    geom_text(aes(label = sprintf("%.2f", hdi[1]), x = hdi[1], y = -(y_max * 0.04)), size = 4) +
    geom_text(aes(label = sprintf("%.2f", hdi[2]), x = hdi[2], y = -(y_max * 0.04)), size = 4)

  # add ROPE interval?
  if (!is.null(rope)) {
    graph <- graph +
      geom_segment(aes(x = rope[1], xend = rope[2], y = y_max * 0.01, yend = y_max * 0.01), size = 3, color = "grey50", na.rm=T)
  }

  # style and labels
  graph <- graph +
    xlab("Value")

  return(graph)
}

