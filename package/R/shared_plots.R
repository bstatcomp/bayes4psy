#' @import ggplot2

# function for visalizsing the difference between two datasets
shared_plot_difference <- function(y1, y2, rope=NULL, bins=30, circular=FALSE, nrow=1) {
  # init local varibales for CRAN check
  value <- NULL

  # difference
  y_diff <- y1 - y2

  # if circular cast differences to a -pi..pi interval or 0..2pi interval
  if (circular) {
    y_diff <- preprocess_circular(y_diff)
    y_diff <- as.numeric(y_diff)
  }

  # create df
  df_diff <- data.frame(value=y_diff)

  # get 95% hdi
  hdi <- mcmc_hdi(df_diff$value)

  # mean difference
  mean_diff <- mean(df_diff$value)

  # get x range
  x_min <- min(df_diff)
  x_max <- max(df_diff)
  if (!is.null(rope)) {
    x_min <- min(x_min, rope[1])
    x_max <- max(x_max, rope[2])
  }

  # basic histogram chart
  graph <- ggplot() +
    geom_histogram(data=df_diff, aes(x=value), fill="#3182bd", alpha=0.4, bins=bins, na.rm=T) +
    xlim(x_min, x_max)

  # add mean
  y_max <- max(ggplot_build(graph)$data[[1]]$count)

  # if mean is near min or max hjust inward
  hjust_range <- (x_max - x_min) * 0.1
  if (mean_diff < (x_min + hjust_range) || mean_diff > (x_max - hjust_range)) {
    graph <- graph +
      geom_segment(aes(x=mean_diff, xend=mean_diff, y=0, yend=y_max * 1.05), size=1.5, color="#3182bd", na.rm=T) +
      geom_text(aes(label=sprintf("%.2f", mean_diff), x=mean_diff, y=y_max * (1.05 + (nrow * 0.05))), size=4, vjust="inward", hjust="inward")
  } else {
    graph <- graph +
      geom_segment(aes(x=mean_diff, xend=mean_diff, y=0, yend=y_max * 1.05), size=1.5, color="#3182bd", na.rm=T) +
      geom_text(aes(label=sprintf("%.2f", mean_diff), x=mean_diff, y=y_max * (1.05 + (nrow * 0.05))), size=4, vjust="inward")
  }

  # add HDI
  graph <- graph +
    geom_segment(aes(x=hdi[1], xend=hdi[2], y=-(y_max * 0.01 * nrow), yend=-(y_max * 0.01 * nrow)), size=3, color="black", na.rm=T)

  if (hdi[1] < (x_min + hjust_range)) {
    graph <- graph +
      geom_text(aes(label=sprintf("%.2f", hdi[1]), x=hdi[1], y=-(y_max * (0.05 * nrow))), size=4, vjust="inward", hjust="inward")
  } else {
    graph <- graph +
      geom_text(aes(label=sprintf("%.2f", hdi[1]), x=hdi[1], y=-(y_max * (0.05 * nrow))), size=4, vjust="inward")
  }

  if (hdi[2] > (x_max - hjust_range)) {
    graph <- graph +
      geom_text(aes(label=sprintf("%.2f", hdi[2]), x=hdi[2], y=-(y_max * (0.05 * nrow))), size=4, vjust="inward", hjust="inward")
  } else {
    graph <- graph +
      geom_text(aes(label=sprintf("%.2f", hdi[2]), x=hdi[2], y=-(y_max * (0.05 * nrow))), size=4, vjust="inward")
  }

  # add ROPE interval?
  if (!is.null(rope)) {
    graph <- graph +
      geom_segment(aes(x=rope[1], xend=rope[2], y=y_max * 0.01 * nrow, yend=y_max * 0.01 * nrow), size=3, color="grey50", na.rm=T)
  }

  # style and labels
  graph <- graph +
    xlab("value")

  return(graph)
}
