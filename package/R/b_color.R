#' @title b_color
#' @description Bayesian model for comparing colors.
#' @import rstan
#' @export
#' @param colors a data frame of colors either in RGB or HSV format. The first column should be the R (or H) component, the second column should be the G (or S) component, and the third columne should be the B (or V) component.
#' @param hsv set to TRUE if colors are provided in HSV format (default = FALSE).
#' @param warmup Integer specifying the number of warmup iterations per chain (default = 2000).
#' @param iter Integer specifying the number of iterations (including warmup, default = 3000).
#' @return An object of class `color_class`
b_color <- function(colors, hsv=FALSE, warmup=2000, iter=3000) {

  n <- length(colors)

  if (!hsv) {
    # extract rgb
    r <- colors[,1]
    g <- colors[,2]
    b <- colors[,3]

    # cast to hsv
    colors[c("h", "s", "v")] <- with(colors, t(rgb2hsv(r, g, b, maxColorValue=255)))
    colors$h <- colors$h * 2 * pi
    h <- colors$h
    s <- colors$s
    v <- colors$v
  } else {
    # extract hsv
    h <- colors[,1]
    s <- colors[,2]
    v <- colors[,3]

    # cast to rgb
    colors[c("r", "g", "b")] <- with(colors, t(hsv2rgb(h, s, v)))
    r <- colors$r
    g <- colors$g
    b <- colors$b
  }

  stan_data <- list(n = n,
                    r = r,
                    g = g,
                    b = b,
                    h = h,
                    s = s,
                    v = v)

  fit <- sampling(stanmodels$color,
                        data = stan_data,
                        iter = iter,
                        warmup = warmup,
                        chains = 1)

  extract <- extract(fit)

  # create output class
  out <- color_class(extract=extract, data=stan_data, fit=fit)

  # return
  return(out)
}
