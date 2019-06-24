# function for printing the difference between two datasets
difference <- function(y1=NULL, y2=NULL, rope=NULL, group1=1, group2=2, y_diff=NULL, max_diff=NULL) {
  if (is.null(y_diff)) {
    y_diff <- y1 - y2
  }

  # max diff
  if (!is.null(max_diff)) {
    y_diff[y_diff > max_diff] <- max_diff
    y_diff[y_diff < -max_diff] <- -max_diff
  }

  n <- length(y_diff)

  result <- vector()

  if (is.null(rope)) {
    # 1 > 2
    diff_smaller <- y_diff < 0
    y1_smaller <- round(sum(diff_smaller) / n, 2)
    diff_larger <- y_diff > 0
    y1_larger <- round(sum(diff_larger) / n, 2)
    cat(sprintf("Probabilities:\n  - Group %d < Group %d: %.2f +/- %.5f",
                group1, group2,
                y1_smaller, mcmcse::mcse(diff_smaller)$se))
    cat(sprintf("\n  - Group %d > Group %d: %.2f +/- %.5f",
                group1, group2,
                y1_larger, mcmcse::mcse(diff_larger)$se))

    result <- c(y1_smaller, y1_larger, NA)
  } else {
    # 1 > 2
    diff_smaller <- y_diff < rope[1]
    y1_smaller <- round(sum(diff_smaller) / n, 2)
    cat(sprintf("Probabilities:\n  - Group %d < Group %d: %.2f +/- %.5f",
                group1, group2,
                y1_smaller, mcmcse::mcse(diff_smaller)$se))

    # 2 > 1
    diff_larger <- y_diff > rope[2]
    y1_larger <- round(sum(diff_larger) / n, 2)
    cat(sprintf("\n  - Group %d > Group %d: %.2f +/- %.5f",
                group1, group2,
                y1_larger, mcmcse::mcse(diff_larger)$se))

    # equal
    diff_equal <- y_diff > rope[1] & y_diff < rope[2]
    equal <- 1 - y1_smaller - y1_larger
    cat(sprintf("\n  - Equal: %.2f +/- %.5f",
                equal, mcmcse::mcse(diff_equal)$se))

    result <- c(y1_smaller, y1_larger, equal)
  }

  hdi <- mcmc_hdi(y_diff)
  y_diff_l <- stats::quantile(hdi[1], 0.025)
  y_diff_h <- stats::quantile(hdi[2], 0.975)
  cat(sprintf("\n95%% HDI:\n  - Group 1 - Group 2: [%.2f, %.2f]\n", y_diff_l, y_diff_h))

  return(result)
}

# function for determining probability that a particular vector of data is the smallest/largest
is_smallest_or_largest <- function(data, rope=NULL) {
  # get length of the shortest vector
  n_min <- .Machine$integer.max
  for (y in data) {
    if (length(y) < n_min)
      n_min <- length(y)
  }

  # calculate probabilities
  n <- length(data)
  largest <- rep(0, times=n)
  smallest <- rep(0, times=n)
  equal <- 0
  for (i in 1:n_min) {
    y <- vector()
    for (j in 1:n) {
      y <- c(y, data[[j]][i])
    }

    if (is.null(rope)) {
      # find the smallest/largest one
      largest_group <- which(y == max(y))
      largest[largest_group] <- largest[largest_group] + 1
      smallest_group <- which(y == min(y))
      smallest[smallest_group] <- smallest[smallest_group] + 1
    }
    else {
      max_diff <- max(y) - min(y)
      # are all equal?
      if (max_diff > rope[1] & max_diff < rope[2]) {
        equal <- equal + 1
      } else {
        largest_group <- which(y == max(y))
        largest[largest_group] <- largest[largest_group] + 1
        smallest_group <- which(y == min(y))
        smallest[smallest_group] <- smallest[smallest_group] + 1
      }
    }
  }

  # normalize
  largest <- largest / n_min
  smallest <- smallest / n_min
  equal <- equal / n_min

  # save to df
  result <- data.frame(largest=numeric(), smallest=numeric(), equal=numeric())
  i <- 1
  for (i in 1:n) {
    result <- rbind(result, data.frame(largest=largest[i], smallest=smallest[i], equal=equal))
  }

  return(result)
}

# shift circular data, shift according to base if provided
preprocess_circular <- function(y, base=NULL) {
  # if mean difference is around 0 use a -pi .. pi interval
  # else use 0..2pi
  suppressWarnings(mean_y <- mean(circular::as.circular(y)))
  if (!is.null(base)) {
    suppressWarnings(mean_y <- mean(circular::as.circular(base)))
  }

  mean_y <- as.numeric(mean_y)

  if (mean_y < pi/2 & mean_y > -pi/2) {
    y[y > pi] <- y[y > pi] - 2*pi
    y[y < -pi] <- y[y < -pi] + 2*pi
  } else if (mean_y < -pi/2) {
    y[y > 0] <- y[y > 0] - 2*pi
  } else {
    y[y < 0] <- y[y < 0] + 2*pi
  }

  return(y)
}

# function for printing the difference between two circular datasets
circular_difference <- function(y1, y2, rope=NULL) {
  y_diff <- y1 - y2

  y_diff <- as.numeric(preprocess_circular(y_diff))

  # cast to -pi .. pi interval
  y_diff[y_diff > pi] <- pi - y_diff[y_diff > pi]
  y_diff[y_diff < -pi] <- y_diff[y_diff < -pi] + pi

  result <- difference(y_diff=y_diff, rope=rope)

  return(result)
}


# cast the rope interval into the format used throughout the whole library
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


# hsv2rgb conversion
# http://www.easyrgb.com/en/math.php
hsv2rgb <- function(hues, saturations, values) {
  n <- length(hues)
  colors <- NULL
  colors <- matrix(nrow = 3, ncol=0, dimnames = list(c("r","g","b")))

  for (i in 1:n) {
    h <- hues[i] / (2 * pi)
    s <- saturations[i]
    v <- values[i]

    if (s == 0) {
      r = as.integer(v * 255)
      g = as.integer(v * 255)
      b = as.integer(v * 255)
    } else {
      h <- h * 6

      if (h == 6) {
        h = 0
      }

      i <- floor(h)

      v1 <- v * (1 - s)
      v2 <- v * (1 - s * (h - i))
      v3 <- v * (1 - s * (1 - (h - i)))

      if (i == 0) {
        r = v
        g = v3
        b = v1
      }
      else if (i == 1) {
        r = v2
        g = v
        b = v1
      }
      else if (i == 2) {
        r = v1
        g = v
        b = v3
      }
      else if (i == 3) {
        r = v1
        g = v2
        b = v
      }
      else if (i == 4) {
        r = v3
        g = v1
        b = v
      }
      else {
        r = v
        g = v1
        b = v2
      }

      r <- as.integer(r * 255)
      g <- as.integer(g * 255)
      b <- as.integer(b * 255)
    }

    colors <- cbind(colors, c(r,g,b))
  }

  return(colors)
}

# clamp vector between 2 values
clamp <- function(y, min, max) {
  y[y < min] <- min
  y[y > max] <- max
  return(y)
}

#' @title mcmc_hdi
#' @author John Kruschke
#' @description A function for calculating the HDI (highest density interval) of a vector of values.
#' @export
#' @param samples vector of values.
#' @param cred_mass credibility mass that the interval should include (default = 0.95).
#' @return Boundaries of the HDI.
mcmc_hdi <- function(samples, cred_mass=0.95) {
  samples <- sort(samples)
  ci_ceil <- ceiling(cred_mass * length(samples))
  n <- length(samples) - ci_ceil

  ci_width <- rep(0, n)
  for (i in 1:n) {
    ci_width[i] <- samples[i + ci_ceil] - samples[i]
  }

  hdi_min <- samples[which.min(ci_width)]
  hdi_max <- samples[which.min(ci_width) + ci_ceil]
  hdi <- c(hdi_min, hdi_max)
  return(hdi)
}
