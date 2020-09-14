plot_output <- function(stoch, det, variables) {
  par(mfrow = c(5, 13), mar = c(2, 2, 2, 2))
  for (i in 1:length(base_check)) {

    var <- base_check[i]
    x_indices <- x_index[[which(names(x_index) == var)]]
    y_indices <- y_index[[which(names(y_index) == var)]]

    if (nchar(var) > 10) {
      title_size <- 0.75
    } else {
      title_size <- 1.25
    }

    if (length(x_indices) == 1) {
      if (var == "oxygen_needed_overall" | var == "oxygen_used" | var == "oxygen_availability") {
        ymax <- max(c(max(x$output[, x_indices, 1]/dt), max(y$output[, y_indices])))
        plot(x$output[, x_index$time, 1], x$output[, x_indices, 1]/dt, main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
             ylim = c(0, ymax))
        lines(y$output[, y_indices], col = "red", type = "l")
      } else {
        ymax <- max(c(max(x$output[, x_indices, 1]), max(y$output[, y_indices])))
        plot(x$output[, x_index$time, 1], x$output[, x_indices, 1], main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
             ylim = c(0, ymax))
        lines(y$output[, y_indices], col = "red", type = "l")
      }
    } else {
      ymax <- max(c(max(apply(x$output[, x_indices, 1], 1, sum)), max(apply(y$output[, y_indices], 1, sum))))
      plot(x$output[, x_index$time, 1], apply(x$output[, x_indices, 1], 1, sum), main = var,
           col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
           ylim = c(0, ymax))
      lines(apply(y$output[, y_indices], 1, sum), col = "red", type = "l")
    }
  }
}

