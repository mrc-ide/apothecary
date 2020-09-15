# Load apothecary
devtools::load_all(".")

# Load squire
library(squire)

# Set seed
set.seed(100)

# Source plotting function
source("plot_output_function.R")

# Comparison with squire
set.seed(10)
x <- run_apothecary(country = "France", hosp_bed_capacity = 0, ICU_bed_capacity = 0,
                    day_return = TRUE, model = "stochastic", dt = 0.1,
                    prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 1000000)
x_index <- apothecary:::odin_index(x$model)

set.seed(10)
y <- run_explicit_SEEIR_model(country = "France", hosp_bed_capacity = 0, ICU_bed_capacity = 0,
                              day_return = TRUE, dt = 0.1)
y_index <- apothecary:::odin_index(y$model)

par(mfrow = c(1, 1))
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$S, 1], 1, sum), col = "black", type = "l")
lines(y$output[, y_index$time, 1], apply(y$output[, y_index$S, 1], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$E1, 1], 1, sum), col = "black", type = "l")
lines(y$output[, y_index$time, 1], apply(y$output[, y_index$E1, 1], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$D_Community, 1], 1, sum), col = "black", type = "l")
lines(y$output[, y_index$time, 1], apply(y$output[, y_index$D_not_get, 1], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$D_Hospital, 1], 1, sum), col = "black", type = "l")
lines(y$output[, y_index$time, 1], apply(y$output[, y_index$D_get, 1], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$R, 1], 1, sum), col = "black", type = "l")
lines(y$output[, y_index$time, 1], apply(y$output[, y_index$R, 1], 1, sum), col = "red", type = "l")

# Running stochastic model
x <- run_apothecary(country = "France", hosp_bed_capacity = 10000, ICU_bed_capacity = 1000,
                    day_return = TRUE, model = "stochastic", dt = 0.1,
                    prop_ox_hosp_beds = 0.5, prop_ox_ICU_beds = 0.5,
                    MV_capacity = 100)
x_index <- apothecary:::odin_index(x$model)

par(mfrow = c(5, 13), mar = c(2, 2, 2, 2))
for (i in 1:length(base_check)) {
  var <- base_check[i]
  x_indices <- x_index[[which(names(x_index) == var)]]

  if (nchar(var) > 10) {
    title_size <- 0.65
  } else {
    title_size <- 1.25
  }

  if (length(x_indices) == 1) {
    plot(x$output[, x_index$time, 1], x$output[, x_indices, 1], main = var,
         col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size)
  } else {
    plot(x$output[, x_index$time, 1], apply(x$output[, x_indices, 1], 1, sum), main = var,
         col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size)
  }
}


