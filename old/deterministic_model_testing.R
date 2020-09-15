# Load apothecary
devtools::load_all(".")
library(tictoc)

# Set seed
set.seed(100)

# Graphical parameters
par(mfrow = c(1, 1))

# Run deterministic model 100x to check time period
tic()
for (i in 1:100) {
  x <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000,
                      day_return = TRUE, dt = 1, model = "deterministic")
}
toc()

# Run base deterministic and stochastic model to check identical output
x <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, day_return = TRUE,
                    dt = 0.01, model = "deterministic")
det_results <- x$output
index <- apothecary:::odin_index(x$model)
plot(det_results[, index$time], apply(det_results[, index$S], 1, sum),
     col = "black", lwd = 2, type = "l", ylim = c(0, max(apply(det_results[, index$S], 1, sum))))

y <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, day_return = TRUE,
                    dt = 0.01, model = "stochastic", replicates = 1)
stoch_results <- y$output
index <- apothecary:::odin_index(y$model)
lines(stoch_results[, index$time, 1], apply(stoch_results[, index$S, 1], 1, sum), col = "red", xlim = c(0, 365), ylim = c(0, max(apply(stoch_results[, index$S, 1], 1, sum))))

# Checking Drug Property 1
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    day_return = FALSE, seeding_cases = 100, dt = 0.01,
                    drug_1_indic = 1, drug_1_effect_size = 0.75,
                    prophylactic_drug_timing_1 = 150,
                    prophylactic_drug_timing_2 = 200,
                    prophylactic_prop_treat = 0.5,
                    prophylactic_drug_wane = 0.00001, model = "stochastic")

y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_1_indic = 1, drug_1_effect_size = 0.75,
                    prophylactic_drug_timing_1 = 150,
                    prophylactic_drug_timing_2 = 200,
                    prophylactic_prop_treat = 0.5,
                    prophylactic_drug_wane = 0.00001, model = "deterministic", dt = 1)

x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_S_PS, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$n_S_PS], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$S, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$S], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$PS, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$PS], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_PS_S, 1], 1, sum)/x$raw_parameters$dt, col = "black", type = "l")
lines(apply(y$output[, y_index$n_PS_S], 1, sum), col = "red", type = "l")

sum(apply(x$output[, x_index$n_PS_S, 1], 1, sum))
sum(apply(y$output[, y_index$n_PS_S], 1, sum))

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_PS_PE1, 1], 1, sum)/x$raw_parameters$dt, col = "black", type = "l")
lines(apply(y$output[, y_index$n_PS_PE1], 1, sum), col = "red", type = "l")

sum(apply(x$output[, x_index$n_PS_PE1, 1], 1, sum))
sum(apply(y$output[, y_index$n_PS_PE1], 1, sum))

plot(x$output[, x_index$time, 1], x$output[, x_index$pop, 1], col = "black", type = "l")
lines(y$output[, y_index$pop], col = "red", type = "l")

