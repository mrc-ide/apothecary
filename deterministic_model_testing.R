# Load apothecary
devtools::load_all(".")

# Set seed
set.seed(100)

# Graphical parameters
par(mfrow = c(1, 1))

# Run deterministic model
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




plot(apply(results[, index$D_Hospital], 1, sum), col = "red")
plot(apply(results[, index$D_Community], 1, sum), col = "red", type = "l")



x$raw_parameters$tt_beta

plot(results_no_drugs[, index$pop])


plot(apply(results_no_drugs[, index$D_Hospital], 1, sum), col = "red", type = "l")
plot(apply(results_no_drugs[, index$R], 1, sum), col = "red", type = "l")
plot(apply(results_no_drugs[, index$S], 1, sum), col = "red", type = "l")



# Run stochastic model
# x_no_drugs <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, day_return = TRUE, dt = 0.01, model = "stochastic")
# results_no_drugs <- x_no_drugs$output
# index <- apothecary:::odin_index(x_no_drugs$model)
# plot(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)/max(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)),
#      type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day")
