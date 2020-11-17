# Load apothecary
devtools::load_all(".")

# Set seed
set.seed(100)

# Source plotting function
source("plot_output_function.R")

# Comparison between stochastic and deterministic (base)
set.seed(10)
x <- run_apothecary(country = "France", hosp_bed_capacity = 100000, ICU_bed_capacity = 1000,
                    day_return = TRUE, model = "stochastic", dt = 0.05,
                    prop_ox_hosp_beds = 0.25, prop_ox_ICU_beds = 0.25, MV_capacity = 1000000)
x_index <- apothecary:::odin_index(x$model)

set.seed(10)
y <- run_apothecary(country = "France", hosp_bed_capacity = 100000, ICU_bed_capacity = 1000,
                    day_return = TRUE, model = "deterministic", dt = 1,
                    prop_ox_hosp_beds = 0.25, prop_ox_ICU_beds = 0.25, MV_capacity = 1000000)
y_index <- apothecary:::odin_index(y$model)

plot_output(x, y, base_check)

# Comparison between stochastic and deterministic (drugs)
set.seed(10)
x <- run_apothecary(country = "France", hosp_bed_capacity = 10000000, ICU_bed_capacity = 10000000,
                    day_return = TRUE, model = "stochastic", dt = 0.05,
                    prop_ox_hosp_beds = 0.25, prop_ox_ICU_beds = 0.25, MV_capacity = 1000000,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 1)
x_index <- apothecary:::odin_index(x$model)

set.seed(10)
y <- run_apothecary(country = "France", hosp_bed_capacity = 10000000, ICU_bed_capacity = 10000000,
                    day_return = TRUE, model = "deterministic", dt = 1,
                    prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 1000000,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.5,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.5,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.5)
y_index <- apothecary:::odin_index(y$model)

plot_output(x, y, base_check)



plot(round(x$output[, x_index$current_prop_ox_hosp_beds, 1] * x$output[, x_index$current_hosp_bed_capacity, 1]))
lines(round(y$output[, y_index$current_prop_ox_hosp_beds] * y$output[, y_index$current_hosp_bed_capacity]))

plot(apply(x$output[, x_index$n_IMod_GetHosp_GetOx_Surv2_R, 1], 1, sum)/0.05)
lines(apply(y$output[, y_index$n_IMod_GetHosp_GetOx_Surv2_R], 1, sum))

plot(apply(x$output[, x_index$n_IMod_GetHosp_GetOx_Die2_D_Hospital, 1], 1, sum)/0.05)
lines(apply(y$output[, y_index$n_IMod_GetHosp_GetOx_Die2_D_Hospital], 1, sum))

plot(x$output[, x_index$hosp_bed_ox_occ, 1])
lines(y$output[, y_index$hosp_bed_ox_occ])

z <- round(x$output[, x_index$current_prop_ox_hosp_beds, 1] * x$output[, x_index$current_hosp_bed_capacity, 1]) +
  apply(x$output[, x_index$n_IMod_GetHosp_GetOx_Surv2_R, 1], 1, sum)/0.05 + apply(x$output[, x_index$n_IMod_GetHosp_GetOx_Die2_D_Hospital, 1], 1, sum) -
  x$output[, x_index$hosp_bed_ox_occ, 1]
min(z)
plot(z, ylab = "")
lines(x$output[, x_index$current_free_hosp_bed_ox, 1], col = "red", type = "l")

a <- round(y$output[, y_index$current_prop_ox_hosp_beds] * y$output[, y_index$current_hosp_bed_capacity]) +
  apply(y$output[, y_index$n_IMod_GetHosp_GetOx_Surv2_R], 1, sum) + apply(y$output[, y_index$n_IMod_GetHosp_GetOx_Die2_D_Hospital], 1, sum) -
  y$output[, y_index$hosp_bed_ox_occ]
min(a)
plot(a, ylab = "")
lines(y$output[, y_index$current_free_hosp_bed_ox], col = "red", type = "l")



current_free_hosp_bed_ox <- round(current_prop_ox_hosp_beds * current_hosp_bed_capacity) +
  sum(n_IMod_GetHosp_GetOx_Surv2_R) + sum(n_IMod_GetHosp_GetOx_Die2_D_Hospital) - hosp_bed_ox_occ


plot(apply(y$output[, y_index$ISev_GetICU_GetOx_Die1], 1, sum), type = "l")
which(apply(y$output[, y_index$ISev_GetICU_GetOx_Die1], 1, sum) == max(apply(y$output[, y_index$ISev_GetICU_GetOx_Die1], 1, sum)))

plot(x$output[, x_index$time, 1], x$output[, x_index$hosp_bed_ox_occ, 1], col = "black", type = "l")
lines(y$output[, y_index$time], y$output[, y_index$hosp_bed_ox_occ], col = "red", type = "l")
min(x$output[, x_index$hosp_bed_ox_occ, 1])
min(y$output[, y_index$hosp_bed_ox_occ])

plot(x$output[, x_index$time, 1], x$output[, x_index$ICU_bed_ox_occ, 1], col = "black", type = "l")
lines(y$output[, y_index$time], y$output[, y_index$ICU_bed_ox_occ], col = "red", type = "l")
min(x$output[, x_index$hosp_bed_ox_occ, 1])
min(y$output[, y_index$hosp_bed_ox_occ])


par(mfrow = c(1, 2))
plot(x$output[, x_index$time, 1], x$output[, x_index$current_free_ICU_bed_ox, 1], col = "black", type = "l")
lines(y$output[, y_index$time], y$output[, y_index$current_free_ICU_bed_ox], col = "red", type = "l")
min(x$output[, x_index$current_free_ICU_bed_ox, 1])
min(y$output[, y_index$current_free_ICU_bed_ox])

plot(x$output[, x_index$time, 1], x$output[, x_index$current_free_hosp_bed_ox, 1], col = "black", type = "l")
lines(y$output[, y_index$time], y$output[, y_index$current_free_hosp_bed_ox], col = "red", type = "l")
min(x$output[, x_index$current_free_hosp_bed_ox, 1])
min(y$output[, y_index$current_free_hosp_bed_ox])

plot(x$output[, x_index$time, 1], x$output[, x_index$current_free_ICU_bed_no_ox, 1], col = "black", type = "l")
lines(y$output[, y_index$current_free_ICU_bed_no_ox], col = "red", type = "l")
min(x$output[, x_index$current_free_ICU_bed_no_ox, 1])
min(y$output[, y_index$current_free_ICU_bed_no_ox])



plot(x$output[, x_index$time, 1], x$output[, x_index$current_free_hosp_bed_no_ox, 1], col = "black", type = "l")
lines(y$output[, y_index$current_free_hosp_bed_no_ox], col = "red", type = "l")
min(x$output[, x_index$current_free_hosp_bed_no_ox, 1])
min(y$output[, y_index$current_free_hosp_bed_no_ox])


plot_output(x, y, base_check)



par(mfrow = c(1, 1))
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$S, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$S], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$E1, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$E1], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$D_Community, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$D_Community], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$D_Hospital, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$D_Hospital], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$R, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$R], 1, sum), col = "red", type = "l")

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

tictoc::tic()
for (i in 1:100) {
  y <- run_apothecary(country = "France", hosp_bed_capacity = 10000000, ICU_bed_capacity = 10000000,
                      day_return = TRUE, model = "deterministic", dt = 1,
                      prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 1000000,
                      drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.5,
                      drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.5,
                      drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.5)
}
tictoc::toc()

