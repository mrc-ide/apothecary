# Load apothecary
devtools::load_all(".")

# Set seed
set.seed(100)

# Graphical parameters
par(mfrow = c(1, 2))

# Run without drugs but unlimited healthcare capacity
x_no_drugs <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000,
                             day_return = TRUE, model = "stochastic", dt = 0.1)
results_no_drugs <- x_no_drugs$output
index <- apothecary:::odin_index(x_no_drugs$model)
plot(apply(results_no_drugs[, index$S, 1], 1, sum)/max(apply(results_no_drugs[, index$S, 1], 1, sum)),
     type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day")
plot(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)/max(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)),
     type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day")

x_no_drugs <- run_apothecary(country = "France", hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, day_return = TRUE)
results_no_drugs <- x_no_drugs$output
index <- apothecary:::odin_index(x_no_drugs$model)
plot(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)/max(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)),
     type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day")

# Run without drugs and unlimited healthcare capacity
x_drugs <- run_apothecary(country = "France", hosp_beds = 100000000, ICU_beds = 10000000,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.7, drug_11_prop_treat = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.7, drug_12_prop_treat = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.7, drug_13_prop_treat = 1,
                    day_return = TRUE)
results_drugs <- x_drugs$output
lines(apply(results_drugs[, index$D_Hospital, 1], 1, sum)/max(apply(results_no_drugs[, index$D_Hospital, 1], 1, sum)),
      col = "red", lwd = 3)

# Run without drugs but limited healthcare capacity
set.seed(100)
x_no_drugs_2 <- run_apothecary(country = "France", hosp_beds = 500000, ICU_beds = 200000, day_return = TRUE)
results_no_drugs_2 <- x_no_drugs_2$output
plot((apply(results_no_drugs_2[, index$D_Hospital, 1], 1, sum) + apply(results_no_drugs_2[, index$D_Community, 1], 1, sum))/max(apply(results_no_drugs_2[, index$D_Hospital, 1], 1, sum) + apply(results_no_drugs_2[, index$D_Community, 1], 1, sum)),
     type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day")

# Run with drugs but limited healthcare capacity
set.seed(100)
x_drugs_2 <- run_apothecary(country = "France", hosp_beds = 500000, ICU_beds = 200000,
                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.7, drug_11_prop_treat = 1,
                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.7, drug_12_prop_treat = 1,
                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.7, drug_13_prop_treat = 1,
                            day_return = TRUE)
results_drugs_2 <- x_drugs_2$output
lines((apply(results_drugs_2[, index$D_Hospital, 1], 1, sum) + apply(results_drugs_2[, index$D_Community, 1], 1, sum))/max(apply(results_no_drugs_2[, index$D_Hospital, 1], 1, sum) + apply(results_no_drugs_2[, index$D_Community, 1], 1, sum)),
       type = "l", lwd = 3, col = "red")

