# Loading apothecary
devtools::load_all()

# Comparing base apothecary to base squire and ensuring they match up
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
index <- squire:::odin_index(x$model)
D_apo <- max(apply(x$output[, index$D], 1, sum))
R_apo <- max(apply(x$output[, index$R], 1, sum))
100 * D_apo/(D_apo + R_apo)
100 * (D_apo + R_apo)/sum(x$parameters$population)

y <- squire:::run_explicit_SEEIR_model(country = "United Kingdom", R0 = 2, day_return = TRUE,
                                       hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000, replicates = 1,
                                       dt = 0.02)
index <- squire:::odin_index(y$model)
D_squ <- max(apply(y$output[, index$D, 1], 1, sum))
R_squ <- max(apply(y$output[, index$R, 1], 1, sum))
100 * D_squ/(D_squ + R_squ)
100 * (D_squ + R_squ)/sum(x$parameters$population)

# Checking Drug 2 Effect
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_2_indic_IPreAsymp = 1, drug_2_indic_IPreMild = 1, drug_2_indic_IPreCase = 1,
                    drug_2_prop_treat = 1, drug_2_effect_size = 1.5)
index <- squire:::odin_index(x$model)
max(apply(x$output[, index$D], 1, sum))
max(apply(y$output[, index$D], 1, sum))
max(apply(x$output[, index$R], 1, sum))
max(apply(y$output[, index$R], 1, sum))
x$parameters$gamma_IPreAsymp
y$parameters$gamma_IPreAsymp
x$output[, index$gamma_IPreAsymp_Drug_2]
y$output[, index$gamma_IPreAsymp_Drug_2]

# Checking Drug 3 Effect
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = 0.5)
index <- squire:::odin_index(x$model)
max(apply(x$output[, index$D], 1, sum))
max(apply(y$output[, index$D], 1, sum))

# Checking Drug 4 Effect
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                    drug_4_prop_treat = 1, drug_4_effect_size = 1.5)
index <- squire:::odin_index(x$model)
max(apply(x$output[, index$D], 1, sum))
max(apply(y$output[, index$D], 1, sum))
max(apply(x$output[, index$R], 1, sum))
max(apply(y$output[, index$R], 1, sum))
x$parameters$gamma_IAsymp
y$parameters$gamma_IAsymp
x$output[, index$gamma_IAsymp_Drug_4]
y$output[, index$gamma_IAsymp_Drug_4]

# Checking Drug 6 Effect (working but lower impact than perhaps expected - due to triaging patterns
# and elderly patients already not really being admitted to the ICU anyway and having high hosp bed mortality)
# (downstream checking only works with effect_size = 1)
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = 1)
index <- squire:::odin_index(x$model)
D_no <- max(apply(x$output[, index$D], 1, sum))
D_drug <- max(apply(y$output[, index$D], 1, sum))
sum(x$output[, c(index$number_ISev_GetICU_GetOx, index$number_ISev_GetICU_NoOx,
                 index$number_ICrit_GetICU_GetOx_GetMV, index$number_ICrit_GetICU_GetOx_NoMV, index$number_ICrit_GetICU_NoOx_NoMV)])
sum(y$output[, c(index$number_ISev_GetICU_GetOx, index$number_ISev_GetICU_NoOx,
                 index$number_ICrit_GetICU_GetOx_GetMV, index$number_ICrit_GetICU_GetOx_NoMV, index$number_ICrit_GetICU_NoOx_NoMV)])
sum(x$output[, c(index$number_IMod_GetHosp_GetOx, index$number_IMod_GetHosp_NoOx)])
sum(y$output[, c(index$number_IMod_GetHosp_GetOx, index$number_IMod_GetHosp_NoOx)])
ICU_avert <- apply(x$output[, index$number_ISev_GetICU_GetOx], 2, sum) + apply(x$output[, index$number_ISev_GetICU_NoOx], 2, sum) +
  apply(x$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) + apply(x$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) +
  apply(x$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
deaths_no_drug <- (probs$prob_critical * ICU_avert * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline) +
                  ((1-probs$prob_critical) * ICU_avert * probs$prob_severe_death_get_ICU_get_ox_baseline) -
                  (ICU_avert * probs$prob_moderate_death_get_hosp_get_ox_baseline)
sum(deaths_no_drug)
D_no - D_drug

# Checking Drug 7 Effect (downstream checking only works with effect_size = 1)
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_7_indic = 1, drug_7_prop_treat = 1, drug_7_effect_size = 1)
index <- squire:::odin_index(x$model)
D_no <- max(apply(x$output[, index$D], 1, sum))
D_drug <- max(apply(y$output[, index$D], 1, sum))
sum(x$output[, c(index$number_ICrit_GetICU_GetOx_GetMV, index$number_ICrit_GetICU_GetOx_NoMV, index$number_ICrit_GetICU_NoOx_NoMV)])
sum(y$output[, c(index$number_ICrit_GetICU_GetOx_GetMV, index$number_ICrit_GetICU_GetOx_NoMV, index$number_ICrit_GetICU_NoOx_NoMV)])
sum(x$output[, c(index$number_ISev_GetICU_GetOx, index$number_ISev_GetICU_NoOx)])
sum(y$output[, c(index$number_ISev_GetICU_GetOx, index$number_ISev_GetICU_NoOx)])
D_no - D_drug
crit_avert <- apply(x$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) + apply(x$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) +
  apply(x$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
deaths_no_drug <- (crit_avert * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline) -
                  (crit_avert * probs$prob_severe_death_get_ICU_get_ox_baseline)
sum(deaths_no_drug)
D_no - D_drug

# Checking Drug 8, 9, 10 Effect
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                    drug_8_GetOx_effect_size = 2, drug_8_NoOx_effect_size = 2,
                    drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = 1,
                    drug_9_GetOx_effect_size = 2, drug_9_NoOx_effect_size = 2,
                    drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = 1,
                    drug_10_GetOx_GetMV_effect_size = 2, drug_10_GetOx_NoMV_effect_size = 2, drug_10_NoOx_NoMV_effect_size = 2)
index <- squire:::odin_index(x$model)
D_no <- max(apply(x$output[, index$D], 1, sum))
D_drug <- max(apply(y$output[, index$D], 1, sum))

plot(x$output[, index$hosp_bed_full_treat_occ])
lines(y$output[, index$hosp_bed_full_treat_occ])

plot(apply(x$output[, index$IMod_GetHosp_GetOx_Surv2], 1, sum), type = "l")
lines(apply(y$output[, index$IMod_GetHosp_GetOx_Surv2], 1, sum),  type = "l", col = "red")
plot(apply(x$output[, index$ISev_GetICU_GetOx_Surv2], 1, sum), type = "l")
lines(apply(y$output[, index$ISev_GetICU_GetOx_Surv2], 1, sum),  type = "l", col = "red")
plot(apply(x$output[, index$ICrit_GetICU_GetOx_GetMV_Surv2], 1, sum), type = "l")
lines(apply(y$output[, index$ICrit_GetICU_GetOx_GetMV_Surv2], 1, sum),  type = "l", col = "red")

# Checking Drug 11, 12, 13 Effect
x <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000)
y <- run_apothecary(country = "United Kingdom", R0 = 2, day_return = TRUE,
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5, drug_13_NoOx_NoMV_effect_size = 0.5)
index <- squire:::odin_index(x$model)
D_no <- max(apply(x$output[, index$D], 1, sum))
D_drug <- max(apply(y$output[, index$D], 1, sum))
