# Figure 2C - Scanning Over Different Bed Levels
hosp_beds <- c(100, 1000, 5000, 10000, 20000, 30000, 50000, 80000, 100000, 300000, 500000, 1000000)
ICU_beds <- hosp_beds/5
low_prop <- c()
high_prop <- c()

for (i in 1:length(hosp_beds)) {

  lowR0_no_drug <- run_apothecary(country = demog_pars_lowR0$country, R0 = demog_pars_lowR0$R0,
                                  population = demog_pars_lowR0$population, contact_matrix_set = demog_pars_lowR0$matrix,
                                  time_period = 365, seeding_cases = demog_pars_lowR0$seeding_cases,
                                  day_return = TRUE,
                                  hosp_bed_capacity = hosp_beds[i], ICU_bed_capacity = ICU_beds[i],
                                  prop_ox_hosp_beds = 0.6, prop_ox_ICU_beds = 0.8,
                                  MV_capacity = ICU_beds[i] * 0.5)
  lowR0_drug <- run_apothecary(country = demog_pars_lowR0$country, R0 = demog_pars_lowR0$R0,
                               population = demog_pars_lowR0$population, contact_matrix_set = demog_pars_lowR0$matrix,
                               time_period = 365, seeding_cases = demog_pars_lowR0$seeding_cases,
                               day_return = TRUE,
                               hosp_bed_capacity = hosp_beds[i], ICU_bed_capacity = ICU_beds[i],
                               prop_ox_hosp_beds = 0.6, prop_ox_ICU_beds = 0.8,
                               MV_capacity = ICU_beds[i] * 0.5,
                               drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                               drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                               drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
                               drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 1,
                               drug_11_GetOx_effect_size = 0.82,
                               drug_11_NoOx_effect_size = 0.82 + 0.5 * (1 - 0.82),
                               drug_12_GetOx_effect_size = 0.64,
                               drug_12_NoOx_effect_size = 0.64 + 0.5 * (1 - 0.64),
                               drug_13_GetOx_GetMV_effect_size = 0.64,
                               drug_13_GetOx_NoMV_effect_size = 1,
                               drug_13_NoOx_NoMV_effect_size = 1)

  low_IFR_no_drug <- 100 * max(apply(lowR0_no_drug$output[, index$D], 1, sum))/
    (max(apply(lowR0_no_drug$output[, index$D], 1, sum)) + max(apply(lowR0_no_drug$output[, index$R], 1, sum)))
  low_IFR_drug <- 100 * max(apply(lowR0_drug$output[, index$D], 1, sum))/
    (max(apply(lowR0_drug$output[, index$D], 1, sum)) + max(apply(lowR0_drug$output[, index$R], 1, sum)))
  low_prop[i] <- 100 * (low_IFR_no_drug - low_IFR_drug)/low_IFR_no_drug

  highR0_no_drug <- run_apothecary(country = demog_pars_highR0$country, R0 = demog_pars_highR0$R0,
                                   population = demog_pars_highR0$population, contact_matrix_set = demog_pars_highR0$matrix,
                                   time_period = 365, seeding_cases = demog_pars_highR0$seeding_cases,
                                   day_return = TRUE,
                                   hosp_bed_capacity = hosp_beds[i], ICU_bed_capacity = ICU_beds[i],
                                   prop_ox_hosp_beds = 0.6, prop_ox_ICU_beds = 0.8,
                                   MV_capacity = ICU_beds[i] * 0.5)
  highR0_drug <- run_apothecary(country = demog_pars_highR0$country, R0 = demog_pars_highR0$R0,
                                population = demog_pars_highR0$population, contact_matrix_set = demog_pars_highR0$matrix,
                                time_period = 365, seeding_cases = demog_pars_highR0$seeding_cases,
                                day_return = TRUE,
                                hosp_bed_capacity = hosp_beds[i], ICU_bed_capacity = ICU_beds[i],
                                prop_ox_hosp_beds = 0.6, prop_ox_ICU_beds = 0.8,
                                MV_capacity = ICU_beds[i] * 0.5,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 1,
                                drug_11_GetOx_effect_size = 0.82,
                                drug_11_NoOx_effect_size = 0.82 + 0.5 * (1 - 0.82),
                                drug_12_GetOx_effect_size = 0.64,
                                drug_12_NoOx_effect_size = 0.64 + 0.5 * (1 - 0.64),
                                drug_13_GetOx_GetMV_effect_size = 0.64,
                                drug_13_GetOx_NoMV_effect_size = 1,
                                drug_13_NoOx_NoMV_effect_size = 1)

  high_IFR_no_drug <- 100 * max(apply(highR0_no_drug$output[, index$D], 1, sum))/
    (max(apply(highR0_no_drug$output[, index$D], 1, sum)) + max(apply(highR0_no_drug$output[, index$R], 1, sum)))
  high_IFR_drug <- 100 * max(apply(highR0_drug$output[, index$D], 1, sum))/
    (max(apply(highR0_drug$output[, index$D], 1, sum)) + max(apply(highR0_drug$output[, index$R], 1, sum)))
  high_prop[i] <- 100 * (high_IFR_no_drug - high_IFR_drug)/high_IFR_no_drug
  print(i)
}

plot(log(ICU_beds), low_prop/max(low_prop), type = "l")
lines(log(ICU_beds), high_prop/max(high_prop))
