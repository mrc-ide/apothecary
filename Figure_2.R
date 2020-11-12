# Loading required libraries and sourcing relevant functions
library(mvtnorm); library(tictoc); library(ggplot2); library(dplyr); library(tidyr)
source("analyses/Paper_Functions.R")

# Set seed
set.seed(1000)

# Generating Draws from Dexamethasone Mortality Uncertainty
num_draws <- 1000
mean_dexy_mod_mort <- 0.82 # source = RECOVERY Trial
sd_dexy_mod_mort <- max(mean_dexy_mod_mort - 0.72, 0.94 - mean_dexy_mod_mort) / 1.95996
mean_dexy_ICU_mort <- 0.64 # source = JAMA Meta-Analysis, Dexamethasone Specific Estimate
sd_dexy_ICU_mort <- max(mean_dexy_ICU_mort - 0.50, 0.82 - mean_dexy_ICU_mort) / 1.95996

dexy_mort_corr <- 0.999
dexy_mort_cov <- dexy_mort_corr * sd_dexy_mod_mort * sd_dexy_ICU_mort
dexy_mort_effect <- mvtnorm::rmvnorm(n = num_draws, mean = c(mean_dexy_mod_mort, mean_dexy_ICU_mort),
                                     sigma = matrix(c(sd_dexy_mod_mort^2, dexy_mort_cov,
                                                      dexy_mort_cov, sd_dexy_ICU_mort^2), nrow = 2))

# Generating Draws for Remdesivir Mortality and Duration Uncertainty (Assume No Impact in Sev/Crit)
mean_rem_mod_mort <- 0.80 # SOLIDARITY meta-analysis
sd_rem_mod_mort <- max(mean_rem_mod_mort - 0.63, 1.01 - mean_rem_mod_mort) / 1.95996

mean_rem_mod_dur <- 1.45 # ACTT NEJM article
sd_rem_mod_dur <- max(mean_rem_mod_dur - 1.18, 1.79 - mean_rem_mod_dur) / 1.95996

rem_corr <- -0.999
rem_cov <- rem_corr * sd_rem_mod_mort * sd_rem_mod_dur
rem_effect <- mvtnorm::rmvnorm(n = num_draws, mean = c(mean_rem_mod_mort, mean_rem_mod_dur),
                               sigma = matrix(c(sd_rem_mod_mort^2, rem_cov,
                                                rem_cov, sd_rem_mod_dur^2), nrow = 2))
rem_effect[rem_effect[, 1] > 1, 1] <- 1 # 24 Rem mortality > 1; have changed these to 1
rem_effect[rem_effect[, 2] < 1, 2] <- 1 # 1 Rem dur < 1; have changed these to 1

# Creating objects for each of the drug's effects
dexy_mod_mort <- dexy_mort_effect[, 1]
dexy_ICU_mort <- dexy_mort_effect[, 2]
rem_mod_mort <- rem_effect[, 1]
rem_mod_dur <- rem_effect[, 2]

# Defining the Demographicand Epidemiological Parameters Used In Each Scenario
country <- "Grenada"
standard_population <- round(rep(50000000/17, 17))
standard_matrix <- matrix(1,16,16)
standard_matrix[,16] <- 2
demog_pars_high_R0 <- list(R0 = 1.75, country = country, population = standard_population,
                           matrix = standard_matrix, time_period = 500, seeding_cases = 100)
demog_pars_low_R0 <- list(R0 = 1.25, country = country, population = standard_population,
                          matrix = standard_matrix, time_period = 500, seeding_cases = 100)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- actual_ICU_beds * 0.4
hc_pars_unlimited <- list(hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                          prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000)
hc_pars_limited <- list(hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                        prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                        MV_capacity = actual_MV_capacity)
hc_pars_nothing <- list(hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                        prop_ox_hosp_beds = 0, prop_ox_ICU_beds = 0,
                        MV_capacity = 0)

# Defining Drug Indicator and Effect Parameters Used in Each Scenario
ind_treatonly_benfull <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 0, drug_8_prop_treat = 1,
                              drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 1,
                              drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 1,
                              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
                              drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 1)
eff_treatonly_benfull <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur = rep(1, num_draws),
                              rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rep(1, num_draws),
                              dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = rep(1, num_draws),
                              dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = rep(1, num_draws),
                              dexy_crit_getox_getmv_mort = dexy_ICU_mort, dexy_crit_getox_nomv_mort = rep(1, num_draws), dexy_crit_noox_nomv_mort = rep(1, num_draws))

ind_allhosp_gradbencons <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbencons <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur = 1 + 0.3333 * (rem_mod_dur - 1),
                                rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rem_mod_mort + 0.3333 * (1 - rem_mod_mort),
                                dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = dexy_mod_mort + 0.5 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = rep(1, num_draws),
                                dexy_crit_noox_nomv_mort = rep(1, num_draws))

ind_allhosp_gradbenopti <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbenopti <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur =  1 + 0.6666 * (rem_mod_dur - 1),
                                rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rem_mod_mort + 0.6666 * (1 - rem_mod_mort),
                                dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = dexy_mod_mort + 0.8 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = dexy_ICU_mort + 0.8 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_noox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort))

ind_allhosp_benfull <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                            drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_benfull <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur = rem_mod_dur,
                            rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rem_mod_mort,
                            dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = dexy_mod_mort,
                            dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = dexy_ICU_mort,
                            dexy_crit_getox_getmv_mort = dexy_ICU_mort, dexy_crit_getox_nomv_mort = dexy_ICU_mort, dexy_crit_noox_nomv_mort = dexy_ICU_mort)

tic()
# Only Fully Treated Individuals Get the Full Benefit
highR0_unlimHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited,
                                                       drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull,
                                                       scenario = "highR0_unlimHC_treatonly_benfull")
highR0_limHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                     drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull,
                                                     scenario = "highR0_limHC_treatonly_benfull")
highR0_noHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                    drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull,
                                                    scenario = "highR0_noHC_treatonly_benfull")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Conservative Scenario)
highR0_unlimHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited,
                                                         drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons,
                                                         scenario = "highR0_unlimHC_allhosp_gradbencons")
highR0_limHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                       drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons,
                                                       scenario = "highR0_limHC_allhosp_gradbencons")
highR0_noHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                      drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons,
                                                      scenario = "highR0_noHC_allhosp_gradbencons")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited,
                                                         drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti,
                                                         scenario = "highR0_unlimHC_allhosp_gradbenopti")
highR0_limHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                       drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti,
                                                       scenario = "highR0_limHC_allhosp_gradbenopti")
highR0_noHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                      drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti,
                                                      scenario = "highR0_noHC_allhosp_gradbenopti")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited,
                                                         drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull,
                                                         scenario = "highR0_unlimHC_allhosp_benfull")
highR0_limHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                       drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull,
                                                       scenario = "highR0_limHC_allhosp_benfull")
highR0_noHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited,
                                                      drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull,
                                                      scenario = "highR0_noHC_allhosp_benfull")
toc()

overall <- rbind(highR0_unlimHC_treatonly_benfull, highR0_limHC_treatonly_benfull, highR0_noHC_treatonly_benfull,
                 highR0_unlimHC_allhosp_gradbencons, highR0_limHC_allhosp_gradbencons, highR0_noHC_allhosp_gradbencons,
                 highR0_unlimHC_allhosp_gradbenopti, highR0_limHC_allhosp_gradbenopti, highR0_noHC_allhosp_gradbenopti,
                 highR0_unlimHC_allhosp_benfull, highR0_limHC_allhosp_benfull, highR0_noHC_allhosp_benfull) %>%
  separate(scenario, c("R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  unite("drug_benefit", drug_benefit:drug_benefit2, remove = TRUE)

table(overall$drug_benefit)

ggplot(overall, aes(x = healthcare, y = IFR, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(~drug_benefit, nrow = 1)


############# SCRAPOLA #############


#
uh_no_drugs <- run_apothecary(country = country, R0 = 1.75, population = standard_population, contact_matrix_set = standard_matrix,
                              hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                              prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                              day_return = TRUE, time_period = 365, seeding_cases = 100)

nd_IFR <- 100 * extract_IFR(uh_no_drugs)
nd_total_bed_days <- sum(get_hosp_occ(uh_no_drugs)$hosp_dem)
nd_days_over_hosp <- sum(get_hosp_occ(uh_no_drugs)$hosp_occ >= uh_no_drugs$raw_parameters$hosp_bed_capacity)
nd_days_over_ICU <- sum(get_ICU_occ(uh_no_drugs)$ICU_occ >= uh_no_drugs$raw_parameters$ICU_bed_capacity)


uh_scen_1 <- lapply(seq_along(dexy_mod_mort), function(x) {

  temp <- run_apothecary(country = country, R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                         hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                         day_return = TRUE, time_period = 365, seeding_cases = 100,
                         drug_8_indic_IMod_GetHosp_GetOx = 1,
                         drug_8_indic_IMod_GetHosp_NoOx = 0,
                         drug_8_prop_treat = 1,
                         drug_8_GetOx_effect_size = rem_mod_dur[x],
                         drug_8_NoOx_effect_size = 1,
                         drug_11_indic_IMod_GetHosp_GetOx = 1,
                         drug_11_indic_IMod_GetHosp_NoOx = 0,
                         drug_11_prop_treat = 1,
                         drug_11_GetOx_effect_size = rem_mod_mort[x] * dexy_mod_mort[x],
                         drug_11_NoOx_effect_size = 1,
                         drug_12_indic_ISev_GetICU_GetOx = 1,
                         drug_12_indic_ISev_GetICU_NoOx = 0,
                         drug_12_prop_treat = 1,
                         drug_12_GetOx_effect_size = dexy_ICU_mort[x],
                         drug_12_NoOx_effect_size = 1,
                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1,
                         drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
                         drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0,
                         drug_13_prop_treat = 1,
                         drug_13_GetOx_GetMV_effect_size = dexy_ICU_mort[x],
                         drug_13_GetOx_NoMV_effect_size = 1,
                         drug_13_NoOx_NoMV_effect_size = 1)

  IFR <- 100 * extract_IFR(temp)
  total_bed_days <- sum(get_hosp_occ(temp)$hosp_dem)
  days_over_hosp <- sum(get_hosp_occ(temp)$hosp_occ >= temp$raw_parameters$hosp_bed_capacity)
  days_over_ICU <- sum(get_ICU_occ(temp)$ICU_occ >= temp$raw_parameters$ICU_bed_capacity)

  return(c(IFR, total_bed_days, days_over_hosp, days_over_ICU))

})


uh_scen_2 <- lapply(seq_along(dexy_mod_mort), function(x) {

  temp <- run_apothecary(country = country, R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                         hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                         day_return = TRUE, time_period = 365, seeding_cases = 100,
                         drug_8_indic_IMod_GetHosp_GetOx = 1,
                         drug_8_indic_IMod_GetHosp_NoOx = 1,
                         drug_8_prop_treat = 1,
                         drug_8_GetOx_effect_size = rem_mod_dur[x],
                         drug_8_NoOx_effect_size = 1 + 0.5 * (rem_mod_dur[x] - 1),
                         drug_11_indic_IMod_GetHosp_GetOx = 1,
                         drug_11_indic_IMod_GetHosp_NoOx = 1,
                         drug_11_prop_treat = 1,
                         drug_11_GetOx_effect_size = rem_mod_mort[x] * dexy_mod_mort[x],
                         drug_11_NoOx_effect_size = dexy_mod_mort[x],
                         drug_12_indic_ISev_GetICU_GetOx = 1,
                         drug_12_indic_ISev_GetICU_NoOx = 1,
                         drug_12_prop_treat = 1,
                         drug_12_GetOx_effect_size = dexy_ICU_mort[x],
                         drug_12_NoOx_effect_size = dexy_ICU_mort[x] + (0.5 * (1 - dexy_ICU_mort[x])),
                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1,
                         drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                         drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                         drug_13_prop_treat = 1,
                         drug_13_GetOx_GetMV_effect_size = dexy_ICU_mort[x],
                         drug_13_GetOx_NoMV_effect_size = dexy_ICU_mort[x] + (0.5 * (1 - dexy_ICU_mort[x])),
                         drug_13_NoOx_NoMV_effect_size = 1)

  IFR <- 100 * extract_IFR(temp)
  total_bed_days <- sum(get_hosp_occ(temp)$hosp_dem)
  days_over_hosp <- sum(get_hosp_occ(temp)$hosp_occ >= temp$raw_parameters$hosp_bed_capacity)
  days_over_ICU <- sum(get_ICU_occ(temp)$ICU_occ >= temp$raw_parameters$ICU_bed_capacity)

  return(c(IFR, total_bed_days, days_over_hosp, days_over_ICU))

})

uh_scen_3 <- lapply(seq_along(dexy_mod_mort), function(x) {

  temp <- run_apothecary(country = country, R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                         hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                         day_return = TRUE, time_period = 365, seeding_cases = 100,
                         drug_8_indic_IMod_GetHosp_GetOx = 1,
                         drug_8_indic_IMod_GetHosp_NoOx = 1,
                         drug_8_prop_treat = 1,
                         drug_8_GetOx_effect_size = rem_mod_dur[x],
                         drug_8_NoOx_effect_size = rem_mod_dur[x],
                         drug_11_indic_IMod_GetHosp_GetOx = 1,
                         drug_11_indic_IMod_GetHosp_NoOx = 1,
                         drug_11_prop_treat = 1,
                         drug_11_GetOx_effect_size = rem_mod_mort[x] * dexy_mod_mort[x],
                         drug_11_NoOx_effect_size = rem_mod_mort[x] * dexy_mod_mort[x],
                         drug_12_indic_ISev_GetICU_GetOx = 1,
                         drug_12_indic_ISev_GetICU_NoOx = 1,
                         drug_12_prop_treat = 1,
                         drug_12_GetOx_effect_size = dexy_ICU_mort[x],
                         drug_12_NoOx_effect_size = dexy_ICU_mort[x],
                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1,
                         drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                         drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                         drug_13_prop_treat = 1,
                         drug_13_GetOx_GetMV_effect_size = dexy_ICU_mort[x],
                         drug_13_GetOx_NoMV_effect_size = dexy_ICU_mort[x],
                         drug_13_NoOx_NoMV_effect_size = 1)

  IFR <- 100 * extract_IFR(temp)
  total_bed_days <- sum(get_hosp_occ(temp)$hosp_dem)
  days_over_hosp <- sum(get_hosp_occ(temp)$hosp_occ >= temp$raw_parameters$hosp_bed_capacity)
  days_over_ICU <- sum(get_ICU_occ(temp)$ICU_occ >= temp$raw_parameters$ICU_bed_capacity)

  return(c(IFR, total_bed_days, days_over_hosp, days_over_ICU))

})
toc()


nd <- data.frame("IFR" = nd_IFR, "bed_days" = nd_total_bed_days, "days_over_hosp" = nd_days_over_hosp,
                 "days_over_ICU" = nd_days_over_ICU, "scenario" = "unlimited_healthcare_no_drugs")

uh_scen_1 <- do.call(rbind, uh_scen_1)
uh_scen_1 <- as.data.frame(uh_scen_1)
uh_scen_1$scenario <- "unlimited_healthcare_drugs_treated_only"
colnames(uh_scen_1) <- colnames(nd)

uh_scen_2 <- do.call(rbind, uh_scen_2)
uh_scen_2 <- as.data.frame(uh_scen_2)
uh_scen_2$scenario <- "unlimited_healthcare_drugs_some_untreated"
colnames(uh_scen_2) <- colnames(nd)

uh_scen_3 <- do.call(rbind, uh_scen_3)
uh_scen_3 <- as.data.frame(uh_scen_3)
uh_scen_3$scenario <- "unlimited_healthcare_drugs_all_untreated"
colnames(uh_scen_3) <- colnames(nd)

x <- rbind(nd, uh_scen_1, uh_scen_2, uh_scen_3)


ggplot(x, aes(x = scenario, y = IFR)) +
  geom_boxplot()




















#####################################################
# no_drugs <- run_apothecary(country = "Grenada", population = standard_population, contact_matrix_set = standard_matrix,
#                            R0 = 3, day_return = TRUE, time_period = 365, seeding_cases = 100)
# output <- no_drugs$output
# index <- apothecary:::odin_index(no_drugs$model)
# hosp_occ_index <- c(index$IMod_GetHosp_GetOx_Surv1, index$IMod_GetHosp_GetOx_Surv2,
#                     index$IMod_GetHosp_GetOx_Die1, index$IMod_GetHosp_GetOx_Die2 , index$IRec1, index$IRec2)
# hosp_dem_index <- c(hosp_occ_index, index$IMod_GetHosp_NoOx_Surv1, index$IMod_GetHosp_NoOx_Surv2,
#                     index$IMod_GetHosp_NoOx_Die1, index$IMod_GetHosp_NoOx_Die2, index$IMod_NoHosp_NoOx_Surv1,
#                     index$IMod_NoHosp_NoOx_Surv2, index$IMod_NoHosp_NoOx_Die1, index$IMod_NoHosp_NoOx_Die2)
# hosp_occ <- apply(output[, hosp_occ_index], 1, sum)
# hosp_dem <- apply(output[, hosp_dem_index], 1, sum)
# plot(hosp_dem)
# lines(hosp_occ, col = "red")
#
# rem_dur_limited_hc <- run_apothecary(country = "Grenada",  population = standard_population, contact_matrix_set = standard_matrix,
#                                      R0 = 2.2, day_return = TRUE, time_period = 365, seeding_cases = 100,
#                                      drug_8_indic_IMod_GetHosp_GetOx = 1,
#                                      drug_8_indic_IMod_GetHosp_NoOx = 0,
#                                      drug_8_prop_treat = 1,
#                                      drug_8_GetOx_effect_size = mean_rem_mod_dur,
#                                      drug_8_NoOx_effect_size = 1)
# drugs_output <- rem_dur_limited_hc$output
# index <- apothecary:::odin_index(rem_dur_limited_hc$model)
# hosp_occ_index <- c(index$IMod_GetHosp_GetOx_Surv1, index$IMod_GetHosp_GetOx_Surv2,
#                     index$IMod_GetHosp_GetOx_Die1, index$IMod_GetHosp_GetOx_Die2 , index$IRec1, index$IRec2)
# hosp_dem_index <- c(hosp_occ_index, index$IMod_GetHosp_NoOx_Surv1, index$IMod_GetHosp_NoOx_Surv2,
#                     index$IMod_GetHosp_NoOx_Die1, index$IMod_GetHosp_NoOx_Die2, index$IMod_NoHosp_NoOx_Surv1,
#                     index$IMod_NoHosp_NoOx_Surv2, index$IMod_NoHosp_NoOx_Die1, index$IMod_NoHosp_NoOx_Die2)
# hosp_occ <- apply(drugs_output[, hosp_occ_index], 1, sum)
# hosp_dem <- apply(drugs_output[, hosp_dem_index], 1, sum)
# plot(hosp_dem)
# lines(hosp_occ, col = "red")
#
# mean_dexy_mod_mort
# mean_dexy_ICU_mort
#
# par(mfrow = c(1, 2))
# no_drugs <- run_apothecary(country = "Grenada", population = standard_population, contact_matrix_set = standard_matrix,
#                            hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
#                            R0 = 3, day_return = TRUE, time_period = 365, seeding_cases = 100)
# plot_hosp_occ(no_drugs)
# plot_ICU_occ(no_drugs)
#
# dexy_limited_hc <- run_apothecary(country = "Grenada",  population = standard_population, contact_matrix_set = standard_matrix,
#                                   hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
#                                   R0 = 3, day_return = TRUE, time_period = 365, seeding_cases = 100,
#                                   drug_11_indic_IMod_GetHosp_GetOx = 1,
#                                   drug_11_indic_IMod_GetHosp_NoOx = 0,
#                                   drug_11_prop_treat = 1,
#                                   drug_11_GetOx_effect_size = 0.5,
#                                   drug_11_NoOx_effect_size = 1,
#                                   drug_12_indic_ISev_GetICU_GetOx = 1,
#                                   drug_12_indic_ISev_GetICU_NoOx = 0,
#                                   drug_12_prop_treat = 1,
#                                   drug_12_GetOx_effect_size = 0.5,
#                                   drug_12_NoOx_effect_size = 1,
#                                   drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1,
#                                   drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
#                                   drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0,
#                                   drug_13_prop_treat = 1,
#                                   drug_13_GetOx_GetMV_effect_size = 0.5,
#                                   drug_13_GetOx_NoMV_effect_size = 1,
#                                   drug_13_NoOx_NoMV_effect_size = 1)
#
# plot_hosp_occ(dexy_limited_hc)
# plot_ICU_occ(dexy_limited_hc)
#
# 100 * extract_IFR(no_drugs)
# 100 * extract_IFR(dexy_limited_hc)
#
#
#
#
#
#
#
#
#
#
# no_drugs_unlimited_hc <- run_apothecary(country = "United Kingdom", R0 = 1.5, hosp_bed_capacity = 10000000000,
#                                         ICU_bed_capacity = 100000000000, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
#                                         MV_capacity = 100000000, day_return = TRUE, time_period = 365, seeding_cases = 100)
# 100 * extract_IFR(no_drugs_unlimited_hc)
#
# rem_dur_unlimited_hc <- run_apothecary(country = "United Kingdom", R0 = 1.5, hosp_bed_capacity = 10000000000,
#                                        ICU_bed_capacity = 100000000000, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
#                                        MV_capacity = 100000000, day_return = TRUE, time_period = 365, seeding_cases = 100,
#                                        drug_8_indic_IMod_GetHosp_GetOx = 1,
#                                        drug_8_indic_IMod_GetHosp_NoOx = 0,
#                                        drug_8_prop_treat = 1,
#                                        drug_8_GetOx_effect_size = mean_rem_mod_dur,
#                                        drug_8_NoOx_effect_size = 1)
# 100 * extract_IFR(rem_dur_unlimited_hc)
#
#
#
#
#
#
#
#
# rem_dur_limited_hc <- run_apothecary(country = "United Kingdom", R0 = 1.75, day_return = TRUE, time_period = 365, seeding_cases = 100,
#                                      drug_8_indic_IMod_GetHosp_GetOx = 1,
#                                      drug_8_indic_IMod_GetHosp_NoOx = 0,
#                                      drug_8_prop_treat = 1,
#                                      drug_8_GetOx_effect_size = 2,#mean_rem_mod_dur,
#                                      drug_8_NoOx_effect_size = 1)
# 100 * extract_IFR(rem_dur_limited_hc)
#
# drugs_output <- rem_dur_limited_hc$output
# index <- apothecary:::odin_index(rem_dur_limited_hc$model)
# lines(apply(drugs_output[, index$n_ICase2_Hosp_tot], 1, sum), col = "red")
#
#
#
#
#
#
#
# for (i in 1:1000) {
#   drugs_unlimited_hc <- run_apothecary(country = "United Kingdom",
#                                        R0 = 1.5,
#                                        hosp_bed_capacity = 10000000000,
#                                        ICU_bed_capacity = 100000000000,
#                                        prop_ox_hosp_beds = 1,
#                                        prop_ox_ICU_beds = 1,
#                                        MV_capacity = 100000000,
#                                        day_return = TRUE,
#                                        time_period = 365,
#                                        seeding_cases = 100,
#                                        drug_8_indic_IMod_GetHosp_GetOx = 1,
#                                        drug_8_indic_IMod_GetHosp_NoOx = 0,
#                                        drug_8_prop_treat = 1,
#                                        drug_8_GetOx_effect_size = rem_mod_dur[i],
#                                        drug_8_NoOx_effect_size = 1,
#                                        )
# }
#
# no_drugs_limited_hc <- run_apothecary(country = "United Kingdom",
#                                       R0 = 1.5,
#                                       day_return = TRUE,
#                                       time_period = 365,
#                                       seeding_cases = 100) # need to add in MV capacity into the run function
#
# 100 * extract_IFR(no_drugs_limited_hc)
#
# extract_IFR(x, age_reduce = FALSE)
#
#
#
#
# x <- squire::run_explicit_SEEIR_model(country = "United Kingdom", R0 = 3, walker = TRUE, day_return = TRUE, time_period = 365, replicates = 1)
# index <- squire:::odin_index(x$model)
# hospital_occupancy = c(index$IOxGetLive1,index$IOxGetLive2, index$IOxGetDie1, index$IOxGetDie2, index$IRec1, index$IRec2)
# hospital_demand = c(index$IOxGetLive1, index$IOxGetLive2, index$IOxGetDie1, index$IOxGetDie2, index$IRec1, index$IRec2,
#                     index$IOxNotGetLive1, index$IOxNotGetLive2, index$IOxNotGetDie1, index$IOxNotGetDie2)
# hosp_occ <- apply(x$output[, hospital_occupancy, 1], 1, sum)
# hosp_dem <- apply(x$output[, hospital_demand, 1], 1, sum)
# plot(hosp_dem)
# lines(hosp_occ, col = "red")
#
# y <- squire::run_explicit_SEEIR_model(country = "Grenada", population = standard_population, contact_matrix_set = contact_matrix,
#                                       R0 = 3, day_return = TRUE, time_period = 365, seeding_cases = 100, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/standard_population
# IFR <- apply(y$output[, index$D, 1], 2, max)/(apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))
#
# plot(apothecary_AR, pch = 20, ylim = c(0.8, 1))
# lines(squire_AR)
#
#
# standard_population <- round(rep(50000000/17, 17))
# ug_matrix <- squire::get_mixing_matrix("India")
# uk_matrix <- squire::get_mixing_matrix("Italy")
# standard_matrix <- matrix(1,16,16)
# standard_matrix[,16] <- 2
#
# par(mfrow = c(1, 4))
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = squire::get_population("Uganda")$n, contact_matrix_set = ug_matrix,
#                                       day_return = TRUE, time_period = 365,  seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# plot(squire_AR, type = "l", ylim = c(0, 1))
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = squire::get_population("Uganda")$n, contact_matrix_set = uk_matrix,
#                                       day_return = TRUE, time_period = 365, seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# lines(squire_AR, type = "l", ylim = c(0, 1), col = "red")
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = squire::get_population("Italy")$n, contact_matrix_set = ug_matrix,
#                                       day_return = TRUE, time_period = 365,  seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# plot(squire_AR, type = "l", ylim = c(0, 1))
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = squire::get_population("Italy")$n, contact_matrix_set = uk_matrix,
#                                       day_return = TRUE, time_period = 365, seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# lines(squire_AR, type = "l", ylim = c(0, 1), col = "red")
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = standard_population, contact_matrix_set = ug_matrix,
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# plot(squire_AR, type = "l", ylim = c(0, 1))
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = standard_population, contact_matrix_set = uk_matrix,
#                                       day_return = TRUE, time_period = 365, seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# lines(squire_AR, type = "l", ylim = c(0, 1), col = "red")
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = standard_population, contact_matrix_set = standard_matrix,
#                                       day_return = TRUE, time_period = 365,  seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# plot(squire_AR, type = "l", ylim = c(0, 1))
#
# y <- squire::run_explicit_SEEIR_model(R0 = 2.5, population = standard_population, contact_matrix_set = standard_matrix,
#                                       day_return = TRUE, time_period = 365, seeding_cases = 25, walker = TRUE, replicates = 1)
# index <- squire:::odin_index(y$model)
# squire_AR <- (apply(y$output[, index$D, 1], 2, max) + apply(y$output[, index$R, 1], 2, max))/y$parameters$population
# lines(squire_AR, type = "l", ylim = c(0, 1), col = "red")
#
# x <- run_apothecary(country = "Grenada", population = standard_population, contact_matrix_set = standard_matrix,
#                     R0 = 1.75, day_return = TRUE, time_period = 365, seeding_cases = 100)
# index <- apothecary:::odin_index(x$model)
# apothecary_AR <- (apply(x$output[, index$D], 2, max) + apply(x$output[, index$R], 2, max))/standard_population
# age_spec_IFR <- apply(x$output[, index$D], 2, max)/(apply(x$output[, index$D], 2, max) + apply(x$output[, index$R], 2, max))
# IFR <- sum(apply(x$output[, index$D], 2, max))/sum((apply(x$output[, index$D], 2, max) + apply(x$output[, index$R], 2, max)))
