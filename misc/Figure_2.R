# Loading required libraries and sourcing relevant functions
library(mvtnorm); library(tictoc); library(ggplot2); library(dplyr); library(tidyr)
source("analyses/Paper_Functions.R")
devtools::document()
devtools::load_all()

# Set seed
set.seed(1000)

# Generating Draws from Dexamethasone Mortality Uncertainty
num_draws <- 300
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
standard_matrix <- matrix(1, 16, 16)
standard_matrix[,16] <- 2
demog_pars_high_R0 <- list(R0 = 2, country = country, population = standard_population,
                           matrix = standard_matrix, time_period = 150, seeding_cases = 1000)
demog_pars_low_R0 <- list(R0 = 1.3, country = country, population = standard_population,
                          matrix = standard_matrix, time_period = 300, seeding_cases = 1000)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- actual_ICU_beds * 0.4

hc_pars_unlimited <- list(hosp_bed_capacity = 100000000,
                          ICU_bed_capacity = 100000000,
                          prop_ox_hosp_beds = 1,
                          prop_ox_ICU_beds = 1,
                          MV_capacity = 100000000)
hc_pars_limited_beds <- list(hosp_bed_capacity = actual_hosp_beds,
                             ICU_bed_capacity = actual_hosp_beds,
                             prop_ox_hosp_beds = 1,
                             prop_ox_ICU_beds = 1,
                             MV_capacity = 100000000)
hc_pars_limited_beds_ox <- list(hosp_bed_capacity = actual_hosp_beds,
                                ICU_bed_capacity = actual_hosp_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds,
                                prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                                MV_capacity = 100000000)
hc_pars_limited_MV <- list(hosp_bed_capacity = 100000000,
                           ICU_bed_capacity = 100000000,
                           prop_ox_hosp_beds = 1,
                           prop_ox_ICU_beds = 1,
                           MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox <- list(hosp_bed_capacity = 100000000,
                              ICU_bed_capacity = 100000000,
                              prop_ox_hosp_beds = round(actual_hosp_beds * actual_prop_ox_hosp_beds)/100000000,
                              prop_ox_ICU_beds = round(actual_ICU_beds * actual_prop_ox_ICU_beds)/100000000,
                              MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox_beds <- list(hosp_bed_capacity = actual_hosp_beds,
                                   ICU_bed_capacity = actual_ICU_beds,
                                   prop_ox_hosp_beds = actual_prop_ox_hosp_beds,
                                   prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                                   MV_capacity = actual_MV_capacity)
hc_pars_nothing <- list(hosp_bed_capacity = 0,
                        ICU_bed_capacity = 0,
                        prop_ox_hosp_beds = 0,
                        prop_ox_ICU_beds = 0,
                        MV_capacity = 0)

# Defining Drug Indicator and Effect Parameters Used in Each Scenario
ind_notreat_notreat <- list(drug_8_indic_IMod_GetHosp_GetOx = 0, drug_8_indic_IMod_GetHosp_NoOx = 0, drug_8_prop_treat = 0,
                            drug_11_indic_IMod_GetHosp_GetOx = 0, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 0,
                            drug_12_indic_ISev_GetICU_GetOx = 0, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 0,
                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
                            drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 0)
eff_notreat_notreat <- list(rem_mod_getox_dur = rep(1, num_draws), rem_mod_noox_dur = rep(1, num_draws),
                            rem_mod_getox_mort = rep(1, num_draws), rem_mod_noox_mort = rep(1, num_draws),
                            dexy_mod_getox_mort = rep(1, num_draws), dexy_mod_noox_mort = rep(1, num_draws),
                            dexy_sev_getox_mort = rep(1, num_draws), dexy_sev_noox_mort = rep(1, num_draws),
                            dexy_crit_getox_getmv_mort = rep(1, num_draws), dexy_crit_getox_nomv_mort = rep(1, num_draws), dexy_crit_noox_nomv_mort = rep(1, num_draws))

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
highR0_unlimHC_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_unlimHC_notreat_notreat")
highR0_limMV_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_limMV_notreat_notreat")
highR0_limMVox_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_limMVox_notreat_notreat")
highR0_limMVoxbeds_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_limMVoxbeds_notreat_notreat")
highR0_noHC_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_noHC_notreat_notreat")

lowR0_unlimHC_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_unlimHC_notreat_notreat")
lowR0_limMV_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_limMV_notreat_notreat")
lowR0_limMVox_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_limMVox_notreat_notreat")
lowR0_limMVoxbeds_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_limMVoxbeds_notreat_notreat")
lowR0_noHC_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_noHC_notreat_notreat")

# Only Fully Treated Individuals Get the Full Benefit
highR0_unlimHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_unlimHC_treatonly_benfull")
highR0_limMV_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_limMV_treatonly_benfull")
highR0_limMVox_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_limMVox_treatonly_benfull")
highR0_limMVoxbeds_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_limMVoxbeds_treatonly_benfull")
highR0_noHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_noHC_treatonly_benfull")

lowR0_unlimHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_unlimHC_treatonly_benfull")
lowR0_limMV_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_limMV_treatonly_benfull")
lowR0_limMVox_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_limMVox_treatonly_benfull")
lowR0_limMVoxbeds_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_limMVoxbeds_treatonly_benfull")
lowR0_noHC_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_noHC_treatonly_benfull")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Conservative Scenario)
highR0_unlimHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_unlimHC_allhosp_gradbencons")
highR0_limMV_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_limMV_allhosp_gradbencons")
highR0_limMVox_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_limMVox_allhosp_gradbencons")
highR0_limMVoxbeds_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_limMVoxbeds_allhosp_gradbencons")
highR0_noHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_noHC_allhosp_gradbencons")

lowR0_unlimHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_unlimHC_allhosp_gradbencons")
lowR0_limMV_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_limMV_allhosp_gradbencons")
lowR0_limMVox_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_limMVox_allhosp_gradbencons")
lowR0_limMVoxbeds_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_limMVoxbeds_allhosp_gradbencons")
lowR0_noHC_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_noHC_allhosp_gradbencons")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_unlimHC_allhosp_gradbenopti")
highR0_limMV_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_limMV_allhosp_gradbenopti")
highR0_limMVox_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_limMVox_allhosp_gradbenopti")
highR0_limMVoxbeds_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_limMVoxbeds_allhosp_gradbenopti")
highR0_noHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_noHC_allhosp_gradbenopti")

lowR0_unlimHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_unlimHC_allhosp_gradbenopti")
lowR0_limMV_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_limMV_allhosp_gradbenopti")
lowR0_limMVox_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_limMVox_allhosp_gradbenopti")
lowR0_limMVoxbeds_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_limMVoxbeds_allhosp_gradbenopti")
lowR0_noHC_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_noHC_allhosp_gradbenopti")

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_unlimHC_allhosp_benfull")
# highR0_limbeds_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_limbeds_allhosp_benfull")
# highR0_limbedsox_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_limbedsox_allhosp_benfull")
highR0_limMV_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_limMV_allhosp_benfull")
highR0_limMVox_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_limMVox_allhosp_benfull")
highR0_limMVoxbeds_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_limMVoxbeds_allhosp_benfull")
highR0_noHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "highR0_noHC_allhosp_benfull")

lowR0_unlimHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_unlimited, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_unlimHC_allhosp_benfull")
# lowR0_limbeds_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_limbeds_allhosp_benfull")
# lowR0_limbedsox_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_limbedsox_allhosp_benfull")
lowR0_limMV_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_limMV_allhosp_benfull")
lowR0_limMVox_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_limMVox_allhosp_benfull")
lowR0_limMVoxbeds_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_MV_ox_beds, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_limMVoxbeds_allhosp_benfull")
lowR0_noHC_allhosp_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_nothing, drug_ind_pars = ind_allhosp_benfull, drug_eff_pars = eff_allhosp_benfull, scenario = "lowR0_noHC_allhosp_benfull")

toc()

overall <- rbind(highR0_unlimHC_treatonly_benfull, highR0_limMV_treatonly_benfull,
                 highR0_limMVox_treatonly_benfull, highR0_limMVoxbeds_treatonly_benfull, highR0_noHC_treatonly_benfull,
                 lowR0_unlimHC_treatonly_benfull, lowR0_limMV_treatonly_benfull,
                 lowR0_limMVox_treatonly_benfull, lowR0_limMVoxbeds_treatonly_benfull, lowR0_noHC_treatonly_benfull,
                 highR0_unlimHC_allhosp_gradbencons, highR0_limMV_allhosp_gradbencons,
                 highR0_limMVox_allhosp_gradbencons, highR0_limMVoxbeds_allhosp_gradbencons, highR0_noHC_allhosp_gradbencons,
                 lowR0_unlimHC_allhosp_gradbencons, lowR0_limMV_allhosp_gradbencons,
                 lowR0_limMVox_allhosp_gradbencons, lowR0_limMVoxbeds_allhosp_gradbencons, lowR0_noHC_allhosp_gradbencons,
                 highR0_unlimHC_allhosp_gradbenopti, highR0_limMV_allhosp_gradbenopti,
                 highR0_limMVox_allhosp_gradbenopti, highR0_limMVoxbeds_allhosp_gradbenopti, highR0_noHC_allhosp_gradbenopti,
                 lowR0_unlimHC_allhosp_gradbenopti, lowR0_limMV_allhosp_gradbenopti,
                 lowR0_limMVox_allhosp_gradbenopti, lowR0_limMVoxbeds_allhosp_gradbenopti, lowR0_noHC_allhosp_gradbenopti,
                 highR0_unlimHC_allhosp_benfull, highR0_limMV_allhosp_benfull,
                 highR0_limMVox_allhosp_benfull, highR0_limMVoxbeds_allhosp_benfull, highR0_noHC_allhosp_benfull,
                 lowR0_unlimHC_allhosp_benfull, lowR0_limMV_allhosp_benfull,
                 lowR0_limMVox_allhosp_benfull, lowR0_limMVoxbeds_allhosp_benfull, lowR0_noHC_allhosp_benfull) %>%
  separate(scenario, c("R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  unite("drug_benefit", drug_benefit:drug_benefit2, remove = TRUE)
overall$healthcare <- factor(overall$healthcare, levels = c("unlimHC", "limbeds", "limbedsox", "limMV", "limMVox", "limMVoxbeds", "noHC"))
overall$drug_benefit <- factor(overall$drug_benefit, levels = c("notreat_notreat", "treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull"))
overall <- overall %>%
  filter(drug_benefit != "notreat_notreat") %>%
  filter(healthcare != "limbeds" & healthcare != "limbedsox")

no_drugs <- rbind(highR0_unlimHC_notreat_notreat, highR0_limMV_notreat_notreat,
                  highR0_limMVox_notreat_notreat, highR0_limMVoxbeds_notreat_notreat, highR0_noHC_notreat_notreat,
                  lowR0_unlimHC_notreat_notreat, lowR0_limMV_notreat_notreat,
                  lowR0_limMVox_notreat_notreat, lowR0_limMVoxbeds_notreat_notreat, lowR0_noHC_notreat_notreat) %>%
  separate(scenario, c("R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  select(IFR, R0, healthcare) %>%
  group_by(R0, healthcare) %>%
  summarise(no_drugs_IFR = mean(IFR))

y <- overall %>%
  left_join(no_drugs, by = c("R0", "healthcare")) %>%
  mutate(IFR_diff = no_drugs_IFR - IFR) %>%
  mutate(prop_IFR_red = IFR_diff/no_drugs_IFR)
y$healthcare <- factor(overall$healthcare, levels = c("unlimHC", "limbeds", "limbedsox", "limMV", "limMVox", "limMVoxbeds", "noHC"))
y$drug_benefit <- factor(overall$drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull"))

saveRDS(y, file = "temp_drugs_scenarios_output.rds")

ggplot(y) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = healthcare), outlier.shape = NA) +
  geom_point(aes(x = healthcare, y = no_drugs_IFR, col = healthcare), shape = 8) +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(y) +
  geom_boxplot(aes(x = healthcare, y = prop_IFR_red, fill = healthcare), outlier.shape = NA) +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)


test <- overall %>%
  filter(R0 == "highR0") %>%
  group_by(healthcare, drug_benefit) %>%
  summarise(mean = mean(IFR))

ggplot(test, aes(x = healthcare, y = mean, group = drug_benefit, col = drug_benefit)) +
  geom_path()

ggplot(overall, aes(x = healthcare, y = hosp_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)
ggplot(overall, aes(x = healthcare, y = hosp_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)


# highR0_limbeds_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_limbeds_notreat_notreat")
# highR0_limbedsox_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "highR0_limbedsox_notreat_notreat")
# lowR0_limbeds_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_limbeds_notreat_notreat")
# lowR0_limbedsox_notreat_notreat <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_notreat_notreat, drug_eff_pars = eff_notreat_notreat, scenario = "lowR0_limbedsox_notreat_notreat")
# highR0_limbeds_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_limbeds_treatonly_benfull")
# highR0_limbedsox_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "highR0_limbedsox_treatonly_benfull")
# lowR0_limbeds_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_limbeds_treatonly_benfull")
# lowR0_limbedsox_treatonly_benfull <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_treatonly_benfull, drug_eff_pars = eff_treatonly_benfull, scenario = "lowR0_limbedsox_treatonly_benfull")
# highR0_limbeds_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_limbeds_allhosp_gradbencons")
# highR0_limbedsox_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "highR0_limbedsox_allhosp_gradbencons")
# lowR0_limbeds_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_limbeds_allhosp_gradbencons")
# lowR0_limbedsox_allhosp_gradbencons <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_gradbencons, drug_eff_pars = eff_allhosp_gradbencons, scenario = "lowR0_limbedsox_allhosp_gradbencons")
# highR0_limbeds_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_limbeds_allhosp_gradbenopti")
# highR0_limbedsox_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_high_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "highR0_limbedsox_allhosp_gradbenopti")
# lowR0_limbeds_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_limbeds_allhosp_gradbenopti")
# lowR0_limbedsox_allhosp_gradbenopti <- run_drugs_hc_combo(demog_pars = demog_pars_low_R0, hc_pars = hc_pars_limited_beds_ox, drug_ind_pars = ind_allhosp_gradbenopti, drug_eff_pars = eff_allhosp_gradbenopti, scenario = "lowR0_limbedsox_allhosp_gradbenopti")


