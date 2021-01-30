# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--dideclusthn", #"fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")

# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure2/Functions/Figure_2_Functions.R")
additional_identifier <- ""
context_name <- paste0("N:/Charlie/apothecary_figure2_runs_", Sys.Date(), additional_identifier)
ctx <- context::context_save(path = context_name,
                             sources = sources,
                             packages = packages,
                             package_sources = provisionr::package_sources(local =
                                                                             c("N:/Charlie/apothecary_fitting/apothecary_0.1.0.zip",
                                                                               "N:/Charlie/apothecary_fitting/dde_1.0.2.zip",
                                                                               "N:/Charlie/apothecary_fitting/odin_1.0.6.zip",
                                                                               "N:/Charlie/apothecary_fitting/squire_0.5.6.zip")))

# Configure the Queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# Functions for Data Preparation and Manipulation
wd <- getwd()
source(paste0(wd, "/analysis_Figure2/Functions/Figure_2_Functions.R"))

# Defining the Demographic and Epidemiological Parameters Used In Each Scenario
country <- "Bhutan"
raw_pop <- squire::population[squire::population$country == country, ]
standard_population <- round(raw_pop$n/sum(raw_pop$n) * 50000000)

standard_population_old_agg <- standard_population
standard_population_old_agg[16] <- standard_population_old_agg[16] + standard_population_old_agg[17]
standard_population_old_agg <- standard_population_old_agg[-17]
prop_pop <- standard_population_old_agg/sum(standard_population_old_agg)
standard_matrix <- matrix(rep(prop_pop, 16), ncol = 16, byrow = TRUE)

demog_pars_high_R0 <- list(R0 = 2, country = country, population = standard_population, matrix = standard_matrix, time_period = 150, seeding_cases = 1000)
demog_pars_low_R0 <- list(R0 = 1.35, country = country, population = standard_population, matrix = standard_matrix, time_period = 300, seeding_cases = 1000)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- round(actual_ICU_beds * 0.5)
unlim <- 100000000

hc_pars_nothing <- list(hosp_bed_capacity = 0, ICU_bed_capacity = 0, prop_ox_hosp_beds = 0, prop_ox_ICU_beds = 0, MV_capacity = 0)
hc_pars_unlimited <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = unlim)
hc_pars_limited_MV <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = round(actual_hosp_beds * actual_prop_ox_hosp_beds)/unlim,
                              prop_ox_ICU_beds = round(actual_ICU_beds * actual_prop_ox_ICU_beds)/unlim, MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox_beds <- list(hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds, prop_ox_hosp_beds = actual_prop_ox_hosp_beds,
                                   prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)

# Defining the Drug Parameters Used In Each Scenario
num_draws <- 500
drug_effs <- generate_drug_effect_draws(num_draws = num_draws)
dexy_mod_mort <- drug_effs$dexy_mod_mort
dexy_ICU_mort <- drug_effs$dexy_ICU_mort

## No Drug Effects
ind_no_drugs <- list(drug_11_indic_IMod_GetHosp_GetOx = 0, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 0,
                     drug_12_indic_ISev_GetICU_GetOx = 0, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 0,
                     drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 0)
eff_no_drugs <- list(dexy_mod_getox_mort = rep(1, num_draws),
                     dexy_mod_noox_mort = rep(1, num_draws),
                     dexy_sev_getox_mort = rep(1, num_draws),
                     dexy_sev_noox_mort = rep(1, num_draws),
                     dexy_crit_getox_getmv_mort = rep(1, num_draws),
                     dexy_crit_getox_nomv_mort = rep(1, num_draws),
                     dexy_crit_noox_nomv_mort = rep(1, num_draws))

## Drug Effects Only in Those Fully Treated
ind_treatonly_benfull <- list(drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 1,
                              drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 1,
                              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 1)
eff_treatonly_benfull <- list(dexy_mod_getox_mort = dexy_mod_mort,
                              dexy_mod_noox_mort = rep(1, num_draws),
                              dexy_sev_getox_mort = dexy_ICU_mort,
                              dexy_sev_noox_mort = rep(1, num_draws),
                              dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                              dexy_crit_getox_nomv_mort = rep(1, num_draws),
                              dexy_crit_noox_nomv_mort = rep(1, num_draws))

## Full Drug Effects In Fully Treated, Limited Impact In Inadequately Treated
ind_allhosp_gradbencons <- list(drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbencons <- list(dexy_mod_getox_mort = dexy_mod_mort,
                                dexy_mod_noox_mort = dexy_mod_mort + 0.5 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort,
                                dexy_sev_noox_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = rep(1, num_draws),
                                dexy_crit_noox_nomv_mort = rep(1, num_draws))

## Full Drug Effects In Fully Treated, Moderate Impact In Inadequately Treated
ind_allhosp_gradbenopti <- list(drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbenopti <- list(dexy_mod_getox_mort = dexy_mod_mort,
                                dexy_mod_noox_mort = dexy_mod_mort + 0.25 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort,
                                dexy_sev_noox_mort = dexy_ICU_mort + 0.25 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_noox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort))

## Full Drug Effects In Fully Treated, Full Impact In Inadequately Treated
ind_allhosp_benfull <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                            drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_benfull <- list(dexy_mod_getox_mort = dexy_mod_mort,
                            dexy_mod_noox_mort = dexy_mod_mort,
                            dexy_sev_getox_mort = dexy_ICU_mort,
                            dexy_sev_noox_mort = dexy_ICU_mort,
                            dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                            dexy_crit_getox_nomv_mort = dexy_ICU_mort,
                            dexy_crit_noox_nomv_mort = dexy_ICU_mort)

# Tester Run
# demog_pars <- demog_pars_high_R0
# hc_pars <- hc_pars_unlimited
# drug_ind_pars <- ind_treatonly_benfull
# drug_eff_pars <- eff_treatonly_benfull
run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_treatonly_benfull, eff_treatonly_benfull, "highR0_unlimHC_treatonly_benfull")

# Running on the cluster
# Only Fully Treated Individuals Get the Full Benefit
highR0_unlimHC_notreat_notreat <-  run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_no_drugs, eff_no_drugs, "1_dexy_highR0_unlimHC_notreat_notreat"))
highR0_limMV_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV, ind_no_drugs, eff_no_drugs, "2_dexy_highR0_limMV_notreat_notreat"))
highR0_limMVox_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox, ind_no_drugs, eff_no_drugs, "3_dexy_highR0_limMVox_notreat_notreat"))
highR0_limMVoxbeds_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox_beds, ind_no_drugs, eff_no_drugs, "4_dexy_highR0_limMVoxbeds_notreat_notreat"))
highR0_noHC_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_nothing, ind_no_drugs, eff_no_drugs, "5_dexy_highR0_noHC_notreat_notreat"))
lowR0_unlimHC_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_unlimited, ind_no_drugs, eff_no_drugs, "6_dexy_lowR0_unlimHC_notreat_notreat"))
lowR0_limMV_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV, ind_no_drugs, eff_no_drugs, "7_dexy_lowR0_limMV_notreat_notreat"))
lowR0_limMVox_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox, ind_no_drugs, eff_no_drugs, "8_dexy_lowR0_limMVox_notreat_notreat"))
lowR0_limMVoxbeds_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox_beds, ind_no_drugs, eff_no_drugs, "9_dexy_lowR0_limMVoxbeds_notreat_notreat"))
lowR0_noHC_notreat_notreat <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_nothing, ind_no_drugs, eff_no_drugs, "10_dexy_lowR0_noHC_notreat_notreat"))

# Only Fully Treated Individuals Get the Full Benefit
highR0_unlimHC_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_treatonly_benfull, eff_treatonly_benfull, "11_dexy_highR0_unlimHC_treatonly_benfull"))
highR0_limMV_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV, ind_treatonly_benfull, eff_treatonly_benfull, "12_dexy_highR0_limMV_treatonly_benfull"))
highR0_limMVox_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox, ind_treatonly_benfull, eff_treatonly_benfull, "13_dexy_highR0_limMVox_treatonly_benfull"))
highR0_limMVoxbeds_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox_beds, ind_treatonly_benfull, eff_treatonly_benfull, "14_dexy_highR0_limMVoxbeds_treatonly_benfull"))
highR0_noHC_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_nothing, ind_treatonly_benfull, eff_treatonly_benfull, "15_dexy_highR0_noHC_treatonly_benfull"))
lowR0_unlimHC_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_unlimited, ind_treatonly_benfull, eff_treatonly_benfull, "16_dexy_lowR0_unlimHC_treatonly_benfull"))
lowR0_limMV_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV, ind_treatonly_benfull, eff_treatonly_benfull, "17_dexy_lowR0_limMV_treatonly_benfull"))
lowR0_limMVox_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox, ind_treatonly_benfull, eff_treatonly_benfull, "18_dexy_lowR0_limMVox_treatonly_benfull"))
lowR0_limMVoxbeds_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox_beds, ind_treatonly_benfull, eff_treatonly_benfull, "19_dexy_lowR0_limMVoxbeds_treatonly_benfull"))
lowR0_noHC_treatonly_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_nothing, ind_treatonly_benfull, eff_treatonly_benfull, "20_dexy_lowR0_noHC_treatonly_benfull"))

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Conservative Scenario)
highR0_unlimHC_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "21_dexy_highR0_unlimHC_allhosp_gradbencons"))
highR0_limMV_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "22_dexy_highR0_limMV_allhosp_gradbencons"))
highR0_limMVox_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "23_dexy_highR0_limMVox_allhosp_gradbencons"))
highR0_limMVoxbeds_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "24_dexy_highR0_limMVoxbeds_allhosp_gradbencons"))
highR0_noHC_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_nothing, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "25_dexy_highR0_noHC_allhosp_gradbencons"))
lowR0_unlimHC_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_unlimited, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "26_dexy_lowR0_unlimHC_allhosp_gradbencons"))
lowR0_limMV_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "27_dexy_lowR0_limMV_allhosp_gradbencons"))
lowR0_limMVox_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "28_dexy_lowR0_limMVox_allhosp_gradbencons"))
lowR0_limMVoxbeds_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "29_dexy_lowR0_limMVoxbeds_allhosp_gradbencons"))
lowR0_noHC_allhosp_gradbencons <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_nothing, ind_allhosp_gradbencons, eff_allhosp_gradbencons, "30_dexy_lowR0_noHC_allhosp_gradbencons"))

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "31_dexy_highR0_unlimHC_allhosp_gradbenopti"))
highR0_limMV_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "32_dexy_highR0_limMV_allhosp_gradbenopti"))
highR0_limMVox_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "33_dexy_highR0_limMVox_allhosp_gradbenopti"))
highR0_limMVoxbeds_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "34_dexy_highR0_limMVoxbeds_allhosp_gradbenopti"))
highR0_noHC_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_nothing, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "35_dexy_highR0_noHC_allhosp_gradbenopti"))
lowR0_unlimHC_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_unlimited, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "36_dexy_lowR0_unlimHC_allhosp_gradbenopti"))
lowR0_limMV_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "37_dexy_lowR0_limMV_allhosp_gradbenopti"))
lowR0_limMVox_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "38_dexy_lowR0_limMVox_allhosp_gradbenopti"))
lowR0_limMVoxbeds_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "39_dexy_lowR0_limMVoxbeds_allhosp_gradbenopti"))
lowR0_noHC_allhosp_gradbenopti <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_nothing, ind_allhosp_gradbenopti, eff_allhosp_gradbenopti, "40_dexy_lowR0_noHC_allhosp_gradbenopti"))

# All Hospitalised Individuals Get Benefit - Non-Fully Treated Individuals Derive Minimal Benefit (Optimistic Scenario)
highR0_unlimHC_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_unlimited, ind_allhosp_benfull, eff_allhosp_benfull, "41_dexy_highR0_unlimHC_allhosp_benfull"))
highR0_limMV_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV, ind_allhosp_benfull, eff_allhosp_benfull, "42_dexy_highR0_limMV_allhosp_benfull"))
highR0_limMVox_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox, ind_allhosp_benfull, eff_allhosp_benfull, "43_dexy_highR0_limMVox_allhosp_benfull"))
highR0_limMVoxbeds_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_benfull, eff_allhosp_benfull, "44_dexy_highR0_limMVoxbeds_allhosp_benfull"))
highR0_noHC_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_high_R0, hc_pars_nothing, ind_allhosp_benfull, eff_allhosp_benfull, "45_dexy_highR0_noHC_allhosp_benfull"))
lowR0_unlimHC_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_unlimited, ind_allhosp_benfull, eff_allhosp_benfull, "46_dexy_lowR0_unlimHC_allhosp_benfull"))
lowR0_limMV_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV, ind_allhosp_benfull, eff_allhosp_benfull, "47_dexy_lowR0_limMV_allhosp_benfull"))
lowR0_limMVox_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox, ind_allhosp_benfull, eff_allhosp_benfull, "48_dexy_lowR0_limMVox_allhosp_benfull"))
lowR0_limMVoxbeds_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_limited_MV_ox_beds, ind_allhosp_benfull, eff_allhosp_benfull, "49_dexy_lowR0_limMVoxbeds_allhosp_benfull"))
lowR0_noHC_allhosp_benfull <- run$enqueue(run_hc_drugs_R0(demog_pars_low_R0, hc_pars_nothing, ind_allhosp_benfull, eff_allhosp_benfull, "50_dexy_lowR0_noHC_allhosp_benfull"))

table(run$task_status())

indices <- unname(which(run$task_status() == "PENDING"))
x <- names(which(run$task_status() == "PENDING"))
run$unsubmit(x)
