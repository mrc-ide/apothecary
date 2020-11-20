# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")


# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure2/Figure_2_Functions")
additional_identifier <- ""
context_name <- paste0("N:/Charlie/apothecary_runs_", Sys.Date(), additional_identifier)
ctx <- context::context_save(path = context_name,
                             sources = sources,
                             packages = packages,
                             package_sources = provisionr::package_sources(local =
                                                                             c("N:/Charlie/apothecary_fitting/apothecary_0.1.0.zip",
                                                                               "N:/Charlie/apothecary_fitting/dde_1.0.2.zip",
                                                                               "N:/Charlie/apothecary_fitting/odin_1.0.6.zip",
                                                                               "N:/Charlie/apothecary_fitting/squire_0.5.3.zip")))

# Configure the Queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# Testing the Running Locally
source("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure2/Figure_2_Functions")

# Defining the Demographic and Epidemiological Parameters Used In Each Scenario
country <- "Grenada"
standard_population <- round(rep(50000000/17, 17))
standard_matrix <- matrix(1, 16, 16)
standard_matrix[,16] <- 2
demog_pars_high_R0 <- list(R0 = 2, country = country, population = standard_population, matrix = standard_matrix, time_period = 150, seeding_cases = 1000)
demog_pars_low_R0 <- list(R0 = 1.3, country = country, population = standard_population, matrix = standard_matrix, time_period = 300, seeding_cases = 1000)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- actual_ICU_beds * 0.4
unlim <- 100000000

hc_pars_nothing <- list(hosp_bed_capacity = 0, ICU_bed_capacity = 0, prop_ox_hosp_beds = 0, prop_ox_ICU_beds = 0, MV_capacity = 0)
hc_pars_unlimited <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = unlim)
hc_pars_limited_MV <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox <- list(hosp_bed_capacity = unlim, ICU_bed_capacity = unlim, prop_ox_hosp_beds = round(actual_hosp_beds * actual_prop_ox_hosp_beds)/unlim,
                              prop_ox_ICU_beds = round(actual_ICU_beds * actual_prop_ox_ICU_beds)/unlim, MV_capacity = actual_MV_capacity)
hc_pars_limited_MV_ox_beds <- list(hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds, prop_ox_hosp_beds = actual_prop_ox_hosp_beds,
                                   prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)


# Defining the Drug Parameters Used In Each Scenario
num_draws <- 300
drug_effs <- generate_drug_effect_draws(num_draws = num_draws)
dexy_mod_mort <- drug_effs$dexy_mod_mort
dexy_ICU_mort <- drug_effs$dexy_ICU_mort
rem_mod_mort <- drug_effs$rem_mod_mort
rem_mod_dur <- drug_effs$rem_mod_dur

## No Drug Effects
ind_no_drugs <- list(drug_8_indic_IMod_GetHosp_GetOx = 0, drug_8_indic_IMod_GetHosp_NoOx = 0, drug_8_prop_treat = 0,
                     drug_11_indic_IMod_GetHosp_GetOx = 0, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 0,
                     drug_12_indic_ISev_GetICU_GetOx = 0, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 0,
                     drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0, drug_13_prop_treat = 0)
eff_no_drugs <- list(rem_mod_getox_dur = rep(1, num_draws), rem_mod_noox_dur = rep(1, num_draws),
                     rem_mod_getox_mort = rep(1, num_draws), rem_mod_noox_mort = rep(1, num_draws),
                     dexy_mod_getox_mort = rep(1, num_draws), dexy_mod_noox_mort = rep(1, num_draws),
                     dexy_sev_getox_mort = rep(1, num_draws), dexy_sev_noox_mort = rep(1, num_draws),
                     dexy_crit_getox_getmv_mort = rep(1, num_draws), dexy_crit_getox_nomv_mort = rep(1, num_draws), dexy_crit_noox_nomv_mort = rep(1, num_draws))

## Drug Effects Only in Those Fully Treated
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

## Full Drug Effects In Fully Treated, Limited Impact In Inadequately Treated
ind_allhosp_gradbencons <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbencons <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur = 1 + 0.3333 * (rem_mod_dur - 1),
                                rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rem_mod_mort + 0.6666 * (1 - rem_mod_mort),
                                dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = dexy_mod_mort + 0.5 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = rep(1, num_draws),
                                dexy_crit_noox_nomv_mort = rep(1, num_draws))

## Full Drug Effects In Fully Treated, Moderate Impact In Inadequately Treated
ind_allhosp_gradbenopti <- list(drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1)
eff_allhosp_gradbenopti <- list(rem_mod_getox_dur = rem_mod_dur, rem_mod_noox_dur =  1 + 0.6666 * (rem_mod_dur - 1),
                                rem_mod_getox_mort = rem_mod_mort, rem_mod_noox_mort = rem_mod_mort + 0.3333 * (1 - rem_mod_mort),
                                dexy_mod_getox_mort = dexy_mod_mort, dexy_mod_noox_mort = dexy_mod_mort + 0.25 * (1 - dexy_mod_mort),
                                dexy_sev_getox_mort = dexy_ICU_mort, dexy_sev_noox_mort = dexy_ICU_mort + 0.25 * (1 - dexy_ICU_mort),
                                dexy_crit_getox_getmv_mort = dexy_ICU_mort,
                                dexy_crit_getox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                                dexy_crit_noox_nomv_mort = dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort))

## Full Drug Effects In Fully Treated, Full Impact In Inadequately Treated
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


























# Establishing Which Countries to Run (and Track Missing Ones)
countries <- names(unlist(lapply(interventions, length))[unlist(lapply(interventions, length)) != 0]) # countries for which we have mobility data
inits_countries <- names(pars_init)
countries <- countries[countries %in% inits_countries]
yes_death_countries <- ecdc %>%
  dplyr::group_by(Region, countryterritoryCode) %>%
  summarise(total_deaths = sum(deaths)) %>%
  filter(total_deaths != 0)
countries <- countries[countries %in% yes_death_countries$countryterritoryCode]

# Running Countries Locally to Check They Work
for (i in 1:length(countries)) {
  test <- run_apothecary_MCMC(country = countries[i], date = "2020-11-16", pars_init = pars_init,
                              mortality_data = mortality_data, interventions = interventions,
                              n_mcmc = 2, replicates = 2, healthcare = "unlimited", n_chains = 1)
  print(i)
}
