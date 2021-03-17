# Loading required libraries
library(tidyverse)

# Loading apothecary
devtools::load_all()

# Sourcing required functions
source("analysis_Figure4/Functions/metric_helper_functions.R")

# Generating standard contact matrix, population etc
country <- "Bhutan"
raw_pop <- squire::population[squire::population$country == country, ]
standard_population <- round(raw_pop$n/sum(raw_pop$n) * 50000000)
standard_population_old_agg <- standard_population
standard_population_old_agg[16] <- standard_population_old_agg[16] + standard_population_old_agg[17]
standard_population_old_agg <- standard_population_old_agg[-17]
prop_pop <- standard_population_old_agg/sum(standard_population_old_agg)
standard_matrix <- matrix(rep(prop_pop, 16), ncol = 16, byrow = TRUE)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.4
actual_prop_ox_ICU_beds <- 0.4
actual_MV_capacity <- round(actual_ICU_beds * 0.4)
time <- 600

# Running and assessing SOF/DAQ (or similar) impact
# Impact comes from a multiple of different sources:
#   1) Directly by shifting individuals from higher risk (ICU) to lower risk (hosp) categories
#   2) Indirectly by remaining ICU individuals being able to get access to better healthcare (due to lower demand)
R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}
type_1_eff <- 0.70
none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
index <- squire:::odin_index(none$model)
none_deaths <- sum(apply(none$output[, index$D], 2, max))
type1 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                       drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                       drug_11_prop_treat = 1, drug_11_GetOx_effect_size = type_1_eff, drug_11_NoOx_effect_size = type_1_eff + 0.5 * (1 - type_1_eff),
                       drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                       drug_12_prop_treat = 1, drug_12_GetOx_effect_size = type_1_eff, drug_12_NoOx_effect_size = type_1_eff + 0.5 * (1 - type_1_eff),
                       drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                       drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = type_1_eff, drug_13_GetOx_NoMV_effect_size = type_1_eff + 0.5 * (1 - type_1_eff), drug_13_NoOx_NoMV_effect_size = 1)
type1_deaths <- sum(apply(type1$output[, index$D], 2, max))
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(type1$output[, index$D], 2, max))
direct_deaths_averted <- total_deaths_averted
indirect_deaths_averted_healthcare <- 0
indirect_deaths_averted_transmission <- 0

# IFR
none_IFR <- calc_IFR(none)
type1_IFR <- calc_IFR(type1)

# Proportion of Individuals Receiving Complete/Incomplete Healthcare
none_hosp_full_receive <- calc_receipt_healthare(none, "hospital_full")
none_hosp_any_receive <- calc_receipt_healthare(none, "hospital_any")
none_ICU_full_receive <- calc_receipt_healthare(none, "ICU_full")
none_ICU_any_receive <- calc_receipt_healthare(none, "ICU_any")

type1_hosp_full_receive <- calc_receipt_healthare(type1, "hospital_full")
type1_hosp_any_receive <- calc_receipt_healthare(type1, "hospital_any")
type1_ICU_full_receive <- calc_receipt_healthare(type1, "ICU_full")
type1_ICU_any_receive <- calc_receipt_healthare(type1, "ICU_any")

# Number of Days Spent Over Capacity
none_hosp_full_days_capacity <- days_over_capacity(none, "hospital_full")
none_hosp_any_days_capacity <- days_over_capacity(none, "hospital_any")
none_ICU_full_days_capacity <- days_over_capacity(none, "ICU_full")
none_ICU_any_days_capacity <- days_over_capacity(none, "ICU_any")

type1_hosp_full_days_capacity <- days_over_capacity(type1, "hospital_full")
type1_hosp_any_days_capacity <- days_over_capacity(type1, "hospital_any")
type1_ICU_full_days_capacity <- days_over_capacity(type1, "ICU_full")
type1_ICU_any_days_capacity <- days_over_capacity(type1, "ICU_any")

# Attack Rates
none_infected <-  max(apply(none$output[, index$R], 1, sum) + apply(none$output[, index$D], 1, sum))
none_AR <- calc_AR(none)

type1_infected <- max(apply(type1$output[, index$R], 1, sum) + apply(type1$output[, index$D], 1, sum))
type1_AR <- calc_AR(type1)

# Doses
none_doses <- 0
type1_doses <- sum(type1$output[, index$number_get_ICU_any_treat]) + sum(type1$output[, index$number_get_hosp_any_treat])

# Creating Dataframe Storing Metrics
none_df <- data.frame(drug = "None", R0 = R, total_infected = none_infected, attack_rate = none_AR,
                      deaths = none_deaths, total_deaths_averted = 0,
                      direct_deaths_averted = 0, indirect_deaths_averted_healthcare = 0,
                      indirect_deaths_averted_transmission = 0, IFR = none_IFR,
                      prop_full_hosp = none_hosp_full_receive, prop_any_hosp = none_hosp_any_receive,
                      prop_full_ICU = none_ICU_full_receive, prop_any_ICU = none_ICU_any_receive,
                      days_over_full_hosp = none_hosp_full_days_capacity, days_over_any_hosp = none_hosp_any_days_capacity,
                      days_over_full_ICU = none_ICU_full_days_capacity, days_over_any_ICU = none_ICU_any_days_capacity,
                      doses = none_doses)
saveRDS(none_df, file = paste0("analysis_Figure4/Outputs/figure4b_central_estimates/none_", R, "_df.rds"))

type1_df <- data.frame(drug = "Type1", R0 = R, total_infected = type1_infected, attack_rate = type1_AR,
                      deaths = type1_deaths, total_deaths_averted = total_deaths_averted,
                      direct_deaths_averted = direct_deaths_averted, indirect_deaths_averted_healthcare = indirect_deaths_averted_healthcare,
                      indirect_deaths_averted_transmission = indirect_deaths_averted_transmission, IFR = type1_IFR,
                      prop_full_hosp = type1_hosp_full_receive, prop_any_hosp = type1_hosp_any_receive,
                      prop_full_ICU = type1_ICU_full_receive, prop_any_ICU = type1_ICU_any_receive,
                      days_over_full_hosp = type1_hosp_full_days_capacity, days_over_any_hosp = type1_hosp_any_days_capacity,
                      days_over_full_ICU = type1_ICU_full_days_capacity, days_over_any_ICU = type1_ICU_any_days_capacity,
                      doses = type1_doses)
saveRDS(type1_df, file = paste0("analysis_Figure4/Outputs/figure4b_central_estimates/type1_", R, "_df.rds"))


