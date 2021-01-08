# Loading required libraries
library(tidyverse)

# Loading apothecary
devtools::load_all()

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
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- round(actual_ICU_beds * 0.5)
time <- 600

# Running and assessing SOF/DAQ (or similar) impact
# Impact comes from a multiple of different sources:
#   1) Directly by shifting individuals from higher risk (ICU) to lower risk (hosp) categories
#   2) Indirectly by remaining ICU individuals being able to get access to better healthcare (due to lower demand)
R0 <- 2
none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
dexy <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                       drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                       drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82, drug_11_NoOx_effect_size = 0.94,
                       drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                       drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64, drug_12_NoOx_effect_size = 0.82,
                       drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                       drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64, drug_13_GetOx_NoMV_effect_size = 0.82, drug_13_NoOx_NoMV_effect_size = 0.82)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(dexy$output[, index$D], 2, max))

# IFR
none_IFR <- max(apply(none$output[, index$D], 1, sum))/ max(apply(none$output[, index$R], 1, sum)) * 100
dexy_IFR <- max(apply(dexy$output[, index$D], 1, sum))/ max(apply(dexy$output[, index$R], 1, sum)) * 100

# Proportion of Individuals Receiving Complete Healthcare
sum(none$output[, index$number_get_hosp_full_treat])/(sum(none$output[, index$number_get_hosp_full_treat]) +
                                                        sum(none$output[, index$number_get_hosp_incomplete_treat]) +
                                                        sum(none$output[, index$number_need_hosp_no_treat]))
sum(dexy$output[, index$number_get_hosp_full_treat])/(sum(dexy$output[, index$number_get_hosp_full_treat]) +
                                                       sum(dexy$output[, index$number_get_hosp_incomplete_treat]) +
                                                       sum(dexy$output[, index$number_need_hosp_no_treat]))

sum(none$output[, index$number_get_ICU_full_treat])/(sum(none$output[, index$number_get_ICU_full_treat]) +
                                                       sum(none$output[, index$number_get_ICU_incomplete_treat]) +
                                                       sum(none$output[, index$number_need_ICU_no_treat]))
sum(dexy$output[, index$number_get_ICU_full_treat])/(sum(dexy$output[, index$number_get_ICU_full_treat]) +
                                                      sum(dexy$output[, index$number_get_ICU_incomplete_treat]) +
                                                      sum(dexy$output[, index$number_need_ICU_no_treat]))

# Number of Days Spent Over Capacity
sum(none$output[, index$number_get_ICU_incomplete_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)
sum(none$output[, index$number_need_ICU_no_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)

sum(dexy$output[, index$number_get_ICU_incomplete_treat] > 0.5 & dexy$output[, index$number_need_ICU] > 0.5)
sum(dexy$output[, index$number_need_ICU_no_treat] > 0.5 & dexy$output[, index$number_need_ICU] > 0.5)

sum(none$output[, index$number_get_hosp_incomplete_treat] > 0.5 & none$output[, index$number_need_hosp] > 0.5)
sum(none$output[, index$number_need_hosp_no_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)

sum(dexy$output[, index$number_get_hosp_incomplete_treat] > 0.5 & dexy$output[, index$number_need_ICU] > 0.5)
sum(dexy$output[, index$number_need_hosp_no_treat] > 0.5 & dexy$output[, index$number_need_ICU] > 0.5)

none_infected <- max(apply(none$output[, index$D], 1, sum)) + max(apply(none$output[, index$R], 1, sum))
drug_infected <- max(apply(dexy$output[, index$D], 1, sum)) + max(apply(dexy$output[, index$R], 1, sum))

none_AR <- none_infected/sum(standard_population)
drug_AR <- none_infected/sum(standard_population)



