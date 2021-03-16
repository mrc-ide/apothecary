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
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- round(actual_ICU_beds * 0.5)
time <- 750

# Running Models
R0 <- 1.35
prop_treat_5a <- 0.35
rate_5a <- 2

prop_treat_5b <- 0.15
rate_5b <- 2

none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                       rel_inf_asymp = 1)
type5a <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                         time_period = time, seeding_cases = 20, day_return = TRUE,
                         hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                         prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                         drug_4_prop_treat = prop_treat_5a,
                         drug_4_indic_IAsymp = 0, drug_4_effect_size_IAsymp = 1,
                         drug_4_indic_IMild = 1, drug_4_effect_size_IMild = rate_5a,
                         drug_4_indic_ICase = 0, drug_4_effect_size_ICase = 1,
                         rel_inf_asymp = 1)
type5b <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                         time_period = time, seeding_cases = 20, day_return = TRUE,
                         hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                         prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                         drug_2_prop_treat = prop_treat_5b,
                         drug_2_indic_IPreAsymp = 1, drug_2_effect_size_IPreAsymp = rate_5b,
                         drug_2_indic_IPreMild = 1,  drug_2_effect_size_IPreMild = rate_5b,
                         drug_2_indic_IPreCase = 0, drug_2_effect_size_IPreCase = 1,
                         drug_4_prop_treat = prop_treat_5b,
                         drug_4_indic_IAsymp = 1, drug_4_effect_size_IAsymp = rate_5b,
                         drug_4_indic_IMild = 1, drug_4_effect_size_IMild = rate_5b,
                         drug_4_indic_ICase = 0, drug_4_effect_size_ICase = 1,
                         rel_inf_asymp = 1)

index <- squire:::odin_index(none$model)
none_deaths <- sum(apply(none$output[, index$D], 2, max))
type5a_deaths <- sum(apply(type5a$output[, index$D], 2, max))
type5b_deaths <- sum(apply(type5b$output[, index$D], 2, max))
