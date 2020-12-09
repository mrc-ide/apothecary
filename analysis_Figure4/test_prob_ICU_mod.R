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

# Running and assessing Remdesivir impact
none <- run_apothecary(country = "Bhutan", R0 = 3, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)
rem <- run_apothecary(country = "Bhutan", R0 = 3, population = standard_population, contact_matrix_set = standard_matrix,
                      time_period = time, seeding_cases = 20, day_return = TRUE,
                      hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                      prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                      MV_capacity = actual_MV_capacity,
                      drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = 0.5,
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                      drug_8_GetOx_effect_size = 1, drug_8_NoOx_effect_size = 1)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(rem$output[, index$D], 2, max))


none_extra_hosps <-

none_averted <- unname(apply(none$output[, index$number_req_ICU_initial] - none$output[, index$number_req_ICU], 2, sum))

# Calculating Averted Severe and Critical Cases and Resulting Hospitalisations
rem_averted <- unname(apply(rem$output[, index$number_req_ICU_initial] - rem$output[, index$number_req_ICU], 2, sum))
rem_averted_sev <- rem_averted * (1 - rem$parameters$prob_critical)
rem_averted_crit <- rem_averted * rem$parameters$prob_critical

rem_extra_hosps <- apply(rem$output[, index$number_req_Hosp] - none$output[, index$number_req_Hosp], 2, sum)

unname(apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum)) - unname(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum))
unname(apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) - unname(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum))
unname(apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) - unname(apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum))




output(hosp_bed_full_treat_occ) <- TRUE
output(hosp_bed_incomplete_treat_occ) <- TRUE
output(hosp_bed_no_treat_occ) <- TRUE

rem_averted_sev_deaths <- ___
rem_averted_crit_deaths <- ___







