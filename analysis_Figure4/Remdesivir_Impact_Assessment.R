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
none <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)

rem <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                      time_period = time, seeding_cases = 20, day_return = TRUE,
                      hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                      prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                      MV_capacity = actual_MV_capacity,
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                      drug_8_GetOx_effect_size = 1.45, drug_8_NoOx_effect_size = 1,
                      drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                      drug_11_GetOx_effect_size = 0.8, drug_11_NoOx_effect_size = 0.9)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(rem$output[, index$D], 2, max))

# Impact for Moderate Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       a) some wouldn't have got benefit otherwise
#       b) some would have got benefit irrespective of the indirect effect

# Model Probabilities and Drug Effects
prob_IMod_death_full <- rem$parameters$prob_moderate_death_get_hosp_get_ox_baseline
prob_IMod_death_inc <- rem$parameters$prob_moderate_death_get_hosp_no_ox_baseline
prob_IMod_death_none <- rem$parameters$prob_moderate_death_no_hosp_no_ox

# Number of Individuals Receiving Healthcare Who Would Have Otherwise Not Received It

# 1a) Deaths averted due to reducing healthcare demand and more people getting better healthcare

# people absent from a compartment
# work from the assumption that the extra people receiving healthcare come from "not receiving healthcare"
num_IMod_not_in_nothing <- unname(apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) # how many extra get incomplete healthcare?
num_IMod_not_in_inc <- unname(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) # how many extra get full healthcare?
IMod_D_avert_none_to_full_incr_hc <- sum(num_IMod_not_in_nothing * prob_IMod_death_none) - sum(num_IMod_not_in_nothing * prob_IMod_death_full)
IMod_D_avert_inc_to_full_incr_hc <- sum(num_IMod_not_in_inc * prob_IMod_death_inc) - sum(num_IMod_not_in_inc * prob_IMod_death_full)
a1 <- IMod_D_avert_none_to_full_incr_hc + IMod_D_avert_inc_to_full_incr_hc

# people absent from a compartment
# work from the assumption that the extra people receiving healthcare come from compartment below
alt_num_IMod_not_in_nothing <- unname(apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) # how many extra get incomplete healthcare?
alt_num_IMod_not_in_inc <- unname(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) # how many extra get full healthcare?
alt_IMod_D_avert_none_to_inc_incr_hc <- sum(num_IMod_not_in_nothing * prob_IMod_death_none) - sum(num_IMod_not_in_nothing * prob_IMod_death_inc)
alt_IMod_D_avert_inc_to_full_incr_hc <- sum((num_IMod_not_in_inc + num_IMod_not_in_nothing) * prob_IMod_death_inc) - sum((num_IMod_not_in_inc + num_IMod_not_in_nothing) * prob_IMod_death_full)
a2 <- alt_IMod_D_avert_none_to_inc_incr_hc + alt_IMod_D_avert_inc_to_full_incr_hc

# 2a) Extra deaths averted from those better treated individuals getting benefit of the drug
# work from the assumption that the extra people receiving healthcare come from "not receiving healthcare"
IMod_D_avert_none_to_full_incr_hc_drug <- sum(num_IMod_not_in_nothing * prob_IMod_death_none * 1) -
                                          sum(num_IMod_not_in_nothing * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size)
IMod_D_avert_inc_to_full_incr_hc_drug <- sum(num_IMod_not_in_inc * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size) -
                                         sum(num_IMod_not_in_inc * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size)
b1 <- IMod_D_avert_none_to_full_incr_hc_drug + IMod_D_avert_inc_to_full_incr_hc_drug - a1

# work from the assumption that the extra people receiving healthcare come from compartment below
alt_IMod_D_avert_none_to_full_incr_hc_drug <- sum(num_IMod_not_in_nothing * prob_IMod_death_none * 1) -
                                              sum(num_IMod_not_in_nothing * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size)
alt_IMod_D_avert_inc_to_full_incr_hc_drug <- sum((num_IMod_not_in_inc + num_IMod_not_in_nothing) * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size) -
                                             sum((num_IMod_not_in_inc + num_IMod_not_in_nothing) * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size)
b2 <- alt_IMod_D_avert_none_to_full_incr_hc_drug + alt_IMod_D_avert_inc_to_full_incr_hc_drug - a2

# needs the one you're considering and the one before it
# i.e. on line 86, the idea is that if you've got the flow from the one before it (not_in_nothing) and you *know* the total not in the current compartment (num_IMod_not_in_inc),
# then even more than num_IMod_not_in_inc must have left, because you've got the num_IMod_not_in_nothing flow coming in. I.e. (num_IMod_not_in_inc + num_IMod_not_in_nothing) must have left inc to go to full.
# this can be applied to Crit where there are 4 stages. Not quite correct because once you get to box #3 you've got to account for both the previous flows.

# 2b) Deaths averted due to set of people receiving the drug irrespective of reduced strain on healthcare
IMod_D_no_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_IMod_death_full) +
                  sum(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) * prob_IMod_death_inc)
IMod_D_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size) +
               sum(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size)
c <- IMod_D_no_drug - IMod_D_drug

(a1+b1+c) - total_deaths_averted
(a2+b2+c) - total_deaths_averted

indirect <- a1 + b1
direct <- c
direct/total_deaths_averted

