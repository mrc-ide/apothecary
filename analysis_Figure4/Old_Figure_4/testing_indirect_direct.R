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
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_prop_treat = 1, drug_8_GetOx_effect_size = 1.45,
                      drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.8)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(rem$output[, index$D], 2, max))

# Model Probabilities and Drug Effects
prob_death_full <- rem$parameters$prob_moderate_death_get_hosp_get_ox_baseline
prob_death_inc <- rem$parameters$prob_moderate_death_get_hosp_no_ox_baseline
prob_death_none <- rem$parameters$prob_moderate_death_no_hosp_no_ox



# Impact for Moderate Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       some wouldn't have got benefit otherwise
#       some would have got benefit irrespective of the indirect effect


# Impact for Severe Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       some wouldn't have got benefit otherwise
#       some would have got benefit irrespective of the indirect effect




# Impact of Rem then is:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       some wouldn't have got benefit otherwise
#       some would have got benefit irrespective of the indirect effect

drug_effect_none <- 1
drug_effect_inc <- 1
drug_effect_full <- rem$parameters$drug_11_GetOx_effect_size

num_get_full_hosp <- apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
num_get_inc_hosp <- apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)
num_get_no_hosp <- apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum)

# Deaths averted due to reducing healthcare demand and more people get better healthcare
deaths_averted_none_to_full <- sum(-unname(num_get_no_hosp) * prob_death_none) - sum(-unname(num_get_no_hosp) * prob_death_full)
deaths_averted_inc_to_full <- sum(-unname(num_get_inc_hosp) * prob_death_inc) - sum(-unname(num_get_inc_hosp) * prob_death_full)
total_deaths_averted_inc_hc <- deaths_averted_none_to_full + deaths_averted_inc_to_full

# Extra deaths averted from those better treated individuals getting benefit of the drug
deaths_averted_none_to_full <- sum(-unname(num_get_no_hosp) * prob_death_none * drug_effect_none) - sum(-unname(num_get_no_hosp) * prob_death_full * drug_effect_full)
deaths_averted_inc_to_full <- sum(-unname(num_get_inc_hosp) * prob_death_inc * drug_effect_inc) - sum(-unname(num_get_inc_hosp) * prob_death_full * drug_effect_full)
drug_eff_total_deaths_inc_hc <- deaths_averted_none_to_full + deaths_averted_inc_to_full
total_deaths_averted_inc_hc_drug_eff <- drug_eff_total_deaths_inc_hc - total_deaths_averted_inc_hc

# Deaths averted due to baseline set of people receiving the drug irrespective of reduced strain on healthcare
deaths_no_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_death_full)
deaths_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_death_full * drug_effect_full)
total_deaths_averted_dir_drug_eff <- deaths_no_drug - deaths_drug

total_deaths_averted_due_to_drug_mort <- sum(apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_death_full) -
  sum(apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_death_full * drug_effect_full)

total_deaths_averted_dir_drug_eff + total_deaths_averted_inc_hc_drug_eff
# can just do total minus the line above to get indrect effects for Remdesivir I believe

deaths_averted
total_deaths_averted_inc_hc_drug_eff + total_deaths_averted_inc_hc + total_deaths_averted_dir_drug_eff

deaths_averted - (total_deaths_averted_inc_hc_drug_eff + total_deaths_averted_inc_hc + total_deaths_averted_dir_drug_eff)


# x <- unname(-num_get_no_hosp[17] * prob_death_none[17] - -num_get_no_hosp[17] * prob_death_full[17])
# y <- unname(-num_get_inc_hosp[17] * prob_death_inc[17] - -num_get_inc_hosp[17] * prob_death_full[17])
# x+y
# x+y deaths avoided through decreasing strain on healthcare

sum(num_get_full_hosp * prob_death_full)
sum(num_get_inc_hosp * prob_death_inc)
sum(num_get_no_hosp * prob_death_none)

apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum)[1] + apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum)[1] + apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum)[1]
apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)[1] + apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)[1] + apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum)[1]

num_get_full_hosp[17]
num_get_inc_hosp[17]
num_get_no_hosp[17]

100 * 0.8
100 * 0.9

0.8 * 0.9

# try two extremes


# Deaths averted due to reducing healthcare demand and more people get better healthcare
deaths_averted_none_to_full <- sum(-unname(num_get_no_hosp) * prob_death_none) - sum(-unname(num_get_no_hosp) * prob_death_full)
deaths_averted_inc_to_full <- sum(-unname(num_get_inc_hosp) * prob_death_inc) - sum(-unname(num_get_inc_hosp) * prob_death_full)
total_deaths_averted_direct_to_full <- deaths_averted_none_to_full + deaths_averted_inc_to_full

deaths_averted_none_to_inc <- sum(-unname(num_get_no_hosp) * prob_death_none) - sum(-unname(num_get_no_hosp) * prob_death_inc)
deaths_averted_all_inc_to_full <- sum(-unname(num_get_no_hosp + num_get_inc_hosp) * prob_death_inc) - sum(-unname(num_get_no_hosp + num_get_inc_hosp) * prob_death_full)
total_deaths_averted_sequential_to_full <- deaths_averted_none_to_inc + deaths_averted_all_inc_to_full

# Extra deaths averted from those better treated individuals getting benefit of the drug
deaths_averted_none_to_full <- sum(-unname(num_get_no_hosp) * prob_death_none * drug_effect_none) - sum(-unname(num_get_no_hosp) * prob_death_full * drug_effect_full)
deaths_averted_inc_to_full <- sum(-unname(num_get_inc_hosp) * prob_death_inc * drug_effect_inc) - sum(-unname(num_get_inc_hosp) * prob_death_full * drug_effect_full)
drug_eff_total_deaths_averted_direct_to_full <- deaths_averted_none_to_full + deaths_averted_inc_to_full

deaths_averted_none_to_inc <- sum(-unname(num_get_no_hosp) * prob_death_none * drug_effect_none) - sum(-unname(num_get_no_hosp) * prob_death_inc * drug_effect_inc)
deaths_averted_all_inc_to_full <- sum(-unname(num_get_no_hosp + num_get_inc_hosp) * prob_death_inc * drug_effect_inc) - sum(-unname(num_get_no_hosp + num_get_inc_hosp) * prob_death_full * drug_effect_full)
drug_eff_total_deaths_averted_sequential_to_full <- deaths_averted_none_to_inc + deaths_averted_all_inc_to_full



sum(-unname(num_get_no_hosp) * prob_death_none * drug_effect_inc)
sum(-unname(num_get_no_hosp) * prob_death_full * drug_effect_full)

sum(-unname(num_get_inc_hosp) * prob_death_inc)
sum(-unname(num_get_inc_hosp) * prob_death_full)
sum(-unname(num_get_inc_hosp) * prob_death_inc * drug_effect_inc)
sum(-unname(num_get_inc_hosp) * prob_death_full * drug_effect_full)


# 1) direct to full
x <- unname(-num_get_no_hosp[17] * prob_death_none[17] - -num_get_no_hosp[17] * prob_death_full[17])
y <- unname(-num_get_inc_hosp[17] * prob_death_inc[17] - -num_get_inc_hosp[17] * prob_death_full[17])
x+y

# 2) sequential bump ups
a <- unname(-num_get_no_hosp[17] * prob_death_none[17] - -num_get_no_hosp[17] * prob_death_inc[17])
b <- unname(-(num_get_no_hosp[17] + num_get_inc_hosp[17]) * prob_death_inc[17] - -(num_get_no_hosp[17] + num_get_inc_hosp[17]) * prob_death_full[17])
a+b

# Define Individuals Getting Care Who Wouldn't Otherwise
num_get_full_hosp <- apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
num_get_inc_hosp <-

num_get_full_hosp_drug <- apply(rem$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
deaths_full_drug <- sum(unname(num_get_full_hosp_drug * prob_death_full))
deaths_inc_drug <- sum(unname(num_get_full_hosp_drug * prob_death_inc))


age_spec_hosp_full_none <- apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
full_deaths_none <- unname(prob_death_full * age_spec_hosp_full_none)

age_spec_hosp_full_inc <- apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)

# how many would have died otherwise
sum(unname(prob_death_inc * apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)))

# how many will now die without Rem's mortality effect
sum(unname(prob_death_full * apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)))

# how many will now die with Rem's mortality effect
sum(unname(prob_death_full * apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum))) * 0.8

# of the 50,000 people who received healthcare when they wouldn't have without Remdesivir
# -> that's averted 7370 deaths that would've occurred otherwise

# for the mortality effect
# there's 50,000 people who received healthcare which they wouldn't have without Remdesivir
# -> another 20% of those 7370 deaths i.e. 1474 will not die because of the mortality impact of Remdesivir

7370 # deaths averted from reducing healthcare strain
1474 # deaths averted from people getting the drug because of reduced healthcare strain
13458 # deaths averted due to direct mortality impact

7370 + 1474 + 13458


sum(unname(prob_death_full * apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum))) * 0.8

sum(unname(prob_death_full * apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)))

sum(unname(prob_death_full * apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum))) * 0.8


hosp_inc_none



# Scrapola
# hosp_full_none <- sum(none$output[, index$number_get_hosp_full_treat])
# hosp_inc_none <- sum(none$output[, index$number_get_hosp_incomplete_treat])
# hosp_none_none <- sum(none$output[, index$number_req_hosp_bed]) - hosp_full_none - hosp_inc_none
#
# hosp_full_rem <- sum(rem$output[, index$number_get_hosp_full_treat])
# hosp_inc_rem <- sum(rem$output[, index$number_get_hosp_incomplete_treat])
# hosp_none_rem <- sum(rem$output[, index$number_req_hosp_bed]) - hosp_full_rem - hosp_inc_rem
#
# ICU_full_none <- sum(none$output[, index$number_get_ICU_full_treat])
# ICU_inc_none <- sum(none$output[, index$number_get_ICU_incomplete_treat])
# ICU_none_none <- sum(none$output[, index$number_req_ICU_bed]) - ICU_full_none - ICU_inc_none
#
# ICU_full_rem <- sum(rem$output[, index$number_get_ICU_full_treat])
# ICU_inc_rem <- sum(rem$output[, index$number_get_ICU_incomplete_treat])
# ICU_none_rem <- sum(rem$output[, index$number_req_ICU_bed]) - ICU_full_rem - ICU_inc_rem
#
# hosp_full_none
# hosp_full_rem
# hosp_inc_none
# hosp_inc_rem
# hosp_none_none
# hosp_none_rem





