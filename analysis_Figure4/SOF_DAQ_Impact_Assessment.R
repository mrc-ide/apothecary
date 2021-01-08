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
drug_6_effect_size <- 0.5
R0 <- 2
none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                                time_period = time, seeding_cases = 20, day_return = TRUE,
                                hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
drug <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                       drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = drug_6_effect_size)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(drug$output[, index$D], 2, max))

# 1) Indirect impact by shifting individuals left in the ICU to a better quality of treatment
extra_ISev_GetICU_GetOx <- apply(drug$output[, index$number_ISev_GetICU_GetOx], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum)
extra_ISev_GetICU_NoOx <- apply(drug$output[, index$number_ISev_GetICU_NoOx], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum)
extra_ISev_NoICU_NoOx <- apply(drug$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum)

extra_ICrit_GetICU_GetOx_GetMV <- apply(drug$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)
extra_ICrit_GetICU_GetOx_NoMV <- apply(drug$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
extra_ICrit_GetICU_NoOx_NoMV <- apply(drug$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
extra_ICrit_NoICU_NoOx_NoMV <- apply(drug$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

a <- extra_ISev_GetICU_GetOx * probs$prob_severe_death_no_ICU_no_ox - extra_ISev_GetICU_GetOx * probs$prob_severe_death_get_ICU_get_ox_baseline
b <- extra_ISev_GetICU_NoOx * probs$prob_severe_death_no_ICU_no_ox - extra_ISev_GetICU_NoOx * probs$prob_severe_death_get_ICU_no_ox_baseline
c <- extra_ICrit_GetICU_GetOx_GetMV * probs$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_GetOx_GetMV * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
d <- extra_ICrit_GetICU_GetOx_NoMV * probs$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_GetOx_NoMV * probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
e <- extra_ICrit_GetICU_NoOx_NoMV * probs$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_NoOx_NoMV * probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline

sum(a + b + c + d + e)

# 2) Directly by shifting individuals from higher risk (ICU) to lower risk (hosp) categories
#      Run the model with and without the drug effect in, then measure reduction in number
#      preventing people from going to hospital.
total_deaths_averted - sum(a + b + c + d + e)

# Scrapola #
# Think I could work this out directly by working out proportions in extra categories and then assigning an avert's total proportionally to each of the extras
extra_IMod_GetHosp_GetOx <- sum(apply(drug$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum))
extra_IMod_GetHosp_NoOx <- sum(apply(drug$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum))
extra_IMod_NoHosp_NoOx <- sum(apply(drug$output[, index$avert_IMod_NoHosp_NoOx], 2, sum) - apply(none$output[, index$avert_IMod_NoHosp_NoOx], 2, sum))

avert_ISev_GetICU_GetOx <- sum(apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_GetOx], 2, sum))
avert_ISev_GetICU_NoOx <- sum(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_NoOx], 2, sum))
avert_ISev_NoICU_NoOx <- sum(apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_NoICU_NoOx], 2, sum))

avert_ICrit_GetICU_GetOx_GetMV <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum))
avert_ICrit_GetICU_GetOx_NoMV <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum))
avert_ICrit_GetICU_NoOx_NoMV <- sum(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum))
avert_ICrit_NoICU_NoOx_NoMV <- sum(apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum))

extra_IMod_GetHosp_GetOx + extra_IMod_GetHosp_NoOx + extra_IMod_NoHosp_NoOx
avert_ISev_GetICU_GetOx + avert_ISev_GetICU_NoOx + avert_ISev_NoICU_NoOx + avert_ICrit_GetICU_GetOx_GetMV + avert_ICrit_GetICU_GetOx_NoMV + avert_ICrit_GetICU_NoOx_NoMV + avert_ICrit_NoICU_NoOx_NoMV


# IFR
none_IFR <- max(apply(none$output[, index$D], 1, sum))/ max(apply(none$output[, index$R], 1, sum)) * 100
drug_IFR <- max(apply(drug$output[, index$D], 1, sum))/ max(apply(drug$output[, index$R], 1, sum)) * 100

# Proportion of Individuals Receiving Complete Healthcare
sum(none$output[, index$number_get_hosp_full_treat])/(sum(none$output[, index$number_get_hosp_full_treat]) +
                                                        sum(none$output[, index$number_get_hosp_incomplete_treat]) +
                                                        sum(none$output[, index$number_need_hosp_no_treat]))
sum(drug$output[, index$number_get_hosp_full_treat])/(sum(drug$output[, index$number_get_hosp_full_treat]) +
                                                       sum(drug$output[, index$number_get_hosp_incomplete_treat]) +
                                                       sum(drug$output[, index$number_need_hosp_no_treat]))

sum(none$output[, index$number_get_ICU_full_treat])/(sum(none$output[, index$number_get_ICU_full_treat]) +
                                                       sum(none$output[, index$number_get_ICU_incomplete_treat]) +
                                                       sum(none$output[, index$number_need_ICU_no_treat]))
sum(drug$output[, index$number_get_ICU_full_treat])/(sum(drug$output[, index$number_get_ICU_full_treat]) +
                                                      sum(drug$output[, index$number_get_ICU_incomplete_treat]) +
                                                      sum(drug$output[, index$number_need_ICU_no_treat]))

# Number of Days Spent Over Capacity
sum(none$output[, index$number_get_ICU_incomplete_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)
sum(none$output[, index$number_need_ICU_no_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)

sum(drug$output[, index$number_get_ICU_incomplete_treat] > 0.5 & drug$output[, index$number_need_ICU] > 0.5)
sum(drug$output[, index$number_need_ICU_no_treat] > 0.5 & drug$output[, index$number_need_ICU] > 0.5)

sum(none$output[, index$number_get_hosp_incomplete_treat] > 0.5 & none$output[, index$number_need_hosp] > 0.5)
sum(none$output[, index$number_need_hosp_no_treat] > 0.5 & none$output[, index$number_need_ICU] > 0.5)

sum(drug$output[, index$number_get_hosp_incomplete_treat] > 0.5 & drug$output[, index$number_need_ICU] > 0.5)
sum(drug$output[, index$number_need_hosp_no_treat] > 0.5 & drug$output[, index$number_need_ICU] > 0.5)




