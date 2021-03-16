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

# Running and assessing mAb (or similar) impact
# Impact from mAbs (or similar drug reducing prob_hosp and also duration of infectiousness) comes
# from a multiple of different sources:
#   1) Directly by preventing individuals from being hospitalised (and dying)
#   2) Indirectly by remaining hospitalised individuals getting better healthcare (due to lower demand)
#   3) Indirectly by reducing duration of infectivity, therefore R0 and therefore final numbers infected
drug_3_effect_size <- 0.5
R0 <- 2
none_lim_hc_1 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                                time_period = time, seeding_cases = 20, day_return = TRUE,
                                hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
mab_lim_hc_no_dur_change <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                                           time_period = time, seeding_cases = 20, day_return = TRUE,
                                           hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                           prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                           drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = drug_3_effect_size)
index <- squire:::odin_index(none_lim_hc_1$model)
deaths_averted_directly_and_indirectly <- sum(apply(none_lim_hc_1$output[, index$D], 2, max)) - sum(apply(mab_lim_hc_no_dur_change$output[, index$D], 2, max))

# 1) Directly by preventing individuals from being hospitalised (and dying)
#      Run the model with only the prob_hosp effect in, and unlimited healthcare. All impact here is from the drug
#      preventing people from going to hospital.
avert_IMod_GetHosp_GetOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
avert_IMod_GetHosp_NoOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_NoOx], 2, sum)
avert_IMod_NoHosp_NoOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_NoHosp_NoOx], 2, sum)
avert_ISev_GetICU_GetOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_GetOx], 2, sum)
avert_ISev_GetICU_NoOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_NoOx], 2, sum)
avert_ISev_NoICU_NoOx <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_NoICU_NoOx], 2, sum)
avert_ICrit_GetICU_GetOx_GetMV <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)
avert_ICrit_GetICU_GetOx_NoMV <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
avert_ICrit_GetICU_NoOx_NoMV <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
avert_ICrit_NoICU_NoOx_NoMV <- drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

# Check the number averted adds up as calculated to the number of people flowing into the ICaseDrug3 state
sum(avert_IMod_GetHosp_GetOx + avert_IMod_GetHosp_NoOx + avert_IMod_NoHosp_NoOx + avert_ISev_GetICU_GetOx + avert_ISev_GetICU_NoOx + avert_ISev_NoICU_NoOx +
      avert_ICrit_GetICU_GetOx_GetMV + avert_ICrit_GetICU_GetOx_NoMV + avert_ICrit_GetICU_NoOx_NoMV + avert_ICrit_NoICU_NoOx_NoMV)
sum(apply(mab_lim_hc_no_dur_change$output[, index$n_E2_IPre1ICaseDrug3], 2, sum))

a <- sum(avert_IMod_GetHosp_GetOx * probs$prob_moderate_death_get_hosp_get_ox_baseline)
b <- sum(avert_IMod_GetHosp_NoOx * probs$prob_moderate_death_get_hosp_no_ox_baseline)
c <- sum(avert_IMod_NoHosp_NoOx * probs$prob_moderate_death_no_hosp_no_ox)

d <- sum(avert_ISev_GetICU_GetOx * probs$prob_severe_death_get_ICU_get_ox_baseline)
e <- sum(avert_ISev_GetICU_NoOx * probs$prob_severe_death_get_ICU_no_ox_baseline)
f <- sum(avert_ISev_NoICU_NoOx * probs$prob_severe_death_no_ICU_no_ox)

g <- sum(avert_ICrit_GetICU_GetOx_GetMV * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline)
h <- sum(avert_ICrit_GetICU_GetOx_NoMV * probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline)
i <- sum(avert_ICrit_GetICU_NoOx_NoMV * probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline)
j <- sum(avert_ICrit_NoICU_NoOx_NoMV * probs$prob_critical_death_no_ICU_no_ox_no_MV)

direct_deaths_averted_prob_hosp_mod <- a+b+c+d+e+f+g+h+i+j

# 2) Indirectly by preventing individuals from being hospitalised (and dying) and meaning that those who do get hospitalised get better quality of care
#      Run the model with only the prob_hosp effect in, and now limited healthcare. Impact here is from the drug
#      preventing people from going to hospital AND from the reduced strain on healthcare.
extra_IModfull_from_inc <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_IMod_GetHosp_NoOx], 2, sum)
extra_IModfull_from_none <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_IMod_NoHosp_NoOx], 2, sum)
extra_ISevfull_from_inc <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_NoOx], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_ISev_GetICU_NoOx], 2, sum)
extra_ISevfull_from_none <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_ISev_NoICU_NoOx], 2, sum)
extra_ICritfull_from_inc_most <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
extra_ICritfull_from_inc_least <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
extra_ICritfull_from_none <- (1 - drug_3_effect_size) * apply(none_lim_hc_1$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(mab_lim_hc_no_dur_change$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

a <- sum((extra_IModfull_from_inc * probs$prob_moderate_death_get_hosp_no_ox_baseline) - (extra_IModfull_from_inc * probs$prob_moderate_death_get_hosp_get_ox_baseline))
b <- sum((extra_IModfull_from_none * probs$prob_moderate_death_no_hosp_no_ox) - (extra_IModfull_from_none * probs$prob_moderate_death_get_hosp_get_ox_baseline))
c <- sum((extra_ISevfull_from_inc * probs$prob_severe_death_get_ICU_no_ox_baseline) - (extra_ISevfull_from_inc * probs$prob_severe_death_get_ICU_get_ox_baseline))
d <- sum((extra_ISevfull_from_none * probs$prob_severe_death_no_ICU_no_ox) - (extra_ISevfull_from_none * probs$prob_severe_death_get_ICU_get_ox_baseline))
e <- sum((extra_ICritfull_from_inc_most * probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline) - (extra_ICritfull_from_inc_most * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline))
f <- sum((extra_ICritfull_from_inc_least * probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline) - (extra_ICritfull_from_inc_least * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline))
g <- sum((extra_ICritfull_from_none * probs$prob_critical_death_no_ICU_no_ox_no_MV) - (extra_ICritfull_from_none * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

indirect_deaths_averted_prob_hosp_mod <- a+b+c+d+e+f+g

direct_deaths_averted_prob_hosp_mod + indirect_deaths_averted_prob_hosp_mod
deaths_averted_directly_and_indirectly

# 3) Indirectly by preventing individuals from being hospitalised (and dying)
#      Run the model with only the prob_hosp effect in, and now limited healthcare. Impact here is from the drug
#      preventing people from going to hospital AND from the reduced strain on healthcare.
none_lim_hc_2 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                              time_period = time, seeding_cases = 20, day_return = TRUE,
                              hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                              prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
mab_lim_hc_dur_change <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                                        time_period = time, seeding_cases = 20, day_return = TRUE,
                                        hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                        prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                        drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = drug_3_effect_size,
                                        drug_2_indic_IPreAsymp = 1, drug_2_indic_IPreMild = 1, drug_2_indic_IPreCase = 1,
                                        drug_2_prop_treat = 1,drug_2_effect_size_IPreAsymp = 1.5, drug_2_effect_size_IPreMild = 1.5, drug_2_effect_size_IPreCase = 1.5,
                                        drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                        drug_4_prop_treat = 1, drug_4_effect_size_IAsymp = 1.5, drug_4_effect_size_IMild = 1.5, drug_4_effect_size_ICase = 1.5)
index <- squire:::odin_index(none_lim_hc_2$model)
total_deaths_averted <- sum(apply(none_lim_hc_2$output[, index$D], 2, max)) - sum(apply(mab_lim_hc_dur_change$output[, index$D], 2, max))

total_deaths_averted

deaths_averted_directly_and_indirectly
direct_deaths_averted_prob_hosp_mod + indirect_deaths_averted_prob_hosp_mod
direct_deaths_averted_prob_hosp_mod
indirect_deaths_averted_prob_hosp_mod

indirect_1 <- total_deaths_averted - (direct_deaths_averted_prob_hosp_mod + indirect_deaths_averted_prob_hosp_mod)
indirect_2 <- indirect_deaths_averted_prob_hosp_mod
direct <- direct_deaths_averted_prob_hosp_mod

direct/total_deaths_averted
(indirect_1 + indirect_2)/total_deaths_averted
