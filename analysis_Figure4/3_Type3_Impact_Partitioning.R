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
R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}

# Running and assessing Type 3 Drug Impact
none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)
index <- squire:::odin_index(none$model)
none_deaths <- sum(apply(none$output[, index$D], 2, max))

type3 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                      time_period = time, seeding_cases = 20, day_return = TRUE,
                      hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                      prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                      MV_capacity = actual_MV_capacity,
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                      drug_8_GetOx_effect_size = 1.5, drug_8_NoOx_effect_size = 1.5,
                      drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = 1,
                      drug_9_GetOx_effect_size = 1.5, drug_9_NoOx_effect_size = 1.5,
                      drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = 1,
                      drug_10_GetOx_GetMV_effect_size = 1.5, drug_10_GetOx_NoMV_effect_size = 1.5, drug_10_NoOx_NoMV_effect_size = 1.5)
type3_deaths <- sum(apply(type3$output[, index$D], 2, max))
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(type3$output[, index$D], 2, max))

# Impact for Moderate Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       a) some wouldn't have got benefit otherwise
#       b) some would have got benefit irrespective of the indirect effect

# Model Probabilities and Drug Effects
prob_IMod_death_full <- type3$parameters$prob_moderate_death_get_hosp_get_ox_baseline
prob_IMod_death_inc <- type3$parameters$prob_moderate_death_get_hosp_no_ox_baseline
prob_IMod_death_none <- type3$parameters$prob_moderate_death_no_hosp_no_ox
prob_ISev_death_full <- type3$parameters$prob_severe_death_get_ICU_get_ox_baseline
prob_ISev_death_inc <- type3$parameters$prob_severe_death_get_ICU_no_ox_baseline
prob_ISev_death_none <- type3$parameters$prob_severe_death_no_ICU_no_ox
prob_ICrit_death_full <- type3$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline
prob_ICrit_death_inc_better <- type3$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline
prob_ICrit_death_inc_worse <- type3$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline
prob_ICrit_death_none <- type3$parameters$prob_critical_death_no_ICU_no_ox_no_MV

# Number of Individuals Receiving Healthcare Who Would Have Otherwise Not Received It

# 1a) Deaths averted due to reducing healthcare demand and more people getting better healthcare

# people absent from a compartment
# work from the assumption that the extra people receiving healthcare come from "not receiving healthcare"
num_IMod_not_in_nothing <- unname(apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(type3$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) # how many extra get incomplete healthcare?
num_IMod_not_in_inc <- unname(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(type3$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) # how many extra get full healthcare?
IMod_D_avert_none_to_full_incr_hc <- sum(num_IMod_not_in_nothing * prob_IMod_death_none) - sum(num_IMod_not_in_nothing * prob_IMod_death_full)
IMod_D_avert_inc_to_full_incr_hc <- sum(num_IMod_not_in_inc * prob_IMod_death_inc) - sum(num_IMod_not_in_inc * prob_IMod_death_full)
a1 <- IMod_D_avert_none_to_full_incr_hc + IMod_D_avert_inc_to_full_incr_hc

num_ISev_not_in_nothing <- unname(apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(type3$output[, index$number_ISev_NoICU_NoOx], 2, sum)) # how many extra get incomplete healthcare?
num_ISev_not_in_inc <- unname(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) - apply(type3$output[, index$number_ISev_GetICU_NoOx], 2, sum)) # how many extra get full healthcare?
ISev_D_avert_none_to_full_incr_hc <- sum(num_ISev_not_in_nothing * prob_ISev_death_none) - sum(num_ISev_not_in_nothing * prob_ISev_death_full)
ISev_D_avert_inc_to_full_incr_hc <- sum(num_ISev_not_in_inc * prob_ISev_death_inc) - sum(num_ISev_not_in_inc * prob_ISev_death_full)
a2 <- ISev_D_avert_none_to_full_incr_hc + ISev_D_avert_inc_to_full_incr_hc

num_ICrit_not_in_nothing <- unname(apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)) # how many extra get incomplete healthcare?
num_ICrit_not_in_inc_worse <- unname(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)) # how many extra get full healthcare?
num_ICrit_not_in_inc_better <- unname(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)) # how many extra get full healthcare?
ICrit_D_avert_none_to_full_incr_hc <- sum(num_ICrit_not_in_nothing * prob_ICrit_death_none) - sum(num_ICrit_not_in_nothing * prob_ICrit_death_full)
ICrit_D_avert_inc_worse_to_full_incr_hc <- sum(num_ICrit_not_in_inc_worse * prob_ICrit_death_inc_worse) - sum(num_ICrit_not_in_inc_worse * prob_ICrit_death_full)
ICrit_D_avert_inc_better_to_full_incr_hc <- sum(num_ICrit_not_in_inc_better * prob_ICrit_death_inc_better) - sum(num_ICrit_not_in_inc_better * prob_ICrit_death_full)
a3 <- ICrit_D_avert_none_to_full_incr_hc + ICrit_D_avert_inc_worse_to_full_incr_hc + ICrit_D_avert_inc_better_to_full_incr_hc

indirect_deaths_averted_healthcare <- a1+a2+a3

# IFR
type3_IFR <- calc_IFR(type3)

# Proportion of Individuals Receiving Complete/Incomplete Healthcare
type3_hosp_full_receive <- calc_receipt_healthare(type3, "hospital_full")
type3_hosp_any_receive <- calc_receipt_healthare(type3, "hospital_any")
type3_ICU_full_receive <- calc_receipt_healthare(type3, "ICU_full")
type3_ICU_any_receive <- calc_receipt_healthare(type3, "ICU_any")

# Number of Days Spent Over Capacity
type3_hosp_full_days_capacity <- days_over_capacity(type3, "hospital_full")
type3_hosp_any_days_capacity <- days_over_capacity(type3, "hospital_any")
type3_ICU_full_days_capacity <- days_over_capacity(type3, "ICU_full")
type3_ICU_any_days_capacity <- days_over_capacity(type3, "ICU_any")

# Attack Rates
type3_infected <- max(apply(type3$output[, index$R], 1, sum) + apply(type3$output[, index$D], 1, sum))
type3_AR <- calc_AR(type3)

# Doses
type3_doses <- sum(type3$output[, index$number_get_hosp_any_treat])

type3_df <- data.frame(drug = "Type3", R0 = R, total_infected = type3_infected, attack_rate = type3_AR,
                 deaths = type3_deaths, total_deaths_averted = total_deaths_averted,
                 direct_deaths_averted = 0, indirect_deaths_averted_healthcare = indirect_deaths_averted_healthcare,
                 indirect_deaths_averted_transmission = 0, IFR = type3_IFR,
                 prop_full_hosp = type3_hosp_full_receive, prop_any_hosp = type3_hosp_any_receive,
                 prop_full_ICU = type3_ICU_full_receive, prop_any_ICU = type3_ICU_any_receive,
                 days_over_full_hosp = type3_hosp_full_days_capacity, days_over_any_hosp = type3_hosp_any_days_capacity,
                 days_over_full_ICU = type3_ICU_full_days_capacity, days_over_any_ICU = type3_ICU_any_days_capacity,
                 doses = type3_doses)
saveRDS(type3_df, file = paste0("analysis_Figure4/Outputs/figure4b_central_estimates/type3_", R, "_df.rds"))


