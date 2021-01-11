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
drug_deaths <- sum(apply(drug$output[, index$D], 2, max))
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

indirect_deaths_averted_healthcare <- sum(a + b + c + d + e)

# 2) Directly by shifting individuals from higher risk (ICU) to lower risk (hosp) categories
#      Run the model with and without the drug effect in, then measure reduction in number
#      preventing people from going to hospital.
from_ISev_GetICU_GetOx <- (drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum) * probs$prob_severe_death_get_ICU_get_ox_baseline
from_ISev_GetICU_NoOx <- (drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) * probs$prob_severe_death_get_ICU_no_ox_baseline
from_ICrit_GetICU_GetOx_GetMV <- (drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
from_ICrit_GetICU_GetOx_NoMV <- (drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) * probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
from_ICrit_GetICU_NoOx_NoMV <- (drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) * probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline

deaths_without_drug <- sum(from_ISev_GetICU_GetOx + from_ISev_GetICU_NoOx + from_ICrit_GetICU_GetOx_GetMV + from_ICrit_GetICU_GetOx_NoMV + from_ICrit_GetICU_NoOx_NoMV)

extra_IMod_GetHosp_GetOx <- (apply(drug$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)) * probs$prob_moderate_death_get_hosp_get_ox_baseline
extra_IMod_GetHosp_NoOx <- (apply(drug$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) * probs$prob_moderate_death_get_hosp_no_ox_baseline
extra_IMod_NoHosp_NoOx <- (apply(drug$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) * probs$prob_moderate_death_no_hosp_no_ox

extra_deaths <- sum(extra_IMod_GetHosp_GetOx + extra_IMod_GetHosp_NoOx + extra_IMod_NoHosp_NoOx)

# deaths_without_drug - extra_deaths
direct_deaths_averted <- total_deaths_averted - sum(a + b + c + d + e)

# IFR
drug_IFR <- calc_IFR(drug)

# Proportion of Individuals Receiving Complete/Incomplete Healthcare
drug_hosp_full_receive <- calc_receipt_healthare(drug, "hospital_full")
drug_hosp_any_receive <- calc_receipt_healthare(drug, "hospital_any")
drug_ICU_full_receive <- calc_receipt_healthare(drug, "ICU_full")
drug_ICU_any_receive <- calc_receipt_healthare(drug, "ICU_any")

# Number of Days Spent Over Capacity
drug_hosp_full_days_capacity <- days_over_capacity(drug, "hospital_full")
drug_hosp_any_days_capacity <- days_over_capacity(drug, "hospital_any")
drug_ICU_full_days_capacity <- days_over_capacity(drug, "ICU_full")
drug_ICU_any_days_capacity <- days_over_capacity(drug, "ICU_any")

# Attack Rates
drug_infected <- max(apply(drug$output[, index$R], 1, sum) + apply(drug$output[, index$D], 1, sum))
drug_AR <- calc_AR(drug)

drug_df <- data.frame(drug = "Ivermetin", total_infected = drug_infected, attack_rate = drug_AR,
                     deaths = drug_deaths, total_deaths_averted = total_deaths_averted,
                     direct_deaths_averted = direct_deaths_averted, indirect_deaths_averted_healthcare = indirect_deaths_averted_healthcare,
                     indirect_deaths_averted_transmission = 0, IFR = drug_IFR,
                     prop_full_hosp = drug_hosp_full_receive, prop_any_hosp = drug_hosp_any_receive,
                     prop_full_ICU = drug_ICU_full_receive, prop_any_ICU = drug_ICU_any_receive,
                     days_over_full_hosp = drug_hosp_full_days_capacity, days_over_any_hosp = drug_hosp_any_days_capacity,
                     days_over_full_ICU = drug_ICU_full_days_capacity, days_over_any_ICU = drug_ICU_any_days_capacity)
saveRDS(drug_df, file = "analysis_Figure4/Outputs/ivm_df.rds")

# Scrapola #
# Think I could work this out directly by working out proportions in extra categories and then assigning an avert's total proportionally to each of the extras
# avert_ISev_GetICU_GetOx <- sum(apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_GetOx], 2, sum))
# avert_ISev_GetICU_NoOx <- sum(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_NoOx], 2, sum))
# avert_ISev_NoICU_NoOx <- sum(apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_NoICU_NoOx], 2, sum))
# avert_ICrit_GetICU_GetOx_GetMV <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum))
# avert_ICrit_GetICU_GetOx_NoMV <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum))
# avert_ICrit_GetICU_NoOx_NoMV <- sum(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum))
# avert_ICrit_NoICU_NoOx_NoMV <- sum(apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum))
# extra_IMod_GetHosp_GetOx + extra_IMod_GetHosp_NoOx + extra_IMod_NoHosp_NoOx
# avert_ISev_GetICU_GetOx + avert_ISev_GetICU_NoOx + avert_ISev_NoICU_NoOx + avert_ICrit_GetICU_GetOx_GetMV + avert_ICrit_GetICU_GetOx_NoMV + avert_ICrit_GetICU_NoOx_NoMV + avert_ICrit_NoICU_NoOx_NoMV
# extra_IMod_GetHosp_GetOx <- (apply(drug$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum)) * probs$prob_moderate_death_get_hosp_get_ox_baseline
# extra_IMod_GetHosp_NoOx <- (apply(drug$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum)) * probs$prob_moderate_death_get_hosp_no_ox_baseline
# extra_IMod_NoHosp_NoOx <- (apply(drug$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum)) * probs$prob_moderate_death_no_hosp_no_ox
# extra_deaths <- sum(extra_IMod_GetHosp_GetOx + extra_IMod_GetHosp_NoOx + extra_IMod_NoHosp_NoOx)
# avert_ISev_GetICU_GetOx <- (apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_GetOx], 2, sum)) * probs$prob_severe_death_get_ICU_get_ox_baseline
# avert_ISev_GetICU_NoOx <- (apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_GetICU_NoOx], 2, sum)) * probs$prob_severe_death_get_ICU_no_ox_baseline
# avert_ISev_NoICU_NoOx <- (apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum)  - apply(drug$output[, index$number_ISev_NoICU_NoOx], 2, sum)) * probs$prob_severe_death_no_ICU_no_ox
# avert_ICrit_GetICU_GetOx_GetMV <- (apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)) * probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
# avert_ICrit_GetICU_GetOx_NoMV <- (apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)) * probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
# avert_ICrit_GetICU_NoOx_NoMV <- (apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)) * probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline
# avert_ICrit_NoICU_NoOx_NoMV <- (apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)  - apply(drug$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)) * probs$prob_critical_death_no_ICU_no_ox_no_MV
# avoided_deaths <- sum(avert_ISev_GetICU_GetOx + avert_ISev_GetICU_NoOx + avert_ISev_NoICU_NoOx + avert_ICrit_GetICU_GetOx_GetMV + avert_ICrit_GetICU_GetOx_NoMV + avert_ICrit_GetICU_NoOx_NoMV + avert_ICrit_NoICU_NoOx_NoMV)
# avoided_deaths - extra_deaths
