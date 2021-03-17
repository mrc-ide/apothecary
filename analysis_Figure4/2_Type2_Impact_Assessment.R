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
drug_6_effect_size <- 0.30
R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}

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

# 1) Calculate Extra Deaths from those who have moved to IMod
extra_IMod_GetHosp_GetOx <- (apply(drug$output[, index$number_IMod_GetHosp_GetOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum))
extra_IMod_GetHosp_NoOx <- (apply(drug$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum))
extra_IMod_NoHosp_NoOx <- (apply(drug$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum))

extra_IMod_GetHosp_GetOx_deaths <- extra_IMod_GetHosp_GetOx * drug$parameters$prob_moderate_death_get_hosp_get_ox_baseline
extra_IMod_GetHosp_NoOx_deaths <- extra_IMod_GetHosp_NoOx * drug$parameters$prob_moderate_death_get_hosp_no_ox_baseline
extra_IMod_NoHosp_NoOx_deaths <- extra_IMod_NoHosp_NoOx * drug$parameters$prob_moderate_death_no_hosp_no_ox
extra_deaths <- sum(extra_IMod_GetHosp_GetOx_deaths + extra_IMod_GetHosp_NoOx_deaths + extra_IMod_NoHosp_NoOx_deaths)

# 1) Indirect impact by shifting individuals left in the ICU to a better quality of treatment
extra_ISev_GetICU_GetOx <- apply(drug$output[, index$number_ISev_GetICU_GetOx], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum)
extra_ISev_GetICU_NoOx <- apply(drug$output[, index$number_ISev_GetICU_NoOx], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum)
extra_ISev_NoICU_NoOx <- apply(drug$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum)

extra_ICrit_GetICU_GetOx_GetMV <- apply(drug$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)
extra_ICrit_GetICU_GetOx_NoMV <- apply(drug$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
extra_ICrit_GetICU_NoOx_NoMV <- apply(drug$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - (1 - drug_6_effect_size) * apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
extra_ICrit_NoICU_NoOx_NoMV <- apply(drug$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

a <- extra_ISev_GetICU_GetOx * drug$parameters$prob_severe_death_no_ICU_no_ox - extra_ISev_GetICU_GetOx * drug$parameters$prob_severe_death_get_ICU_get_ox_baseline
b <- extra_ISev_GetICU_NoOx * drug$parameters$prob_severe_death_no_ICU_no_ox - extra_ISev_GetICU_NoOx * drug$parameters$prob_severe_death_get_ICU_no_ox_baseline
c <- extra_ICrit_GetICU_GetOx_GetMV * drug$parameters$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_GetOx_GetMV * drug$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline
d <- extra_ICrit_GetICU_GetOx_NoMV * drug$parameters$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_GetOx_NoMV * drug$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline
e <- extra_ICrit_GetICU_NoOx_NoMV * drug$parameters$prob_critical_death_no_ICU_no_ox_no_MV - extra_ICrit_GetICU_NoOx_NoMV * drug$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline

indirect_deaths_averted_healthcare <- sum(a + b + c + d + e)

# deaths_without_drug - extra_deaths
direct_deaths_averted <- total_deaths_averted - indirect_deaths_averted_healthcare

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

# Doses
drug_doses <- sum(drug$output[, index$number_get_ICU_any_treat]) + sum(drug$output[, index$number_get_hosp_any_treat])

drug_df <- data.frame(drug = "Type2", R0 = R, total_infected = drug_infected, attack_rate = drug_AR,
                     deaths = drug_deaths, total_deaths_averted = total_deaths_averted,
                     direct_deaths_averted = direct_deaths_averted, indirect_deaths_averted_healthcare = indirect_deaths_averted_healthcare,
                     indirect_deaths_averted_transmission = 0, IFR = drug_IFR,
                     prop_full_hosp = drug_hosp_full_receive, prop_any_hosp = drug_hosp_any_receive,
                     prop_full_ICU = drug_ICU_full_receive, prop_any_ICU = drug_ICU_any_receive,
                     days_over_full_hosp = drug_hosp_full_days_capacity, days_over_any_hosp = drug_hosp_any_days_capacity,
                     days_over_full_ICU = drug_ICU_full_days_capacity, days_over_any_ICU = drug_ICU_any_days_capacity,
                     doses = drug_doses)
saveRDS(drug_df, file = paste0("analysis_Figure4/Outputs/figure4b_central_estimates/type2_", R, "_df.rds"))
