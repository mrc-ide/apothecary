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

# Running and assessing mAb (or similar) impact
# Impact from mAbs (or similar drug reducing prob_hosp and also duration of infectiousness) comes
# from a multiple of different sources:
#   1) Directly by preventing individuals from being hospitalised (and dying)
#   2) Indirectly by remaining hospitalised individuals getting better healthcare (due to lower demand)
#   3) Indirectly by reducing duration of infectivity, therefore R0 and therefore final numbers infected

# 3) Indirectly by preventing individuals from being hospitalised (and dying)
#      Run the model with only the duration of infection effect in, and now limited healthcare. Impact here is from the drug
#      preventing people from going to hospital AND from the reduced strain on healthcare.
R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}
none_lim_hc_1 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                                time_period = time, seeding_cases = 20, day_return = TRUE,
                                hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)

# Adding in the drug 3 effect
prop_treat <- 0.5
drug_3_effect_size <- 0.5
hosp_change <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                              time_period = time, seeding_cases = 20, day_return = TRUE,
                              hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                              prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                              drug_3_indic = 1, drug_3_prop_treat = prop_treat, drug_3_effect_size = drug_3_effect_size)
index <- squire:::odin_index(none_lim_hc_1$model)
drug_deaths <- sum(apply(hosp_change$output[, index$D], 2, max))
total_deaths_averted <- sum(apply(none_lim_hc_1$output[, index$D], 2, max)) - sum(apply(hosp_change$output[, index$D], 2, max))

# 1) Directly by preventing individuals from being hospitalised (and dying)
#      Calculating the number of individuals belonging to different disease classes in the non-drug run that should have received and benefitted
#      from the effects of Drug 3.
avert_IMod_GetHosp_GetOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_GetOx], 2, sum)
avert_IMod_GetHosp_NoOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_NoOx], 2, sum)
avert_IMod_NoHosp_NoOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_IMod_NoHosp_NoOx], 2, sum)
avert_ISev_GetICU_GetOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_GetOx], 2, sum)
avert_ISev_GetICU_NoOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_NoOx], 2, sum)
avert_ISev_NoICU_NoOx <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ISev_NoICU_NoOx], 2, sum)
avert_ICrit_GetICU_GetOx_GetMV <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)
avert_ICrit_GetICU_GetOx_NoMV <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
avert_ICrit_GetICU_NoOx_NoMV <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
avert_ICrit_NoICU_NoOx_NoMV <- prop_treat * drug_3_effect_size * apply(none_lim_hc_1$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

a <- sum(avert_IMod_GetHosp_GetOx * hosp_change$parameters$prob_moderate_death_get_hosp_get_ox_baseline)
b <- sum(avert_IMod_GetHosp_NoOx * hosp_change$parameters$prob_moderate_death_get_hosp_no_ox_baseline)
c <- sum(avert_IMod_NoHosp_NoOx * hosp_change$parameters$prob_moderate_death_no_hosp_no_ox)
d <- sum(avert_ISev_GetICU_GetOx * hosp_change$parameters$prob_severe_death_get_ICU_get_ox_baseline)
e <- sum(avert_ISev_GetICU_NoOx * hosp_change$parameters$prob_severe_death_get_ICU_no_ox_baseline)
f <- sum(avert_ISev_NoICU_NoOx * hosp_change$parameters$prob_severe_death_no_ICU_no_ox)
g <- sum(avert_ICrit_GetICU_GetOx_GetMV * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline)
h <- sum(avert_ICrit_GetICU_GetOx_NoMV * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline)
i <- sum(avert_ICrit_GetICU_NoOx_NoMV * hosp_change$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline)
j <- sum(avert_ICrit_NoICU_NoOx_NoMV * hosp_change$parameters$prob_critical_death_no_ICU_no_ox_no_MV)

direct_deaths_averted_prob_hosp_mod <- a+b+c+d+e+f+g+h+i+j

apply(none_lim_hc_1$output[, index$number_ISev_GetICU_GetOx], 2, sum)[15] - apply(hosp_change$output[, index$number_ISev_GetICU_GetOx], 2, sum)[15]
extra_ISevfull_from_inc[15]
extra_ISevfull_from_none[15]

# 2) Indirectly by preventing individuals from being hospitalised (and dying)
#      Run the model with only the prob_hosp effect in, and now limited healthcare. Impact here is from the drug
#      preventing people from going to hospital AND from the reduced strain on healthcare.
extra_IModfull_from_inc <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(hosp_change$output[, index$number_IMod_GetHosp_NoOx], 2, sum)
extra_IModfull_from_none <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(hosp_change$output[, index$number_IMod_NoHosp_NoOx], 2, sum)
extra_ISevfull_from_inc <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_ISev_GetICU_NoOx], 2, sum) - apply(hosp_change$output[, index$number_ISev_GetICU_NoOx], 2, sum)
extra_ISevfull_from_none <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(hosp_change$output[, index$number_ISev_NoICU_NoOx], 2, sum)
extra_ICritfull_from_inc_most <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - apply(hosp_change$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
extra_ICritfull_from_inc_least <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - apply(hosp_change$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
extra_ICritfull_from_none <- (1 - drug_3_effect_size * prop_treat) * apply(none_lim_hc_1$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(hosp_change$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)

a <- sum((extra_IModfull_from_inc * hosp_change$parameters$prob_moderate_death_get_hosp_no_ox_baseline) - (extra_IModfull_from_inc * hosp_change$parameters$prob_moderate_death_get_hosp_get_ox_baseline))
b <- sum((extra_IModfull_from_none * hosp_change$parameters$prob_moderate_death_no_hosp_no_ox) - (extra_IModfull_from_none * hosp_change$parameters$prob_moderate_death_get_hosp_get_ox_baseline))
c <- sum((extra_ISevfull_from_inc * hosp_change$parameters$prob_severe_death_get_ICU_no_ox_baseline) - (extra_ISevfull_from_inc * hosp_change$parameters$prob_severe_death_get_ICU_get_ox_baseline))
d <- sum((extra_ISevfull_from_none * hosp_change$parameters$prob_severe_death_no_ICU_no_ox) - (extra_ISevfull_from_none * hosp_change$parameters$prob_severe_death_get_ICU_get_ox_baseline))
e <- sum((extra_ICritfull_from_inc_most * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline) - (extra_ICritfull_from_inc_most * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))
f <- sum((extra_ICritfull_from_inc_least * hosp_change$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline) - (extra_ICritfull_from_inc_least * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))
g <- sum((extra_ICritfull_from_none * hosp_change$parameters$prob_critical_death_no_ICU_no_ox_no_MV) - (extra_ICritfull_from_none * hosp_change$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

indirect_deaths_averted_prob_hosp_mod <- a+b+c+d+e+f+g

total_deaths_averted
direct_deaths_averted_prob_hosp_mod + indirect_deaths_averted_prob_hosp_mod

# IFR
drug_IFR <- calc_IFR(hosp_change)

# Proportion of Individuals Receiving Complete/Incomplete Healthcare
drug_hosp_full_receive <- calc_receipt_healthare(hosp_change, "hospital_full")
drug_hosp_any_receive <- calc_receipt_healthare(hosp_change, "hospital_any")
drug_ICU_full_receive <- calc_receipt_healthare(hosp_change, "ICU_full")
drug_ICU_any_receive <- calc_receipt_healthare(hosp_change, "ICU_any")

# Number of Days Spent Over Capacity
drug_hosp_full_days_capacity <- days_over_capacity(hosp_change, "hospital_full")
drug_hosp_any_days_capacity <- days_over_capacity(hosp_change, "hospital_any")
drug_ICU_full_days_capacity <- days_over_capacity(hosp_change, "ICU_full")
drug_ICU_any_days_capacity <- days_over_capacity(hosp_change, "ICU_any")

# Attack Rates
drug_infected <- max(apply(hosp_change$output[, index$R], 1, sum) + apply(hosp_change$output[, index$D], 1, sum))
drug_AR <- calc_AR(hosp_change)

# Doses
drug_doses <- sum(hosp_change$output[, index$n_E2_IPre])

drug_df <- data.frame(drug = "Type4", R0 = R, total_infected = drug_infected, attack_rate = drug_AR,
                      deaths = drug_deaths, total_deaths_averted = total_deaths_averted,
                      direct_deaths_averted = direct_deaths_averted_prob_hosp_mod, indirect_deaths_averted_healthcare = indirect_deaths_averted_prob_hosp_mod,
                      indirect_deaths_averted_transmission = 0, IFR = drug_IFR,
                      prop_full_hosp = drug_hosp_full_receive, prop_any_hosp = drug_hosp_any_receive,
                      prop_full_ICU = drug_ICU_full_receive, prop_any_ICU = drug_ICU_any_receive,
                      days_over_full_hosp = drug_hosp_full_days_capacity, days_over_any_hosp = drug_hosp_any_days_capacity,
                      days_over_full_ICU = drug_ICU_full_days_capacity, days_over_any_ICU = drug_ICU_any_days_capacity,
                      doses = drug_doses)
saveRDS(drug_df, file = paste0("analysis_Figure4/Outputs/figure4b_central_estimates/type4_", R, "_df.rds"))
