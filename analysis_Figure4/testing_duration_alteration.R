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
R <- "low"
if (R == "high") {
  R0 <- 2
} else {
  R0 <- 1.35
}

# Running and assessing Type 3 Drug Impact
unlim <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                        time_period = time, seeding_cases = 20, day_return = TRUE,
                        hosp_bed_capacity = 1000000000, ICU_bed_capacity = 1000000000,
                        prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
                        MV_capacity = 1000000000)

none <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)

type3 <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                        time_period = time, seeding_cases = 20, day_return = TRUE,
                        hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                        prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                        MV_capacity = actual_MV_capacity,
                        drug_8_prop_treat = 1,
                        drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1,
                        drug_8_GetOx_effect_size = 100, drug_8_NoOx_effect_size = 100,
                        drug_9_prop_treat = 1,
                        drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1,
                        drug_9_GetOx_effect_size = 100, drug_9_NoOx_effect_size = 100,
                        drug_10_prop_treat = 1,
                        drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1,
                        drug_10_GetOx_GetMV_effect_size = 100, drug_10_GetOx_NoMV_effect_size = 100, drug_10_NoOx_NoMV_effect_size = 100)

index <- squire:::odin_index(none$model)
unlim_deaths <- sum(apply(unlim$output[, index$D], 2, max))
none_deaths <- sum(apply(none$output[, index$D], 2, max))
type3_deaths <- sum(apply(type3$output[, index$D], 2, max))
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(type3$output[, index$D], 2, max))

plot(none$output[, index$hosp_bed_full_treat_occ], type = "l")
lines(type3$output[, index$hosp_bed_full_treat_occ], type = "l", col = "red")

plot(none$output[, index$number_get_hosp_full_treat], type = "l")
lines(type3$output[, index$number_get_hosp_full_treat], type = "l", col = "red")

plot(none$output[, index$number_get_hosp_any_treat], type = "l")
lines(type3$output[, index$number_get_hosp_any_treat], type = "l", col = "red")

plot(none$output[, index$ICU_bed_full_treat_occ], type = "l")
lines(type3$output[, index$ICU_bed_full_treat_occ], type = "l", col = "red")

plot(apply(type3$output[, index$ICrit_GetICU_GetOx_GetMV_Die1], 1, sum), type = "l", col = "red")
lines(apply(none$output[, index$ICrit_GetICU_GetOx_GetMV_Die1], 1, sum), type = "l")

plot(apply(type3$output[, index$ICrit_GetICU_GetOx_GetMV_Surv1], 1, sum), type = "l", col = "red")
lines(apply(none$output[, index$ICrit_GetICU_GetOx_GetMV_Surv1], 1, sum), type = "l")


plot(type3$output[, index$number_get_ICU_full_treat], type = "l", col = "red")
lines(none$output[, index$number_get_ICU_full_treat], type = "l")

plot(type3$output[, index$number_get_ICU_any_treat], type = "l", col = "red")
lines(none$output[, index$number_get_ICU_any_treat], type = "l")

plot(apply(none$output[, index$number_NoICU], 1, sum), type = "l")
lines(apply(type3$output[, index$number_NoICU], 1, sum), type = "l", col = "red")

sum(apply(none$output[, index$number_NoICU], 1, sum))
sum(apply(type3$output[, index$number_NoICU], 1, sum))

sum(none$output[, index$number_get_ICU_any_treat])
sum(type3$output[, index$number_get_ICU_any_treat])

sum(none$output[, index$number_get_ICU_full_treat])
sum(type3$output[, index$number_get_ICU_full_treat])



#a <- sum(type3$output[, index$number_ISev_GetICU_GetOx]) - sum(none$output[, index$number_ISev_GetICU_GetOx])
b <- sum(none$output[, index$number_ISev_GetICU_NoOx]) - sum(type3$output[, index$number_ISev_GetICU_NoOx])
c <- sum(none$output[, index$number_ISev_NoICU_NoOx]) - sum(type3$output[, index$number_ISev_NoICU_NoOx])

b <- apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) - apply(type3$output[, index$number_ISev_GetICU_NoOx], 2, sum)
sum((b * none$parameters$prob_severe_death_get_ICU_no_ox_baseline) - (b * none$parameters$prob_severe_death_get_ICU_get_ox_baseline))

c <- apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(type3$output[, index$number_ISev_NoICU_NoOx], 2, sum)
sum((c * none$parameters$prob_severe_death_no_ICU_no_ox) - (c * none$parameters$prob_severe_death_get_ICU_get_ox_baseline))

sum(none$output[, index$number_ICrit_GetICU_GetOx_GetMV]) - sum(type3$output[, index$number_ICrit_GetICU_GetOx_GetMV])
sum(none$output[, index$number_ICrit_GetICU_GetOx_NoMV]) - sum(type3$output[, index$number_ICrit_GetICU_GetOx_NoMV])
sum(none$output[, index$number_ICrit_GetICU_NoOx_NoMV]) - sum(type3$output[, index$number_ICrit_GetICU_NoOx_NoMV])
sum(none$output[, index$number_ICrit_NoICU_NoOx_NoMV]) - sum(type3$output[, index$number_ICrit_NoICU_NoOx_NoMV])

d <- apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum)
sum((d * none$parameters$prob_critical_death_no_ICU_no_ox_no_MV) - (d * none$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

e <- apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum)
sum((e * none$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline) - (e * none$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

f <- apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - apply(type3$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum)
sum((f * none$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline) - (f * none$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

g <- apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) - apply(type3$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum)
sum((g * none$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline) - (g * none$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline))

sum(type3$output[, index$number_ICrit_GetICU_GetOx_GetMV]) - sum(none$output[, index$number_ICrit_GetICU_GetOx_GetMV])



unlim <- run_apothecary(country = "Bhutan", R0 = R0, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = 1000000000, ICU_bed_capacity = 1000000000,
                       prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
                       MV_capacity = 1000000000)

plot(unlim$output[, index$overall_ICU_occ])


none$parameters$gamma_IMod_GetHosp_GetOx_Die
none$parameters$gamma_IMod_GetHosp_GetOx_Surv

type3$parameters$gamma_IMod_GetHosp_GetOx_Die
type3$parameters$gamma_IMod_GetHosp_GetOx_Surv


none$output[, index$gamma_IMod_GetHosp_GetOx_Surv][1]
none$output[, index$gamma_IMod_GetHosp_GetOx_Surv_Drug_8][1]
type3$output[, index$gamma_IMod_GetHosp_GetOx_Surv][1]
type3$output[, index$gamma_IMod_GetHosp_GetOx_Surv_Drug_8][1]

none$output[, index$gamma_ISev_GetICU_GetOx_Surv][1]
none$output[, index$gamma_ISev_GetICU_GetOx_Surv_Drug_9][1]
type3$output[, index$gamma_ISev_GetICU_GetOx_Surv][1]
type3$output[, index$gamma_ISev_GetICU_GetOx_Surv_Drug_9][1]
