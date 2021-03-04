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

# Running the Model With No Drugs
none_low <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                           time_period = time, seeding_cases = 20, day_return = TRUE,
                           hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                           prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
none_high <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                            time_period = time, seeding_cases = 20, day_return = TRUE,
                            hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                            prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
index <- apothecary:::odin_index(none_low$model)
none_low_deaths <- max(apply(none_low$output[, index$D], 1, sum))
none_high_deaths <- max(apply(none_high$output[, index$D], 1, sum))

# Type 1 - e.g. Dexamethasone
num_splits <- 21
type1_effectiveness <- seq(0, 1, length.out = num_splits)
type1_coverage <- seq(0, 1, length.out = num_splits)
type1_low <- matrix(nrow = num_splits, ncol = num_splits)
type1_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type1_effectiveness)) {

  for (j in 1:length(type1_coverage)) {

    type1_cov <- type1_coverage[j]
    type1_crit_effect <- 1 - type1_effectiveness[i]
    type1_sev_effect <- 1 - type1_effectiveness[i]
    type1_mod_effect <- 1 - type1_effectiveness[i]

    type1_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                                    drug_11_prop_treat = type1_cov, drug_11_GetOx_effect_size = type1_mod_effect, drug_11_NoOx_effect_size = type1_mod_effect + 0.5 * (1 - type1_mod_effect),
                                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                                    drug_12_prop_treat = type1_cov, drug_12_GetOx_effect_size = type1_sev_effect, drug_12_NoOx_effect_size = type1_sev_effect + 0.5 * (1 - type1_sev_effect),
                                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                                    drug_13_prop_treat = type1_cov, drug_13_GetOx_GetMV_effect_size = type1_crit_effect,
                                    drug_13_GetOx_NoMV_effect_size = type1_crit_effect + 0.5 * (1 - type1_crit_effect), drug_13_NoOx_NoMV_effect_size = 1)
    type1_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                                     drug_11_prop_treat = type1_cov, drug_11_GetOx_effect_size = type1_mod_effect, drug_11_NoOx_effect_size = type1_mod_effect + 0.5 * (1 - type1_mod_effect),
                                     drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                                     drug_12_prop_treat = type1_cov, drug_12_GetOx_effect_size = type1_sev_effect, drug_12_NoOx_effect_size = type1_sev_effect + 0.5 * (1 - type1_sev_effect),
                                     drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                                     drug_13_prop_treat = type1_cov, drug_13_GetOx_GetMV_effect_size = type1_crit_effect,
                                     drug_13_GetOx_NoMV_effect_size = type1_crit_effect + 0.5 * (1 - type1_crit_effect), drug_13_NoOx_NoMV_effect_size = 1)

    type1_low_deaths <- max(apply(type1_low_run$output[, index$D], 1, sum))
    type1_high_deaths <- max(apply(type1_high_run$output[, index$D], 1, sum))

    type1_low[i, j] <- type1_low_deaths
    type1_high[i, j] <- type1_high_deaths

    print(j)

  }

  print(i)
}

heatmap(type1_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type1_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")
saveRDS(type1_low, "analysis_Figure4/Outputs/sensitivity_analysis/type1_low.rds")
saveRDS(type1_high, "analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds")

# Type 2 - e.g. Ivermectin
type2_effectiveness <- seq(0, 1, length.out = num_splits)
type2_coverage <- seq(0, 1, length.out = num_splits)
type2_low <- matrix(nrow = num_splits, ncol = num_splits)
type2_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type2_effectiveness)) {

  for (j in 1:length(type2_coverage)) {

    type2_cov <- type2_coverage[j]
    type2_effect <- type2_effectiveness[i]

    type2_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_6_indic = 1, drug_6_prop_treat = type2_cov, drug_6_effect_size = type2_effect)

    type2_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_6_indic = 1, drug_6_prop_treat = type2_cov, drug_6_effect_size = type2_effect)

    type2_low_deaths <- max(apply(type2_low_run$output[, index$D], 1, sum))
    type2_high_deaths <- max(apply(type2_high_run$output[, index$D], 1, sum))

    type2_low[i, j] <- type2_low_deaths
    type2_high[i, j] <- type2_high_deaths

    print(c(i, j, type2_low_deaths, type2_high_deaths))

  }

}

for (i in 1:num_splits) {
  type2_low[i, ] <- approx(x = seq(1, num_splits), y = type2_low[i, ], n = num_splits)$y
  type2_high[i, ] <- approx(x = seq(1, num_splits), y = type2_high[i, ], n = num_splits)$y
}

heatmap(type2_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type2_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")

saveRDS(type2_low, "analysis_Figure4/Outputs/sensitivity_analysis/type2_low.rds")
saveRDS(type2_high, "analysis_Figure4/Outputs/sensitivity_analysis/type2_high.rds")

# Type 3 - e.g. Remdesivir
duration_prop <- seq(0.01, 1, length.out = num_splits)
type3_effectiveness <- 1/duration_prop
type3_effectiveness <- type3_effectiveness
type3_coverage <- seq(0, 1, length.out = num_splits)
type3_low <- matrix(nrow = num_splits, ncol = num_splits)
type3_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type3_effectiveness)) {

  for (j in 1:length(type3_coverage)) {

    type3_cov <- type3_coverage[j]
    type3_dur_effect <- type3_effectiveness[i]

    type3_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = type3_cov,
                                    drug_8_GetOx_effect_size = type3_dur_effect, drug_8_NoOx_effect_size = 1,
                                    drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = type3_cov,
                                    drug_9_GetOx_effect_size = type3_dur_effect, drug_9_NoOx_effect_size = 1,
                                    drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = type3_cov,
                                    drug_10_GetOx_GetMV_effect_size = type3_dur_effect, drug_10_GetOx_NoMV_effect_size = 1, drug_10_NoOx_NoMV_effect_size = 1)

    type3_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = type3_cov,
                                     drug_8_GetOx_effect_size = type3_dur_effect, drug_8_NoOx_effect_size = 1,
                                     drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = type3_cov,
                                     drug_9_GetOx_effect_size = type3_dur_effect, drug_9_NoOx_effect_size = 1,
                                     drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = type3_cov,
                                     drug_10_GetOx_GetMV_effect_size = type3_dur_effect, drug_10_GetOx_NoMV_effect_size = 1, drug_10_NoOx_NoMV_effect_size = 1)

    type3_low_deaths <- max(apply(type3_low_run$output[, index$D], 1, sum))
    type3_high_deaths <- max(apply(type3_high_run$output[, index$D], 1, sum))

    type3_low[i, j] <- type3_low_deaths
    type3_high[i, j] <- type3_high_deaths

    print(c(i, j, type3_low_deaths, type3_high_deaths))
  }

}

heatmap(type3_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type3_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")

saveRDS(type3_low, "analysis_Figure4/Outputs/sensitivity_analysis/type3_low.rds")
saveRDS(type3_high, "analysis_Figure4/Outputs/sensitivity_analysis/type3_high.rds")

# Type 4 - e.g. monoclonal antibodies (prevent hospitalisation)
type4_effectiveness <- seq(0, 1, length.out = num_splits)
type4_coverage <- seq(0, 1, length.out = num_splits)
type4_low <- matrix(nrow = num_splits, ncol = num_splits)
type4_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type4_effectiveness)) {

  for (j in 1:length(type4_coverage)) {

    type4_cov <- type4_coverage[j]
    type4_effect <- type4_effectiveness[i]

    type4_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_3_indic = 1, drug_3_prop_treat = type4_cov, drug_3_effect_size = type4_effect)

    type4_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_3_indic = 1, drug_3_prop_treat = type4_cov, drug_3_effect_size = type4_effect)

    type4_low_deaths <- max(apply(type4_low_run$output[, index$D], 1, sum))
    type4_high_deaths <- max(apply(type4_high_run$output[, index$D], 1, sum))

    type4_low[i, j] <- type4_low_deaths
    type4_high[i, j] <- type4_high_deaths

    print(c(i, j, type4_low_deaths, type4_high_deaths))

  }

}

heatmap(type4_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type4_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")
saveRDS(type4_low, "analysis_Figure4/Outputs/sensitivity_analysis/type4_low.rds")
saveRDS(type4_high, "analysis_Figure4/Outputs/sensitivity_analysis/type4_high.rds")

# Type 5 - e.g. monoclonal antibodies (reduce duration of infection)
duration_prop <- seq(0.01, 1, length.out = num_splits)
type5_effectiveness <- 1/duration_prop
type5_effectiveness <- type5_effectiveness
type5_coverage <- seq(0, 1, length.out = num_splits)
type5_low <- matrix(nrow = num_splits, ncol = num_splits)
type5_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type5_effectiveness)) {

  for (j in 1:length(type5_coverage)) {

    type5_cov <- type5_coverage[j]
    type5_dur_effect <- type5_effectiveness[i]

    type5_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                    drug_4_prop_treat = type5_cov, drug_4_effect_size_IAsymp = 1, drug_4_effect_size_IMild = type5_dur_effect, drug_4_effect_size_ICase = 1) # drug_4_effect_size_ICase only applied to those where hosp is averted through drug_3_effect so not relevant here

    type5_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                     drug_4_prop_treat = type5_cov, drug_4_effect_size_IAsymp = 1, drug_4_effect_size_IMild = type5_dur_effect, drug_4_effect_size_ICase = 1) # drug_4_effect_size_ICase only applied to those where hosp is averted through drug_3_effect so not relevant here

    type5_low_deaths <- max(apply(type5_low_run$output[, index$D], 1, sum))
    type5_high_deaths <- max(apply(type5_high_run$output[, index$D], 1, sum))

    type5_low[i, j] <- type5_low_deaths
    type5_high[i, j] <- type5_high_deaths

    print(c(i, j, type5_low_deaths, type5_high_deaths))

  }

}

heatmap(type5_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type5_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")
saveRDS(type5_low, "analysis_Figure4/Outputs/sensitivity_analysis/type5_low.rds")
saveRDS(type5_high, "analysis_Figure4/Outputs/sensitivity_analysis/type5_high.rds")

# Type 6 - e.g. monoclonal antibodies (reduce duration of infection) but given to asymptomatics
duration_prop <- seq(0.01, 1, length.out = num_splits)
type6_effectiveness <- 1/duration_prop
type6_effectiveness <- type6_effectiveness
type6_coverage <- seq(0, 1, length.out = num_splits)
type6_low <- matrix(nrow = num_splits, ncol = num_splits)
type6_high <- matrix(nrow = num_splits, ncol = num_splits)
for (i in 1:length(type6_effectiveness)) {

  for (j in 1:length(type6_coverage)) {

    type6_cov <- type6_coverage[j]
    type6_dur_effect <- type6_effectiveness[i]

    type6_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                    time_period = time, seeding_cases = 20, day_return = TRUE,
                                    hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                    prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                    drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                    drug_4_prop_treat = type6_cov, drug_4_effect_size_IAsymp = type6_dur_effect, drug_4_effect_size_IMild = type6_dur_effect, drug_4_effect_size_ICase = 1)

    type6_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                     time_period = time, seeding_cases = 20, day_return = TRUE,
                                     hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                     prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                     drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                     drug_4_prop_treat = type6_cov, drug_4_effect_size_IAsymp = type6_dur_effect, drug_4_effect_size_IMild = type6_dur_effect, drug_4_effect_size_ICase = 1)

    type6_low_deaths <- max(apply(type6_low_run$output[, index$D], 1, sum))
    type6_high_deaths <- max(apply(type6_high_run$output[, index$D], 1, sum))

    type6_low[i, j] <- type6_low_deaths
    type6_high[i, j] <- type6_high_deaths

    print(c(i, j, type6_low_deaths, type6_high_deaths))

  }

}

heatmap(type6_low/none_low_deaths, Colv = NA, Rowv = NA, scale="none")
heatmap(type6_high/none_high_deaths, Colv = NA, Rowv = NA, scale = "none")
saveRDS(type6_low, "analysis_Figure4/Outputs/sensitivity_analysis/type6_low.rds")
saveRDS(type6_high, "analysis_Figure4/Outputs/sensitivity_analysis/type6_high.rds")

# par(mfrow = c(1, 2))
# plot(type1_effectiveness, type1_low/none_low_deaths, type = "l", ylim = c(0, 1))
# lines(type2_effectiveness, type2_low/none_low_deaths, col = "red")
# lines(1-duration_prop, type3_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "blue")
# lines(type4_effectiveness, type4_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "orange")
# lines(1-duration_prop, type5_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "dark green")
#
# plot(type1_effectiveness, type1_high/none_high_deaths, type = "l", ylim = c(0, 1))
# lines(type2_effectiveness, type2_high/none_high_deaths, col = "red")
# lines(1-duration_prop, type3_high/none_high_deaths, type = "l", col = "blue")
# lines(type4_effectiveness, type4_high/none_high_deaths, type = "l", col = "orange")
# lines(1-duration_prop, type5_high/none_high_deaths, type = "l", col = "dark green")
#
# total_length <- length(type1_effectiveness) + length(type2_effectiveness) + length(type3_effectiveness) +
#   length(type4_effectiveness) + length(type5_effectiveness)
#
# df <- data.frame(
#   drug = c(rep("type_1", length(type1_effectiveness)),
#            rep("type_2", length(type2_effectiveness)),
#            rep("type_3", length(type3_effectiveness)),
#            rep("type_4", length(type4_effectiveness)),
#            rep("type_5", length(type5_effectiveness))),
#   R0 = c(rep("low", total_length), rep("high", total_length)),
#   averted = c(type1_low/none_low_deaths, type2_low/none_low_deaths, type3_low/none_low_deaths,
#               type4_low/none_low_deaths, type5_low/none_low_deaths,
#               type1_high/none_high_deaths, type2_high/none_high_deaths, type3_high/none_high_deaths,
#               type4_high/none_high_deaths, type5_high/none_high_deaths),
#   effectiveness = c(type1_effectiveness, type2_effectiveness, 1-duration_prop,
#                     type4_effectiveness, 1-duration_prop))
# saveRDS(df, "analysis_Figure4/Outputs/effectiveness_scan.rds")
#
# # nice red: e12f4a
# ggplot(df, aes(x = effectiveness, y = averted, col = drug)) +
#   geom_path() +
#   scale_y_continuous(position = "right") +
#   facet_grid(R0~.) +
#   labs(x = "Drug Effectiveness", y = "Proportion of Deaths Averted") +
#   scale_colour_manual(values = c("#8b82f8", "#f32bb1", "#f76c07", "#98cb66", "#009bfd")) +
#   theme(legend.position = "none",
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(size = 12))
#
#
