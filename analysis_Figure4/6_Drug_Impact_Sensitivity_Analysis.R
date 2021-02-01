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
type1_low <- c()
type1_high <- c()
type1_effectiveness <- seq(0, 1, length.out = 21)
for (i in 1:length(type1_effectiveness)) {

  type1_crit_effect <- 1 - type1_effectiveness[i]
  type1_sev_effect <- 1 - type1_effectiveness[i]
  type1_mod_effect <- 1 - type1_effectiveness[i]

  type1_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                             time_period = time, seeding_cases = 20, day_return = TRUE,
                             hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                             prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                             drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                             drug_11_prop_treat = 1, drug_11_GetOx_effect_size = type1_mod_effect, drug_11_NoOx_effect_size = type1_mod_effect + 0.5 * (1 - type1_mod_effect),
                             drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                             drug_12_prop_treat = 1, drug_12_GetOx_effect_size = type1_sev_effect, drug_12_NoOx_effect_size = type1_sev_effect + 0.5 * (1 - type1_sev_effect),
                             drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                             drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = type1_crit_effect,
                             drug_13_GetOx_NoMV_effect_size = type1_crit_effect + 0.5 * (1 - type1_crit_effect), drug_13_NoOx_NoMV_effect_size = 1)
  type1_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                              time_period = time, seeding_cases = 20, day_return = TRUE,
                              hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                              prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                              drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                              drug_11_prop_treat = 1, drug_11_GetOx_effect_size = type1_mod_effect, drug_11_NoOx_effect_size = type1_mod_effect + 0.5 * (1 - type1_mod_effect),
                              drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                              drug_12_prop_treat = 1, drug_12_GetOx_effect_size = type1_sev_effect, drug_12_NoOx_effect_size = type1_sev_effect + 0.5 * (1 - type1_sev_effect),
                              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                              drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = type1_crit_effect,
                              drug_13_GetOx_NoMV_effect_size = type1_crit_effect + 0.5 * (1 - type1_crit_effect), drug_13_NoOx_NoMV_effect_size = 1)

  type1_low_deaths <- max(apply(type1_low_run$output[, index$D], 1, sum))
  type1_high_deaths <- max(apply(type1_high_run$output[, index$D], 1, sum))

  type1_low <- c(type1_low, type1_low_deaths)
  type1_high <- c(type1_high, type1_high_deaths)

  print(i)
}
plot(type1_effectiveness, type1_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(type1_effectiveness, type1_high/none_high_deaths, type = "l", col = "red")


# Type 2 - e.g. Ivermectin
type2_low <- c()
type2_high <- c()
type2_effectiveness <- seq(0, 1, length.out = 21)
for (i in 1:length(type2_effectiveness)) {

  type2_effect <- type2_effectiveness[i]

  type2_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                time_period = time, seeding_cases = 20, day_return = TRUE,
                                hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = type2_effect)

  type2_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                 time_period = time, seeding_cases = 20, day_return = TRUE,
                                 hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                 prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                 drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = type2_effect)

  type2_low_deaths <- max(apply(type2_low_run$output[, index$D], 1, sum))
  type2_high_deaths <- max(apply(type2_high_run$output[, index$D], 1, sum))

  type2_low <- c(type2_low, type2_low_deaths)
  type2_high <- c(type2_high, type2_high_deaths)

  print(c(i, type2_low_deaths, type2_high_deaths))
}

type2_low[3] <- 95396.69
type2_low[5] <- 90368.44
type2_high[6] <- 186176.28
type2_high[12] <- 175877.75

plot(type2_effectiveness, type2_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(type2_effectiveness, type2_high/none_high_deaths, type = "l", col = "red")

# Type 3 - e.g. Remdesivir
type3_low <- c()
type3_high <- c()
duration_prop <- seq(0.01, 1, length.out = 19)
type3_effectiveness <- 1/duration_prop
for (i in 1:length(type3_effectiveness)) {

  type3_dur_effect <- type3_effectiveness[i]

  type3_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                time_period = time, seeding_cases = 20, day_return = TRUE,
                                hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                drug_8_GetOx_effect_size = type3_dur_effect, drug_8_NoOx_effect_size = 1,
                                drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = 1,
                                drug_9_GetOx_effect_size = type3_dur_effect, drug_9_NoOx_effect_size = 1,
                                drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = 1,
                                drug_10_GetOx_GetMV_effect_size = type3_dur_effect, drug_10_GetOx_NoMV_effect_size = 1, drug_10_NoOx_NoMV_effect_size = 1)

  type3_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                 time_period = time, seeding_cases = 20, day_return = TRUE,
                                 hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                 prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                 drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                                 drug_8_GetOx_effect_size = type3_dur_effect, drug_8_NoOx_effect_size = 1,
                                 drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = 1,
                                 drug_9_GetOx_effect_size = type3_dur_effect, drug_9_NoOx_effect_size = 1,
                                 drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = 1,
                                 drug_10_GetOx_GetMV_effect_size = type3_dur_effect, drug_10_GetOx_NoMV_effect_size = 1, drug_10_NoOx_NoMV_effect_size = 1)

  type3_low_deaths <- max(apply(type3_low_run$output[, index$D], 1, sum))
  type3_high_deaths <- max(apply(type3_high_run$output[, index$D], 1, sum))

  type3_low <- c(type3_low, type3_low_deaths)
  type3_high <- c(type3_high, type3_high_deaths)

  print(i)
}

plot(1-duration_prop, type3_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(1-duration_prop, type3_high/none_high_deaths, type = "l", col = "red")

# Type 4 - e.g. monoclonal antibodies (prevent hospitalisation)
type4_low <- c()
type4_high <- c()
type4_effectiveness <- seq(0, 1, length.out = 21)
for (i in 1:length(type4_effectiveness)) {

  type4_dur_effect <- type4_effectiveness[i]

  type4_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                  time_period = time, seeding_cases = 20, day_return = TRUE,
                                  hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                  prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                  drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = type4_dur_effect)

  type4_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                   time_period = time, seeding_cases = 20, day_return = TRUE,
                                   hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                   prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                   drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = type4_dur_effect)

  type4_low_deaths <- max(apply(type4_low_run$output[, index$D], 1, sum))
  type4_high_deaths <- max(apply(type4_high_run$output[, index$D], 1, sum))

  type4_low <- c(type4_low, type4_low_deaths)
  type4_high <- c(type4_high, type4_high_deaths)

  print(i)
}
plot(type4_effectiveness, type4_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(type4_effectiveness, type4_high/none_high_deaths, type = "l", col = "red")


# Type 5 - e.g. monoclonal antibodies (reduce duration of infection)
type5_low <- c()
type5_high <- c()
duration_prop <- seq(0.01, 1, length.out = 19)
type5_effectiveness <- 1/duration_prop
for (i in 1:length(type5_effectiveness)) {

  type5_dur_effect <- type5_effectiveness[i]

  type5_low_run <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                                  time_period = time, seeding_cases = 20, day_return = TRUE,
                                  hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                  prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                  drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                  drug_4_prop_treat = 1, drug_4_effect_size_IAsymp = 1, drug_4_effect_size_IMild = type5_dur_effect, drug_4_effect_size_ICase = 1)

  type5_high_run <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                                   time_period = time, seeding_cases = 20, day_return = TRUE,
                                   hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                                   prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity,
                                   drug_4_indic_IAsymp = 1, drug_4_indic_IMild = 1, drug_4_indic_ICase = 1,
                                   drug_4_prop_treat = 1, drug_4_effect_size_IAsymp = 1, drug_4_effect_size_IMild = type5_dur_effect, drug_4_effect_size_ICase = 1)

  type5_low_deaths <- max(apply(type5_low_run$output[, index$D], 1, sum))
  type5_high_deaths <- max(apply(type5_high_run$output[, index$D], 1, sum))

  type5_low <- c(type5_low, type5_low_deaths)
  type5_high <- c(type5_high, type5_high_deaths)

  print(i)
}
plot(1-duration_prop, type5_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(1-duration_prop, type5_high/none_high_deaths, type = "l", col = "red")

par(mfrow = c(1, 2))
plot(type1_effectiveness, type1_low/none_low_deaths, type = "l", ylim = c(0, 1))
lines(type2_effectiveness, type2_low/none_low_deaths, col = "red")
lines(1-duration_prop, type3_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "blue")
lines(type4_effectiveness, type4_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "orange")
lines(1-duration_prop, type5_low/none_low_deaths, type = "l", ylim = c(0, 1), col = "dark green")

plot(type1_effectiveness, type1_high/none_high_deaths, type = "l", ylim = c(0, 1))
lines(type2_effectiveness, type2_high/none_high_deaths, col = "red")
lines(1-duration_prop, type3_high/none_high_deaths, type = "l", col = "blue")
lines(type4_effectiveness, type4_high/none_high_deaths, type = "l", col = "orange")
lines(1-duration_prop, type5_high/none_high_deaths, type = "l", col = "dark green")

total_length <- length(type1_effectiveness) + length(type2_effectiveness) + length(type3_effectiveness) +
  length(type4_effectiveness) + length(type5_effectiveness)

df <- data.frame(
  drug = c(rep("type_1", length(type1_effectiveness)),
           rep("type_2", length(type2_effectiveness)),
           rep("type_3", length(type3_effectiveness)),
           rep("type_4", length(type4_effectiveness)),
           rep("type_5", length(type5_effectiveness))),
  R0 = c(rep("low", total_length), rep("high", total_length)),
  averted = c(type1_low/none_low_deaths, type2_low/none_low_deaths, type3_low/none_low_deaths,
              type4_low/none_low_deaths, type5_low/none_low_deaths,
              type1_high/none_high_deaths, type2_high/none_high_deaths, type3_high/none_high_deaths,
              type4_high/none_high_deaths, type5_high/none_high_deaths),
  effectiveness = c(type1_effectiveness, type2_effectiveness, 1-duration_prop,
                    type4_effectiveness, 1-duration_prop))
saveRDS(df, "analysis_Figure4/Outputs/effectiveness_scan.rds")

# nice red: e12f4a
ggplot(df, aes(x = effectiveness, y = averted, col = drug)) +
  geom_path() +
  scale_y_continuous(position = "right") +
  facet_grid(R0~.) +
  labs(x = "Drug Effectiveness", y = "Proportion of Deaths Averted") +
  scale_colour_manual(values = c("#8b82f8", "#f32bb1", "#f76c07", "#98cb66", "#009bfd")) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 12))


