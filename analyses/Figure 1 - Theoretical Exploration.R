# Loading required libraries
library(dplyr); library(scales); library(countrycode); library(ggplot2); library(tidyr)

# Loading apothecary and associated functions
devtools::load_all()

# Default parameters for the model
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
total_population <- 50000000
prop_hosp_beds_oxygen <- c(0.1, 0.3, 0.6, 1)
prop_ICU_beds_oxygen <- c(0.2, 0.4, 0.8, 1)
MVs <- c(200, 1000, 4000, 10000)
hosp_bed_capacity <- squire::income_strata_healthcare_capacity$hosp_beds * total_population/1000
ICU_bed_capacity <- squire::income_strata_healthcare_capacity$ICU_beds * total_population/1000
R0_seq <- seq(1.25, 3, 0.25)
time_period <- 1000
overall_results <- vector(mode = "list", length = 4)

# Running the Model
for (i in 1:4) {

  # Creating Results Dataframes for Storage
  unlimited_results <- data.frame(income_strata = rep(income_strata[i],length(R0_seq)), R0 = R0_seq,
                                  constraint = rep("none", length(R0_seq)), no_drug = NA, drug = NA)
  limited_beds_results <- data.frame(income_strata = rep(income_strata[i],length(R0_seq)), R0 = R0_seq,
                                     constraint = rep("beds", length(R0_seq)), no_drug = NA, drug = NA)
  limited_oxygen_results <- data.frame(income_strata = rep(income_strata[i],length(R0_seq)), R0 = R0_seq,
                                       constraint = rep("oxygen", length(R0_seq)), no_drug = NA, drug = NA)

  # Creating Standard Population for Each Income Strata
  country_population <- get_population(countries[i])
  population <- round((country_population$n * total_population) / sum(country_population$n))

  # Model Running for Unlimited Healthcare
  unlimited_no_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                           prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  unlimited_results$no_drug <- unlist(unlimited_no_drug)

  unlimited_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                           prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5, drug_11_prop_treat = 1,
                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5, drug_12_prop_treat = 1,
                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_prop_treat = 1,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  unlimited_results$drug <- unlist(unlimited_drug)

  # Model Running for Limited Healthcare - Beds Only
  limited_beds_no_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
                           MV_capacity = 10000000000,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_beds_results$no_drug <- unlist(limited_beds_no_drug)

  limited_beds_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1,
                           MV_capacity = 10000000000,
                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5, drug_11_prop_treat = 1,
                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5, drug_12_prop_treat = 1,
                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_prop_treat = 1,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_beds_results$drug <- unlist(limited_beds_drug)

  # Model Running for Limited Healthcare - Beds and Oxygen
  limited_beds_limited_oxygen_no_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = prop_hosp_beds_oxygen[i], prop_ox_ICU_beds = prop_ICU_beds_oxygen[i],
                           MV_capacity = 10000000000,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_oxygen_results$no_drug <- unlist(limited_beds_limited_oxygen_no_drug)

  limited_beds_limited_oxygen_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = prop_hosp_beds_oxygen[i], prop_ox_ICU_beds = prop_ICU_beds_oxygen[i],
                           MV_capacity = 10000000000,
                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5, drug_11_prop_treat = 1,
                           #drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.25,
                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5, drug_12_prop_treat = 1,
                           #drug_11_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.25,
                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_prop_treat = 1,
                           #drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_GetOx_NoMV_effect_size = 0.5,
                           #drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_NoOx_NoMV_effect_size = 0.5,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_oxygen_results$drug <- unlist(limited_beds_limited_oxygen_drug)

  # Creating and Storing Overall Results Dataframe
  overall_results[[i]] <- rbind(unlimited_results, limited_beds_results, limited_oxygen_results)

  print(i)
}

plotting <- do.call(rbind, overall_results) %>%
  mutate(prop_averted = (1 - (drug/no_drug))/0.5) %>%
  filter(constraint == "beds")
ggplot(plotting, aes(x = R0, y = prop_averted, col = income_strata, group = interaction(income_strata, constraint))) +
  geom_line() +
  lims(y = c(0, 1.05))

ggplot(plotting, aes(x = R0, y = prop_averted, fill = constraint, group = interaction(constraint,income_strata))) +
  geom_bar(stat = "identity", position = "dodge") +
  lims(y = c(0, 1.05))


# Running the Model
time_period <- 500
var_prop_hosp_bed_oxygen <- seq(0, 1, 0.2)
var_prop_ICU_bed_oxygen <- seq(0, 1, 0.2)
oxygen_variables <- expand.grid(prop_hosp_beds_oxygen = var_prop_hosp_bed_oxygen, prop_ICU_beds_oxygen = var_prop_ICU_bed_oxygen)
combinations <- length(oxygen_variables$prop_hosp_beds_oxygen)
R0 <- 1.5
overall_results <- vector(mode = "list", length = 4)
for (i in 1:4) {

  # Creating Results Dataframes for Storage
  limited_oxygen_results <- data.frame(income_strata = rep(income_strata[i], combinations),
                                       prop_hosp_oxygen = oxygen_variables$prop_hosp_beds_oxygen,
                                       prop_ICU_oxygen =  oxygen_variables$prop_ICU_beds_oxygen,
                                       no_drug = NA,
                                       drug = NA)

  # Creating Standard Population for Each Income Strata
  country_population <- get_population(countries[i])
  population <- round((country_population$n * total_population) / sum(country_population$n))

  # Model Running for Unlimited Healthcare
  no_drug <- lapply(seq_along(oxygen_variables$prop_hosp_beds_oxygen), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0,
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = oxygen_variables$prop_hosp_beds_oxygen[x],
                           prop_ox_ICU_beds = oxygen_variables$prop_ICU_beds_oxygen[x],
                           MV_capacity = 10000000000,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_oxygen_results$no_drug <- unlist(no_drug)

  drug <- lapply(seq_along(oxygen_variables$prop_hosp_beds_oxygen), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0,
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = oxygen_variables$prop_hosp_beds_oxygen[x],
                           prop_ox_ICU_beds = oxygen_variables$prop_ICU_beds_oxygen[x],
                           MV_capacity = 10000000000,
                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5, drug_11_prop_treat = 1,
                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5, drug_12_prop_treat = 1,
                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_prop_treat = 1,
                           model = "deterministic", time_period = time_period, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    return(deaths)
  })
  limited_oxygen_results$drug <- unlist(drug)

  # Creating and Storing Overall Results Dataframe
  overall_results[[i]] <- limited_oxygen_results
  print(i)

}

plotting <- do.call(rbind, overall_results) %>%
  mutate(prop_averted = (1 - (drug/no_drug))/0.5)

palettes <- list(Greens, Reds, Yellows, Blues)
ggplot(plotting, aes(x = prop_hosp_oxygen, y = prop_ICU_oxygen, fill = prop_averted)) +
  geom_tile() +
  theme_minimal() +
  facet_grid(~income_strata, scales = "free", space = "free") +
  scale_fill_viridis_c(option = "magma", begin = 1, end = 0)








plot(R0_seq, unlist(limited_no_drug), type = "l", ylim = c(0, max(unlist(limited))))
lines(R0_seq, unlist(limited_drug), type = "l", ylim = c(0, max(unlist(limited))))

lines(R0_seq, unlist(unlimited_no_drug), type = "l", ylim = c(0, max(unlist(limited))))
lines(R0_seq, unlist(unlimited_drug), type = "l", ylim = c(0, max(unlist(limited))))

potential_deaths_that_can_be_averted <- 0.5
potential_deaths_that_are_averted <- (1 - unlist(limited_drug)/unlist(limited_no_drug))/potential_deaths_that_can_be_averted
plot(potential_deaths_that_are_averted, ylim = c(0, 1), type = "l")



plot(R0_seq, unlist(1 - ((unlist(limited_drug)/unlist(limited_no_drug))/0.5))


lines(R0_seq, unlist(unlimited))
lines(R0_seq, unlist(no_drug))


rop_deaths_averted <- 1 -

plot(R0_seq, unlist(unlimited)/unlist(limited), ylim = c(0, 1), type = "l")

plot(R0_seq, unlist(no_drug))
lines(R0_seq, unlist(drug))

plot(R0_seq, unlist(drug)/unlist(no_drug) + unlist(drug))








i <- 1
for (i in 1:4) {
  country_population <- get_population(countries[i])
  population <- round((country_population$n * total_population) / sum(country_population$n))
  no_drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = prop_hosp_beds_oxygen[i], prop_ox_ICU_beds = prop_ICU_beds_oxygen[i],
                           model = "deterministic", time_period = 1250, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    print(x)
    return(deaths)
  })
  drug <- lapply(seq_along(R0_seq), function(x) {
    temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                           hosp_bed_capacity = hosp_bed_capacity[i], ICU_bed_capacity = ICU_bed_capacity[i],
                           prop_ox_hosp_beds = 1,# prop_hosp_beds_oxygen[i],
                           prop_ox_ICU_beds = 1, #prop_ox_ICU_beds = prop_ICU_beds_oxygen[i],
                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5,
                           drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.5, drug_11_prop_treat = 1,
                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5,
                           drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.5, drug_12_prop_treat = 1,
                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5,
                           drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_prop_treat = 1,
                           model = "deterministic", time_period = 1250, day_return = TRUE)
    index <- squire:::odin_index(temp$model)
    deaths <- max(apply(temp$output[, index$D], 1, sum))
    print(x)
    return(deaths)
  })
}

deaths_averted <- unlist(no_drug) - unlist(drug)
plot(R0_seq, deaths_averted/unlist(no_drug), ylim = c(0, 1))

plot(R0_seq, unlist(no_drug))
lines(R0_seq, unlist(drug))

plot(R0_seq, unlist(drug)/unlist(no_drug) + unlist(drug))


unlimited_no_drug <- lapply(seq_along(R0_seq), function(x) {
  temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                         hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                         model = "deterministic", time_period = time_period, day_return = TRUE)
  index <- squire:::odin_index(temp$model)
  deaths <- max(apply(temp$output[, index$D], 1, sum))
  print(x)
  return(deaths)
})

unlimited_drug <- lapply(seq_along(R0_seq), function(x) {
  temp <- run_apothecary(country = countries[i], population = population, R0 = R0_seq[x],
                         hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                         drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5,
                         drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.5, drug_11_prop_treat = 1,
                         drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5,
                         drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.5, drug_12_prop_treat = 1,
                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5,
                         drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_GetOx_NoMV_effect_size = 0.5,
                         drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_prop_treat = 1,
                         model = "deterministic", time_period = time_period, day_return = TRUE)
  index <- squire:::odin_index(temp$model)
  deaths <- max(apply(temp$output[, index$D], 1, sum))
  print(x)
  return(deaths)
})
