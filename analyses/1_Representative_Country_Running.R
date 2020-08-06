# Loading required libraries
library(dplyr); library(scales); library(countrycode); library(ggplot2); library(tidyr)

# Loading apothecary and associated functions
devtools::load_all()

# Representative countries for each income group
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
colours_1 <- c("#76B523", "#DBD3AD", "#657ED4", "#EF6F6C")
colours_2 <- alpha(colours_1, alpha = 0.5)

# Loading in estimates of hospital and ICU bed capacity
hosp_beds <- income_strata_healthcare_capacity$hosp_beds # hosp beds per 1000 pop by income strata
ICU_beds <- income_strata_healthcare_capacity$ICU_beds # ICU beds per 1000 pop by income strata

# Function to calculate standard population demographic structure with 50,000,000 people
calc_std_pop <- function(country) {
  population <- get_population(country)
  raw_pop <- population$n
  raw_total_pop <- sum(raw_pop)
  adj_pop <- round(50000000/raw_total_pop * raw_pop)
  return(adj_pop)
}

# Processing Rt estimates
cut_date <- "2020-03-10"
initial_Rts <- readRDS(file = "C:/Users/cw1716/Downloads/rts_spline_08_03.rds") %>%
  mutate(country = countrycode(iso, "iso3c", "country.name")) %>%
  left_join(income_group, by = c("country" = "country")) %>%
  filter(date <= cut_date, country != "Equatorial Guinea", !is.na(income_group)) %>%
  select(iso, income_group, Rt_median) %>%
  group_by(income_group) %>%
  summarise(initial_Rt = median(Rt_median))

Rts <- readRDS(file = "C:/Users/cw1716/Downloads/rts_spline_08_03.rds") %>%
  mutate(country = countrycode(iso, "iso3c", "country.name")) %>%
  left_join(income_group, by = c("country" = "country")) %>%
  left_join(initial_Rts, by = "income_group") %>%
  filter(country != "Equatorial Guinea", !is.na(income_group)) %>%
  select(iso, income_group, date, Rt_median, initial_Rt) %>%
  mutate(Rt_median = ifelse(date <= cut_date, initial_Rt, Rt_median)) %>%
  group_by(date) %>%
  group_by(income_group, date) %>%
  summarise(Rt = median(Rt_median), initial_Rt = min(Rt_median)) %>%
  complete(date = seq.Date(as.Date("2019-12-21"), max(date) + 365 - 227, by = "day"),
           fill = list(Rt = NA)) %>%
  mutate(date = as.Date(date)) %>%
  fill(Rt, .direction = "updown") %>%
  mutate(Rt = ifelse(date > "2020-08-31", 1.1, Rt))

ggplot(Rts, aes(x = date, y = Rt, col = income_group)) +
  geom_line()

input_Rts <- Rts %>%
  mutate(tt_R0 = as.numeric(date - min(date))) %>%
  select(tt_R0, income_group, Rt) %>%
  pivot_wider(names_from = income_group, values_from = Rt)


# Running for each of the different representative settings for an unmitigated epidemic
unmitigated <- run_rep_settings(countries = countries, R0 = list(3, 3, 3, 3), tt_R0 = list(0, 0, 0, 0),
                                drug_effect = 0.5, scenario = "unlimited_healthcare")
unmitigated$control_scenario <- "unmitigated"

# Running for each of the different representative settings for a mitigated epidemic
R0 <- list(input_Rts$`Low income`, input_Rts$`Lower middle income`, input_Rts$`Upper middle income`, input_Rts$`High income`)
tt_R0 <- list(input_Rts$tt_R0, input_Rts$tt_R0, input_Rts$tt_R0, input_Rts$tt_R0)
mitigated <- run_rep_settings(countries = countries, R0 = R0, tt_R0 = tt_R0,
                              drug_effect = 0.5, scenario = "unlimited_healthcare")

par(mfrow = c(2, 2))
unmit_max_healthcare_results <- data.frame(scenario =  "unmit_max_healthcare", deaths_no_drugs = rep(0, 4), deaths_drugs = rep(0, 4))
for (i in 1:4) {

  # Picking a country and extracting relevant standardised population and mixing matrix
  set.seed(10)
  country <- countries[i]
  adj_pop <- calc_std_pop(country)
  mixing_matrix <- get_mixing_matrix(country)

  # Running an unmitigated scenario with unlimited healthcare capacity
  unmit_no_drugs <- run_apothecary(country = country,
                                   population = adj_pop,
                                   contact_matrix_set = mixing_matrix,
                                   R0 = R0[[i]],
                                   tt_R0 = tt_R0[[i]],
                                   hosp_beds = 100000000,
                                   ICU_beds = 10000000,
                                   day_return = TRUE)

  unmit_no_drugs_results <- unmit_no_drugs$output
  index <- apothecary:::odin_index(unmit_no_drugs$model)
  plot(apply(unmit_no_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_no_drugs_results[, index$D_Community, 1], 1, sum),
       type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day", col = colours_1[i])


  # Running an unmitigated scenario with unlimited healthcare capacity AND drug effects
  set.seed(10)
  drug_effect <- 0.5
  unmit_drugs <- run_apothecary(country = country,
                                population = adj_pop,
                                contact_matrix_set = mixing_matrix,
                                R0 = R0[[i]],
                                tt_R0 = tt_R0[[i]],
                                hosp_beds = 100000000,
                                ICU_beds = 10000000,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = drug_effect, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = drug_effect, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = drug_effect, drug_13_prop_treat = 1,
                                day_return = TRUE)

  unmit_drugs_results <- unmit_drugs$output
  index <- apothecary:::odin_index(unmit_drugs$model)
  lines(apply(unmit_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_drugs_results[, index$D_Community, 1], 1, sum),
        type = "l", lwd = 3, col = colours_2[i])

  unmit_max_healthcare_results[i, 2] <- max(apply(unmit_no_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_no_drugs_results[, index$D_Community, 1], 1, sum))
  unmit_max_healthcare_results[i, 3] <- max(apply(unmit_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_drugs_results[, index$D_Community, 1], 1, sum))

}

# Running for each of the different representative settings
par(mfrow = c(2, 2))
unmit_limited_healthcare_results <- data.frame(scenario =  "unmit_limited_healthcare", deaths_no_drugs = rep(0, 4), deaths_drugs = rep(0, 4))
for (i in 1:4) {

  # Picking a country and extracting relevant standardised population and mixing matrix
  set.seed(10)
  country <- countries[i]
  adj_pop <- calc_std_pop(country)
  mixing_matrix <- get_mixing_matrix(country)

  # Running an unmitigated scenario with unlimited healthcare capacity
  unmit_no_drugs <- run_apothecary(country = country,
                                   population = adj_pop,
                                   contact_matrix_set = mixing_matrix,
                                   R0 = 3,
                                   tt_R0 = 3,
                                   hosp_beds = NULL,
                                   ICU_beds = NULL,
                                   day_return = TRUE)

  unmit_no_drugs_results <- unmit_no_drugs$output
  index <- apothecary:::odin_index(unmit_no_drugs$model)
  plot(apply(unmit_no_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_no_drugs_results[, index$D_Community, 1], 1, sum),
       type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day", col = colours_1[i])


  # Running an unmitigated scenario with unlimited healthcare capacity AND drug effects
  set.seed(10)
  drug_effect <- 0.5
  unmit_drugs <- run_apothecary(country = country,
                                population = adj_pop,
                                contact_matrix_set = mixing_matrix,
                                R0 = 3,
                                tt_R0 = 3,
                                hosp_beds = NULL,
                                ICU_beds = NULL,
                                drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = drug_effect, drug_11_prop_treat = 1,
                                drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = drug_effect, drug_12_prop_treat = 1,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = drug_effect, drug_13_prop_treat = 1,
                                day_return = TRUE)

  unmit_drugs_results <- unmit_drugs$output
  index <- apothecary:::odin_index(unmit_drugs$model)
  lines(apply(unmit_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_drugs_results[, index$D_Community, 1], 1, sum),
        type = "l", lwd = 3, col = colours_2[i])

  unmit_limited_healthcare_results[i, 2] <- max(apply(unmit_no_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_no_drugs_results[, index$D_Community, 1], 1, sum))
  unmit_limited_healthcare_results[i, 3] <- max(apply(unmit_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_drugs_results[, index$D_Community, 1], 1, sum))

}
