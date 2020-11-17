# Loading required libraries
library(dplyr); library(scales); library(countrycode); library(ggplot2); library(tidyr)

# Loading apothecary and associated functions
devtools::load_all()

# Loading required functions
source("run_rep_settings_function.R")

# Representative countries for each income group
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
colours_1 <- c("#76B523", "#DBD3AD", "#657ED4", "#EF6F6C")
colours_2 <- alpha(colours_1, alpha = 0.5)

# Loading in estimates of hospital and ICU bed capacity
hosp_beds <- income_strata_healthcare_capacity$hosp_beds # hosp beds per 1000 pop by income strata
ICU_beds <- income_strata_healthcare_capacity$ICU_beds # ICU beds per 1000 pop by income strata

# Running for each of the different representative settings for an unmitigated epidemic for a healthcare based drug
R0_seq <- seq(1.1, 3.5, 0.1)
tt_R0 <- 0
x <- lapply(seq_along(R0_seq), function(x) {
  R0 <- R0_seq[x]
  tt_R0 <- 0
  unmitigated_lim_healthcare <- run_rep_settings(countries = countries, R0 = list(R0, R0, R0, R0), tt_R0 = list(tt_R0, tt_R0, tt_R0, tt_R0),
                                                 scenario = "limited_healthcare", time_period = 1500,
                                                 drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.5, drug_11_prop_treat = 1,
                                                 drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.5, drug_12_prop_treat = 1,
                                                 drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_prop_treat = 1)
  results <- unmitigated_lim_healthcare %>%
    mutate(prop_deaths_averted = (1 - deaths_drugs/deaths_no_drugs)/drug_effect)
  results$income_group = rep(c("LIC", "LMIC", "UMIC", "HIC"), 1)
  results$R0 <- R0
  print(x)
  return(results)
})
y <- do.call(rbind, x)
ggplot(y, aes(x = R0, y = prop_deaths_averted, col = income_group)) +
  geom_line()

# Running for each of the different representative settings for an unmitigated epidemic for a community based drug
R0_seq <- seq(1.1, 3.5, 0.1)
tt_R0 <- 0
x <- lapply(seq_along(R0_seq), function(x) {
  R0 <- R0_seq[x]
  tt_R0 <- 0
  unmitigated_lim_healthcare <- run_rep_settings(countries = countries, R0 = list(R0, R0, R0, R0), tt_R0 = list(tt_R0, tt_R0, tt_R0, tt_R0),
                                                 scenario = "limited_healthcare", time_period = 1500,
                                                 drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = 0.5)
results <- unmitigated_lim_healthcare %>%
    mutate(prop_deaths_averted = (1 - deaths_drugs/deaths_no_drugs)/drug_effect)
  results$income_group = rep(c("LIC", "LMIC", "UMIC", "HIC"), 1)
  results$R0 <- R0
  print(x)
  return(results)
})
y <- do.call(rbind, x)
ggplot(y, aes(x = R0, y = prop_deaths_averted, col = income_group)) +
  geom_line()


# Processing Rt estimates
# cut_date <- "2020-03-10"
# initial_Rts <- readRDS(file = "C:/Users/cw1716/Downloads/rts_spline_08_03.rds") %>%
#   mutate(country = countrycode(iso, "iso3c", "country.name")) %>%
#   left_join(income_group, by = c("country" = "country")) %>%
#   filter(date <= cut_date, country != "Equatorial Guinea", !is.na(income_group)) %>%
#   select(iso, income_group, Rt_median) %>%
#   group_by(income_group) %>%
#   summarise(initial_Rt = median(Rt_median))
#
# Rts <- readRDS(file = "C:/Users/cw1716/Downloads/rts_spline_08_03.rds") %>%
#   mutate(country = countrycode(iso, "iso3c", "country.name")) %>%
#   left_join(income_group, by = c("country" = "country")) %>%
#   left_join(initial_Rts, by = "income_group") %>%
#   filter(country != "Equatorial Guinea", !is.na(income_group)) %>%
#   select(iso, income_group, date, Rt_median, initial_Rt) %>%
#   mutate(Rt_median = ifelse(date <= cut_date, initial_Rt, Rt_median)) %>%
#   group_by(date) %>%
#   group_by(income_group, date) %>%
#   summarise(Rt = median(Rt_median), initial_Rt = min(Rt_median)) %>%
#   complete(date = seq.Date(as.Date("2019-12-21"), max(date) + 600 - 227, by = "day"),
#            fill = list(Rt = NA)) %>%
#   mutate(date = as.Date(date)) %>%
#   fill(Rt, .direction = "updown") %>%
#   mutate(Rt = ifelse(date > "2020-08-31", 1.1, Rt)) %>%
#   filter(date >= "2020-02-01")
#
# ggplot(Rts, aes(x = date, y = Rt, col = income_group)) +
#   geom_line()
#
# input_Rts <- Rts %>%
#   mutate(tt_R0 = as.numeric(date - min(date))) %>%
#   select(tt_R0, income_group, Rt) %>%
#   pivot_wider(names_from = income_group, values_from = Rt)
# R0 <- list(input_Rts$`Low income`, input_Rts$`Lower middle income`, input_Rts$`Upper middle income`, input_Rts$`High income`)
# tt_R0 <- list(input_Rts$tt_R0, input_Rts$tt_R0, input_Rts$tt_R0, input_Rts$tt_R0)

# par(mfrow = c(2, 4))
# unmit_max_healthcare_results <- data.frame(scenario =  "unmit_max_healthcare", deaths_no_drugs = rep(0, 4), deaths_drugs = rep(0, 4))
# for (i in 1:4) {
#
#   # Picking a country and extracting relevant standardised population and mixing matrix
#   set.seed(10)
#   country <- countries[i]
#   adj_pop <- calc_std_pop(country)
#   mixing_matrix <- get_mixing_matrix(country)
#
#   # Running an unmitigated scenario with unlimited healthcare capacity
#   unmit_no_drugs <- run_apothecary(country = country,
#                                    population = adj_pop,
#                                    contact_matrix_set = mixing_matrix,
#                                    R0 = R0[[i]],
#                                    tt_R0 = tt_R0[[i]],
#                                    hosp_beds = NULL,
#                                    ICU_beds = NULL,
#                                    day_return = TRUE,
#                                    time_period = length(R0[[1]]))
#
#   unmit_no_drugs_results <- unmit_no_drugs$output
#   index <- apothecary:::odin_index(unmit_no_drugs$model)
#
#   # Running an unmitigated scenario with unlimited healthcare capacity AND drug effects
#   set.seed(10)
#   drug_effect <- 0.5
#   unmit_drugs <- run_apothecary(country = country,
#                                 population = adj_pop,
#                                 contact_matrix_set = mixing_matrix,
#                                 R0 = R0[[i]],
#                                 tt_R0 = tt_R0[[i]],
#                                 hosp_beds = NULL,
#                                 ICU_beds = NULL,
#                                 drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = drug_effect, drug_11_prop_treat = 1,
#                                 drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = drug_effect, drug_12_prop_treat = 1,
#                                 drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_GetOx_GetMV_effect_size = drug_effect, drug_13_prop_treat = 1,
#                                 day_return = TRUE,
#                                 time_period = length(R0[[1]]))
#
#   unmit_drugs_results <- unmit_drugs$output
#
#   plot(apply(unmit_no_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_no_drugs_results[, index$D_Community, 1], 1, sum),
#        type = "l", lwd = 3, ylab = "Cumulative Deaths", xlab = "Day", col = colours_1[i])
#   lines(apply(unmit_drugs_results[, index$D_Hospital, 1], 1, sum) + apply(unmit_drugs_results[, index$D_Community, 1], 1, sum),
#         type = "l", lwd = 3, col = colours_2[i])
#
#   plot(apply(unmit_no_drugs_results[, index$n_E2_I, 1], 1, sum),
#        type = "l", lwd = 3, ylab = "Daily Infections", xlab = "Day", col = colours_1[i])
#   lines(apply(unmit_drugs_results[, index$n_E2_I, 1], 1, sum),
#         type = "l", lwd = 3, col = colours_2[i])
# }

# z <- y %>%
#   mutate(deaths_no_drugs_plot = deaths_no_drugs - deaths_drugs) %>%
#   select(R0, income_group, deaths_drugs, deaths_no_drugs_plot) %>%
#   pivot_longer(cols = c(deaths_no_drugs_plot, deaths_drugs), names_to = "drug_scenario", values_to = "deaths") %>%
#   mutate(drug_scenario = factor(drug_scenario, levels = c("deaths_no_drugs_plot", "deaths_drugs"))) %>%
#   mutate(income_group = factor(income_group, levels = c("LIC", "LMIC", "UMIC", "HIC")))
#
# ggplot(z, aes(x = income_group, y = deaths, fill = income_group, alpha = drug_scenario)) +
#   geom_bar(stat = "identity") +
#   scale_alpha_manual(values = c(0.5, 1)) +
#   facet_wrap(~ R0, nrow = 1) + theme(panel.spacing = unit(0, "lines"),
#                                      axis.text.x = element_blank(),
#                                      axis.title.x= element_blank())
#
#
#
# R0 <- 1.5
# tt_R0 <- 0
# unmitigated_inf_healthcare <- run_rep_settings(countries = countries, R0 = list(R0, R0, R0, R0), tt_R0 = list(tt_R0, tt_R0, tt_R0, tt_R0),
#                                                drug_effect = 0.5, scenario = "unlimited_healthcare")
#
# unmitigated_lim_healthcare <- run_rep_settings(countries = countries, R0 = list(R0, R0, R0, R0), tt_R0 = list(tt_R0, tt_R0, tt_R0, tt_R0),
#                                                drug_effect = 0.5, scenario = "limited_healthcare")
#
# results <- rbind(unmitigated_inf_healthcare, unmitigated_lim_healthcare) %>%
#   mutate(prop_deaths_averted = 1 - deaths_drugs/deaths_no_drugs)
#
# results <- rbind(unmitigated_inf_healthcare, unmitigated_lim_healthcare) %>%
#   mutate(deaths_no_drugs = deaths_no_drugs - deaths_drugs)
# results$income_group = rep(c("LIC", "LMIC", "UMIC", "HIC"), 2)
# outputs <- results %>%
#   pivot_longer(cols = c(deaths_no_drugs, deaths_drugs), names_to = "drug_scenario", values_to = "deaths") %>%
#   mutate(drug_scenario = factor(drug_scenario, levels = c("deaths_no_drugs", "deaths_drugs"))) %>%
#   mutate(income_group = factor(income_group, levels = c("LIC", "LMIC", "UMIC", "HIC")))
#
# ggplot(outputs, aes(x = income_group, y = deaths, fill = income_group, alpha = drug_scenario, group = drug_scenario)) +
#   geom_bar(stat = "identity") +
#   scale_alpha_manual(values = c(0.5, 1)) +
#   facet_wrap(~healthcare_scenario, scales = "free_y") +
#   theme(legend.position = "none",
#         axis.title.x = element_blank())
#

