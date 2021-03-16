# Loading required libraries
library(tidyverse); library(qpdf)

# Loading apothecary
devtools::load_all()

# Sourcing required functions
source("analysis_Figure3/Functions/Cluster_Output_Plotting_Functions.R")
source("analysis_Figure3/Functions/Cluster_Output_Projectiong_Functions.R")

# Loading in country dataframe
country <- "Antigua and Barbuda"
iso3c <- "ATG"
filenames <- list.files("N:/Charlie/apothecary_fitting/apothecary_run_results/")
filename <- filenames[grep(iso3c, filenames)]
pmcmc_res <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", filename))
data <- pmcmc_res$pmcmc_results$inputs$data

index <- squire:::odin_index(pmcmc_res$model)
past_t <- pmcmc_res$output[, "t", ]
past_deaths <- apply(pmcmc_res$output[, index$D, ], c(1, 3), sum)

dims <- dim(pmcmc_res$output)[1]
pmcmc_res$output <- pmcmc_res$output[(dims-2):dims, , ]
x <- cluster_projections(pmcmc_res = pmcmc_res, iso = iso3c, country = country,
                         number_replicates = 5, timepoints = past_t,  cumulative_deaths_data = past_deaths)


# Plotting Model Output & Data
deaths <- deaths_extraction(pmcmc_res)
daily_death_summary <- deaths %>%
  group_by(date) %>%
  summarise(median_deaths = median(deaths, na.rm = TRUE),
            lower_deaths = quantile(deaths, 0.05, na.rm = TRUE),
            upper_deaths = quantile(deaths, 0.95, na.rm = TRUE))
ggplot() +
  geom_line(data = deaths, aes(x = as.Date(date), y = deaths, group = replicate), alpha = 0.2, col = "grey") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = lower_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = upper_deaths), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths, col = "red"))

# Comparing Counterfactual Drug Availability at Outset
all_deaths <- past_assess(pmcmc_res = pmcmc_res, country = country,
                          drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                          drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                          drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64)
death_summary <- all_deaths %>%
  group_by(date, scenario) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95))
ggplot() +
  geom_line(data = all_deaths, aes(x = as.Date(date), y = value, col = scenario, group = interaction(replicate, scenario)), alpha = 0.2) +
  geom_line(data = death_summary, aes(x = as.Date(date), y = median, group = scenario), col = "black") +
  geom_line(data = death_summary, aes(x = as.Date(date), y = lower, group = scenario), col = "black") +
  geom_line(data = death_summary, aes(x = as.Date(date), y = upper, group = scenario), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths), col = "black")

# Projecting Forwards Under Various Scenarios
actual_hosp <- get_hosp_bed_capacity(country)
actual_ICU <- get_ICU_bed_capacity(country)
end_date <- names(pmcmc_res$output[, 1, 1])[length(names(pmcmc_res$output[, 1, 1]))]

index <- squire:::odin_index(pmcmc_res$model)
past_t <- pmcmc_res$output[, "t", ]
past_deaths <- apply(pmcmc_res$output[, index$D, ], c(1, 3), sum)
pmcmc_res$output <- pmcmc_res$output[287:292, , ]

object.size(pmcmc_r)
# Low R0 Scenarios
lowR0_no_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 300,
                                              R0 = 1.35, number_replicates = 500,
                                              timepoints = past_t, cumulative_deaths_data = past_deaths,
                                              hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

# test_death_summary <- lowR0_no_drugs_unlimited %>%
#   group_by(date) %>%
#   summarise(median = median(projection_deaths),
#             lower = quantile(projection_deaths, 0.05),
#             upper = quantile(projection_deaths, 0.95))
#
# for_comp <- death_summary %>%
#   filter(scenario == "deaths")
#
# ggplot() +
#   geom_line(data = lowR0_no_drugs_unlimited, aes(x = as.Date(date), y = projection_deaths, group = replicate), col = "black", alpha = 0.05) +
#   geom_line(data = test_death_summary, aes(x = as.Date(date), y = median), col = "blue") +
#   geom_line(data = test_death_summary, aes(x = as.Date(date), y = lower), col = "blue") +
#   geom_line(data = test_death_summary, aes(x = as.Date(date), y = upper), col = "blue") +
#   geom_line(data = for_comp, aes(x = as.Date(date), y = median), col = "red") +
#   geom_line(data = for_comp, aes(x = as.Date(date), y = lower), col = "red") +
#   geom_line(data = for_comp, aes(x = as.Date(date), y = upper), col = "red") +
#   ylim(c(0, 1500)) +
#   theme(legend.position = "none") +
#   geom_point(data = data, aes(x = date, y = deaths), col = "black")

lowR0_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 300,
                                           R0 = 1.35, number_replicates = 500,
                                           timepoints = past_t, cumulative_deaths_data = past_deaths,
                                           hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

lowR0_no_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 300,
                                            R0 = 1.35, number_replicates = 500,
                                            timepoints = past_t, cumulative_deaths_data = past_deaths,
                                            hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

lowR0_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 300,
                                         R0 = 1.35, number_replicates = 500,
                                         timepoints = past_t, cumulative_deaths_data = past_deaths,
                                         hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                         drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                         drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

a <- lowR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
b <- lowR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
c <- lowR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
d <- lowR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

b/a
d/c

# High R0 Scenarios
highR0_no_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 200,
                                               R0 = 2, number_replicates = 500,
                                               timepoints = past_t, cumulative_deaths_data = past_deaths,
                                               hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 200,
                                            R0 = 2, number_replicates = 500,
                                            timepoints = past_t, cumulative_deaths_data = past_deaths,
                                            hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_no_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 200,
                                             R0 = 2, number_replicates = 500,
                                             timepoints = past_t, cumulative_deaths_data = past_deaths,
                                             hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 200,
                                          R0 = 2, number_replicates = 500,
                                          timepoints = past_t, cumulative_deaths_data = past_deaths,
                                          hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                          drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                          drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                          drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

e <- highR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
f <- highR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
g <- highR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
h <- highR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

f/e
h/g

c(a[[1]], b[[1]], c[[1]], d[[1]], e[[1]], f[[1]], g[[1]], h[[1]])

x <- cluster_projections(pmcmc_res = pmcmc_res, iso = "FRA", country = "France",
                         number_replicates = 500, timepoints = past_t, cumulative_deaths_data = past_deaths)


x <- readRDS("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Outputs/Projections/FRA_proj.rds")



# scrapola
# out <- pmcmc_res$output
# data <- pmcmc_res$pmcmc_results$inputs$data
# index <- apothecary:::odin_index(pmcmc_res$model)
# index <- index$D
# deaths <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(y) {
#   temp <- c(0, diff(rowSums(pmcmc_res$output[, index, y], na.rm = TRUE)))
#   names(temp)[1] <- rownames(pmcmc_res$output)[1]
#   return(temp)
# })
# deaths <- as.data.frame(do.call(cbind, deaths))
# dates <- rownames(deaths)
# deaths$date <- rownames(deaths)
# deaths <- deaths %>%
#   mutate(date = rownames(deaths)) %>%
#   pivot_longer(cols = V1:V500, names_to = "replicate")

# past <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(i) {
#
#   # Prepping Inputs & Running the Model
#   tt <- squire:::intervention_dates_for_odin(dates = pmcmc_res$interventions$date_R0_change,
#                                              change = pmcmc_res$interventions$R0_change,
#                                              start_date = pmcmc_res$replicate_parameters$start_date[i],
#                                              steps_per_day = 1/pmcmc_res$parameters$dt)
#   Rt <- squire:::evaluate_Rt_pmcmc(R0_change = tt$change,
#                                    date_R0_change = tt$dates,
#                                    R0 = pmcmc_res$replicate_parameters$R0[i],
#                                    pars = as.list(pmcmc_res$replicate_parameters[i, ]),
#                                    Rt_args = pmcmc_res$pmcmc_results$inputs$Rt_args)
#   y <- run_apothecary(country = country, model = "deterministic", hosp_bed_capacity = 10000000000,
#                       ICU_bed_capacity = 10000000000, day_return = TRUE, seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
#                       R0 = Rt,
#                       tt_R0 = tt$tt * pmcmc_res$parameters$dt,
#                       time_period = as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 2)
#
#   # Extracting Output and Rerunning Model As If Drug Had Been Available
#   index <- squire:::odin_index(y$model)
#   mod_output <- pmcmc_res$output[, index$E1, i]
#   first_day <- min(which(!is.na(mod_output[, 1])))
#   initial_infections <- mod_output[first_day, ]
#   initials <- seq_along(y$model$initial()) + 1L
#   initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]
#   get <- y$model$run(0:(as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 1), y = initial_values, use_names = TRUE, replicate = 1)
#
#   cum_deaths <- get[, index$D]
#   deaths <- c(0, diff(rowSums(cum_deaths)))
#   plot(deaths, col = "red", type = "l", xlim = c(0, 378))
#
#   pos <- which(y$output[, "time"] == max(y$output[, "time"]))
#   initial_values <- get[pos-1, initials, drop = TRUE]
#   new_get <- y$model$run(0:100, y = initial_values, use_names = TRUE, replicate = 1)
#
#   new_cum_deaths <- new_get[, index$D]
#   new_deaths <- c(0, diff(rowSums(new_cum_deaths)))
#   new_time <- new_get[, "t"] + pos
#   lines(new_time[-1], new_deaths[-1], col = "blue")
#
#   x <- c(deaths, new_deaths[-1])
#
#   pos <- which(y$output[, "time"] == max(y$output[, "time"]))
#   initial_values <- get[pos-1, initials, drop = TRUE]
#   beta <- y$model$contents()$beta_set
#   tt_beta <- y$model$contents()$tt_beta
#   y$model$set_user(beta_set  = rep(0.05500576/2, 101), tt_beta  = 0:100)
#   new_get_2 <- y$model$run(0:100, y = initial_values, use_names = TRUE, replicate = 1)
#
#   new_cum_deaths <- new_get_2[, index$D]
#   new_deaths <- c(0, diff(rowSums(new_cum_deaths)))
#   new_time <- new_get_2[, "t"] + pos
#
#   lines(new_time[-1], new_deaths[-1], col = "green")
#
#   y <- c(deaths, new_deaths[-1])
#
#
#
# })

# Simulating As If Dexamethasone Had Been Available Since the Beginning
# past <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(i) {
#
#   # Prepping Inputs & Running the Model
#   tt <- squire:::intervention_dates_for_odin(dates = pmcmc_res$interventions$date_R0_change,
#                                              change = pmcmc_res$interventions$R0_change,
#                                              start_date = pmcmc_res$replicate_parameters$start_date[i],
#                                              steps_per_day = 1/pmcmc_res$parameters$dt)
#   Rt <- squire:::evaluate_Rt_pmcmc(R0_change = tt$change,
#                                    date_R0_change = tt$dates,
#                                    R0 = pmcmc_res$replicate_parameters$R0[i],
#                                    pars = as.list(pmcmc_res$replicate_parameters[i, ]),
#                                    Rt_args = pmcmc_res$pmcmc_results$inputs$Rt_args)
#   y <- run_apothecary(country = country, model = "deterministic", hosp_bed_capacity = 10000000000,
#                       ICU_bed_capacity = 10000000000, day_return = TRUE, seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
#                       R0 = Rt,
#                       tt_R0 = tt$tt * pmcmc_res$parameters$dt,
#                       time_period = as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 2,
#                       drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
#                       drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
#                       drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64)
#
#   # Extracting Output and Rerunning Model As If Drug Had Been Available
#   index <- squire:::odin_index(y$model)
#   mod_output <- pmcmc_res$output[, index$E1, i]
#   first_day <- min(which(!is.na(mod_output[, 1])))
#   initial_infections <- mod_output[first_day, ]
#   initials <- seq_along(y$model$initial()) + 1L
#   pos <- which(y$output[, "time"] == max(y$output[, "time"]))
#   initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]
#   get <- y$model$run(0:(as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 1), y = initial_values, use_names = TRUE, replicate = 1)
#   cum_deaths <- get[, index$D]
#   single_deaths <- c(0, diff(rowSums(cum_deaths)))
#   temp <- data.frame(drug_deaths = single_deaths, date = c(as.Date(tt$dates), range(tt$dates)[2] + 1), replicate = paste0("V", i))
#   if (i %% 100 == 0) {
#     print(i)
#   }
#   return(temp)
# })
# drug_deaths <- as.data.frame(do.call(rbind, past))
# drug_deaths$date <- as.Date(drug_deaths$date)
# deaths$date <- as.Date(deaths$date)
# all_deaths <- deaths %>%
#   left_join(drug_deaths, by = c("date", "replicate")) %>%
#   mutate(drug_deaths = ifelse(is.na(drug_deaths), 0, drug_deaths)) %>%
#   pivot_longer(value:drug_deaths, names_to = "scenario")
# future <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(i) {
#
#   # need to refit all the country results with healthcare capacity in already
#
#   # Prepping Inputs & Running the Model
#   hosp_beds <- 100000000# get_hosp_bed_capacity("Ethiopia")
#   ICU_beds <- 100000000# get_ICU_bed_capacity("Ethiopia")
#
#   tt <- squire:::intervention_dates_for_odin(dates = pmcmc_res$interventions$date_R0_change,
#                                              change = pmcmc_res$interventions$R0_change,
#                                              start_date = pmcmc_res$replicate_parameters$start_date[i],
#                                              steps_per_day = 1/pmcmc_res$parameters$dt)
#   Rt <- squire:::evaluate_Rt_pmcmc(R0_change = tt$change,
#                                    date_R0_change = tt$dates,
#                                    R0 = pmcmc_res$replicate_parameters$R0[i],
#                                    pars = as.list(pmcmc_res$replicate_parameters[i, ]),
#                                    Rt_args = pmcmc_res$pmcmc_results$inputs$Rt_args)
#   y <- run_apothecary(country = country, model = "deterministic",
#                       hosp_bed_capacity = hosp_beds, ICU_bed_capacity = ICU_beds,
#                       day_return = TRUE, seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
#                       R0 = Rt,
#                       tt_R0 = tt$tt * pmcmc_res$parameters$dt,
#                       time_period = as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 2)
#
#   # Extracting Output and Running Model For Future Drug Availability
#   index <- squire:::odin_index(y$model)
#   mod_output <- pmcmc_res$output[, index$E1, i]
#
#   # Running the Model to Get Some Output
#   first_day <- min(which(!is.na(mod_output[, 1])))
#   initial_infections <- mod_output[first_day, ]
#   initials <- seq_along(y$model$initial()) + 1L
#   initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]
#   get <- y$model$run(0:(as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 1), y = initial_values, use_names = TRUE, replicate = 1)
#
#   cum_deaths <- get[, index$D]
#   deaths <- c(0, diff(rowSums(cum_deaths)))
#   plot(deaths, col = "red", type = "l", xlim = c(0, 378))
#
#   # Simulating Forward Off This Output
#   project <- 365
#   pos <- which(y$output[, "time"] == max(y$output[, "time"]))
#   initial_values <- get[pos - 1, initials, drop = TRUE]
#   baseline_matrix <- process_contact_matrix_scaled_age(y$parameters$contact_matrix_set[[1]], y$parameters$population)
#   new_beta <- apothecary:::beta_est_apothecary(dur_IAsymp = 1/y$parameters$gamma_IAsymp, dur_IMild = 1/y$parameters$gamma_IMild,
#                                                dur_ICase = 2/y$parameters$gamma_ICase, mixing_matrix = baseline_matrix,
#                                                prob_asymp = y$parameters$prob_asymp, prob_hosp = y$parameters$prob_hosp,
#                                                rel_inf_asymp = 1, rel_inf_mild = 1, R0 = 2)
#   # Low R0, No Drugs
#   y$model$set_user(beta_set  = rep(new_beta, project), tt_beta  = 0:(project - 1))
#   no_drugs <- y$model$run(0:(project - 1), y = initial_values, use_names = TRUE, replicate = 1)
#
#   no_drugs_deaths <- no_drugs[, index$D]
#   no_drugs_deaths <- c(0, diff(rowSums(no_drugs_deaths)))
#   no_drugs_time <- no_drugs[, "t"] + pos
#   plot(no_drugs_time[-1], no_drugs_deaths[-1], col = "blue", type = "l")
#
#   # Low R0, Drugs
#   y$model$set_user(drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
#                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
#                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64)
#   drugs <- y$model$run(0:(project - 1), y = initial_values, use_names = TRUE, replicate = 1)
#
#   drugs_deaths <- drugs[, index$D]
#   drugs_deaths <- c(0, diff(rowSums(drugs_deaths)))
#   drugs_time <- drugs[, "t"] + pos
#   lines(drugs_time[-1], drugs_deaths[-1], col = "green")
#
#   sum(drugs_deaths)/sum(no_drugs_deaths)
#
#
# })

