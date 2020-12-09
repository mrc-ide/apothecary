# Loading Required Libraries
library(lubridate); library(squire); library(tictoc); library(dplyr); library(tidyverse);
library(apothecary); library(scales)

# Load apothecary stuff
devtools::load_all()

# Loading In Example Initial Parameters and Defining Country and Date Fitting Being Applied To
pars_init <- readRDS("analysis_Figure3/Inputs/pars_init.rds")
can_parms <- pars_init$CAN
iso3c <- "CAN"
date <- "2020-11-17"

# Loading in ECDC/Worldometer Deaths Data
if (iso3c %in% c("BOL", "ITA", "FRA", "ECU", "CHL", "COD", "ESP", "IRN",
                 "JPN", "GUF","KGZ", "PER", "MEX", "HKG", "MAC", "TWN",
                 "SDN")) {
  ecdc <- readRDS("analysis_Figure3/Inputs/worldometers_all.rds")
} else {
  ecdc <- readRDS("analysis_Figure3/Inputs/ecdc_all.rds")
}

# Removing Deaths Followed by 21 Days of No Deaths
country <- squire::population$country[match(iso3c, squire::population$iso3c)[1]]
df <- ecdc[which(ecdc$countryterritoryCode == iso3c),]
if(sum(df$deaths>0)>1) {
  if(tail(diff(which(df$deaths>0)),1) > 21) {
    df$deaths[tail(which(df$deaths>0),1)] <- 0
  }
}

data <- df[,c("dateRep", "deaths", "cases")]
names(data)[1] <- "date"
data <- data[order(data$date),]
data$date <- as.Date(data$date)

# Removing Dates Up to First Death After This
first_report <- which(data$deaths>0)[1]
missing <- which(data$deaths == 0 | is.na(data$deaths))
to_remove <- missing[missing<first_report]
if(length(to_remove) > 0) {
  if(length(to_remove) == (nrow(data)-1)) {
    data <- data[-head(to_remove,-1),]
  } else {
    data <- data[-to_remove,]
  }
}

# Loading in BRT Mobility Data and Processing
interventions <- readRDS("analysis_Figure3/Inputs/google_brt.rds")
R0_change <- interventions[[iso3c]]$C
date_R0_change <- interventions[[iso3c]]$date
R0_change <- R0_change[as.Date(date_R0_change) <= date]
date_R0_change <- date_R0_change[as.Date(date_R0_change) <= date]

# Setting Up the Fortnightly Splines
date_0 <- date
Rt_rw_duration <- 14 # change to pars_init
last_shift_date <- as.Date(can_parms$date_Meff_change) + 7
remaining_days <- as.Date(date_0) - last_shift_date - 21
rw_needed <- as.numeric(round(remaining_days/Rt_rw_duration))
pars_init_rw <- as.list(can_parms[grep("Rt_rw_\\d",names(can_parms))])
if (is.null(can_parms)) {
  pars_init_rw <- as.list(rep(0, rw_needed))
} else {
  pars_init_rw <- as.list(can_parms[grep("Rt_rw_\\d",names(can_parms))])
  if(length(pars_init_rw) < rw_needed) {
    pars_init_rw[[rw_needed]] <- 0
  }
  pars_init_rw <- lapply(pars_init_rw, function(x) {
    if(is.null(x)){
      return(0)
    } else {
      return(x)
    }})
}
pars_min_rw <- as.list(rep(-5, rw_needed))
pars_max_rw <- as.list(rep(5, rw_needed))
pars_discrete_rw <- as.list(rep(FALSE, rw_needed))
names(pars_init_rw) <- names(pars_min_rw) <- names(pars_max_rw) <- names(pars_discrete_rw) <- paste0("Rt_rw_", seq_len(rw_needed))

# PMCMC Prior Bounds, Initial Parameters and Observation Model Parameters
pars_init <- list('start_date' = can_parms$start_date,
                  'R0' = can_parms$R0,
                  'Meff' = can_parms$Meff,
                  'Meff_pl' = can_parms$Meff_pl,
                  "Rt_shift" = 0,
                  "Rt_shift_scale" = can_parms$Rt_shift_scale)
pars_min <- list('start_date' = "2020-01-15",  # come back to this and sort so that we can define off start_date initial
                 'R0' = 1.6,
                 'Meff' = 0.5,
                 'Meff_pl' = 0,
                 "Rt_shift" = 0,
                 "Rt_shift_scale" = 0.1)
pars_max <- list('start_date' = "2020-02-29", # come back to this and sort so that we can define off start_date initial
                 'R0' = 5.6,
                 'Meff' = 10,
                 'Meff_pl' = 1,
                 "Rt_shift" = 0.001,
                 "Rt_shift_scale" = 10)
pars_discrete <- list('start_date' = TRUE,
                      'R0' = FALSE,
                      'Meff' = FALSE,
                      'Meff_pl' = FALSE,
                      "Rt_shift" = FALSE,
                      "Rt_shift_scale" = FALSE)
pars_obs <- list(phi_cases = 1,
                 k_cases = 2,
                 phi_death = 1,
                 k_death = 2,
                 exp_noise = 1e6)

# add in the spline list
pars_init <- append(pars_init, pars_init_rw)
pars_min <- append(pars_min, pars_min_rw)
pars_max <- append(pars_max, pars_max_rw)
pars_discrete <- append(pars_discrete, pars_discrete_rw)

# Proposal Covariance Matrix
proposal_kernel <- diag(length(names(pars_init))) * 0.3
rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)
proposal_kernel["start_date", "start_date"] <- 1.5 # consider changing

# proposal_kernel <- diag(length(names(pars_init)) - 1) * 0.3
# rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)[-1]

# MCMC Functions - Prior and Likelihood Calculation
logprior <- function(pars){
  ret <- dunif(x = pars[["start_date"]], min = -55, max = -10, log = TRUE) +
    dnorm(x = pars[["R0"]], mean = 3, sd = 1, log = TRUE) +
    dnorm(x = pars[["Meff"]], mean = 3, sd = 3, log = TRUE) +
    dunif(x = pars[["Meff_pl"]], min = 0, max = 1, log = TRUE) +
    dnorm(x = pars[["Rt_shift"]], mean = 0, sd = 1, log = TRUE) +
    dunif(x = pars[["Rt_shift_scale"]], min = 0.1, max = 10, log = TRUE)

  # get rw spline parameters
  if(any(grepl("Rt_rw", names(pars)))) {
    Rt_rws <- pars[grepl("Rt_rw", names(pars))]
    for (i in seq_along(Rt_rws)) {
      ret <- ret + dnorm(x = Rt_rws[[i]], mean = 0, sd = 0.2, log = TRUE)
    }
  }
  return(ret)
}

# Extracting Relevant Mobility Data and Creating R0_change & date_R0_change Objects
# suppressWarnings(future::plan(future::multiprocess()))
Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

tic()
n_mcmc <- 2000
replicates <- 1
pmcmc_res <- squire::pmcmc(data = data,
                           n_mcmc = n_mcmc,
                           log_prior = logprior,
                           n_particles = 1,
                           steps_per_day = 1,
                           log_likelihood = NULL,
                           squire_model = apothecary_deterministic_model(),
                           output_proposals = FALSE,
                           n_chains = 1,
                           pars_obs = pars_obs,
                           pars_init = pars_init,
                           pars_min = pars_min,
                           pars_max = pars_max,
                           pars_discrete = pars_discrete,
                           proposal_kernel = proposal_kernel,
                           country = country,
                           R0_change = R0_change,
                           date_R0_change = date_R0_change,
                           Rt_args = squire:::Rt_args_list(
                             plateau_duration = 7,
                             date_Meff_change = can_parms$date_Meff_change,
                             scale_Meff_pl = TRUE,
                             Rt_shift_duration = 7,
                             Rt_rw_duration = Rt_rw_duration),
                           burnin = ceiling(n_mcmc/2),
                           seeding_cases = 5,
                           replicates = replicates,
                           required_acceptance_ratio = 0.20,
                           start_adaptation = 500,
                           baseline_hosp_bed_capacity = 10000000000,
                           baseline_ICU_bed_capacity = 10000000000)
toc()

saveRDS(pmcmc_res, "bloop.rds")
pmcmc_res <- readRDS("N:/Charlie/apothecary_fitting/apothecary_run_results/CAN_2020-11-16_20000_iterations.rds")

index <- apothecary:::odin_index(pmcmc_res$model)
deaths <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(y) {
  temp <- c(0, diff(rowSums(pmcmc_res$output[, index$D, y], na.rm = TRUE)))
  names(temp)[1] <- rownames(pmcmc_res$output)[1]
  return(temp)
})
deaths <- do.call(cbind, deaths)

out <- pmcmc_res$output
cum_deaths <- out[, index$D, ]
cum_deaths <- apply(cum_deaths, c(1, 3), sum)
cum_deaths <- apply(cum_deaths, 2, diff)
cum_deaths <- as.data.frame(cum_deaths)
cum_deaths$date <- row.names(cum_deaths)

daily_deaths <- cum_deaths %>%
  pivot_longer(cols = V1:V500, names_to = "replicate")

daily_death_summary <- daily_deaths %>%
  group_by(date) %>%
  summarise(median_deaths = median(value, na.rm = TRUE),
            lower_deaths = quantile(value, 0.05, na.rm = TRUE),
            upper_deaths = quantile(value, 0.95, na.rm = TRUE))

ggplot() +
  geom_ribbon(data = daily_death_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.2) +
  geom_point(data = data, aes(x = date, y = deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), size = 1) +
  scale_x_date(breaks = "1 month", labels = date_format("%b %Y")) +
  labs(y = "Daily Deaths Due to COVID-19", x = "", title = "Model Fitting to Canada's COVID-19 Epidemic") +
  theme(legend.position = "none")

ggplot() +
  geom_line(data = daily_deaths, aes(x = as.Date(date), y = value, group = replicate), alpha = 0.2, col = "grey") +
  geom_ribbon(data = daily_death_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.2) +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = lower_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = upper_deaths), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths, col = "point"))


data <- pmcmc_res$pmcmc_results$inputs$data
#plot(data$date, data$deaths, col = "black", pch = 20)
for (i in 1:500) {
  tt <- squire:::intervention_dates_for_odin(dates = pmcmc_res$interventions$date_R0_change,
                                             change = pmcmc_res$interventions$R0_change,
                                             start_date = pmcmc_res$replicate_parameters$start_date[i],
                                             steps_per_day = 1/pmcmc_res$parameters$dt)
  Rt <- squire:::evaluate_Rt_pmcmc(R0_change = tt$change,
                                   date_R0_change = tt$dates,
                                   R0 = pmcmc_res$replicate_parameters$R0[i],
                                   pars = as.list(pmcmc_res$replicate_parameters[i, ]),
                                   Rt_args = pmcmc_res$pmcmc_results$inputs$Rt_args)
  y <- run_apothecary(country = "Canada", model = "deterministic",
                      hosp_bed_capacity = 10000000000,
                      ICU_bed_capacity = 10000000000,
                      R0 = Rt,
                      tt_R0 = tt$tt * pmcmc_res$parameters$dt,
                      time_period = as.numeric(range(tt$dates)[2]-range(tt$dates)[1]),
                      day_return = TRUE,
                      seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_prop_treat = 1, drug_8_GetOx_effect_size = 1.45,
                      drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82 * 0.8,
                      drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                      drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64)
  index <- squire:::odin_index(y$model)
  mod_output <- pmcmc_res$output[, index$E1, i]
  first_day <- min(which(!is.na(mod_output[, 1])))
  initial_infections <- mod_output[first_day, ]

  initials <- seq_along(y$model$initial()) + 1L
  pos <- which(y$output[,"time"] == max(y$output[,"time"]))
  initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]

  get <- y$model$run(0:as.numeric(range(tt$dates)[2]-range(tt$dates)[1]),
                     y = initial_values,
                     use_names = TRUE,
                     replicate = 1)
  cum_deaths <- get[, index$D]
  deaths <- c(0, diff(rowSums(cum_deaths)))

  single_daily_deaths <- daily_deaths %>%
    filter(replicate == paste0("V", i)) %>%
    mutate(value = ifelse(is.na(value), 0, value))

  drugs_deaths <- data.frame(date = tt$dates, deaths = deaths, replicates = paste0("V", i))

  if (i == 1) {
    overall <- drugs_deaths
  } else {
    overall <- rbind(overall, drugs_deaths)
  }

  if (i %% 100 == 0) {
    print(i)
  }

  #lines(tt$dates, deaths, col = "red")
  #lines(as.Date(single_daily_deaths$date), single_daily_deaths$value, col = "blue")

}

overall_drugs_summary <- overall %>%
  group_by(date) %>%
  summarise(median_deaths = median(deaths, na.rm = TRUE),
            lower_deaths = quantile(deaths, 0.05, na.rm = TRUE),
            upper_deaths = quantile(deaths, 0.95, na.rm = TRUE))

ggplot() +
  geom_ribbon(data = daily_death_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.2) +
  geom_point(data = data, aes(x = date, y = deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), size = 1) +
  scale_x_date(breaks = "1 month", labels = date_format("%b %Y")) +
  labs(y = "Daily Deaths Due to COVID-19", x = "", title = "Model Fitting to Canada's COVID-19 Epidemic") +
  theme(legend.position = "none")

ggplot() +
  geom_ribbon(data = daily_death_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.3, fill = "grey") +
  geom_point(data = data, aes(x = date, y = deaths), col = "grey") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), size = 1, colour = "grey") +
  geom_ribbon(data = overall_drugs_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.2, fill = "#D36135") +
  geom_line(data = overall_drugs_summary, aes(x = as.Date(date), y = median_deaths), size = 1, col = "#D36135") +
  scale_x_date(breaks = "1 month", labels = date_format("%b %Y")) +
  labs(y = "Daily Deaths Due to COVID-19", x = "", title = "Model Fitting to Canada's COVID-19 Epidemic") +
  theme(legend.position = "none")






# Changing any parameters in the model
# y$model$set_user(user = model_user_args) more generic example where model_user_args is a list of named elements
y <- run_apothecary(country = "Canada", model = "deterministic",
                    hosp_bed_capacity = 10000000000,
                    ICU_bed_capacity = 10000000000,
                    R0 = Rt,
                    tt_R0 = tt$tt * pmcmc_res$parameters$dt,
                    time_period = as.numeric(range(tt$dates)[2]-range(tt$dates)[1]),
                    day_return = TRUE,
                    seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0))
index <- squire:::odin_index(y$model)
mod_output <- pmcmc_res$output[, index$E1, i]
first_day <- min(which(!is.na(mod_output[, 1])))
initial_infections <- mod_output[first_day, ]

initials <- seq_along(y$model$initial()) + 1L
pos <- which(y$output[,"time"] == max(y$output[,"time"]))
initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]

get <- y$model$run(0:as.numeric(range(tt$dates)[2]-range(tt$dates)[1]),
                   y = initial_values,
                   use_names = TRUE,
                   replicate = 1)
cum_deaths <- get[, index$D]
past_time <- get[, "t"]
past_deaths <- c(0, diff(rowSums(cum_deaths)))

# Extracting final run values to sub in as initials moving forward
pos <- which(get[,"time"] == max(get[,"time"]))
initial_values <- get[pos, initials, drop = TRUE]

beta <- y$model$contents()$beta_set
tt_beta <- y$model$contents()$tt_beta
y$model$set_user(beta_set  = rep(beta[length(beta)]/0.9, 101),
                 tt_beta  = 0:100)
get <- y$model$run(0:100,
                   y = initial_values,
                   use_names = TRUE,
                   replicate = 1)

new_cum_deaths <- get[, index$D]
new_deaths_no_drugs <- c(0, diff(rowSums(new_cum_deaths)))
new_time_no_drugs <- get[, "t"] + pos

plot(past_deaths, col = "red", type = "l", xlim = c(0, 379), ylim = c(0, max(new_deaths_no_drugs)))
lines(new_time_no_drugs[-1], new_deaths_no_drugs[-1], col = "blue")

y$model$set_user(drug_4_indic = 1, drug_4_prop_treat = 0.5, drug_4_effect_size = 1.33,
                 drug_3_indic = 1, drug_3_prop_treat = 0.5, drug_3_effect_size = 0.66)
get <- y$model$run(0:100,
                   y = initial_values,
                   use_names = TRUE,
                   replicate = 1)

new_cum_deaths <- get[, index$D]
new_deaths_drugs <- c(0, diff(rowSums(new_cum_deaths)))
new_time_drugs <- get[, "t"] + pos
dim(get)

lines(new_time_drugs[-1], new_deaths_drugs[-1], col = "green")

y <- data.frame(time = past_time,
                deaths = past_deaths)
x <- data.frame(scenario = c(rep("drugs", length(new_deaths_drugs[-1])), rep("no_drugs", length(new_deaths_no_drugs[-1]))),
                deaths = c(new_deaths_drugs[-1], new_deaths_no_drugs[-1]),
                time = c(new_time_drugs[-1], new_time_no_drugs[-1]))
  geom_ribbon(data = daily_death_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.3, fill = "grey") +
  geom_point(data = data, aes(x = date, y = deaths), col = "grey") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), size = 1, colour = "grey") +
  geom_ribbon(data = overall_drugs_summary, aes(x = as.Date(date), ymin = lower_deaths, ymax = upper_deaths), alpha = 0.2, fill = "#D36135") +
  geom_line(data = overall_drugs_summary, aes(x = as.Date(date), y = median_deaths), size = 1, col = "#D36135") +
  scale_x_date(breaks = "1 month", labels = date_format("%b %Y")) +
  labs(y = "Daily Deaths Due to COVID-19", x = "", title = "Model Fitting to Canada's COVID-19 Epidemic") +
  theme(legend.position = "none")


ggplot() +
  geom_line(data = y, aes(x = time, y = deaths), col = "grey", size = 1) +
  geom_line(data = x, aes(x = time, y = deaths, colour = scenario), size = 1) +
  scale_colour_manual(values = c("#B548A8", "#0EC056")) +
  labs(y = "Daily Deaths Due to COVID-19", x = "Time Since Epidemic Start (Days)",
       title = "Future Projections of Epidemic Trajectory") +
  theme(legend.position = "none")



y$model$contents()$beta_set
y$model$set_user(beta_set  = c(beta, 0.1)) #example
y$model$contents()$beta_set

get <- y$model$run(0:100,
                   y = initial_values,
                   use_names = TRUE,
                   replicate = 1)

new_cum_deaths <- get[, index$D]
new_deaths <- c(0, diff(rowSums(new_cum_deaths)))
new_time <- get[, "t"] + pos
dim(get)

lines(new_time[-1], new_deaths[-1], col = "yellow")




x <- y$model$contents()
x$beta_set <- 0.04964971/2





r$model$set_user(tt_beta = round(tt_R0/dt_step))
r$model$set_user(beta_set = beta)
r$model$set_user(tt_matrix = round(tt_contact_matrix/dt_step))
r$model$set_user(mix_mat_set = matrices_set)
r$model$set_user(tt_hosp_beds = round(tt_hosp_beds/dt_step))
r$model$set_user(hosp_beds = hosp_bed_capacity)
r$model$set_user(tt_ICU_beds = round(tt_ICU_beds/dt_step))
r$model$set_user(ICU_beds = ICU_bed_capacity)

# make sure these time varying parameters are also updated
r$model$set_user(tt_dur_get_mv_die = 0)
r$model$set_user(tt_dur_get_ox_die = 0)
r$model$set_user(tt_dur_get_mv_survive = 0)
r$model$set_user(tt_dur_get_ox_survive = 0)
r$model$set_user(gamma_get_mv_die = finals[[x]]$gamma_get_mv_die)
r$model$set_user(gamma_get_ox_die = finals[[x]]$gamma_get_ox_die)
r$model$set_user(gamma_get_mv_survive = finals[[x]]$gamma_get_mv_survive)
r$model$set_user(gamma_get_ox_survive = finals[[x]]$gamma_get_ox_survive)

# and update any custom model user args
if (!is.null(model_user_args)) {
  r$model$set_user(user = model_user_args[[x]])
}

# run the model
get <- r$model$run(step,
                   y = as.numeric(r$output[state_pos[x], initials, x, drop=TRUE]),
                   use_names = TRUE,
                   replicate = 1)

y$model$run(user = pars())

