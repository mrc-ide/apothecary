# Questions for OJ:
#   Q1: how does setting Rt_shift to 0 here mean that Lines 1085 through 1092 on pmcmc.R and the
#       associated defining of Rt_shift don't get called?
#   Q2: remind me what Rt_shift_scale does again?
#   Q3:

# Loading Required Libraries
library(lubridate); library(squire); library(tictoc); library(dplyr); library(tidyverse)

# Loading In Example Initial Parameters and Defining Country and Date Fitting Being Applied To
pars_init <- readRDS("C:/Users/cw1716/Downloads/pars_init.rds")
can_parms <- pars_init$CAN
iso3c <- "CAN"
date <- "2020-08-31"

# Loading in ECDC Deaths Data, Reoving Deaths Followed by 21 Days of No Deaths, Rmoving Dates Up to First Death After This
ecdc <- readRDS("C:/Users/cw1716/Downloads/ecdc_all.rds")
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

# Setting Up the Fortnightly Splines
date_0 <- date
Rt_rw_duration <- 14
last_shift_date <- as.Date(can_parms$date_Meff_change) + 7
remaining_days <- as.Date(date_0) - last_shift_date - 21
rw_needed <- as.numeric(round(remaining_days/Rt_rw_duration))
pars_init_rw <- as.list(can_parms[grep("Rt_rw_\\d",names(can_parms))])
if(length(pars_init_rw) < rw_needed) {
  pars_init_rw[[rw_needed]] <- 0
}
pars_min_rw <- as.list(rep(-5, rw_needed))
pars_max_rw <- as.list(rep(5, rw_needed))
pars_discrete_rw <- as.list(rep(FALSE, rw_needed))
names(pars_init_rw) <- names(pars_min_rw) <- names(pars_max_rw) <- names(pars_discrete_rw) <- paste0("Rt_rw_", seq_len(rw_needed))

# Loading in BRT Mobility Data and Processing
interventions <- readRDS("C:/Users/cw1716/Downloads/google_brt.rds")
R0_change <- interventions[[iso3c]]$C
date_R0_change <- interventions[[iso3c]]$date
R0_change <- R0_change[as.Date(date_R0_change) <= date]
date_R0_change <- date_R0_change[as.Date(date_R0_change) <= date]

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
proposal_kernel["start_date", "start_date"] <- 1.5

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
tic()
n_mcmc <- 1000
pmcmc_res <- squire::pmcmc(data = data,
                           n_mcmc = n_mcmc,
                           log_prior = logprior,
                           n_particles = 1,
                           steps_per_day = 1,
                           log_likelihood = NULL,
                           squire_model = squire:::deterministic_model(),
                           output_proposals = FALSE,
                           n_chains = 3,
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
                           burnin = n_mcmc/2,
                           seeding_cases = 5,
                           replicates = 100,
                           required_acceptance_ratio = 0.20,
                           start_adaptation = 500,
                           baseline_hosp_bed_capacity = 10000000000,
                           baseline_ICU_bed_capacity = 10000000000)
toc()

out <- pmcmc_res$output
index <- apothecary:::odin_index(pmcmc_res$model)
cum_deaths <- out[, index$D, ]
cum_deaths <- apply(cum_deaths, c(1, 3), sum)
cum_deaths <- apply(cum_deaths, 2, diff)
cum_deaths <- as.data.frame(cum_deaths)
cum_deaths$date <- row.names(cum_deaths)

daily_deaths <- cum_deaths %>%
  pivot_longer(cols = V1:V100, names_to = "replicate")

daily_death_summary <- daily_deaths %>%
  group_by(date) %>%
  summarise(median_deaths = median(value, na.rm = TRUE),
            lower_deaths = quantile(value, 0.05, na.rm = TRUE),
            upper_deaths = quantile(value, 0.95, na.rm = TRUE))


ggplot() +
  geom_line(data = daily_deaths, aes(x = as.Date(date), y = value, group = replicate), alpha = 0.2, col = "grey") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = lower_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = upper_deaths), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths, col = "red"))


out <- pmcmc_res
out$pmcmc_results$inputs$squire_model <- apothecary_explicit_model()
out$pmcmc_results$inputs$model_params$dt <- 0.05
pmcmc <- out$pmcmc_results
out <- generate_draws_pmcmc(pmcmc = pmcmc,
                            burnin = ceiling(n_mcmc/10),
                            n_chains = 3,
                            squire_model = out$pmcmc_results$inputs$squire_model,
                            replicates = 100,
                            n_particles = 50,
                            forecast = 0,
                            country = country,
                            population = squire::get_population(iso3c = iso3c)$n,
                            interventions = out$interventions,
                            data = out$pmcmc_results$inputs$data)
out$pmcmc_results$inputs$prior <- as.function(c(formals(logprior),
                                                body(logprior)),
                                              envir = new.env(parent = environment(stats::acf)))

all_chains <- do.call(rbind,lapply(pmcmc_res$pmcmc_results$chains, "[[", "results"))

plot(all_chains$start_date, type = "l")
plot(all_chains$R0, type = "l")
plot(all_chains$log_posterior, type = "l")


hist(pmcmc_res$pmcmc_results$results$start_date[1000:4000])
hist(pmcmc_res$pmcmc_results$results$R0[1000:4000])
hist(pmcmc_res$pmcmc_results$results$Meff[1000:4000])

pars_init

plot(pmcmc_res$pmcmc_results$results$start_date, type = "l")
plot(pmcmc_res$pmcmc_results$results$R0, type = "l")
plot(pmcmc_res$pmcmc_results$results$log_posterior, type = "l")





index <- grep("D\\[", colnames(pmcmc_res$output[, , 1]))
plot(apply(pmcmc_res$output[, index, 1], 1, sum))




plot(pmcmc_res$pmcmc_results$results$start_date, type = "l")
plot(pmcmc_res$pmcmc_results$results$Rt_rw_2, type = "l")

index <- squire:::odin_index(pmcmc_res$output)

plot(df$dateRep, df$deaths, pch = 20)
lines()

sum(pmcmc_res$output[, "D_Community[17]", 1])
plot(pmcmc_res$output[, "D_Hospital[15]", 1])


dim(pmcmc_res$output)
