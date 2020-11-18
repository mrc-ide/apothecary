run_apothecary_MCMC <- function(country, date, pars_init, mortality_data, interventions, n_mcmc, replicates, healthcare) {

  # Checking Correct ISO Specification
  if (!(country %in% unique(squire::population$iso3c))) {
    stop("incorrect ISO specified")
  }

  # Loading In Example Initial Parameters and Defining Country and Date Fitting Being Applied To
  parms <- pars_init[[country]]
  iso3c <- country

  # Generating Healthcare Scenarios
  if (healthcare == "unlimited") {
    baseline_hosp_bed_capacity <- 10000000000
    baseline_ICU_bed_capacity <- 10000000000
  } else if (healthcare == "limited") {
    baseline_hosp_bed_capacity <- squire:::get_hosp_bed_capacity(country)
    baseline_ICU_bed_capacity <- squire:::get_ICU_bed_capacity(country)
  } else {
    stop("healthcare must be specified as unlimited or limited")
  }

  # Loading in Mortality Data - Default is ECDC Unless Certain Countries, In Which Case Worldometer
  if (iso3c %in% c("BOL", "ITA", "FRA", "ECU", "CHL", "COD", "ESP", "IRN", "JPN",
                   "GUF","KGZ", "PER", "MEX", "HKG", "MAC", "TWN", "SDN")) {
    mortality_data <- mortality_data[["worldometer"]]
  } else {
    mortality_data <- mortality_data[["ecdc"]]
  }

  # Removing Deaths Followed by 21 Days of No Deaths
  country <- squire::population$country[match(iso3c, squire::population$iso3c)[1]]
  df <- mortality_data[which(mortality_data$countryterritoryCode == iso3c),]
  if(sum(df$deaths > 0) > 1) {
    if(tail(diff(which(df$deaths > 0)), 1) > 21) {
      df$deaths[tail(which(df$deaths > 0), 1)] <- 0
      deaths_removed <- sum(df$deaths[tail(which(df$deaths>0), 1)])
    } else {
      deaths_removed <- 0
    }
  } else {
    deaths_removed <- 0
  }
  data <- df[,c("dateRep", "deaths", "cases")]
  names(data)[1] <- "date"
  data <- data[order(data$date),]
  data$date <- as.Date(data$date)

  # Handle for countries that have eliminated and had reintroduction events
  if (iso3c %in% c("MMR", "BLZ", "TTO", "BHS", "HKG", "ABW", "GUM")) {
    deaths_removed <- deaths_removed + sum(data$deaths[data$date < as.Date("2020-06-01")])
    data$deaths[data$date < as.Date("2020-06-01")] <- 0
  }

  # Removing Dates Up to First Death After This Period
  first_report <- which(data$deaths > 0)[1]
  missing <- which(data$deaths == 0 | is.na(data$deaths))
  to_remove <- missing[missing < first_report]
  if(length(to_remove) > 0) {
    if(length(to_remove) == (nrow(data)-1)) {
      data <- data[-head(to_remove,-1),]
    } else {
      data <- data[-to_remove,]
    }
  }
  min_death_date <- data$date[which(data$deaths > 0)][1]
  last_start_date <- as.Date(min_death_date) - 10
  first_start_date <- as.Date(min_death_date) - 75

  # Loading in BRT Mobility Data and Processing
  R0_change <- interventions[[iso3c]]$C
  date_R0_change <- interventions[[iso3c]]$date
  R0_change <- R0_change[as.Date(date_R0_change) <= date]
  date_R0_change <- date_R0_change[as.Date(date_R0_change) <= date]

  # Setting Up the Fortnightly Splines
  date_0 <- date
  Rt_rw_duration <- parms$Rt_rw_duration
  last_shift_date <- as.Date(parms$date_Meff_change) + 7
  remaining_days <- as.Date(date_0) - last_shift_date - 21
  rw_needed <- as.numeric(round(remaining_days/Rt_rw_duration))
  pars_init_rw <- as.list(parms[grep("Rt_rw_\\d",names(parms))])
  if (is.null(parms)) {
    pars_init_rw <- as.list(rep(0, rw_needed))
  } else {
    pars_init_rw <- as.list(parms[grep("Rt_rw_\\d",names(parms))])
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
  pars_init <- list('start_date' = parms$start_date,
                    'R0' = parms$R0,
                    'Meff' = parms$Meff,
                    'Meff_pl' = parms$Meff_pl,
                    "Rt_shift" = 0,
                    "Rt_shift_scale" = parms$Rt_shift_scale)
  pars_init <- append(pars_init, pars_init_rw)

  pars_min <- list('start_date' = first_start_date,
                   'R0' = 1.6,
                   'Meff' = 0.5,
                   'Meff_pl' = 0,
                   "Rt_shift" = 0,
                   "Rt_shift_scale" = 0.1)
  pars_min <- append(pars_min, pars_min_rw)

  pars_max <- list('start_date' = last_start_date,
                   'R0' = 5.6,
                   'Meff' = 10,
                   'Meff_pl' = 1,
                   "Rt_shift" = 0.001,
                   "Rt_shift_scale" = 10)
  pars_max <- append(pars_max, pars_max_rw)

  pars_discrete <- list('start_date' = TRUE,
                        'R0' = FALSE,
                        'Meff' = FALSE,
                        'Meff_pl' = FALSE,
                        "Rt_shift" = FALSE,
                        "Rt_shift_scale" = FALSE)
  pars_discrete <- append(pars_discrete, pars_discrete_rw)

  pars_obs <- list(phi_cases = 1, k_cases = 2, phi_death = 1, k_death = 2, exp_noise = 1e6)

  # Proposal Covariance Matrix NEED CHANGING
  proposal_kernel <- diag(length(names(pars_init))) * 0.3
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)
  proposal_kernel["start_date", "start_date"] <- 1.5

  # MCMC Functions - Prior and Likelihood Calculation (removed uniform start date prior as uniform)
  logprior <- function(pars){
    ret <- dnorm(x = pars[["R0"]], mean = 3, sd = 1, log = TRUE) +
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

  pmcmc_res <- squire::pmcmc(data = data,
                             n_mcmc = n_mcmc,
                             log_prior = logprior,
                             n_particles = 1,
                             steps_per_day = 1,
                             log_likelihood = NULL,
                             squire_model = apothecary:::apothecary_deterministic_model(),
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
                               date_Meff_change = parms$date_Meff_change,
                               scale_Meff_pl = TRUE,
                               Rt_shift_duration = 7,
                               Rt_rw_duration = Rt_rw_duration),
                             burnin = n_mcmc/2,
                             seeding_cases = 5,
                             replicates = ifelse(n_mcmc < replicates, n_mcmc, replicates),
                             required_acceptance_ratio = 0.20,
                             start_adaptation = 500,
                             baseline_hosp_bed_capacity = baseline_hosp_bed_capacity,
                             baseline_ICU_bed_capacity = baseline_ICU_bed_capacity)

  run_name <- paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", iso3c, "_", date, "_", n_mcmc, "_iterations.rds")

  saveRDS(pmcmc_res, run_name)

  return(pmcmc_res)

}



