deaths_extraction <- function(pmcmc_res) {

  number_replicates <- dim(pmcmc_res$output)[3]
  out <- pmcmc_res$output
  data <- pmcmc_res$pmcmc_results$inputs$data
  index <- apothecary:::odin_index(pmcmc_res$model)
  index <- index$D
  deaths <- lapply(seq_len(dim(pmcmc_res$output)[3]), function(y) {
    temp <- c(0, diff(rowSums(pmcmc_res$output[, index, y], na.rm = TRUE)))
    names(temp)[1] <- rownames(pmcmc_res$output)[1]
    return(temp)
  })
  deaths <- as.data.frame(do.call(cbind, deaths))
  dates <- rownames(deaths)
  deaths$date <- rownames(deaths)
  deaths <- deaths %>%
    mutate(date = rownames(deaths)) %>%
    pivot_longer(cols = starts_with("V"), names_to = "replicate", values_to = "deaths")
  return(deaths)

}

model_replicate_rerun <- function(pmcmc_res, ...) {

  number_replicates <- dim(pmcmc_res$output)[3]
  temp <- lapply(seq_len(number_replicates), function(i) {

    # Prepping Inputs & Running the Model
    tt <- squire:::intervention_dates_for_odin(dates = pmcmc_res$interventions$date_R0_change,
                                               change = pmcmc_res$interventions$R0_change,
                                               start_date = pmcmc_res$replicate_parameters$start_date[i],
                                               steps_per_day = 1/pmcmc_res$parameters$dt)
    Rt <- squire:::evaluate_Rt_pmcmc(R0_change = tt$change,
                                     date_R0_change = tt$dates,
                                     R0 = pmcmc_res$replicate_parameters$R0[i],
                                     pars = as.list(pmcmc_res$replicate_parameters[i, ]),
                                     Rt_args = pmcmc_res$pmcmc_results$inputs$Rt_args)
    y <- run_apothecary(country = country, model = "deterministic",
                        hosp_bed_capacity = 10000000000, # need to change this with runs ft healthcare limitations
                        ICU_bed_capacity = 10000000000,  # need to change with runs ft healthcare limitations
                        day_return = TRUE,
                        seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
                        R0 = Rt,
                        tt_R0 = tt$tt * pmcmc_res$parameters$dt,
                        time_period = as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 2,
                        ...)
    index <- squire:::odin_index(y$model)
    print(y$parameters$drug_10_GetOx_GetMV_effect_size)

    # Subbing In Initials (Exact Seeding Cases) Used Within the No Drugs Scenario and Re-Running
    E1 <- pmcmc_res$output[, index$E1, i]
    first_day <- min(which(!is.na(E1[, 1])))
    initials <- seq_along(y$model$initial()) + 1L
    initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]
    model_run <- y$model$run(0:(as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 1), y = initial_values, use_names = TRUE, replicate = 1)
    cum_deaths <- model_run[, index$D]
    single_deaths <- c(0, diff(rowSums(cum_deaths)))
    temp <- data.frame(rerun_deaths = single_deaths, date = c(as.Date(tt$dates), range(tt$dates)[2] + 1), replicate = paste0("V", i))
    return(temp)

  })

  temp <- as.data.frame(do.call(rbind, temp))
  temp$date <- as.Date(temp$date)
  return(temp)

}


past_assess <- function(pmcmc_res, ...) {

  reality_deaths <- deaths_extraction(pmcmc_res)
  reality_deaths$date <- as.Date(reality_deaths$date)
  rerun_deaths <- model_replicate_rerun(pmcmc_res, ...)

  all_deaths <- reality_deaths %>%
    left_join(rerun_deaths, by = c("date", "replicate")) %>%
    mutate(rerun_deaths = ifelse(is.na(rerun_deaths), 0, rerun_deaths)) %>%
    pivot_longer(deaths:rerun_deaths, names_to = "scenario")

  return(all_deaths)

}


future_projection <- function(pmcmc_res, R0, project_period, ...) {


}
