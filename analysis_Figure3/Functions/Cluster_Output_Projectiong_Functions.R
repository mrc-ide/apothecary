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

future_projection <- function(pmcmc_res, projection_period, R0 = NULL, ...) {

  number_replicates <- dim(pmcmc_res$output)[3]
  temp <- lapply(seq_len(number_replicates), function(i) {

    # Prepping Inputs & Running Dummy Model Run to Get ... Into Parameters
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
                        day_return = TRUE,
                        seeding_cases = sum(pmcmc_res$pmcmc_results$inputs$model_params$E1_0),
                        R0 = Rt,
                        tt_R0 = tt$tt * pmcmc_res$parameters$dt,
                        time_period = as.numeric(range(tt$dates)[2] - range(tt$dates)[1]) + 1,
                        ...)
    index <- squire:::odin_index(pmcmc_res$model)

    # Extracting Output and Projecting Model Forwards
    mod_output <- pmcmc_res$output[, index$E1, i]
    first_day <- min(which(!is.na(mod_output[, 1])))
    initial_infections <- mod_output[first_day, ]

    # Recreating the Model Run Using MCMC Parameters, Up to Current Date
    initials <- seq_along(pmcmc_res$model$initial()) + 1L
    initial_values <- pmcmc_res$output[first_day, initials, i, drop = TRUE]
    rerun <- pmcmc_res$output[, , i]
    cum_deaths <- rerun[, index$D]
    past_t <- rerun[, "t"]
    past_deaths <- c(0, diff(rowSums(cum_deaths)))
    #plot(past_t, past_deaths, type = "l", xlim = c(0, length(past_deaths) + projection_period))

    # Simulating Forward Off This Output
    pos <- which(y$output[, "time"] == max(y$output[, "time"]))
    initial_values <- rerun[pos + first_day - 1, initials, drop = TRUE]
    if (!is.null(R0)) {
      baseline_matrix <- process_contact_matrix_scaled_age(y$parameters$contact_matrix_set[[1]], y$parameters$population)
      new_beta <- apothecary:::beta_est_apothecary(dur_IAsymp = 1/y$parameters$gamma_IAsymp,
                                                   dur_IMild = 1/y$parameters$gamma_IMild,
                                                   dur_ICase = 2/y$parameters$gamma_ICase,
                                                   mixing_matrix = baseline_matrix,
                                                   prob_asymp = y$parameters$prob_asymp,
                                                   prob_hosp = y$parameters$prob_hosp,
                                                   rel_inf_asymp = y$parameters$rel_inf_asymp,
                                                   rel_inf_mild = y$parameters$rel_inf_mild,
                                                   R0 = R0)
      y$model$set_user(beta_set  = rep(new_beta, projection_period), tt_beta  = 0:(projection_period - 1))
    }
    projection <- y$model$run(0:projection_period, y = initial_values, use_names = TRUE, replicate = 1)
    projection_deaths <- projection[, index$D]
    projection_deaths <- diff(rowSums(projection_deaths))
    projection_time <- projection[-1, "t"] + pos
    #lines(projection_time, projection_deaths, col = "blue")

    past_dates <- as.Date(names(past_t))
    projection_dates <- past_dates[length(past_dates)] + seq(1:projection_period)
    temp <- data.frame(projection_deaths = unname(c(past_deaths, projection_deaths)),
                       date = c(past_dates, projection_dates), replicate = paste0("V", i))

  })

  temp <- as.data.frame(do.call(rbind, temp))
  temp$date <- as.Date(temp$date)
  return(temp)
}

cluster_projections <- function(pmcmc_res, iso) {

  # Low R0 Scenarios
  lowR0_no_drugs_unlimited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_drugs_unlimited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                             drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                             drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                             drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_no_drugs_limited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_drugs_limited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  a <- lowR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  b <- lowR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  c <- lowR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  d <- lowR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

  # High R0 Scenarios
  highR0_no_drugs_unlimited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_drugs_unlimited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                              drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                              drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_no_drugs_limited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_drugs_limited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  e <- highR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  f <- highR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  g <- highR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  h <- highR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))


  final <- data.frame(country = iso,
                      low_nodrugs_unlimited = a, low_drugs_unlimited = b, low_nodrugs_limited = c, low_drugs_limited = d,
                      high_nodrugs_unlimited = e, high_drugs_unlimited = f, high_nodrugs_limited = g, high_drugs_limited = h)

  saveRDS(final, paste0("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Outputs/Projections/", iso, "_proj.rds"))
  return(final)

}

