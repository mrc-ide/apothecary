
future_projection <- function(pmcmc_res, country, projection_period, R0 = NULL,
                              number_replicates, timepoints, cumulative_deaths_data,
                              ...) {

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

    # Recreating the Model Run Using MCMC Parameters, Up to Current Date
    initials <- seq_along(y$model$initial(0)) + 1L
    past_t <- as.Date(names(timepoints[, i]))
    past_deaths <- c(0, diff(cumulative_deaths_data[, i]))
    past_deaths[is.na(past_deaths)] <- 0

    # Simulating Forward Off This Output
    final <- dim(pmcmc_res$output)[1]
    initial_values <- pmcmc_res$output[final, initials, i]
    if (!is.null(R0)) {
      baseline_matrix <- process_contact_matrix_scaled_age(y$parameters$contact_matrix_set[[1]], y$parameters$population)
      new_beta <- apothecary:::beta_est_apothecary(dur_IAsymp = 1/y$parameters$gamma_IAsymp,
                                                   dur_IMild = 1/y$parameters$gamma_IMild,
                                                   dur_ICase = 2/y$parameters$gamma_ICase,
                                                   dur_IPreAsymp = 2/y$parameters$gamma_IPreAsymp,
                                                   dur_IPreMild = 2/y$parameters$gamma_IPreMild,
                                                   dur_IPreCase = 2/y$parameters$gamma_IPreCase,
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
    projection_time <- projection[-1, "t"] + final
    projection_time <- projection[-1, "t"] + max(past_t)
    temp <- data.frame(projection_deaths = unname(c(past_deaths, projection_deaths)),
                       date = c(past_t, projection_time), replicate = paste0("V", i))

  })

  temp <- as.data.frame(do.call(rbind, temp))
  temp$date <- as.Date(temp$date)
  return(temp)
}

cluster_projections <- function(pmcmc_res, iso, country, income_strata, projection_period,
                                number_replicates, timepoints, cumulative_deaths_data,
                                ...) {

  actual_hosp <- squire:::get_hosp_bed_capacity(country)
  actual_ICU <- squire:::get_ICU_bed_capacity(country)
  end_date <-  max(as.Date(names(timepoints[, 1])))
  if (income_strata == "Low income") {
    actual_prop_ox_hosp <- 0.2
    actual_prop_ox_ICU <- 0.2
    actual_MV_capacity <- round(actual_ICU * 0.2)
  } else if (income_strata == "Lower middle income") {
    actual_prop_ox_hosp <- 0.4
    actual_prop_ox_ICU <- 0.4
    actual_MV_capacity <- round(actual_ICU * 0.4)
  } else if (income_strata == "Upper middle income") {
    actual_prop_ox_hosp <- 0.6
    actual_prop_ox_ICU <- 0.6
    actual_MV_capacity <- round(actual_ICU * 0.6)
  } else if (income_strata == "High income") {
    actual_prop_ox_hosp <- 1
    actual_prop_ox_ICU <- 1
    actual_MV_capacity <- round(actual_ICU * 1)
  }

  # Low R0 Scenarios
  lowR0_no_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                                R0 = 1.35, number_replicates = number_replicates,
                                                timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                                hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                                prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                             R0 = 1.35, number_replicates = number_replicates,
                                             timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                             hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                             prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                                             drug_11_prop_treat = 1,
                                             drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.82,
                                             drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.91,
                                             drug_12_prop_treat = 1,
                                             drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.64,
                                             drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.82,
                                             drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_no_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                              R0 = 1.35, number_replicates = number_replicates,
                                              timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                              hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                              prop_ox_hosp_beds = actual_prop_ox_hosp, prop_ox_ICU_beds = actual_prop_ox_ICU, MV_capacity = actual_MV_capacity) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  lowR0_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                           R0 = 1.35, number_replicates = number_replicates,
                                           timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                           hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                           prop_ox_hosp_beds = actual_prop_ox_hosp, prop_ox_ICU_beds = actual_prop_ox_ICU, MV_capacity = actual_MV_capacity,
                                           drug_11_prop_treat = 1,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.91,
                                           drug_12_prop_treat = 1,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.82,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  a <- lowR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  b <- lowR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  c <- lowR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  d <- lowR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

  # High R0 Scenarios
  highR0_no_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                                 R0 = 2, number_replicates = number_replicates,
                                                 timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                                 hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                                 prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_drugs_unlimited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                              R0 = 2, number_replicates = number_replicates,
                                              timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                              hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                              prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                                              drug_11_prop_treat = 1,
                                              drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.82,
                                              drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.91,
                                              drug_12_prop_treat = 1,
                                              drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.64,
                                              drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.82,
                                              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_no_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                               R0 = 2, number_replicates = number_replicates,
                                               timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                               hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                               prop_ox_hosp_beds = actual_prop_ox_hosp, prop_ox_ICU_beds = actual_prop_ox_ICU, MV_capacity = actual_MV_capacity) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  highR0_drugs_limited <- future_projection(pmcmc_res = pmcmc_res, country = country, projection_period = 450,
                                            R0 = 2, number_replicates = number_replicates,
                                            timepoints = timepoints, cumulative_deaths_data = cumulative_deaths_data,
                                            hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                            prop_ox_hosp_beds = actual_prop_ox_hosp, prop_ox_ICU_beds = actual_prop_ox_ICU, MV_capacity = actual_MV_capacity,
                                            drug_11_prop_treat = 1,
                                            drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_GetOx_effect_size = 0.82,
                                            drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_NoOx_effect_size = 0.91,
                                            drug_12_prop_treat = 1,
                                            drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_GetOx_effect_size = 0.64,
                                            drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_NoOx_effect_size = 0.82,
                                            drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
    mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

  e <- highR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  f <- highR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  g <- highR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
  h <- highR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

  final <- data.frame(country = iso,
                      low_nodrugs_unlimited = a[[1]], low_drugs_unlimited = b[[1]], low_nodrugs_limited = c[[1]], low_drugs_limited = d[[1]],
                      high_nodrugs_unlimited = e[[1]], high_drugs_unlimited = f[[1]], high_nodrugs_limited = g[[1]], high_drugs_limited = h[[1]])

  saveRDS(final, paste0("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Outputs/Projections/", iso, "_proj.rds"))
  return(final)

}

