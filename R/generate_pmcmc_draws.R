generate_draws_pmcmc <- function(pmcmc, burnin, n_chains, squire_model, replicates, n_particles, forecast,
                                 country, population, interventions, data) {


  #--------------------------------------------------------
  # Section 3 of pMCMC Wrapper: Sample PMCMC Results
  #--------------------------------------------------------
  pmcmc_samples <- squire:::sample_pmcmc(pmcmc_results = pmcmc,
                                         burnin = burnin,
                                         n_chains = n_chains,
                                         n_trajectories = replicates,
                                         n_particles = n_particles,
                                         forecast_days = forecast)

  #--------------------------------------------------------
  # Section 4 of pMCMC Wrapper: Tidy Output
  #--------------------------------------------------------

  # create a fake run object and fill in the required elements
  r <- squire_model$run_func(country = country,
                             contact_matrix_set = pmcmc$inputs$model_params$contact_matrix_set,
                             tt_contact_matrix = pmcmc$inputs$model_params$tt_matrix,
                             hosp_bed_capacity = pmcmc$inputs$model_params$hosp_bed_capacity,
                             tt_hosp_beds = pmcmc$inputs$model_params$tt_hosp_beds,
                             ICU_bed_capacity = pmcmc$inputs$model_params$ICU_bed_capacity,
                             tt_ICU_beds = pmcmc$inputs$model_params$tt_ICU_beds,
                             population = population,
                             day_return = TRUE,
                             replicates = 1,
                             time_period = nrow(pmcmc_samples$trajectories))

  # and add the parameters that changed between each simulation, i.e. posterior draws
  r$replicate_parameters <- pmcmc_samples$sampled_PMCMC_Results

  # as well as adding the pmcmc chains so it's easy to draw from the chains again in the future
  r$pmcmc_results <- pmcmc

  # then let's create the output that we are going to use
  names(pmcmc_samples)[names(pmcmc_samples) == "trajectories"] <- "output"
  dimnames(pmcmc_samples$output) <- list(dimnames(pmcmc_samples$output)[[1]], dimnames(r$output)[[2]], NULL)
  r$output <- pmcmc_samples$output

  # and adjust the time as before
  full_row <- match(0, apply(r$output[,"time",],2,function(x) { sum(is.na(x)) }))
  saved_full <- r$output[,"time",full_row]
  for(i in seq_len(replicates)) {
    na_pos <- which(is.na(r$output[,"time",i]))
    full_to_place <- saved_full - which(rownames(r$output) == as.Date(max(data$date))) + 1L
    if(length(na_pos) > 0) {
      full_to_place[na_pos] <- NA
    }
    r$output[,"time",i] <- full_to_place
  }

  # second let's recreate the output
  r$model <- pmcmc_samples$inputs$squire_model$odin_model(
    user = pmcmc_samples$inputs$model_params, unused_user_action = "ignore"
  )

  # we will add the interventions here so that we know what times are needed for projection
  r$interventions <- interventions

  # and fix the replicates
  r$parameters$replicates <- replicates
  r$parameters$time_period <- as.numeric(diff(as.Date(range(rownames(r$output)))))
  r$parameters$dt <- pmcmc$inputs$model_params$dt

  return(r)

}
