# Function to calculate standard population demographic structure with 50,000,000 people
calc_std_pop <- function(country) {
  population <- get_population(country)
  raw_pop <- population$n
  raw_total_pop <- sum(raw_pop)
  adj_pop <- round(50000000/raw_total_pop * raw_pop)
  return(adj_pop)
}

# Function to run and collect cumulative deaths for different healthcre capacity, drug and mitigation scenarios
run_rep_settings <- function(countries, R0, tt_R0, scenario, time_period, ...) {

  # Running for each of the different representative settings
  par(mfrow = c(2, 2))
  results <- data.frame(healthcare_scenario =  rep(scenario, 4), deaths_no_drugs = rep(0, 4), deaths_drugs = rep(0, 4))
  for (i in 1:4) {

    # Picking a country and extracting relevant standardised population and mixing matrix
    set.seed(10)
    country <- countries[i]
    adj_pop <- calc_std_pop(country)
    mixing_matrix <- get_mixing_matrix(country)

    # Extracting relevant R0 and tt_R0
    mod_R0 <- R0[[i]]
    mod_tt_R0 <- tt_R0[[i]]

    # Running an unmitigated scenario with unlimited healthcare capacity
    if (scenario == "unlimited_healthcare") {
      set.seed(10)
      output_no_drugs <- run_apothecary(country = country,
                                        population = adj_pop,
                                        contact_matrix_set = mixing_matrix,
                                        R0 = mod_R0,
                                        tt_R0 = mod_tt_R0,
                                        hosp_bed_capacity = 100000000000,
                                        ICU_bed_capacity = 10000000000,
                                        day_return = TRUE,
                                        time_period = max(time_period, length(R0)),
                                        seeding_cases = 100)
      no_drugs_model_results <- output_no_drugs$output
      index <- apothecary:::odin_index(output_no_drugs$model)
      results[i, 2] <- max(apply(no_drugs_model_results[, index$D_Hospital, 1], 1, sum) + apply(no_drugs_model_results[, index$D_Community, 1], 1, sum))

      set.seed(10)
      output_drugs <- run_apothecary(country = country,
                                     population = adj_pop,
                                     contact_matrix_set = mixing_matrix,
                                     R0 = mod_R0,
                                     tt_R0 = mod_tt_R0,
                                     hosp_bed_capacity = 100000000000,
                                     ICU_bed_capacity = 10000000000,
                                     day_return = TRUE,
                                     time_period = max(time_period, length(R0)),
                                     seeding_cases = 100,
                                     ... = ...)
      drugs_model_results <- output_drugs$output
      index <- apothecary:::odin_index(output_drugs$model)
      results[i, 3] <- max(apply(drugs_model_results[, index$D_Hospital, 1], 1, sum) + apply(drugs_model_results[, index$D_Community, 1], 1, sum))

    } else if (scenario == "limited_healthcare") {
      set.seed(10)
      output_no_drugs <- run_apothecary(country = country,
                                        population = adj_pop,
                                        contact_matrix_set = mixing_matrix,
                                        R0 = mod_R0,
                                        tt_R0 = mod_tt_R0,
                                        hosp_bed_capacity = NULL,
                                        ICU_bed_capacity = NULL,
                                        day_return = TRUE,
                                        time_period = max(time_period, length(R0)),
                                        seeding_cases = 100)
      no_drugs_model_results <- output_no_drugs$output
      index <- apothecary:::odin_index(output_no_drugs$model)
      results[i, 2] <- max(apply(no_drugs_model_results[, index$D_Hospital, 1], 1, sum) + apply(no_drugs_model_results[, index$D_Community, 1], 1, sum))

      set.seed(10)
      output_drugs <- run_apothecary(country = country,
                                     population = adj_pop,
                                     contact_matrix_set = mixing_matrix,
                                     R0 = mod_R0,
                                     tt_R0 = mod_tt_R0,
                                     hosp_bed_capacity = NULL,
                                     ICU_bed_capacity = NULL,
                                     day_return = TRUE,
                                     time_period = max(time_period, length(R0)),
                                     seeding_cases = 100,
                                     ... = ...)
      drugs_model_results <- output_drugs$output
      index <- apothecary:::odin_index(output_drugs$model)
      results[i, 3] <- max(apply(drugs_model_results[, index$D_Hospital, 1], 1, sum) + apply(drugs_model_results[, index$D_Community, 1], 1, sum))

    } else {
      stop("input either unlimited_healthcare or limited_healthcare")
    }
  }
  return(results)
}
