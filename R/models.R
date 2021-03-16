#' Create an apothecary model
#'
#' @title Apothecary model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
#' @export
apothecary_explicit_model <- function() {

  model_class <- "apothecary_model"
  compare_model <- function(model, pars_obs, data) {
    squire:::compare_output(model, pars_obs, data, type=model_class)
  }

  explicit_model <- list(odin_model = apothecary_SEIR,
                         generate_beta_func = beta_est_apothecary,
                         parameter_func = apothecary_parameters,
                         run_func = run_apothecary,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "stochastic", "squire_model")
  explicit_model

}

#' Create an apothecary model
#'
#' @title Apothecary model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
#' @export
apothecary_deterministic_model <- function() {

  model_class <- "apothecary_model"
  compare_model <- function(model, pars_obs, data) {
    squire:::compare_output(model, pars_obs, data, type=model_class)
  }

  parameter_func <-  function(..., tt_vaccine, max_vaccine) {
    # build model parameters with no vaccine being passed through as this is not the vaccine model
    apothecary_parameters(...)
  }

  run_func <-  function(country,
                        contact_matrix_set,
                        tt_contact_matrix,
                        hosp_bed_capacity,
                        tt_hosp_beds,
                        ICU_bed_capacity,
                        tt_ICU_beds,
                        max_vaccine,
                        tt_vaccine,
                        population,
                        replicates,
                        day_return,
                        time_period,
                        ...) {

    # build model run with no vaccine being passed through as
    # this is not the vaccine model
    run_apothecary(country = country,
                   contact_matrix_set = contact_matrix_set,
                   tt_contact_matrix = tt_contact_matrix,
                   hosp_bed_capacity = hosp_bed_capacity,
                   tt_hosp_beds = tt_hosp_beds,
                   ICU_bed_capacity = ICU_bed_capacity,
                   tt_ICU_beds = tt_ICU_beds,
                   population = population,
                   replicates = replicates,
                   day_return = day_return,
                   time_period = time_period,
                   ...)
  }

  explicit_model <- list(odin_model = deterministic_apothecary_SEIR,
                         generate_beta_func = beta_est_apothecary,
                         parameter_func = parameter_func,
                         run_func = run_func,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "deterministic", "squire_model")
  explicit_model

}


