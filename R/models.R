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

