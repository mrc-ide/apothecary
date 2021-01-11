# IFR
calc_IFR <- function(model_output) {
  index <- apothecary::odin_index(model_output$model)
  IFR <- max(apply(model_output$output[, index$D], 1, sum))/ max(apply(model_output$output[, index$R], 1, sum) + apply(model_output$output[, index$D], 1, sum)) * 100
  return(IFR)
}

# Attack Rates
calc_AR <- function(model_output) {
  index <- apothecary::odin_index(model_output$model)
  infected <- max(apply(model_output$output[, index$D], 1, sum)) + max(apply(model_output$output[, index$R], 1, sum))
  population <- sum(model_output$output[1, index$S])
  return(infected/population)
}

# Proportion Individuals Receiving Full/Incomplete Healthcare
calc_receipt_healthare <- function(model_output, type) {
  index <- apothecary::odin_index(model_output$model)
  if (type == "hospital_full") {
    number_receive_full_treatment <- sum(model_output$output[, index$number_get_hosp_full_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_hosp])
    return(number_receive_full_treatment/number_need_treatment)
  } else if (type == "hospital_any") {
    number_receive_any_treatment <- sum(model_output$output[, index$number_get_hosp_any_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_hosp])
    return(number_receive_any_treatment/number_need_treatment)
  } else if (type == "ICU_full") {
    number_receive_full_treatment <- sum(model_output$output[, index$number_get_ICU_full_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_ICU])
    return(number_receive_full_treatment/number_need_treatment)
  } else if (type == "ICU_any") {
    number_receive_any_treatment <- sum(model_output$output[, index$number_get_ICU_any_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_ICU])
    return(number_receive_any_treatment/number_need_treatment)
  } else {
    stop("Error - input hospital_full, hospital_any, ICU_full or ICU_any")
  }
}

# Days Over Healthcare Capacity
days_over_capacity <- function(model_output, type) {
  index <- apothecary::odin_index(model_output$model)
  if (type == "hospital_full") {
    return(sum(model_output$output[, index$number_get_hosp_incomplete_treat] > 0.5 & model_output$output[, index$number_need_hosp] > 0.5))
  } else if (type == "hospital_any") {
    return(sum(model_output$output[, index$number_need_hosp_no_treat] > 0.5 & model_output$output[, index$number_need_hosp] > 0.5))
  } else if (type == "ICU_full") {
    return(sum(model_output$output[, index$number_get_ICU_incomplete_treat] > 0.5 & model_output$output[, index$number_need_ICU] > 0.5))
  } else if (type == "ICU_any") {
    return(sum(model_output$output[, index$number_need_ICU_no_treat] > 0.5 & model_output$output[, index$number_need_ICU] > 0.5))
  } else {
    stop("Error - input hospital_full, hospital_any, ICU_full or ICU_any")
  }
}

