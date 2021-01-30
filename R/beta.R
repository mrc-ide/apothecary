#' Compute age-adjusted eigenvalue for mixing matrix
#'
#' @param dur_IAsymp Duration of asymptomatic infectiousness (days)
#' @param dur_IMild Duration of mild infectiousness (days)
#' @param dur_ICase Delay between symptom onset and requiring hospitalisation (days)
#' @param rel_inf_asymp Relative infectiousness of asymptomatics relative to ICase
#' @param rel_inf_mild Relative infeciousness of mild infections relative to ICase
#' @param prob_asymp Probability of asymptomatic infection by age
#' @param prob_hosp Probability of hospitilisation by age
#' @param mixing_matrix Mixing matrix
#'
#' @return Eigenvalue
#' @export
#'
adjusted_eigen <- function(dur_IAsymp, dur_IMild, dur_ICase,
                           dur_IPreAsymp, dur_IPreMild, dur_IPreCase,
                           rel_inf_asymp, rel_inf_mild,
                           prob_asymp, prob_hosp, mixing_matrix) {

  # assertions
  assert_single_pos(dur_IAsymp, zero_allowed = FALSE)
  assert_single_pos(dur_IMild, zero_allowed = FALSE)
  assert_single_pos(dur_ICase, zero_allowed = FALSE)
  assert_single_pos(dur_IPreAsymp, zero_allowed = FALSE)
  assert_single_pos(dur_IPreMild, zero_allowed = FALSE)
  assert_single_pos(dur_IPreCase, zero_allowed = FALSE)
  assert_numeric(rel_inf_asymp)
  assert_numeric(rel_inf_mild)
  assert_numeric(prob_asymp)
  assert_numeric(prob_hosp)
  assert_numeric(mixing_matrix)
  assert_square_matrix(mixing_matrix)
  assert_same_length(mixing_matrix[,1], prob_hosp)

  if(sum(is.na(prob_hosp)) > 0) {
    stop("prob_hosp must not contain NAs")
  }

  if(sum(is.na(mixing_matrix)) > 0) {
    stop("mixing_matrix must not contain NAs")
  }

  relative_R0_by_age <- (prob_hosp * (dur_ICase + dur_IPreCase)) +
                        ((1 - prob_hosp) * prob_asymp * rel_inf_asymp * (dur_IAsymp + dur_IPreAsymp)) +
                        ((1 - prob_hosp) * (1 - prob_asymp) * rel_inf_mild * (dur_IMild + dur_IPreMild))
  Re(eigen(mixing_matrix*relative_R0_by_age)$values[1])
}

#' Estimate beta parameter for apothecary model
#'
#' @param dur_IAsymp Duration of asymptomatic infectiousness (days)
#' @param dur_IMild Duration of mild infectiousness (days)
#' @param dur_ICase Delay between symptom onset and requiring hospitalisation (days)
#' @param rel_inf_asymp Relative infectiousness of asymptomatics relative to ICase
#' @param rel_inf_mild Relative infeciousness of mild infections relative to ICase
#' @param prob_asymp Probability of asymptomatic infection by age
#' @param prob_hosp Probability of hospitilisation by age
#' @param mixing_matrix Mixing matrix
#'
#' @return Eigenvalue
#' @export
#'
beta_est_apothecary <- function(dur_IAsymp, dur_IMild, dur_ICase,
                                dur_IPreAsymp, dur_IPreMild, dur_IPreCase,
                                rel_inf_asymp, rel_inf_mild,
                                prob_asymp, prob_hosp, mixing_matrix, R0) {
  assert_pos(R0, zero_allowed = FALSE)
  R0 / adjusted_eigen(dur_IAsymp, dur_IMild, dur_ICase,
                      dur_IPreAsymp, dur_IPreMild, dur_IPreCase,
                      rel_inf_asymp, rel_inf_mild,
                      prob_asymp, prob_hosp, mixing_matrix)
}

