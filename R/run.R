#' Return the default probabilities for modelling
#' @return list of default probabilities
#' @export
default_probs <- function() {

  mod_RR <- 2.5
  sev_RR <- 4
  bed_RR <- 0.8

  prob_moderate_death_get_hosp_get_ox_baseline <- c(0.04969081, 0.034949088, 0.015567823, 0.004113525, 0.005732181,
                                                   0.009877905, 0.014594344, 0.019478001, 0.024524998, 0.031162932,
                                                   0.042022883, 0.060081582, 0.087016688, 0.121168582, 0.159224961,
                                                   0.201019838, 0.360560907)
  prob_severe_death_get_ICU_get_ox_baseline <- c(0.03681241, 0.087739471, 0.137767472, 0.204670635, 0.242622179,
                                                0.247393585, 0.235003577, 0.220870825, 0.215536992, 0.220555609,
                                                0.231900658, 0.246696831, 0.266373182, 0.295980441, 0.344879517,
                                                0.423912558, 0.504077838)
  prob_critical_death_get_ICU_get_ox_get_MV_baseline <- c(0.065139505, 0.181319731, 0.338411385, 0.619745364, 0.777729592,
                                                          0.823887225, 0.783630102, 0.698250855, 0.60521159, 0.523400655,
                                                          0.46107198, 0.422259073, 0.409731376, 0.428255014, 0.486756144,
                                                          0.597160004, 0.716732063)

  prob_moderate_death_get_hosp_get_ox_baseline = prob_moderate_death_get_hosp_get_ox_baseline
  prob_moderate_death_get_hosp_no_ox_baseline = ifelse(prob_moderate_death_get_hosp_get_ox_baseline * mod_RR * bed_RR > 1, 1, prob_moderate_death_get_hosp_get_ox_baseline * mod_RR * bed_RR)
  prob_moderate_death_no_hosp_no_ox = ifelse(prob_moderate_death_get_hosp_get_ox_baseline * mod_RR > 1, 1, prob_moderate_death_get_hosp_get_ox_baseline * mod_RR)

  prob_severe_death_get_ICU_get_ox_baseline = prob_severe_death_get_ICU_get_ox_baseline
  prob_severe_death_get_ICU_no_ox_baseline = ifelse(prob_severe_death_get_ICU_get_ox_baseline * sev_RR * bed_RR > 1, 1, prob_severe_death_get_ICU_get_ox_baseline * sev_RR * bed_RR)
  prob_severe_death_no_ICU_no_ox = ifelse(prob_severe_death_get_ICU_get_ox_baseline * sev_RR > 1, 1, prob_severe_death_get_ICU_get_ox_baseline * sev_RR)

  prob_critical_death_get_ICU_get_ox_get_MV_baseline = prob_critical_death_get_ICU_get_ox_get_MV_baseline
  prob_critical_death_get_ICU_get_ox_no_MV_baseline = ifelse(prob_severe_death_no_ICU_no_ox > 0.95, prob_severe_death_no_ICU_no_ox, 0.95)
  prob_critical_death_get_ICU_no_ox_no_MV_baseline = ifelse(prob_severe_death_no_ICU_no_ox > 0.95, prob_severe_death_no_ICU_no_ox, 0.95)
  prob_critical_death_no_ICU_no_ox_no_MV = ifelse(prob_severe_death_no_ICU_no_ox > 0.95, prob_severe_death_no_ICU_no_ox, 0.95)

  list(
    mod_RR = mod_RR,
    sev_RR = sev_RR,
    bed_RR = bed_RR,
    prob_asymp = c(0.3, 0.3, rep(0.2, 15)),
    prob_hosp = c(0.000840764, 0.001182411, 0.001662887, 0.002338607, 0.003288907,
                  0.004625365, 0.006504897, 0.009148183, 0.012865577, 0.018093546,
                  0.025445917, 0.035785947, 0.050327683, 0.0707785, 0.099539573,
                  0.1399878, 0.233470395),
    prob_severe = c(0.181354223, 0.181354223, 0.181354223, 0.137454906, 0.121938236,
                    0.122775613, 0.136057441, 0.160922182, 0.196987378, 0.242011054,
                    0.289368845, 0.326537862, 0.337229819, 0.309082553, 0.243794865,
                    0.160480254, 0.057084366),
    prob_critical = c(0.7842655, 0.7482687, 0.7143478, 0.6884863, 0.6805465,
                      0.691762, 0.7156094, 0.7451861, 0.7735551, 0.7959893,
                      0.8104545, 0.8166465, 0.8150072, 0.805598, 0.7891258,
                      0.7669679, 0.741433),

    prob_moderate_death_get_hosp_get_ox_baseline = prob_moderate_death_get_hosp_get_ox_baseline,
    prob_moderate_death_get_hosp_no_ox_baseline = prob_moderate_death_get_hosp_no_ox_baseline,
    prob_moderate_death_no_hosp_no_ox = prob_moderate_death_no_hosp_no_ox,

    prob_severe_death_get_ICU_get_ox_baseline = prob_severe_death_get_ICU_get_ox_baseline,
    prob_severe_death_get_ICU_no_ox_baseline = prob_severe_death_get_ICU_no_ox_baseline,
    prob_severe_death_no_ICU_no_ox = prob_severe_death_no_ICU_no_ox,

    prob_critical_death_get_ICU_get_ox_get_MV_baseline = prob_critical_death_get_ICU_get_ox_get_MV_baseline,
    prob_critical_death_get_ICU_get_ox_no_MV_baseline = prob_critical_death_get_ICU_get_ox_no_MV_baseline,
    prob_critical_death_get_ICU_no_ox_no_MV_baseline = prob_critical_death_get_ICU_no_ox_no_MV_baseline,
    prob_critical_death_no_ICU_no_ox_no_MV = prob_critical_death_no_ICU_no_ox_no_MV
  )
}
probs <- default_probs()

#' Return the default hospital durations for modelling
#' @return List of default durations
#' @export
default_durations <- function() {
  list(
    dur_E  = 4.6,

    dur_IPreAsymp = 0.5,
    dur_IPreMild = 0.5,
    dur_IPreCase = 0.5,

    dur_IAsymp = 1.6, # was 2.1 until adding in presymptomatic
    dur_IMild = 1.6, # was 2.1 until adding in presymptomatic
    dur_ICase = 4, # was 4.5 until adding in presymptomatic
    dur_rec = 4,

    dur_IMod_GetHosp_GetOx_Surv = 7.25,
    dur_IMod_GetHosp_GetOx_Die = 8,
    dur_IMod_GetHosp_NoOx_Surv = 7.25,
    dur_IMod_GetHosp_NoOx_Die = 8 * 0.5,
    dur_IMod_NoHosp_NoOx_Surv = 7.25,
    dur_IMod_NoHosp_NoOx_Die = 8 * 0.5,

    dur_ISev_GetICU_GetOx_Surv = 6.5,
    dur_ISev_GetICU_GetOx_Die = 9.5,
    dur_ISev_GetICU_NoOx_Surv = 6.5,
    dur_ISev_GetICU_NoOx_Die = 9.5 * 0.5,
    dur_ISev_NoICU_NoOx_Surv = 6.5,
    dur_ISev_NoICU_NoOx_Die = 9.5 * 0.5,

    dur_ICrit_GetICU_GetOx_GetMV_Surv = 13.5,
    dur_ICrit_GetICU_GetOx_GetMV_Die = 11.75,
    dur_ICrit_GetICU_GetOx_NoMV_Surv = 13.5,
    dur_ICrit_GetICU_GetOx_NoMV_Die = 1,
    dur_ICrit_GetICU_NoOx_NoMV_Surv = 13.5,
    dur_ICrit_GetICU_NoOx_NoMV_Die = 1,
    dur_ICrit_NoICU_NoOx_NoMV_Surv = 13.5,
    dur_ICrit_NoICU_NoOx_NoMV_Die = 1
  )
}
durations <- default_durations()

#' Run the apothecary SEIR model
#'
#' @export
run_apothecary <- function(

  # model type
  model = "deterministic",

  # demography
  country = NULL,
  population = NULL,
  tt_contact_matrix = 0,
  contact_matrix_set = NULL,

  # healthcare related quantities
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0,

  # oxygen related quantities
  prop_ox_hosp_beds = 1,
  prop_ox_ICU_beds = 1,
  tt_prop_ox_hosp_beds = 0,
  tt_prop_ox_ICU_beds = 0,

  # mechanival ventilation related quantities
  MV_capacity = 1000000000,

  # transmission related parameters
  R0 = 3,
  tt_R0 = 0,
  beta_set = NULL,
  rel_inf_asymp = 1,
  rel_inf_mild = 1,
  tt_matrix = 0,

  # miscellaneous parameters
  dt = NULL,
  N_age = 17,
  time_period = 365,
  init = NULL,
  seeding_cases = NULL,

  # probabilities
  prob_asymp = NULL,
  prob_hosp = NULL,
  prob_severe = NULL,
  prob_critical = NULL,
  prob_moderate_death_get_hosp_get_ox_baseline = NULL,
  prob_moderate_death_get_hosp_no_ox_baseline = NULL,
  prob_moderate_death_no_hosp_no_ox = NULL,
  prob_severe_death_get_ICU_get_ox_baseline = NULL,
  prob_severe_death_get_ICU_no_ox_baseline = NULL,
  prob_severe_death_no_ICU_no_ox = NULL,
  prob_critical_death_get_ICU_get_ox_get_MV_baseline = NULL,
  prob_critical_death_get_ICU_get_ox_no_MV_baseline = NULL,
  prob_critical_death_get_ICU_no_ox_no_MV_baseline = NULL,
  prob_critical_death_no_ICU_no_ox_no_MV = NULL,

  # durations
  dur_E  = NULL,

  dur_IPreAsymp = NULL,
  dur_IPreMild = NULL,
  dur_IPreCase = NULL,

  dur_IAsymp = NULL,
  dur_IMild = NULL,
  dur_ICase = NULL,
  dur_rec = NULL,

  dur_IMod_GetHosp_GetOx_Surv = NULL,
  dur_IMod_GetHosp_GetOx_Die = NULL,
  dur_IMod_GetHosp_NoOx_Surv = NULL,
  dur_IMod_GetHosp_NoOx_Die = NULL,
  dur_IMod_NoHosp_NoOx_Surv = NULL,
  dur_IMod_NoHosp_NoOx_Die = NULL,

  dur_ISev_GetICU_GetOx_Surv = NULL,
  dur_ISev_GetICU_GetOx_Die = NULL,
  dur_ISev_GetICU_NoOx_Surv = NULL,
  dur_ISev_GetICU_NoOx_Die = NULL,
  dur_ISev_NoICU_NoOx_Surv = NULL,
  dur_ISev_NoICU_NoOx_Die = NULL,

  dur_ICrit_GetICU_GetOx_GetMV_Surv = NULL,
  dur_ICrit_GetICU_GetOx_GetMV_Die = NULL,
  dur_ICrit_GetICU_GetOx_NoMV_Surv = NULL,
  dur_ICrit_GetICU_GetOx_NoMV_Die = NULL,
  dur_ICrit_GetICU_NoOx_NoMV_Surv = NULL,
  dur_ICrit_GetICU_NoOx_NoMV_Die = NULL,
  dur_ICrit_NoICU_NoOx_NoMV_Surv = NULL,
  dur_ICrit_NoICU_NoOx_NoMV_Die = NULL,

  # drug related parameters
  prophylactic_drug_timing_1 = 100000,
  prophylactic_drug_timing_2 = 100000,
  prophylactic_prop_treat = 0,
  prophylactic_drug_wane = 1000000,
  drug_1_indic = 0,
  drug_1_effect_size = 0,

  drug_2_indic_IPreAsymp = 0,
  drug_2_indic_IPreMild = 0,
  drug_2_indic_IPreCase = 0,
  drug_2_prop_treat = 0,
  drug_2_effect_size = 1,

  drug_3_indic = 0,
  drug_3_prop_treat = 0,
  drug_3_effect_size = 1,

  drug_4_indic_IAsymp = 0,
  drug_4_indic_IMild = 0,
  drug_4_indic_ICase = 0,
  drug_4_prop_treat = 0,
  drug_4_effect_size = 1,

  drug_5_indic_IMild = 0,
  drug_5_indic_ICase = 0,
  drug_5_prop_treat = 0,
  drug_5_effect_size = 0,

  drug_6_indic = 0,
  drug_6_prop_treat = 0,
  drug_6_effect_size = 0,

  drug_7_indic = 0,
  drug_7_prop_treat = 0,
  drug_7_effect_size = 0,

  drug_8_indic_IMod_GetHosp_GetOx = 0,
  drug_8_indic_IMod_GetHosp_NoOx = 0,
  drug_8_prop_treat = 0,
  drug_8_GetOx_effect_size = 1,
  drug_8_NoOx_effect_size = 1,

  drug_9_indic_ISev_GetICU_GetOx = 0,
  drug_9_indic_ISev_GetICU_NoOx = 0,
  drug_9_prop_treat = 0,
  drug_9_GetOx_effect_size = 1,
  drug_9_NoOx_effect_size = 1,

  drug_10_indic_ICrit_GetICU_GetOx_GetMV = 0,
  drug_10_indic_ICrit_GetICU_GetOx_NoMV = 0,
  drug_10_indic_ICrit_GetICU_NoOx_NoMV = 0,
  drug_10_prop_treat = 0,
  drug_10_GetOx_GetMV_effect_size = 1,
  drug_10_GetOx_NoMV_effect_size = 1,
  drug_10_NoOx_NoMV_effect_size = 1,

  drug_11_indic_IMod_GetHosp_GetOx = 0,
  drug_11_indic_IMod_GetHosp_NoOx = 0,
  drug_11_prop_treat = 0,
  drug_11_GetOx_effect_size = 1,
  drug_11_NoOx_effect_size = 1,

  drug_12_indic_ISev_GetICU_GetOx = 0,
  drug_12_indic_ISev_GetICU_NoOx = 0,
  drug_12_prop_treat = 0,
  drug_12_GetOx_effect_size = 1,
  drug_12_NoOx_effect_size = 1,

  drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0,
  drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
  drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0,
  drug_13_prop_treat = 0,
  drug_13_GetOx_GetMV_effect_size = 1,
  drug_13_GetOx_NoMV_effect_size = 1,
  drug_13_NoOx_NoMV_effect_size = 1,

  day_return = TRUE,
  replicates = 1,
  walker_params = FALSE

) {

  if (model == "deterministic") {
    dt <- 1
  } else if (model == "stochastic") {
    dt <- 0.1
  }

  # create parameter list
  pars <- apothecary_parameters(country = country,
                                population = population,
                                tt_contact_matrix = tt_contact_matrix,
                                contact_matrix_set = contact_matrix_set,
                                hosp_bed_capacity = hosp_bed_capacity,
                                ICU_bed_capacity = ICU_bed_capacity,
                                tt_hosp_beds = tt_hosp_beds,
                                tt_ICU_beds = tt_ICU_beds,
                                prop_ox_hosp_beds = prop_ox_hosp_beds,
                                prop_ox_ICU_beds = prop_ox_ICU_beds,
                                tt_prop_ox_hosp_beds = tt_prop_ox_hosp_beds,
                                tt_prop_ox_ICU_beds = tt_prop_ox_ICU_beds,
                                MV_capacity = MV_capacity,
                                R0 = R0,
                                tt_R0 = tt_R0,
                                beta_set = beta_set,
                                rel_inf_asymp = rel_inf_asymp,
                                rel_inf_mild = rel_inf_mild,
                                tt_matrix = tt_matrix,
                                dt = dt,
                                N_age = N_age,
                                time_period = time_period,
                                seeding_cases = seeding_cases,
                                prob_asymp = prob_asymp,
                                prob_hosp = prob_hosp,
                                prob_severe = prob_severe,
                                prob_critical = prob_critical,
                                prob_moderate_death_get_hosp_get_ox_baseline = prob_moderate_death_get_hosp_get_ox_baseline,
                                prob_moderate_death_get_hosp_no_ox_baseline = prob_moderate_death_get_hosp_no_ox_baseline,
                                prob_moderate_death_no_hosp_no_ox = prob_moderate_death_no_hosp_no_ox,
                                prob_severe_death_get_ICU_get_ox_baseline = prob_severe_death_get_ICU_get_ox_baseline,
                                prob_severe_death_get_ICU_no_ox_baseline = prob_severe_death_get_ICU_no_ox_baseline,
                                prob_severe_death_no_ICU_no_ox = prob_severe_death_no_ICU_no_ox,
                                prob_critical_death_get_ICU_get_ox_get_MV_baseline = prob_critical_death_get_ICU_get_ox_get_MV_baseline,
                                prob_critical_death_get_ICU_get_ox_no_MV_baseline = prob_critical_death_get_ICU_get_ox_no_MV_baseline,
                                prob_critical_death_get_ICU_no_ox_no_MV_baseline = prob_critical_death_get_ICU_no_ox_no_MV_baseline,
                                prob_critical_death_no_ICU_no_ox_no_MV = prob_critical_death_no_ICU_no_ox_no_MV,
                                dur_E  = dur_E,
                                dur_IAsymp = dur_IAsymp,
                                dur_IMild = dur_IMild,
                                dur_ICase = dur_ICase,
                                dur_rec = dur_rec,
                                dur_IMod_GetHosp_GetOx_Surv = dur_IMod_GetHosp_GetOx_Surv,
                                dur_IMod_GetHosp_GetOx_Die = dur_IMod_GetHosp_GetOx_Die,
                                dur_IMod_GetHosp_NoOx_Surv = dur_IMod_GetHosp_NoOx_Surv,
                                dur_IMod_GetHosp_NoOx_Die = dur_IMod_GetHosp_NoOx_Die,
                                dur_IMod_NoHosp_NoOx_Surv = dur_IMod_NoHosp_NoOx_Surv,
                                dur_IMod_NoHosp_NoOx_Die = dur_IMod_NoHosp_NoOx_Die,
                                dur_ISev_GetICU_GetOx_Surv = dur_ISev_GetICU_GetOx_Surv,
                                dur_ISev_GetICU_GetOx_Die = dur_ISev_GetICU_GetOx_Die,
                                dur_ISev_GetICU_NoOx_Surv = dur_ISev_GetICU_NoOx_Surv,
                                dur_ISev_GetICU_NoOx_Die = dur_ISev_GetICU_NoOx_Die,
                                dur_ISev_NoICU_NoOx_Surv = dur_ISev_NoICU_NoOx_Surv,
                                dur_ISev_NoICU_NoOx_Die = dur_ISev_NoICU_NoOx_Die,
                                dur_ICrit_GetICU_GetOx_GetMV_Surv = dur_ICrit_GetICU_GetOx_GetMV_Surv,
                                dur_ICrit_GetICU_GetOx_GetMV_Die = dur_ICrit_GetICU_GetOx_GetMV_Die,
                                dur_ICrit_GetICU_GetOx_NoMV_Surv = dur_ICrit_GetICU_GetOx_NoMV_Surv,
                                dur_ICrit_GetICU_GetOx_NoMV_Die = dur_ICrit_GetICU_GetOx_NoMV_Die,
                                dur_ICrit_GetICU_NoOx_NoMV_Surv = dur_ICrit_GetICU_NoOx_NoMV_Surv,
                                dur_ICrit_GetICU_NoOx_NoMV_Die = dur_ICrit_GetICU_NoOx_NoMV_Die,
                                dur_ICrit_NoICU_NoOx_NoMV_Surv = dur_ICrit_NoICU_NoOx_NoMV_Surv,
                                dur_ICrit_NoICU_NoOx_NoMV_Die = dur_ICrit_NoICU_NoOx_NoMV_Die,
                                prophylactic_drug_timing_1 = prophylactic_drug_timing_1,
                                prophylactic_drug_timing_2 = prophylactic_drug_timing_2,
                                prophylactic_prop_treat = prophylactic_prop_treat,
                                prophylactic_drug_wane = prophylactic_drug_wane,
                                drug_1_indic = drug_1_indic,
                                drug_1_effect_size = drug_1_effect_size,
                                drug_2_indic_IPreAsymp = drug_2_indic_IPreAsymp,
                                drug_2_indic_IPreMild = drug_2_indic_IPreMild,
                                drug_2_indic_IPreCase = drug_2_indic_IPreCase,
                                drug_2_prop_treat = drug_2_prop_treat,
                                drug_2_effect_size = drug_2_effect_size,
                                drug_3_indic = drug_3_indic,
                                drug_3_prop_treat = drug_3_prop_treat,
                                drug_3_effect_size = drug_3_effect_size,
                                drug_4_indic_IAsymp = drug_4_indic_IAsymp,
                                drug_4_indic_IMild = drug_4_indic_IMild,
                                drug_4_indic_ICase = drug_4_indic_ICase,
                                drug_4_prop_treat = drug_4_prop_treat,
                                drug_4_effect_size = drug_4_effect_size,
                                drug_5_indic_IMild = drug_5_indic_IMild,
                                drug_5_indic_ICase = drug_5_indic_ICase,
                                drug_5_prop_treat = drug_5_prop_treat,
                                drug_5_effect_size = drug_5_effect_size,
                                drug_6_indic = drug_6_indic,
                                drug_6_prop_treat = drug_6_prop_treat,
                                drug_6_effect_size = drug_6_effect_size,
                                drug_7_indic = drug_7_indic,
                                drug_7_prop_treat = drug_7_prop_treat,
                                drug_7_effect_size = drug_7_effect_size,
                                drug_8_indic_IMod_GetHosp_GetOx = drug_8_indic_IMod_GetHosp_GetOx,
                                drug_8_indic_IMod_GetHosp_NoOx = drug_8_indic_IMod_GetHosp_NoOx,
                                drug_8_prop_treat = drug_8_prop_treat,
                                drug_8_GetOx_effect_size = drug_8_GetOx_effect_size,
                                drug_8_NoOx_effect_size = drug_8_NoOx_effect_size,
                                drug_9_indic_ISev_GetICU_GetOx = drug_9_indic_ISev_GetICU_GetOx,
                                drug_9_indic_ISev_GetICU_NoOx = drug_9_indic_ISev_GetICU_NoOx,
                                drug_9_prop_treat = drug_9_prop_treat,
                                drug_9_GetOx_effect_size = drug_9_GetOx_effect_size,
                                drug_9_NoOx_effect_size = drug_9_NoOx_effect_size,
                                drug_10_indic_ICrit_GetICU_GetOx_GetMV = drug_10_indic_ICrit_GetICU_GetOx_GetMV,
                                drug_10_indic_ICrit_GetICU_GetOx_NoMV = drug_10_indic_ICrit_GetICU_GetOx_NoMV,
                                drug_10_indic_ICrit_GetICU_NoOx_NoMV = drug_10_indic_ICrit_GetICU_NoOx_NoMV,
                                drug_10_prop_treat = drug_10_prop_treat,
                                drug_10_GetOx_GetMV_effect_size = drug_10_GetOx_GetMV_effect_size,
                                drug_10_GetOx_NoMV_effect_size = drug_10_GetOx_NoMV_effect_size,
                                drug_10_NoOx_NoMV_effect_size = drug_10_NoOx_NoMV_effect_size,
                                drug_11_indic_IMod_GetHosp_GetOx = drug_11_indic_IMod_GetHosp_GetOx,
                                drug_11_indic_IMod_GetHosp_NoOx = drug_11_indic_IMod_GetHosp_NoOx,
                                drug_11_prop_treat = drug_11_prop_treat,
                                drug_11_GetOx_effect_size = drug_11_GetOx_effect_size,
                                drug_11_NoOx_effect_size = drug_11_NoOx_effect_size,
                                drug_12_indic_ISev_GetICU_GetOx = drug_12_indic_ISev_GetICU_GetOx,
                                drug_12_indic_ISev_GetICU_NoOx = drug_12_indic_ISev_GetICU_NoOx,
                                drug_12_prop_treat = drug_12_prop_treat,
                                drug_12_GetOx_effect_size = drug_12_GetOx_effect_size,
                                drug_12_NoOx_effect_size = drug_12_NoOx_effect_size,
                                drug_13_indic_ICrit_GetICU_GetOx_GetMV = drug_13_indic_ICrit_GetICU_GetOx_GetMV,
                                drug_13_indic_ICrit_GetICU_GetOx_NoMV = drug_13_indic_ICrit_GetICU_GetOx_NoMV,
                                drug_13_indic_ICrit_GetICU_NoOx_NoMV = drug_13_indic_ICrit_GetICU_NoOx_NoMV,
                                drug_13_prop_treat = drug_13_prop_treat,
                                drug_13_GetOx_GetMV_effect_size = drug_13_GetOx_GetMV_effect_size,
                                drug_13_GetOx_NoMV_effect_size = drug_13_GetOx_NoMV_effect_size,
                                drug_13_NoOx_NoMV_effect_size = drug_13_NoOx_NoMV_effect_size,
                                walker_params = walker_params)

  # Running the Model
  if (model == "deterministic") {
    mod <- deterministic_apothecary_SEIR(user = pars, unused_user_action = "ignore")
    t <- seq(from = 0, to = time_period)
    if (day_return) {
      t <- seq(from = 0, to = time_period, by = 1)
    }
    results <- mod$run(t, replicate = 1)
  } else if (model == "stochastic") {
    mod <- apothecary_SEIR(user = pars, unused_user_action = "ignore")
    t <- seq(from = 1, to = time_period/dt)
    if (day_return) {
      t <- round(seq(1/dt, length(t)+(1/dt), by=1/dt))
    }
    results <- mod$run(t, replicate = replicates)
  } else {
    stop("Error: specify model as deterministic or stochastic")
  }

  pars$day_return <- day_return
  pars$replicates <- replicates
  out <- list(output = results, parameters = pars, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}
