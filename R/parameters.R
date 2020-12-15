#' Generate initial parameters for the apothecary model
#'
#' @param country country
#' @param population population
#' @param seeding_cases seeding_cases
#'
#' @return inits
#' @export
#'
initial_params <- function(country = NULL, population = NULL, seeding_cases = 20) {

  if (!is.null(population) & length(population) != 17) {
    stop("population input is wrong - needs to be 17 in length")
  }

  if (is.null(population) & is.null(country)) {
    stop("one of either population or country must not be NULL")
  }

  if (is.null(population) & !is.null(country)) {
    country_population <- get_population(country)
    population <- country_population$n
  }

  raw_seeding_cases <- rep(0, length(population))
  age_group_indices <- c(8, 9, 10, 11) # age_group indices corresponding to middle-aged travellers
  raw_seeding_cases[age_group_indices] <- as.vector(stats::rmultinom(1, size = seeding_cases, prob = rep(0.25, 4)))

  inits <- list(S_0 = population,
                E1_0 = raw_seeding_cases,
                E2_0 = rep(0, 17),
                IAsymp_0 = rep(0, 17),
                IMild_0 = rep(0, 17),
                ICase1_0 = rep(0, 17),
                ICase2_0 = rep(0, 17),
                IPre1Mild_0 = rep(0, 17),
                IPre2Mild_0 = rep(0, 17),
                IPre1Asymp_0 = rep(0, 17),
                IPre2Asymp_0 = rep(0, 17),
                IPre1Case_0 = rep(0, 17),
                IPre2Case_0 = rep(0, 17),
                IPre1CaseDrug3_0 = rep(0, 17),
                IPre2CaseDrug3_0 = rep(0, 17),
                ICase1Drug3_0 = rep(0, 17),
                ICase2Drug3_0 = rep(0, 17),
                IRec1_0 = rep(0, 17),
                IRec2_0 = rep(0, 17),
                R_0 = rep(0, 17),
                D_Community_0 = rep(0, 17),
                D_Hospital_0 = rep(0, 17),
                PS_0 = rep(0, 17),
                PE1_0 = rep(0, 17),
                PE2_0 = rep(0, 17),
                IMild_Drug_5_0 = rep(0, 17),
                ICase1_Drug_5_0 = rep(0, 17),
                ICase2_Drug_5_0 = rep(0, 17),
                IMod_GetHosp_GetOx_Surv1_0 = rep(0, 17),
                IMod_GetHosp_GetOx_Surv2_0 = rep(0, 17),
                IMod_GetHosp_GetOx_Die1_0 = rep(0, 17),
                IMod_GetHosp_GetOx_Die2_0 = rep(0, 17),
                IMod_GetHosp_NoOx_Surv1_0 = rep(0, 17),
                IMod_GetHosp_NoOx_Surv2_0 = rep(0, 17),
                IMod_GetHosp_NoOx_Die1_0 = rep(0, 17),
                IMod_GetHosp_NoOx_Die2_0 = rep(0, 17),
                IMod_NoHosp_NoOx_Surv1_0 = rep(0, 17),
                IMod_NoHosp_NoOx_Surv2_0 = rep(0, 17),
                IMod_NoHosp_NoOx_Die1_0 = rep(0, 17),
                IMod_NoHosp_NoOx_Die2_0 = rep(0, 17),
                ISev_GetICU_GetOx_Surv1_0 = rep(0, 17),
                ISev_GetICU_GetOx_Surv2_0 = rep(0, 17),
                ISev_GetICU_GetOx_Die1_0 = rep(0, 17),
                ISev_GetICU_GetOx_Die2_0 = rep(0, 17),
                ISev_GetICU_NoOx_Surv1_0 = rep(0, 17),
                ISev_GetICU_NoOx_Surv2_0 = rep(0, 17),
                ISev_GetICU_NoOx_Die1_0 = rep(0, 17),
                ISev_GetICU_NoOx_Die2_0 = rep(0, 17),
                ISev_NoICU_NoOx_Surv1_0 = rep(0, 17),
                ISev_NoICU_NoOx_Surv2_0 = rep(0, 17),
                ISev_NoICU_NoOx_Die1_0 = rep(0, 17),
                ISev_NoICU_NoOx_Die2_0 = rep(0, 17),
                ICrit_GetICU_GetOx_GetMV_Surv1_0 = rep(0, 17),
                ICrit_GetICU_GetOx_GetMV_Surv2_0 = rep(0, 17),
                ICrit_GetICU_GetOx_GetMV_Die1_0 = rep(0, 17),
                ICrit_GetICU_GetOx_GetMV_Die2_0 = rep(0, 17),
                ICrit_GetICU_GetOx_NoMV_Surv1_0 = rep(0, 17),
                ICrit_GetICU_GetOx_NoMV_Surv2_0 = rep(0, 17),
                ICrit_GetICU_GetOx_NoMV_Die1_0 = rep(0, 17),
                ICrit_GetICU_GetOx_NoMV_Die2_0 = rep(0, 17),
                ICrit_GetICU_NoOx_NoMV_Surv1_0 = rep(0, 17),
                ICrit_GetICU_NoOx_NoMV_Surv2_0 = rep(0, 17),
                ICrit_GetICU_NoOx_NoMV_Die1_0 = rep(0, 17),
                ICrit_GetICU_NoOx_NoMV_Die2_0 = rep(0, 17),
                ICrit_NoICU_NoOx_NoMV_Surv1_0 = rep(0, 17),
                ICrit_NoICU_NoOx_NoMV_Surv2_0 = rep(0, 17),
                ICrit_NoICU_NoOx_NoMV_Die1_0 = rep(0, 17),
                ICrit_NoICU_NoOx_NoMV_Die2_0 = rep(0, 17))

  return(inits)

}


# -----------------------------------------------------------------------------
#' Parmaters for Apothecary SEIR model
#' @export
apothecary_parameters <- function(

  # demography
  country = NULL,
  population = NULL,
  tt_contact_matrix = 0,
  contact_matrix_set = NULL,

  # healthcare related quantities
  hosp_bed_capacity  = NULL,
  ICU_bed_capacity  = NULL,
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
  dt = 0.1,
  N_age = 17,
  time_period = 365,
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
  dur_IAsymp = NULL,
  dur_IMild = NULL,
  dur_ICase = NULL,
  dur_IPreAsymp = NULL,
  dur_IPreMild = NULL,
  dur_IPreCase = NULL,
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
  drug_3_effect_size = 0,
  drug_4_indic_IAsymp = 0,
  drug_4_indic_IMild = 0,
  drug_4_indic_ICase = 0,
  drug_4_prop_treat = 0,
  drug_4_effect_size = 0,
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
  drug_8_GetOx_effect_size = 0,
  drug_8_NoOx_effect_size = 0,
  drug_9_indic_ISev_GetICU_GetOx = 0,
  drug_9_indic_ISev_GetICU_NoOx = 0,
  drug_9_prop_treat = 0,
  drug_9_GetOx_effect_size = 0,
  drug_9_NoOx_effect_size = 0,
  drug_10_indic_ICrit_GetICU_GetOx_GetMV = 0,
  drug_10_indic_ICrit_GetICU_GetOx_NoMV = 0,
  drug_10_indic_ICrit_GetICU_NoOx_NoMV = 0,
  drug_10_prop_treat = 0,
  drug_10_GetOx_GetMV_effect_size = 0,
  drug_10_GetOx_NoMV_effect_size = 0,
  drug_10_NoOx_NoMV_effect_size = 0,
  drug_11_indic_IMod_GetHosp_GetOx = 0,
  drug_11_indic_IMod_GetHosp_NoOx = 0,
  drug_11_prop_treat = 0,
  drug_11_GetOx_effect_size = 0,
  drug_11_NoOx_effect_size = 0,
  drug_12_indic_ISev_GetICU_GetOx = 0,
  drug_12_indic_ISev_GetICU_NoOx = 0,
  drug_12_prop_treat = 0,
  drug_12_GetOx_effect_size = 0,
  drug_12_NoOx_effect_size = 0,
  drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0,
  drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0,
  drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0,
  drug_13_prop_treat = 0,
  drug_13_GetOx_GetMV_effect_size = 0,
  drug_13_GetOx_NoMV_effect_size = 0,
  drug_13_NoOx_NoMV_effect_size = 0,

  walker_params = FALSE

) {

  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # Handle severity parameters and possible 80+ demographic adjustment
  severity_params <- parse_country_severity(country = country,
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
                                            walker_params = walker_params)

  prob_asymp <- severity_params$prob_asymp
  prob_hosp <- severity_params$prob_hosp
  prob_severe <- severity_params$prob_severe
  prob_critical <- severity_params$prob_critical
  prob_moderate_death_get_hosp_get_ox_baseline <- severity_params$prob_moderate_death_get_hosp_get_ox_baseline
  prob_moderate_death_get_hosp_no_ox_baseline <- severity_params$prob_moderate_death_get_hosp_no_ox_baseline
  prob_moderate_death_no_hosp_no_ox <- severity_params$prob_moderate_death_no_hosp_no_ox
  prob_severe_death_get_ICU_get_ox_baseline <- severity_params$prob_severe_death_get_ICU_get_ox_baseline
  prob_severe_death_get_ICU_no_ox_baseline <- severity_params$prob_severe_death_get_ICU_no_ox_baseline
  prob_severe_death_no_ICU_no_ox <- severity_params$prob_severe_death_no_ICU_no_ox
  prob_critical_death_get_ICU_get_ox_get_MV_baseline <- severity_params$prob_critical_death_get_ICU_get_ox_get_MV_baseline
  prob_critical_death_get_ICU_get_ox_no_MV_baseline <- severity_params$prob_critical_death_get_ICU_get_ox_no_MV_baseline
  prob_critical_death_get_ICU_no_ox_no_MV_baseline <- severity_params$prob_critical_death_get_ICU_no_ox_no_MV_baseline
  prob_critical_death_no_ICU_no_ox_no_MV <- severity_params$prob_critical_death_no_ICU_no_ox_no_MV

  # Handle duration parameters
  duration_params <- parse_durations(dur_E = dur_E,
                                     dur_IAsymp = dur_IAsymp,
                                     dur_IMild = dur_IMild,
                                     dur_ICase = dur_ICase,
                                     dur_IPreAsymp = dur_IPreAsymp,
                                     dur_IPreMild = dur_IPreMild,
                                     dur_IPreCase = dur_IPreCase,
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
                                     dur_rec = dur_rec,
                                     walker_params = walker_params)

  dur_E <- duration_params$dur_E
  dur_IAsymp <- duration_params$dur_IAsymp
  dur_IMild <- duration_params$dur_IMild
  dur_ICase <- duration_params$dur_ICase
  dur_IPreAsymp <- duration_params$dur_IPreAsymp
  dur_IPreMild <- duration_params$dur_IPreMild
  dur_IPreCase <- duration_params$dur_IPreCase
  dur_IMod_GetHosp_GetOx_Surv <- duration_params$dur_IMod_GetHosp_GetOx_Surv
  dur_IMod_GetHosp_GetOx_Die <- duration_params$dur_IMod_GetHosp_GetOx_Die
  dur_IMod_GetHosp_NoOx_Surv <- duration_params$dur_IMod_GetHosp_NoOx_Surv
  dur_IMod_GetHosp_NoOx_Die <- duration_params$dur_IMod_GetHosp_NoOx_Die
  dur_IMod_NoHosp_NoOx_Surv <- duration_params$dur_IMod_NoHosp_NoOx_Surv
  dur_IMod_NoHosp_NoOx_Die <- duration_params$dur_IMod_NoHosp_NoOx_Die
  dur_ISev_GetICU_GetOx_Surv <- duration_params$dur_ISev_GetICU_GetOx_Surv
  dur_ISev_GetICU_GetOx_Die <- duration_params$dur_ISev_GetICU_GetOx_Die
  dur_ISev_GetICU_NoOx_Surv <- duration_params$dur_ISev_GetICU_NoOx_Surv
  dur_ISev_GetICU_NoOx_Die <- duration_params$dur_ISev_GetICU_NoOx_Die
  dur_ISev_NoICU_NoOx_Surv <- duration_params$dur_ISev_NoICU_NoOx_Surv
  dur_ISev_NoICU_NoOx_Die <- duration_params$dur_ISev_NoICU_NoOx_Die
  dur_ICrit_GetICU_GetOx_GetMV_Surv <- duration_params$dur_ICrit_GetICU_GetOx_GetMV_Surv
  dur_ICrit_GetICU_GetOx_GetMV_Die <- duration_params$dur_ICrit_GetICU_GetOx_GetMV_Die
  dur_ICrit_GetICU_GetOx_NoMV_Surv <- duration_params$dur_ICrit_GetICU_GetOx_NoMV_Surv
  dur_ICrit_GetICU_GetOx_NoMV_Die <- duration_params$dur_ICrit_GetICU_GetOx_NoMV_Die
  dur_ICrit_GetICU_NoOx_NoMV_Surv <- duration_params$dur_ICrit_GetICU_NoOx_NoMV_Surv
  dur_ICrit_GetICU_NoOx_NoMV_Die <- duration_params$dur_ICrit_GetICU_NoOx_NoMV_Die
  dur_ICrit_NoICU_NoOx_NoMV_Surv <- duration_params$dur_ICrit_NoICU_NoOx_NoMV_Surv
  dur_ICrit_NoICU_NoOx_NoMV_Die <- duration_params$dur_ICrit_NoICU_NoOx_NoMV_Die
  dur_rec <- duration_params$dur_rec

  # Standardise contact matrix set
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }

  # populate contact matrix set if not provided
  if (length(contact_matrix_set) == 1) {
    baseline <- contact_matrix_set[[1]]
    contact_matrix_set <- vector("list", length(tt_contact_matrix))
    for(i in seq_along(tt_contact_matrix)) {
      contact_matrix_set[[i]] <- baseline
    }
  }

  # populate hospital and ICU bed capacity if not provided
  if (is.null(hosp_bed_capacity)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      hosp_bed_capacity <- beds$hosp_beds
      hosp_bed_capacity <- rep(round(hosp_bed_capacity * sum(population)/1000), length(tt_hosp_beds))
    } else {
      hosp_bed_capacity <- round(5 * sum(population)/1000)
    }
  }
  if (is.null(ICU_bed_capacity)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      ICU_bed_capacity <- beds$ICU_beds
      ICU_bed_capacity <- rep(round(ICU_bed_capacity * sum(population)/1000), length(tt_ICU_beds))
    } else {
      ICU_bed_capacity <- round(3 * hosp_bed_capacity/100)
    }
  }


  # Initial state and matrix formatting
  # ----------------------------------------------------------------------------

  # Initialise initial conditions
  if (!is.null(seeding_cases)) {
    assert_int(seeding_cases)
    mod_init <- initial_params(country = country, population = population, seeding_cases = seeding_cases)
  } else {
    mod_init <- initial_params(country = country, population = population)
  }

  # Initialise drug parameters

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set_explicit(contact_matrix_set, population)

  # Convert and Generate Parameters As Required
  # ----------------------------------------------------------------------------

  # durations
  gamma_E = 2 * 1/dur_E
  gamma_IAsymp = 1/dur_IAsymp
  gamma_IMild = 1/dur_IMild
  gamma_ICase = 2 * 1/dur_ICase

  gamma_IPreAsymp = 2 * 1/dur_IPreAsymp
  gamma_IPreMild = 2 * 1/dur_IPreMild
  gamma_IPreCase = 2 * 1/dur_IPreCase

  gamma_rec = 2 * 1/dur_rec

  gamma_IMod_GetHosp_GetOx_Surv = 2 * 1/dur_IMod_GetHosp_GetOx_Surv
  gamma_IMod_GetHosp_GetOx_Die = 2 * 1/dur_IMod_GetHosp_GetOx_Die
  gamma_IMod_GetHosp_NoOx_Surv =  2 * 1/dur_IMod_GetHosp_NoOx_Surv
  gamma_IMod_GetHosp_NoOx_Die = 2 * 1/dur_IMod_GetHosp_NoOx_Die
  gamma_IMod_NoHosp_NoOx_Surv = 2 * 1/dur_IMod_NoHosp_NoOx_Surv
  gamma_IMod_NoHosp_NoOx_Die = 2 * 1/dur_IMod_NoHosp_NoOx_Die

  gamma_ISev_GetICU_GetOx_Surv = 2 * 1/dur_ISev_GetICU_GetOx_Surv
  gamma_ISev_GetICU_GetOx_Die = 2 * 1/dur_ISev_GetICU_GetOx_Die
  gamma_ISev_GetICU_NoOx_Surv = 2 * 1/dur_ISev_GetICU_NoOx_Surv
  gamma_ISev_GetICU_NoOx_Die = 2 * 1/dur_ISev_GetICU_NoOx_Die
  gamma_ISev_NoICU_NoOx_Surv = 2 * 1/dur_ISev_NoICU_NoOx_Surv
  gamma_ISev_NoICU_NoOx_Die = 2 * 1/dur_ISev_NoICU_NoOx_Die

  gamma_ICrit_GetICU_GetOx_GetMV_Surv = 2 * 1/dur_ICrit_GetICU_GetOx_GetMV_Surv
  gamma_ICrit_GetICU_GetOx_GetMV_Die = 2 * 1/dur_ICrit_GetICU_GetOx_GetMV_Die
  gamma_ICrit_GetICU_GetOx_NoMV_Surv = 2 * 1/dur_ICrit_GetICU_GetOx_NoMV_Surv
  gamma_ICrit_GetICU_GetOx_NoMV_Die = 2 * 1/dur_ICrit_GetICU_GetOx_NoMV_Die
  gamma_ICrit_GetICU_NoOx_NoMV_Surv = 2 * 1/dur_ICrit_GetICU_NoOx_NoMV_Surv
  gamma_ICrit_GetICU_NoOx_NoMV_Die = 2 * 1/dur_ICrit_GetICU_NoOx_NoMV_Die
  gamma_ICrit_NoICU_NoOx_NoMV_Surv = 2 * 1/dur_ICrit_NoICU_NoOx_NoMV_Surv
  gamma_ICrit_NoICU_NoOx_NoMV_Die = 2 * 1/dur_ICrit_NoICU_NoOx_NoMV_Die

  # Generate beta
  if (is.null(beta_set)) {
    baseline_matrix <- process_contact_matrix_scaled_age(contact_matrix_set[[1]], population)
    beta_set <- beta_est_apothecary(dur_IAsymp = dur_IAsymp,
                                    dur_IMild = dur_IMild,
                                    dur_ICase = dur_ICase,
                                    dur_IPreAsymp = dur_IPreAsymp,
                                    dur_IPreMild = dur_IPreMild,
                                    dur_IPreCase = dur_IPreCase,
                                    rel_inf_asymp = rel_inf_asymp,
                                    rel_inf_mild = rel_inf_mild,
                                    prob_asymp = prob_asymp,
                                    prob_hosp = prob_hosp,
                                    mixing_matrix = baseline_matrix,
                                    R0 = R0)
    tt_beta <- tt_R0
  }

  # Collate Parameters Into List
  overall <- c(mod_init,
               seeding_cases = sum(mod_init$E1_0),
               list(tt_hosp_beds = tt_hosp_beds/dt,
                    hosp_bed_capacity = hosp_bed_capacity,
                    tt_ICU_beds = tt_ICU_beds/dt,
                    ICU_bed_capacity = ICU_bed_capacity,
                    prop_ox_hosp_beds = prop_ox_hosp_beds,
                    prop_ox_ICU_beds = prop_ox_ICU_beds,
                    tt_prop_ox_hosp_beds = tt_prop_ox_hosp_beds/dt,
                    tt_prop_ox_ICU_beds = tt_prop_ox_ICU_beds/dt,
                    MV_capacity = MV_capacity,
                    dt = dt,
                    N_age = N_age,
                    tt_matrix = tt_matrix/dt,
                    mix_mat_set = matrices_set,
                    tt_beta = tt_beta/dt,
                    beta_set = beta_set,
                    rel_inf_asymp = rel_inf_asymp,
                    rel_inf_mild = rel_inf_mild,
                    gamma_E = gamma_E,
                    gamma_IAsymp = gamma_IAsymp,
                    gamma_IMild = gamma_IMild,
                    gamma_ICase = gamma_ICase,
                    gamma_IPreAsymp = gamma_IPreAsymp,
                    gamma_IPreMild = gamma_IPreMild,
                    gamma_IPreCase = gamma_IPreCase,
                    gamma_rec = gamma_rec,
                    gamma_IMod_GetHosp_GetOx_Surv =gamma_IMod_GetHosp_GetOx_Surv,
                    gamma_IMod_GetHosp_GetOx_Die = gamma_IMod_GetHosp_GetOx_Die,
                    gamma_IMod_GetHosp_NoOx_Surv =  gamma_IMod_GetHosp_NoOx_Surv,
                    gamma_IMod_GetHosp_NoOx_Die = gamma_IMod_GetHosp_NoOx_Die,
                    gamma_IMod_NoHosp_NoOx_Surv = gamma_IMod_NoHosp_NoOx_Surv,
                    gamma_IMod_NoHosp_NoOx_Die = gamma_IMod_NoHosp_NoOx_Die,
                    gamma_ISev_GetICU_GetOx_Surv = gamma_ISev_GetICU_GetOx_Surv,
                    gamma_ISev_GetICU_GetOx_Die = gamma_ISev_GetICU_GetOx_Die,
                    gamma_ISev_GetICU_NoOx_Surv = gamma_ISev_GetICU_NoOx_Surv,
                    gamma_ISev_GetICU_NoOx_Die = gamma_ISev_GetICU_NoOx_Die,
                    gamma_ISev_NoICU_NoOx_Surv = gamma_ISev_NoICU_NoOx_Surv,
                    gamma_ISev_NoICU_NoOx_Die = gamma_ISev_NoICU_NoOx_Die,
                    gamma_ICrit_GetICU_GetOx_GetMV_Surv = gamma_ICrit_GetICU_GetOx_GetMV_Surv,
                    gamma_ICrit_GetICU_GetOx_GetMV_Die = gamma_ICrit_GetICU_GetOx_GetMV_Die,
                    gamma_ICrit_GetICU_GetOx_NoMV_Surv = gamma_ICrit_GetICU_GetOx_NoMV_Surv,
                    gamma_ICrit_GetICU_GetOx_NoMV_Die = gamma_ICrit_GetICU_GetOx_NoMV_Die,
                    gamma_ICrit_GetICU_NoOx_NoMV_Surv = gamma_ICrit_GetICU_NoOx_NoMV_Surv,
                    gamma_ICrit_GetICU_NoOx_NoMV_Die = gamma_ICrit_GetICU_NoOx_NoMV_Die,
                    gamma_ICrit_NoICU_NoOx_NoMV_Surv = gamma_ICrit_NoICU_NoOx_NoMV_Surv,
                    gamma_ICrit_NoICU_NoOx_NoMV_Die = gamma_ICrit_NoICU_NoOx_NoMV_Die,
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
                    time_period = time_period,
                    contact_matrix_set = contact_matrix_set,
                    population = population))

  class(overall) <- c("explicit_SEEIR_parameters", "squire_parameters")

  return(overall)

}
