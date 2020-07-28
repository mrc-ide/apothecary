# -----------------------------------------------------------------------------
#' Generate initial parameters for the apothecary model
#'
#' @return Parameters
#' @export
#'
initial_params <- function(country = NULL, population = NULL, seeding_cases = 20) {

  if (!is.null(population) & length(population != 17)) {
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

  inits <- list(S0 = population,
                E1_0 = raw_seeding_cases,
                E2_0 = rep(0, 17),
                IAsymp_0 = rep(0, 17),
                IMild_0 = rep(0, 17),
                ICase1_0 = rep(0, 17),
                ICase2_0 = rep(0, 17),
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
#' Generate drug based parameters for the apothecary model
#'
#' @return Parameters
#' @export
#'
drug_params <- function(
  prophylactic_drug_timing_1 = 100000,
  prophylactic_drug_timing_2 = 100000,
  prophylactic_prop_treat = 0,
  prophylactic_drug_wane = 1000000,
  drug_1_indic = 0,
  drug_1_effect_size = 0,
  drug_2_indic = 0,
  drug_2_effect_size = 0,
  drug_3_indic = 0,
  drug_3_prop_treat = 0,
  drug_3_effect_size = 0,
  drug_4_indic = 0,
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
  drug_13_NoOx_NoMV_effect_size = 0
) {

  drugs <- list(
    prophylactic_drug_timing_1 = prophylactic_drug_timing_1,
    prophylactic_drug_timing_2 = prophylactic_drug_timing_2,
    prophylactic_prop_treat = prophylactic_prop_treat,
    prophylactic_drug_wane = prophylactic_drug_wane,
    drug_1_indic = drug_1_indic,
    drug_1_effect_size = drug_1_effect_size,
    drug_2_indic = drug_2_indic,
    drug_2_effect_size = drug_2_effect_size,
    drug_3_indic = drug_3_indic,
    drug_3_prop_treat = drug_3_prop_treat,
    drug_3_effect_size = drug_3_effect_size,
    drug_4_indic = drug_4_indic,
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
    drug_13_NoOx_NoMV_effect_size = drug_13_NoOx_NoMV_effect_size)

  return(drugs)
}




# -----------------------------------------------------------------------------
#' Parmaters for Apothecary SEIR model
#'
#' @return Paramater List
#' @export
#'
apothecary_parameters <- function(

  # demography
  country = NULL,
  population = NULL,
  tt_contact_matrix = 0,
  contact_matrix_set = NULL,

  # healthcare related quantities
  hosp_beds = NULL,
  ICU_beds = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0,
  tt_oxygen_supply = 0,
  input_oxygen_supply = NULL,
  tt_baseline_oxygen_demand = 0,
  input_baseline_oxygen_demand = NULL,
  oxygen_availability_0 = NULL,
  max_leftover = NULL,
  severe_critical_case_oxygen_consumption_multiplier = 1.5,
  MV_capacity = NULL,

  # transmission related parameters
  R0 = 3,
  tt_R0 = 0,
  beta_set = NULL,
  tt_beta = 0,
  rel_inf_asymp = 1,
  rel_inf_mild = 1,
  tt_matrix = 0,
  mix_mat_set = matrices_set,

  # miscellaneous parameters
  dt = 0.1,
  N_age = 17,
  time_period = 365,
  init = NULL,
  seeding_cases = NULL,

  # probabilities
  prob_asymp = rep(0.5, 17),
  prob_hosp = c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
                0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
                0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
                0.176634654 ,0.180000000),
  prob_severe = c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
                  0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
                  0.103612417, 0.149427991, 0.223777304,	0.306985918,
                  0.385779555, 0.461217861, 0.709444444),
  prob_critical = rep(0.8, 17),
  prob_moderate_death_get_hosp_get_ox_baseline = c(0.0125702,	0.0125702,	0.0125702,	0.0125702, 0.0125702,
                                                   0.0125702,	0.0125702,	0.013361147, 0.015104687,	0.019164124,
                                                   0.027477519,	0.041762108, 0.068531658,	0.105302319, 0.149305732,
                                                   0.20349534,	0.5804312),
  prob_moderate_death_get_hosp_no_ox_baseline = rep(0.6, 17),
  prob_moderate_death_no_hosp_no_ox = rep(0.6, 17),
  prob_severe_death_get_ICU_get_ox_baseline = rep(0.5, 17),
  prob_severe_death_get_ICU_no_ox_baseline = rep(0.95, 17),
  prob_severe_death_no_ICU_no_ox = rep(0.95, 17),
  prob_critical_death_get_ICU_get_ox_get_MV_baseline = rep(0.5, 17),
  prob_critical_death_get_ICU_get_ox_no_MV_baseline = rep(0.95, 17),
  prob_critical_death_get_ICU_no_ox_no_MV_baseline = rep(0.95, 17),
  prob_critical_death_no_ICU_no_ox_no_MV = rep(0.95, 17),

  # durations
  dur_E  = 4.6,
  dur_IAsymp = 2.1,
  dur_IMild = 2.1,
  dur_ICase = 4.5,
  dur_rec = 3.4,

  dur_IMod_GetHosp_GetOx_Surv = 9.6,
  dur_IMod_GetHosp_GetOx_Die = 7.6,
  dur_IMod_GetHosp_NoOx_Surv = 9.6/2,
  dur_IMod_GetHosp_NoOx_Die = 7.6/2,
  dur_IMod_NoHosp_NoOx_Surv = 9.6/2,
  dur_IMod_NoHosp_NoOx_Die = 7.6/2,

  dur_ISev_GetICU_GetOx_Surv = 11.3,
  dur_ISev_GetICU_GetOx_Die = 10.1,
  dur_ISev_GetICU_NoOx_Surv = 11.3/2,
  dur_ISev_GetICU_NoOx_Die = 10.1/2,
  dur_ISev_NoICU_NoOx_Surv = 11.3/2,
  dur_ISev_NoICU_NoOx_Die = 10.1/2,

  dur_ICrit_GetICU_GetOx_GetMV_Surv = 11.3,
  dur_ICrit_GetICU_GetOx_GetMV_Die = 10.1,
  dur_ICrit_GetICU_GetOx_NoMV_Surv = 11.3/2,
  dur_ICrit_GetICU_GetOx_NoMV_Die = 10.1/2,
  dur_ICrit_GetICU_NoOx_NoMV_Surv = 11.3/2,
  dur_ICrit_GetICU_NoOx_NoMV_Die = 10.1/2,
  dur_ICrit_NoICU_NoOx_NoMV_Surv = 11.3/2,
  dur_ICrit_NoICU_NoOx_NoMV_Die = 10.1/2

) {

  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

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
  if (is.null(hosp_beds)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      hosp_beds <- beds$hosp_beds
      hosp_beds <- rep(round(hosp_beds * sum(population)/1000), length(tt_hosp_beds))
    } else {
      hosp_beds <- round(5 * sum(population)/1000)
    }
  }
  if (is.null(ICU_beds)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      ICU_beds <- beds$ICU_beds
      ICU_beds <- rep(round(ICU_beds * sum(population)/1000), length(tt_ICU_beds))
    } else {
      ICU_beds <- round(3 * hosp_beds/100)
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

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set_explicit(contact_matrix_set, population)

  # Input checks
  # ----------------------------------------------------------------------------
  mc <- matrix_check(population[-1], contact_matrix_set)
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  stopifnot(length(hosp_bed_capacity) == length(tt_hosp_beds))
  stopifnot(length(ICU_bed_capacity) == length(tt_ICU_beds))
  tc <- lapply(list(tt_R0/dt, tt_contact_matrix/dt), check_time_change, time_period/dt)
  tc2 <- lapply(list(tt_hosp_beds/dt, tt_ICU_beds/dt), check_time_change, time_period/dt)

  assert_pos(dt)
  assert_pos(dur_E)
  assert_pos(dur_IMild)
  assert_pos(dur_ICase)
  assert_pos(dur_get_ox_survive)
  assert_pos(dur_get_ox_die)
  assert_pos(dur_not_get_ox_survive)
  assert_pos(dur_not_get_ox_die)
  assert_pos(dur_get_mv_survive)
  assert_pos(dur_get_mv_die)
  assert_pos(dur_not_get_mv_survive)
  assert_pos(dur_not_get_mv_die)
  assert_pos(time_period)
  assert_pos(hosp_bed_capacity)
  assert_pos(ICU_bed_capacity)

  assert_length(prob_hosp, length(population))
  assert_length(prob_severe, length(population))
  assert_length(prob_non_severe_death_treatment, length(population))
  assert_length(prob_non_severe_death_no_treatment, length(population))
  assert_length(prob_severe_death_treatment, length(population))
  assert_length(prob_severe_death_no_treatment, length(population))
  assert_length(p_dist, length(population))

  assert_numeric(prob_hosp, length(population))
  assert_numeric(prob_severe, length(population))
  assert_numeric(prob_non_severe_death_treatment, length(population))
  assert_numeric(prob_non_severe_death_no_treatment, length(population))
  assert_numeric(prob_severe_death_treatment, length(population))
  assert_numeric(prob_severe_death_no_treatment, length(population))
  assert_numeric(p_dist, length(population))

  assert_leq(prob_hosp, 1)
  assert_leq(prob_severe, 1)
  assert_leq(prob_non_severe_death_treatment, 1)
  assert_leq(prob_non_severe_death_no_treatment, 1)
  assert_leq(prob_severe_death_treatment, 1)
  assert_leq(prob_severe_death_no_treatment, 1)
  assert_leq(p_dist, 1)

  assert_greq(prob_hosp, 0)
  assert_greq(prob_severe, 0)
  assert_greq(prob_non_severe_death_treatment, 0)
  assert_greq(prob_non_severe_death_no_treatment, 0)
  assert_greq(prob_severe_death_treatment, 0)
  assert_greq(prob_severe_death_no_treatment, 0)
  assert_greq(p_dist, 0)


  # Convert and Generate Parameters As Required
  # ----------------------------------------------------------------------------

  # durations
  gamma_E = 2 * 1/dur_E
  gamma_IMild = 1/dur_IMild
  gamma_ICase = 2 * 1/dur_ICase
  gamma_get_ox_survive = 2 * 1/dur_get_ox_survive
  gamma_get_ox_die = 2 * 1/dur_get_ox_die
  gamma_not_get_ox_survive = 2 * 1/dur_not_get_ox_survive
  gamma_not_get_ox_die = 2 * 1/dur_not_get_ox_die
  gamma_get_mv_survive = 2 * 1/dur_get_mv_survive
  gamma_get_mv_die = 2 * 1/dur_get_mv_die
  gamma_not_get_mv_survive = 2 * 1/dur_not_get_mv_survive
  gamma_not_get_mv_die = 2 * 1/dur_not_get_mv_die
  gamma_rec = 2 * 1/dur_rec

  if (is.null(beta_set)) {
    baseline_matrix <- process_contact_matrix_scaled_age(contact_matrix_set[[1]], population)
    beta_set <- beta_est_explicit(dur_IMild = dur_IMild,
                                  dur_ICase = dur_ICase,
                                  prob_hosp = prob_hosp,
                                  mixing_matrix = baseline_matrix,
                                  R0 = R0)
  }

  # normalise to sum to 1
  p_dist <- p_dist/mean(p_dist)

  # Collate Parameters Into List
  pars <- list(N_age = length(population),
               S_0 = mod_init$S,
               E1_0 = mod_init$E1,
               E2_0 = mod_init$E2,
               IMild_0 = mod_init$IMild,
               ICase1_0 = mod_init$ICase1,
               ICase2_0 = mod_init$ICase2,
               IOxGetLive1_0 = mod_init$IOxGetLive1,
               IOxGetLive2_0 = mod_init$IOxGetLive2,
               IOxGetDie1_0 = mod_init$IOxGetDie1,
               IOxGetDie2_0 = mod_init$IOxGetDie2,
               IOxNotGetLive1_0 = mod_init$IOxNotGetLive1,
               IOxNotGetLive2_0 = mod_init$IOxNotGetLive2,
               IOxNotGetDie1_0 = mod_init$IOxNotGetDie1,
               IOxNotGetDie2_0 = mod_init$IOxNotGetDie2,
               IMVGetLive1_0 = mod_init$IMVGetLive1,
               IMVGetLive2_0 = mod_init$IMVGetLive2,
               IMVGetDie1_0 = mod_init$IMVGetDie1,
               IMVGetDie2_0 = mod_init$IMVGetDie2,
               IMVNotGetLive1_0 = mod_init$IMVNotGetLive1,
               IMVNotGetLive2_0 = mod_init$IMVNotGetLive2,
               IMVNotGetDie1_0 = mod_init$IMVNotGetDie1,
               IMVNotGetDie2_0 = mod_init$IMVNotGetDie2,
               IRec1_0 = mod_init$IRec1,
               IRec2_0 = mod_init$IRec2,
               R_0 = mod_init$R,
               D_0 = mod_init$D,
               gamma_E = gamma_E,
               gamma_IMild = gamma_IMild,
               gamma_ICase = gamma_ICase,
               gamma_get_ox_survive = gamma_get_ox_survive,
               gamma_get_ox_die = gamma_get_ox_die,
               gamma_not_get_ox_survive = gamma_not_get_ox_survive,
               gamma_not_get_ox_die = gamma_not_get_ox_die,
               gamma_get_mv_survive = gamma_get_mv_survive,
               gamma_get_mv_die = gamma_get_mv_die,
               gamma_not_get_mv_survive = gamma_not_get_mv_survive,
               gamma_not_get_mv_die = gamma_not_get_mv_die,
               gamma_rec = gamma_rec,
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death_treatment = prob_non_severe_death_treatment,
               prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
               prob_severe_death_treatment = prob_severe_death_treatment,
               prob_severe_death_no_treatment = prob_severe_death_no_treatment,
               p_dist = p_dist,
               hosp_beds = hosp_bed_capacity,
               ICU_beds = ICU_bed_capacity,
               tt_hosp_beds = round(tt_hosp_beds/dt),
               tt_ICU_beds = round(tt_ICU_beds/dt),
               tt_matrix = round(tt_contact_matrix/dt),
               mix_mat_set = matrices_set,
               tt_beta = round(tt_R0/dt),
               beta_set = beta_set,
               dt = dt,
               population = population,
               contact_matrix_set = contact_matrix_set)

  class(pars) <- c("explicit_SEEIR_parameters", "squire_parameters")

  return(pars)

}
