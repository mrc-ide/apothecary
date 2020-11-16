## Index locations of outputs in odin model
#' @export
odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial(0))
  model$transform_variables(seq_len(1L + n_state + n_out))
}

#' Divide matrix by population
#'
#' @param contact Matrix
#' @param population Population vector
#'
#' @return Matrix
div_pop <- function(contact, population){
  t(t(contact) / population)
}


## Generate Matrix
#' @export
matrix_set_explicit <- function(contact_matrix_set, population){
  contact <- lapply(contact_matrix_set, process_contact_matrix_scaled_age,
                    population = population)
  mixing <- lapply(contact, div_pop, population = population)

  aperm(array(unlist(mixing), dim = c(dim(mixing[[1]]), length(mixing))), c(3, 1, 2))
}

#' Get mixing matrix
#'
#' @param country Country name
#' @param iso3c ISO 3C Country Code
#'
#' @return Age mixing matrix
#' @export
get_mixing_matrix <-  function(country = NULL, iso3c = NULL){

  if(!is.null(country) && !is.null(iso3c)) {
    message("Both iso3c and country were provided. Country will be used")
    iso3c <- NULL
  }

  pop <- get_population(country, iso3c)

  mm <- pop$matrix[1]
  mm <- squire::contact_matrices[[mm]]

  return(mm)
}

## Parsing country, population and associated mixing matrix data
#' @export
parse_country_population_mixing_matrix <- function(country = NULL,
                                                   population = NULL,
                                                   contact_matrix_set = NULL) {

  # Handle country population args
  if (is.null(country) &&
      (is.null(population) || is.null(contact_matrix_set))) {
    stop("User must provide either the country being simulated or
         both the population size and contact_matrix_set")
  }

  # If a country was provided then grab the population and matrices if needed
  if (is.null(population)) {
    population <- get_population(country)
    population <- population$n
  }

  if (is.null(contact_matrix_set)) {
    contact_matrix_set <- get_mixing_matrix(country)
  }

  ret <- list(population = population,
              country = country,
              contact_matrix_set = contact_matrix_set)

  return(ret)

  }

#' Process a contact matrix with an extra
#'
#' @param contact_matrix A contact matrix
#' @param population Vector of population by age
#'
#' @return Processed matrix
#' @export
#'
process_contact_matrix_scaled_age <- function(contact_matrix, population) {
  # Convert Unbalanced Matrix of Per-Capita Rates to Total Number of Contacts
  # Between Diff Age Groups and Balance By Taking the Mean of i->j and j->i

  contact_matrix <- rbind(contact_matrix, contact_matrix[16,])
  contact_matrix <- cbind(contact_matrix, contact_matrix[,16]*population[17] / sum(population[16:17]))
  contact_matrix[,16] <- contact_matrix[,16]*population[16] / sum(population[16:17])

  MIJ <- t(vapply(seq(population),function(x){
    contact_matrix[x,] * population[x]
  }, FUN.VALUE = numeric(length(population))))

  adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced

  # Convert to New Per-Capita Rates By Dividing By Population
  # Resulting Matrix Is Asymmetric But Balanced
  # Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
  # Is Balanced (so when we divide by pop at end, will be balanced)
  processed_matrix <- t(vapply(seq(population), function(x) {
    adjust_mat[x, ] / population[x]
  }, FUN.VALUE = numeric(length(population))))

  # Adjusting to create input for model i.e. per capita rates divided by
  # population to give the number of contacts made on each individual
  return(processed_matrix)
}

# Get ICU bed capacity
#' @noRd
get_ICU_bed_capacity <- function(country) {

  beds <- get_healthcare_capacity(country)
  ICU_beds <- beds$ICU_beds
  population <- get_population(country)$n
  ICU_bed_capacity <- round(ICU_beds * sum(population)/1000)
  ICU_bed_capacity

}

# Get hospital bed capacity
#' @noRd
get_hosp_bed_capacity <- function(country = NULL) {

  beds <- get_healthcare_capacity(country)
  population <- get_population(country)$n
  hosp_beds <- beds$hosp_beds
  hosp_bed_capacity <- round(hosp_beds * sum(population)/1000)

}

#' Get healthcare capacity data
#'
#' @export
get_healthcare_capacity <-  function(country){

  if(!country %in% unique(apothecary::population$country)){
    stop("Country not found")
  }

  if(country %in% unique(apothecary::country_specific_healthcare_capacity$country)) {
    beds <- apothecary::country_specific_healthcare_capacity[match(country, apothecary::country_specific_healthcare_capacity$country), ]
    hosp_beds <- beds$hosp_beds
    ICU_beds <- beds$ICU_beds
    hc <- list(hosp_beds = hosp_beds, ICU_beds = ICU_beds)
  } else {
    income_group <- apothecary::income_group$income_group[match(country, apothecary::income_group$country)]
    if (is.na(income_group)) {
      stop("healthcare capacity data not available for this country - specify hospital and ICU beds in the run_explicit_SEEIR call manually")
    }
    beds <- apothecary::income_strata_healthcare_capacity[apothecary::income_strata_healthcare_capacity$income_group == income_group, ]
    hosp_beds <- as.vector(beds$hosp_beds)
    ICU_beds <- as.vector(beds$ICU_beds)
    hc <- list(hosp_beds = hosp_beds, ICU_beds = ICU_beds)
  }

  return(hc)
}

#' Get population data
#'
#' @param country Country name
#' @param iso3c ISO 3C Country Code
#' @param simple_SEIR Logical. Is the population for the \code{simple_SEIR}.
#'   Default = FALSE
#'
#' @return Population data.frame
#' @importFrom utils head tail
#' @export
get_population <-  function(country = NULL, iso3c = NULL, simple_SEIR = FALSE){

  ## country route
  if(!is.null(country)) {
    assert_string(country)
    if(!country %in% unique(apothecary::population$country)){
      stop("Country not found")
    }
    pc <- apothecary::population[apothecary::population$country == country, ] %>%
      dplyr::arrange(.data$age_group)
  }

  # iso3c route
  if(!is.null(iso3c)) {
    assert_string(iso3c)
    if(!iso3c %in% unique(apothecary::population$iso3c)){
      stop("iso3c not found")
    }
    pc <- apothecary::population[apothecary::population$iso3c == iso3c, ] %>%
      dplyr::arrange(.data$age_group)
  }

  return(pc)
}

#' Parse country severity parameters
#' @noRd
parse_country_severity <- function(country = NULL,
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
                                   walker_params = FALSE) {

  # If walker_params == TRUE, use the original squire parameters described in Walker et al.
  squire:::assert_logical(walker_params)
  if (walker_params) {
    if (is.null(prob_asymp)) {
      prob_asymp <- rep(0.2, 17)
    }
    if (is.null(prob_hosp)) {
      prob_hosp <- c(
        0.000744192, 0.000634166, 0.001171109, 0.002394593, 0.005346437 ,
        0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
        0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
        0.176634654 ,0.180000000)
    }
    if (is.null(prob_severe)) {
      prob_severe <- c(
        0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
        0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
        0.103612417, 0.149427991, 0.223777304,	0.306985918,
        0.385779555, 0.461217861, 0.709444444)
    }
    if (is.null(prob_critical)) {
      prob_critical <- rep(0.8, 17)
    }

    # Moderate Disease
    if (is.null(prob_moderate_death_get_hosp_get_ox_baseline)) {
      prob_moderate_death_get_hosp_get_ox_baseline <- c(
        0.0125702, 0.0125702,	0.0125702, 0.0125702,
        0.0125702, 0.0125702,	0.0125702, 0.013361147,
        0.015104687, 0.019164124,	0.027477519, 0.041762108,
        0.068531658, 0.105302319,	0.149305732, 0.20349534, 0.5804312)
    }
    if (is.null(prob_moderate_death_get_hosp_no_ox_baseline)) {
      prob_moderate_death_get_hosp_no_ox_baseline <- rep(0.6, length(prob_hosp))
    }
    if (is.null(prob_moderate_death_no_hosp_no_ox)) {
      prob_moderate_death_no_hosp_no_ox <- rep(0.6, length(prob_hosp))
    }

    # Severe Disease
    if (is.null(prob_severe_death_get_ICU_get_ox_baseline)) {
      prob_severe_death_get_ICU_get_ox_baseline <- rep(0.5, length(prob_hosp))
    }
    if (is.null(prob_severe_death_get_ICU_no_ox_baseline)) {
      prob_severe_death_get_ICU_no_ox_baseline <- rep(0.95, length(prob_hosp))
    }
    if (is.null(prob_severe_death_no_ICU_no_ox)) {
      prob_severe_death_no_ICU_no_ox <- rep(0.95, length(prob_hosp))
    }

    # Critical Disease
    if (is.null(prob_critical_death_get_ICU_get_ox_get_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_get_MV_baseline <- rep(0.5, length(prob_hosp))
    }
    if (is.null(prob_critical_death_get_ICU_get_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_no_MV_baseline <- rep(0.95, length(prob_hosp))
    }
    if (is.null(prob_critical_death_get_ICU_no_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_no_ox_no_MV_baseline <- rep(0.95, length(prob_hosp))
    }
    if (is.null(prob_critical_death_no_ICU_no_ox_no_MV)) {
      prob_critical_death_no_ICU_no_ox_no_MV <- rep(0.95, length(prob_hosp))
    }

    ret <- list(country = country,
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
                prob_critical_death_no_ICU_no_ox_no_MV = prob_critical_death_no_ICU_no_ox_no_MV)
  }

  # Filling in any missing parameters
  if (is.null(prob_asymp)) {
    prob_asymp <- probs$prob_asymp
  }
  if (is.null(prob_hosp)) {
    prob_hosp <- probs$prob_hosp
  }
  if (is.null(prob_severe)) {
    prob_severe <- probs$prob_severe
  }
  if (is.null(prob_critical)) {
    prob_critical <- probs$prob_critical
  }

  # If no country specified, fill in remaining missing probs with defaults, make no adjustment
  if (is.null(country)) {

    # Moderate Disease
    if (is.null(prob_moderate_death_get_hosp_get_ox_baseline)) {
      prob_moderate_death_get_hosp_get_ox_baseline <- probs$prob_moderate_death_get_hosp_get_ox_baseline
    }
    if (is.null(prob_moderate_death_get_hosp_no_ox_baseline)) {
      prob_moderate_death_get_hosp_no_ox_baseline <- probs$prob_moderate_death_get_hosp_no_ox_baseline
    }
    if (is.null(prob_moderate_death_no_hosp_no_ox)) {
      prob_moderate_death_no_hosp_no_ox <- probs$prob_moderate_death_no_hosp_no_ox
    }

    # Severe Disease
    if (is.null(prob_severe_death_get_ICU_get_ox_baseline)) {
      prob_severe_death_get_ICU_get_ox_baseline <- probs$prob_severe_death_get_ICU_get_ox_baseline
    }
    if (is.null(prob_severe_death_get_ICU_no_ox_baseline)) {
      prob_severe_death_get_ICU_no_ox_baseline <- probs$prob_severe_death_get_ICU_no_ox_baseline
    }
    if (is.null(prob_severe_death_no_ICU_no_ox)) {
      prob_severe_death_no_ICU_no_ox <- probs$prob_severe_death_no_ICU_no_ox
    }

    # Critical Disease
    if (is.null(prob_critical_death_get_ICU_get_ox_get_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_get_MV_baseline <- probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
    }
    if (is.null(prob_critical_death_get_ICU_get_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_no_MV_baseline <- probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
    }
    if (is.null(prob_critical_death_get_ICU_no_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_no_ox_no_MV_baseline <- probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline
    }
    if (is.null(prob_critical_death_no_ICU_no_ox_no_MV)) {
      prob_critical_death_no_ICU_no_ox_no_MV <- probs$prob_critical_death_no_ICU_no_ox_no_MV
    }
    ret <- list(country = country,
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
                prob_critical_death_no_ICU_no_ox_no_MV = prob_critical_death_no_ICU_no_ox_no_MV)
  }

  # If country is specified, check valid and then adjust default probs based on demography
  if (!is.null(country)) {

    # Check country valid and then grab relevant elderly population
    if(!country %in% unique(squire::population$country)){
      stop("Country not found")
    }
    population <- squire:::get_population(country)
    population <- population$n
    elderly_pop <- squire:::get_elderly_population(country)
    elderly_pop <- elderly_pop$n

    # Adjusting death probability for country-specific 80+ demographic compositions
    index <- length(prob_severe)
    prop_deaths_ICU_80plus <- 0.10 # based off CHESS data
    prop_deaths_ICU_vent_80plus <- 0.8030396 # based off CHESS data
    elderly_IFR <- c(0.05659,	0.08862, 0.17370) # from Brazeau et al, for 80-84, 85-89 and 90+
    IFR_80plus <- sum(elderly_pop/sum(elderly_pop) * elderly_IFR)
    CFR_hosp_overall_80plus <- IFR_80plus/prob_hosp[index]
    CFR_ICU_80plus <- CFR_hosp_overall_80plus * prop_deaths_ICU_80plus/prob_severe[index]
    CFR_mod_80plus <- (CFR_hosp_overall_80plus - (CFR_ICU_80plus * prob_severe[index]))/(1 - prob_severe[index])
    CFR_crit_80_plus <- CFR_ICU_80plus * prop_deaths_ICU_vent_80plus/prob_critical[index]
    CFR_sev_80_plus <- (CFR_ICU_80plus - CFR_crit_80_plus * prob_critical[index])/(1 - prob_critical[index])

    # Moderate Disease
    if (is.null(prob_moderate_death_get_hosp_get_ox_baseline)) {
      prob_moderate_death_get_hosp_get_ox_baseline <- probs$prob_moderate_death_get_hosp_get_ox_baseline
      prob_moderate_death_get_hosp_get_ox_baseline[index] <- min(1, CFR_mod_80plus)
    }
    if (is.null(prob_moderate_death_get_hosp_no_ox_baseline)) {
      prob_moderate_death_get_hosp_no_ox_baseline <- probs$prob_moderate_death_get_hosp_no_ox_baseline
      prob_moderate_death_get_hosp_no_ox_baseline[index] <- min(1, prob_moderate_death_get_hosp_get_ox_baseline * probs$mod_RR * probs$bed_RR)
    }
    if (is.null(prob_moderate_death_no_hosp_no_ox)) {
      prob_moderate_death_no_hosp_no_ox <- probs$prob_moderate_death_no_hosp_no_ox
      prob_moderate_death_no_hosp_no_ox[index] <- min(1, prob_moderate_death_get_hosp_get_ox_baseline * probs$mod_RR)
    }

    # Severe Disease
    if (is.null(prob_severe_death_get_ICU_get_ox_baseline)) {
      prob_severe_death_get_ICU_get_ox_baseline <- probs$prob_severe_death_get_ICU_get_ox_baseline
      prob_severe_death_get_ICU_get_ox_baseline[index] <- min(1, CFR_sev_80_plus)
    }
    if (is.null(prob_severe_death_get_ICU_no_ox_baseline)) {
      prob_severe_death_get_ICU_no_ox_baseline <- probs$prob_severe_death_get_ICU_no_ox_baseline
      prob_severe_death_get_ICU_no_ox_baseline[index] <- min(1, prob_severe_death_get_ICU_get_ox_baseline * probs$sev_RR * probs$bed_RR)
    }
    if (is.null(prob_severe_death_no_ICU_no_ox)) {
      prob_severe_death_no_ICU_no_ox <- probs$prob_severe_death_no_ICU_no_ox
      prob_severe_death_no_ICU_no_ox[index] <- min(1, prob_severe_death_get_ICU_get_ox_baseline * probs$sev_RR)
    }

    # Critical Disease
    if (is.null(prob_critical_death_get_ICU_get_ox_get_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_get_MV_baseline <- probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
      prob_critical_death_get_ICU_get_ox_get_MV_baseline[index] <- min(1, CFR_crit_80_plus)
    }
    if (is.null(prob_critical_death_get_ICU_get_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_get_ox_no_MV_baseline <- probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
      prob_critical_death_get_ICU_get_ox_no_MV_baseline[index] <- min(1, max(0.95, prob_severe_death_no_ICU_no_ox))
    }
    if (is.null(prob_critical_death_get_ICU_no_ox_no_MV_baseline)) {
      prob_critical_death_get_ICU_no_ox_no_MV_baseline <- probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline
      prob_critical_death_get_ICU_no_ox_no_MV_baseline[index] <- min(1, max(0.95, prob_severe_death_no_ICU_no_ox))
    }
    if (is.null(prob_critical_death_no_ICU_no_ox_no_MV)) {
      prob_critical_death_no_ICU_no_ox_no_MV <- probs$prob_critical_death_no_ICU_no_ox_no_MV
      prob_critical_death_no_ICU_no_ox_no_MV[index] <- min(1, max(0.95, prob_severe_death_no_ICU_no_ox))
    }

    ret <- list(country = country,
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
                prob_critical_death_no_ICU_no_ox_no_MV = prob_critical_death_no_ICU_no_ox_no_MV)
  }

  return(ret)

}

#' @noRd
parse_durations <- function(dur_E = NULL,
                            dur_IAsymp = NULL,
                            dur_IMild = NULL,
                            dur_ICase = NULL,
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
                            dur_rec = NULL,
                            walker_params = FALSE) {

  # If walker_params == TRUE, use the original squire parameters described in Walker et al.
  squire:::assert_logical(walker_params)
  if (walker_params) {
    if (is.null(dur_E)) {
      dur_E <- 4.6
    }
    if (is.null(dur_IAsymp)) {
      dur_IAsymp <- 2.1
    }
    if (is.null(dur_IMild)) {
      dur_IMild <- 2.1
    }
    if (is.null(dur_ICase)) {
      dur_ICase <- 4.5
    }
    if (is.null(dur_IMod_GetHosp_GetOx_Surv)) {
      dur_IMod_GetHosp_GetOx_Surv <- 9.5
    }
    if (is.null(dur_IMod_GetHosp_GetOx_Die)) {
      dur_IMod_GetHosp_GetOx_Die <- 7.6
    }
    if (is.null(dur_IMod_GetHosp_NoOx_Surv)) {
      dur_IMod_GetHosp_NoOx_Surv <- 9.5 * 0.5
    }
    if (is.null(dur_IMod_GetHosp_NoOx_Die)) {
      dur_IMod_GetHosp_NoOx_Die <- 7.6 * 0.5
    }
    if (is.null(dur_IMod_NoHosp_NoOx_Surv)) {
      dur_IMod_NoHosp_NoOx_Die <- 9.5 * 0.5
    }
    if (is.null(dur_IMod_NoHosp_NoOx_Die)) {
      dur_IMod_NoHosp_NoOx_Die <- 7.6 * 0.5
    }
    if (is.null(dur_ISev_GetICU_GetOx_Surv)) {
      dur_ISev_GetICU_GetOx_Surv  <- 11.3
    }
    if (is.null(dur_ISev_GetICU_GetOx_Die)) {
      dur_ISev_GetICU_GetOx_Die  <- 10.1
    }
    if (is.null(dur_ISev_GetICU_NoOx_Surv)) {
      dur_ISev_GetICU_NoOx_Surv  <- 11.3 * 0.5
    }
    if (is.null(dur_ISev_GetICU_NoOx_Die)) {
      dur_ISev_GetICU_NoOx_Die  <- 1
    }
    if (is.null(dur_ISev_NoICU_NoOx_Surv)) {
      dur_ISev_NoICU_NoOx_Surv  <- 11.3 * 0.5
    }
    if (is.null(dur_ISev_NoICU_NoOx_Die)) {
      dur_ISev_NoICU_NoOx_Die <- 1
    }
    if (is.null(dur_ICrit_GetICU_GetOx_GetMV_Surv )) {
      dur_ICrit_GetICU_GetOx_GetMV_Surv <- 11.3
    }
    if (is.null(dur_ICrit_GetICU_GetOx_GetMV_Die)) {
      dur_ICrit_GetICU_GetOx_GetMV_Die  <- 10.1
    }
    if (is.null(dur_ICrit_GetICU_GetOx_NoMV_Surv)) {
      dur_ICrit_GetICU_GetOx_NoMV_Surv  <- 11.3 * 0.5
    }
    if (is.null(dur_ICrit_GetICU_GetOx_NoMV_Die)) {
      dur_ICrit_GetICU_GetOx_NoMV_Die <- 1
    }
    if (is.null(dur_ICrit_GetICU_NoOx_NoMV_Surv)) {
      dur_ICrit_GetICU_NoOx_NoMV_Surv <- 11.3 * 0.5
    }
    if (is.null(dur_ICrit_GetICU_NoOx_NoMV_Die)) {
      dur_ICrit_GetICU_NoOx_NoMV_Die <- 1
    }
    if (is.null(dur_ICrit_NoICU_NoOx_NoMV_Surv)) {
      dur_ICrit_NoICU_NoOx_NoMV_Surv <- 11.3 * 0.5
    }
    if (is.null(dur_ICrit_NoICU_NoOx_NoMV_Die)) {
      dur_ICrit_NoICU_NoOx_NoMV_Die <- 1
    }
    if (is.null(dur_rec)) {
      dur_rec <- 3.4
    }

  } else {
    if (is.null(dur_E)) {
      dur_E <- durations$dur_E
    }
    if (is.null(dur_IAsymp)) {
      dur_IAsymp <- durations$dur_IAsymp
    }
    if (is.null(dur_IMild)) {
      dur_IMild <- durations$dur_IMild
    }
    if (is.null(dur_ICase)) {
      dur_ICase <- durations$dur_ICase
    }
    if (is.null(dur_IMod_GetHosp_GetOx_Surv)) {
      dur_IMod_GetHosp_GetOx_Surv <- durations$dur_IMod_GetHosp_GetOx_Surv
    }
    if (is.null(dur_IMod_GetHosp_GetOx_Die)) {
      dur_IMod_GetHosp_GetOx_Die <- durations$dur_IMod_GetHosp_GetOx_Die
    }
    if (is.null(dur_IMod_GetHosp_NoOx_Surv)) {
      dur_IMod_GetHosp_NoOx_Surv <- durations$dur_IMod_GetHosp_NoOx_Surv
    }
    if (is.null(dur_IMod_GetHosp_NoOx_Die)) {
      dur_IMod_GetHosp_NoOx_Die <- durations$dur_IMod_GetHosp_NoOx_Die
    }
    if (is.null(dur_IMod_NoHosp_NoOx_Surv)) {
      dur_IMod_NoHosp_NoOx_Surv <- durations$dur_IMod_NoHosp_NoOx_Surv
    }
    if (is.null(dur_IMod_NoHosp_NoOx_Die)) {
      dur_IMod_NoHosp_NoOx_Die <- durations$dur_IMod_NoHosp_NoOx_Die
    }
    if (is.null(dur_ISev_GetICU_GetOx_Surv)) {
      dur_ISev_GetICU_GetOx_Surv  <- durations$dur_ISev_GetICU_GetOx_Surv
    }
    if (is.null(dur_ISev_GetICU_GetOx_Die)) {
      dur_ISev_GetICU_GetOx_Die  <- durations$dur_ISev_GetICU_GetOx_Die
    }
    if (is.null(dur_ISev_GetICU_NoOx_Surv)) {
      dur_ISev_GetICU_NoOx_Surv  <- durations$dur_ISev_GetICU_NoOx_Surv
    }
    if (is.null(dur_ISev_GetICU_NoOx_Die)) {
      dur_ISev_GetICU_NoOx_Die  <- durations$dur_ISev_GetICU_NoOx_Die
    }
    if (is.null(dur_ISev_NoICU_NoOx_Surv)) {
      dur_ISev_NoICU_NoOx_Surv  <- durations$dur_ISev_NoICU_NoOx_Surv
    }
    if (is.null(dur_ISev_NoICU_NoOx_Die)) {
      dur_ISev_NoICU_NoOx_Die <- durations$dur_ISev_NoICU_NoOx_Die
    }
    if (is.null(dur_ICrit_GetICU_GetOx_GetMV_Surv )) {
      dur_ICrit_GetICU_GetOx_GetMV_Surv <- durations$dur_ICrit_GetICU_GetOx_GetMV_Surv
    }
    if (is.null(dur_ICrit_GetICU_GetOx_GetMV_Die)) {
      dur_ICrit_GetICU_GetOx_GetMV_Die  <- durations$dur_ICrit_GetICU_GetOx_GetMV_Die
    }
    if (is.null(dur_ICrit_GetICU_GetOx_NoMV_Surv)) {
      dur_ICrit_GetICU_GetOx_NoMV_Surv  <- durations$dur_ICrit_GetICU_GetOx_NoMV_Surv
    }
    if (is.null(dur_ICrit_GetICU_GetOx_NoMV_Die)) {
      dur_ICrit_GetICU_GetOx_NoMV_Die <- durations$dur_ICrit_GetICU_GetOx_NoMV_Die
    }
    if (is.null(dur_ICrit_GetICU_NoOx_NoMV_Surv)) {
      dur_ICrit_GetICU_NoOx_NoMV_Surv <- durations$dur_ICrit_GetICU_NoOx_NoMV_Surv
    }
    if (is.null(dur_ICrit_GetICU_NoOx_NoMV_Die)) {
      dur_ICrit_GetICU_NoOx_NoMV_Die <- durations$dur_ICrit_GetICU_NoOx_NoMV_Die
    }
    if (is.null(dur_ICrit_NoICU_NoOx_NoMV_Surv)) {
      dur_ICrit_NoICU_NoOx_NoMV_Surv <- durations$dur_ICrit_NoICU_NoOx_NoMV_Surv
    }
    if (is.null(dur_ICrit_NoICU_NoOx_NoMV_Die)) {
      dur_ICrit_NoICU_NoOx_NoMV_Die <- durations$dur_ICrit_NoICU_NoOx_NoMV_Die
    }
    if (is.null(dur_rec)) {
      dur_rec <- durations$dur_rec
    }
  }

  ret <- list(dur_E = dur_E,
              dur_IAsymp = dur_IAsymp,
              dur_IMild = dur_IMild,
              dur_ICase = dur_ICase,
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
              dur_rec = dur_rec)
  return(ret)

}
