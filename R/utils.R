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

