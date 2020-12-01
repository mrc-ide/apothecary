# Loading Required Libraries


# Loading apothecary
devtools::load_all()

# Defining the Demographic and Epidemiological Parameters Used In Our Generic Scenario
country <- "Bhutan"
raw_pop <- squire::population[squire::population$country == country, ]
standard_population <- round(raw_pop$n/sum(raw_pop$n) * 50000000)

standard_population_old_agg <- standard_population
standard_population_old_agg[16] <- standard_population_old_agg[16] + standard_population_old_agg[17]
standard_population_old_agg <- standard_population_old_agg[-17]
prop_pop <- standard_population_old_agg/sum(standard_population_old_agg)
standard_matrix <- matrix(rep(prop_pop, 16), ncol = 16, byrow = TRUE)

# Defining the Healthcare Capacity Parameters Used In Each Scenario
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- actual_ICU_beds * 0.4
unlim <- 100000000

temp <- run_apothecary(

  # Demographic and Epidemiological Parameter Specification
  country = "Bhutan",
  R0 = 2,
  population = standard_population,
  contact_matrix_set = standard_matrix,
  time_period = 365,
  seeding_cases = 20,
  day_return = TRUE,

  # Healthcare Capacity Parameter Specification
  hosp_bed_capacity = 10000000000,
  ICU_bed_capacity = 10000000000,
  prop_ox_hosp_beds = 1,
  prop_ox_ICU_beds = 1,
  MV_capacity = 10000000000)

index <- apothecary::odin_index(temp$model)

R <- apply(temp$output[, index$R], 2, max)
D <- apply(temp$output[, index$D], 2, max)

(R + D)/standard_population
