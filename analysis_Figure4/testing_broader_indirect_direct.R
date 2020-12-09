# Loading required libraries
library(tidyverse)

# Loading apothecary
devtools::load_all()

# Generating standard contact matrix, population etc
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
actual_MV_capacity <- round(actual_ICU_beds * 0.5)
time <- 600

# Running and assessing Remdesivir impact
none <- run_apothecary(country = "Bhutan", R0 = 3, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)
rem <- run_apothecary(country = "Bhutan", R0 = 3, population = standard_population, contact_matrix_set = standard_matrix,
                      time_period = time, seeding_cases = 20, day_return = TRUE,
                      hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                      prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                      MV_capacity = actual_MV_capacity,
                      drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = 0.5,
                      drug_7_indic = 1, drug_7_prop_treat = 1, drug_7_effect_size = 0,
                      drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1, drug_8_prop_treat = 1,
                      drug_8_GetOx_effect_size = 1, drug_8_NoOx_effect_size = 1,
                      drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1, drug_9_prop_treat = 1,
                      drug_9_GetOx_effect_size = 1, drug_9_NoOx_effect_size = 1,
                      drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_10_prop_treat = 1,
                      drug_10_GetOx_GetMV_effect_size = 1, drug_10_GetOx_NoMV_effect_size = 1, drug_10_NoOx_NoMV_effect_size = 1,
                      drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_prop_treat = 1,
                      drug_11_GetOx_effect_size = 1, drug_11_NoOx_effect_size = 1,
                      drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_prop_treat = 1,
                      drug_12_GetOx_effect_size = 1, drug_12_NoOx_effect_size = 1,
                      drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_prop_treat = 1,
                      drug_13_GetOx_GetMV_effect_size = 1, drug_13_GetOx_NoMV_effect_size = 1, drug_13_NoOx_NoMV_effect_size = 1)
index <- squire:::odin_index(none$model)
total_deaths_averted <- sum(apply(none$output[, index$D], 2, max)) - sum(apply(rem$output[, index$D], 2, max))

# Impact for Moderate Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       a) some wouldn't have got benefit otherwise
#       b) some would have got benefit irrespective of the indirect effect

# Model Probabilities and Drug Effects
prob_IMod_death_full <- rem$parameters$prob_moderate_death_get_hosp_get_ox_baseline
prob_IMod_death_inc <- rem$parameters$prob_moderate_death_get_hosp_no_ox_baseline
prob_IMod_death_none <- rem$parameters$prob_moderate_death_no_hosp_no_ox

# Number of Individuals Receiving Healthcare Who Would Have Otherwise Not Received It
num_IMod_extra_from_none <- unname(apply(none$output[, index$number_IMod_NoHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_NoHosp_NoOx], 2, sum))
num_IMod_extra_from_inc <- unname(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) - apply(rem$output[, index$number_IMod_GetHosp_NoOx], 2, sum))

# 1a) Deaths averted due to reducing healthcare demand and more people getting better healthcare
IMod_D_avert_none_to_full_incr_hc <- sum(num_IMod_extra_from_none * prob_IMod_death_none) - sum(num_IMod_extra_from_none * prob_IMod_death_full)
IMod_D_avert_inc_to_full_incr_hc <- sum(num_IMod_extra_from_inc * prob_IMod_death_inc) - sum(num_IMod_extra_from_inc * prob_IMod_death_full)
IMod_D_avert_incr_hc <- IMod_D_avert_none_to_full_incr_hc + IMod_D_avert_inc_to_full_incr_hc

# 2a) Extra deaths averted from those better treated individuals getting benefit of the drug
IMod_D_avert_none_to_full_incr_hc_drug <- sum(num_IMod_extra_from_none * prob_IMod_death_none * 1) -
                                          sum(num_IMod_extra_from_none * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size)
IMod_D_avert_inc_to_full_incr_hc_drug <- sum(num_IMod_extra_from_inc * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size) -
                                         sum(num_IMod_extra_from_inc * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size)
IMod_D_avert_incr_hc_drug <- IMod_D_avert_none_to_full_incr_hc_drug + IMod_D_avert_inc_to_full_incr_hc_drug - IMod_D_avert_incr_hc

# 2b) Deaths averted due to set of people receiving the drug irrespective of reduced strain on healthcare
IMod_D_no_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_IMod_death_full) +
                  sum(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) * prob_IMod_death_inc)
IMod_D_drug <- sum(apply(none$output[, index$number_IMod_GetHosp_GetOx], 2, sum) * prob_IMod_death_full * rem$parameters$drug_11_GetOx_effect_size) +
               sum(apply(none$output[, index$number_IMod_GetHosp_NoOx], 2, sum) * prob_IMod_death_inc * rem$parameters$drug_11_NoOx_effect_size)
IMod_D_avert_drug <- IMod_D_no_drug - IMod_D_drug

# Impact for Severe Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       a) some wouldn't have got benefit otherwise
#       b) some would have got benefit irrespective of the indirect effect

# ISSUE IS THAT WE'RE CURRENTLY ASSUMING IF THEY'RE NO LONGER IN ISEV_NONE THEY MUST HAVE GONE TO ANOTHER ISEV COMPARTMENT, BUT ACTUALLY, THEY MIGHT
# HAVE GONE TO IMILD INSTEAD.
# THINK I WANT TO SET UP AN INITIAL BIT HERE THAT CHOPS OUT THE PEOPLE WHO GET TAKEN AWAY FROM SEV AND THROWN INTO MODERATE INSTEAD MAYBE
# NOT SURE HOW THAT INTERACTS WITH ALL THE OTHER BITS AND BOBS I'VE SET UP HERE TBH

# I THINK AT EACH TIMEPOINT I'M GOING TO HAVE TO WORK OUT PROPORTION OF MODERATE CASES THAT ARE FROM SEV/CRIT -> AND SUBTRACT THESE
# FROM THE BONA FIDE MODERATE (NO DRUG) CASES. THEN DO THE MODERATE STUFF FOR THE BONA FIDE.
# THEN FOR THE OTHERWISE CRIT, AT EACH TIMEPOINT WORK OUT WHAT PROPORTION ETC THEN ASSIGN THEM TO EACH OF THE DIFFERENT HEALTHCARE GROUP
# ACCORDINGLY (BASED ON THEIR PROPORTION OF TOTAL MODERATES). AND THEN COMPARE THEIR MORTALITY IN MOD TO THEIR MORTALITY OTHERWISE IN SEV/CRIT.

# GONNA END UP WITH OUR BONA FIDE MODERATE CASES AND OUR OTHERWISE-ICU MODERATE CASES.

# Model Probabilities and Drug Effects
prob_ISev_death_full <- rem$parameters$prob_severe_death_get_ICU_get_ox_baseline
prob_ISev_death_inc <- rem$parameters$prob_severe_death_get_ICU_no_ox_baseline
prob_ISev_death_none <- rem$parameters$prob_severe_death_no_ICU_no_ox

# Number of Individuals Receiving Healthcare Who Would Have Otherwise Not Received It
num_ISev_extra_from_none <- unname(apply(none$output[, index$number_ISev_NoICU_NoOx], 2, sum) - apply(rem$output[, index$number_ISev_NoICU_NoOx], 2, sum))
num_ISev_extra_from_inc <- unname(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) - apply(rem$output[, index$number_ISev_GetICU_NoOx], 2, sum))

# 1a) Deaths averted due to reducing healthcare demand and more people getting better healthcare
ISev_D_avert_none_to_full_incr_hc <- sum(num_ISev_extra_from_none * prob_ISev_death_none) - sum(num_ISev_extra_from_none * prob_ISev_death_full)
ISev_D_avert_inc_to_full_incr_hc <- sum(num_ISev_extra_from_inc * prob_ISev_death_inc) - sum(num_ISev_extra_from_inc * prob_ISev_death_full)
ISev_D_avert_incr_hc <- ISev_D_avert_none_to_full_incr_hc + ISev_D_avert_inc_to_full_incr_hc

# 2a) Extra deaths averted from those better treated individuals getting benefit of the drug
ISev_D_avert_none_to_full_incr_hc_drug <- sum(num_ISev_extra_from_none * prob_ISev_death_none * 1) - sum(num_ISev_extra_from_none * prob_ISev_death_full * rem$parameters$drug_12_GetOx_effect_size)
ISev_D_avert_inc_to_full_incr_hc_drug <- sum(num_ISev_extra_from_inc * prob_ISev_death_inc * rem$parameters$drug_12_NoOx_effect_size) - sum(num_ISev_extra_from_inc * prob_ISev_death_full * rem$parameters$drug_12_GetOx_effect_size)
ISev_D_avert_incr_hc_drug <- ISev_D_avert_none_to_full_incr_hc_drug + ISev_D_avert_inc_to_full_incr_hc_drug - ISev_D_avert_incr_hc

# 2b) Deaths averted due to set of people receiving the drug irrespective of reduced strain on healthcare
ISev_D_no_drug <- sum(apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum) * prob_ISev_death_full) + sum(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) * prob_ISev_death_inc)
ISev_D_drug <- sum(apply(none$output[, index$number_ISev_GetICU_GetOx], 2, sum) * prob_ISev_death_full * rem$parameters$drug_12_GetOx_effect_size) + sum(apply(none$output[, index$number_ISev_GetICU_NoOx], 2, sum) * prob_ISev_death_inc * rem$parameters$drug_12_NoOx_effect_size)
ISev_D_avert_drug <- ISev_D_no_drug - ISev_D_drug

# Impact for Critical Patients:
# 1) individuals got full treatment rather than no treatment
# 2) individuals got effect of mortality reducing treatment
#       a) some wouldn't have got benefit otherwise
#       b) some would have got benefit irrespective of the indirect effect

# Model Probabilities and Drug Effects
prob_ICrit_death_full <- rem$parameters$prob_critical_death_get_ICU_get_ox_get_MV_baseline
prob_ICrit_death_inc_1 <- rem$parameters$prob_critical_death_get_ICU_get_ox_no_MV_baseline
prob_ICrit_death_inc_2 <- rem$parameters$prob_critical_death_get_ICU_no_ox_no_MV_baseline
prob_ICrit_death_none <- rem$parameters$prob_critical_death_no_ICU_no_ox_no_MV

# Number of Individuals Receiving Healthcare Who Would Have Otherwise Not Received It
num_ICrit_extra_from_none <- unname(apply(none$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum) - apply(rem$output[, index$number_ICrit_NoICU_NoOx_NoMV], 2, sum))
num_ICrit_extra_from_inc2 <- unname(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) - apply(rem$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum))
num_ICrit_extra_from_inc1 <- unname(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) - apply(rem$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum))

# 1a) Deaths averted due to reducing healthcare demand and more people getting better healthcare
ICrit_D_avert_none_to_full_incr_hc <- sum(num_ICrit_extra_from_none * prob_ICrit_death_none) - sum(num_ICrit_extra_from_none * prob_ICrit_death_full)
ICrit_D_avert_inc2_to_full_incr_hc <- sum(num_ICrit_extra_from_inc2 * prob_ICrit_death_inc_2) - sum(num_ICrit_extra_from_inc2 * prob_ICrit_death_full)
ICrit_D_avert_inc1_to_full_incr_hc <- sum(num_ICrit_extra_from_inc1 * prob_ICrit_death_inc_1) - sum(num_ICrit_extra_from_inc1 * prob_ICrit_death_full)
ICrit_D_avert_incr_hc <- ICrit_D_avert_none_to_full_incr_hc + ICrit_D_avert_inc2_to_full_incr_hc + ICrit_D_avert_inc1_to_full_incr_hc

# 2a) Extra deaths averted from those better treated individuals getting benefit of the drug
ICrit_D_avert_none_to_full_incr_hc_drug <- sum(num_ICrit_extra_from_none * prob_ICrit_death_none * 1) - sum(num_ICrit_extra_from_none * prob_ICrit_death_full * rem$parameters$drug_13_GetOx_GetMV_effect_size)
ICrit_D_avert_inc2_to_full_incr_hc_drug <- sum(num_ICrit_extra_from_inc2 * prob_ICrit_death_inc_2 * rem$parameters$drug_13_GetOx_NoMV_effect_size) - sum(num_ICrit_extra_from_inc2 * prob_ICrit_death_full * rem$parameters$drug_13_GetOx_GetMV_effect_size)
ICrit_D_avert_inc1_to_full_incr_hc_drug <- sum(num_ICrit_extra_from_inc1 * prob_ICrit_death_inc_1 * rem$parameters$drug_13_NoOx_NoMV_effect_size) - sum(num_ICrit_extra_from_inc1 * prob_ICrit_death_full * rem$parameters$drug_13_GetOx_GetMV_effect_size)
ICrit_D_avert_incr_hc_drug <- ICrit_D_avert_none_to_full_incr_hc_drug + ICrit_D_avert_inc2_to_full_incr_hc_drug + ICrit_D_avert_inc1_to_full_incr_hc_drug - ICrit_D_avert_incr_hc

# 2b) Deaths averted due to set of people receiving the drug irrespective of reduced strain on healthcare
ICrit_D_no_drug <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) * prob_ICrit_death_full) +
                   sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) * prob_ICrit_death_inc_1) +
                   sum(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) * prob_ICrit_death_inc_2)
ICrit_D_drug <- sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_GetMV], 2, sum) * prob_ICrit_death_full * rem$parameters$drug_13_GetOx_GetMV_effect_size) +
                sum(apply(none$output[, index$number_ICrit_GetICU_GetOx_NoMV], 2, sum) * prob_ICrit_death_inc_1 * rem$parameters$drug_13_GetOx_NoMV_effect_size) +
                sum(apply(none$output[, index$number_ICrit_GetICU_NoOx_NoMV], 2, sum) * prob_ICrit_death_inc_2 * rem$parameters$drug_13_NoOx_NoMV_effect_size)
ICrit_D_avert_drug <- ICrit_D_no_drug - ICrit_D_drug

# Comparing
a <- (IMod_D_avert_incr_hc + IMod_D_avert_incr_hc_drug + IMod_D_avert_drug)
b <- (ISev_D_avert_incr_hc + ISev_D_avert_incr_hc_drug + ISev_D_avert_drug)
c <- (ICrit_D_avert_incr_hc + ICrit_D_avert_incr_hc_drug + ICrit_D_avert_drug)
total_deaths_averted - (a + b + c)
