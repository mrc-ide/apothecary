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
time <- 200

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
                      drug_6_indic = 1, drug_6_prop_treat = 1, drug_6_effect_size = 0.75)
index <- squire:::odin_index(rem$model)

# Differences in Deaths, ICU Admissions & Hospital Admissions
sum(apply(none$output[, index$D], 2, max) - apply(rem$output[, index$D], 2, max))
sum(none$output[, index$number_req_hosp_bed]) - sum(rem$output[, index$number_req_hosp_bed])
sum(none$output[, index$number_need_ICU]) - sum(rem$output[, index$number_need_ICU])


sum(none$output[, index$number_get_ICU_full_treat]) - sum(rem$output[, index$number_get_ICU_full_treat])
sum(none$output[, index$number_get_ICU_any_treat]) - sum(rem$output[, index$number_get_ICU_any_treat])

sum(none$output[, index$number_get_hosp_full_treat]) - sum(rem$output[, index$number_get_hosp_full_treat])
sum(none$output[, index$number_get_hosp_any_treat]) - sum(rem$output[, index$number_get_hosp_any_treat])



sum(none$output[, index$number_get_ICU_incomplete_treat])
sum(rem$output[, index$number_get_ICU_incomplete_treat])


none$output[, index$total_GetICU_GetOx_initial]/rem$output[, index$total_GetICU_GetOx_initial]

none$output[, index$total_GetICU_GetOx_to_IMod]
rem$output[, index$total_GetICU_GetOx_to_IMod]



rem$output[, index$total_GetICU_NoOx_to_IMod]

rem$output[, index$total_GetICU_NoOx_initial] # 1


plot(rem$output[, index$total_GetICU_GetOx_to_IMod])

plot(rem$output[, index$pop])


apply(rem$output[, index$number_GetICU_GetOx], 1, sum)
apply(rem$output[, index$number_GetICU_GetOx_to_IMod], 1, sum) >= 0

rem$output[, index$total_req_ICU] # 99

rem$output[, index$total_req_ICU] # 99
rem$output[, index$total_GetICU_GetOx_initial] # 99
rem$output[, index$total_GetICU_GetOx_to_IMod] # 99

rem$output[1, index$number_req_ICU]/sum(rem$output[1, index$number_req_ICU])

rem$output[, index$number_GetICU_GetOx_to_IMod]

total_GetICU_GetOx_initial <- if(current_free_ICU_bed_ox <= 0) 0 else(if(current_free_ICU_bed_ox - total_req_ICU >= 0) total_req_ICU else(current_free_ICU_bed_ox)) # Working out the number of new ICU requiring infections that get a bed
total_GetICU_GetOx_to_IMod <- if (drug_6_indic == 1) total_GetICU_GetOx_initial * (drug_6_prop_treat * drug_6_effect_size) else 0
number_GetICU_GetOx_to_IMod[] <- 0#if (total_req_ICU == 0) 0 else number_req_ICU[i]/sum(number_req_ICU) * total_GetICU_GetOx_to_IMod # note this only works because drug applied equally across ages. Otherwise, would need to do a different fraction for


rem$output[, index$total_GetICU_GetOx] # 99
rem$output[, index$total_GetICU_GetOx_to_IMod] # 99
rem$output[, index$current_free_ICU_bed_ox] # 99
rem$output[, index$current_free_ICU_bed_no_ox] # 1
rem$output[, index$number_GetICU_GetOx]

rem$output[, index$n_ISev_GetICU_NoOx_Surv2_Rec] # 1
rem$output[, index$n_ISev_GetICU_NoOx_Die2_D_Hospital] # 1
rem$output[, index$n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec] # 1
rem$output[, index$n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital] # 1
rem$output[, index$current_free_ICU_bed_no_ox] # 1

rem$output[, index$total_GetICU_NoOx_initial] # 1

rem$output[, index$total_GetICU_NoOx_initial][1]

rem$output[, index$current_free_ICU_bed_no_ox][1] - (rem$output[, index$total_req_ICU][1] - rem$output[, index$total_GetICU_GetOx][1])
rem$output[, index$total_GetICU_NoOx_to_IMod] # 1
rem$output[, index$number_GetICU_NoOx_to_IMod]

rem$output[, index$total_GetICU_NoOx]
rem$output[, index$number_GetICU_NoOx]

output(total_GetICU_NoOx) <- TRUE
output(number_GetICU_NoOx) <- TRUE

colnames(rem$output)[which(is.na(unname(rem$output[2, ])))]


current_free_ICU_bed_no_ox - (total_req_ICU - total_GetICU_GetOx)



