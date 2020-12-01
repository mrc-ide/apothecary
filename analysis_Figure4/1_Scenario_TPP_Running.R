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

none <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = 600, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                       prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000)

red_sev <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = 600, seeding_cases = 20, day_return = TRUE,
                          hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                          prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                          drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = 0.5)

red_dur <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = 600, seeding_cases = 20, day_return = TRUE,
                          hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                          prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                          drug_4_indic = 1, drug_4_prop_treat = 1, drug_4_effect_size = 2)

red_inf <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = 600, seeding_cases = 20, day_return = TRUE,
                          hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                          prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000,
                          drug_5_indic_IMild = 1, drug_5_indic_ICase = 1,  drug_5_prop_treat = 1, drug_5_effect_size = 0.5)

index <- apothecary::odin_index(none$model)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))

D_none <- c(0, diff(apply(none$output[, index$D], 1, sum)))
D_sev <- c(0, diff(apply(red_sev$output[, index$D], 1, sum)))
D_dur <- c(0, diff(apply(red_dur$output[, index$D], 1, sum)))
D_inf <- c(0, diff(apply(red_inf$output[, index$D], 1, sum)))

plot(D_none, type = "l", col = "#011627", lwd = 2, ylab = "Daily Deaths", las = 1, xlab = "Days Since Seeding")
lines(D_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(D_dur, type = "l", col = "#D36135", lwd = 2)
lines(D_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 3300, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)

hosp_none <- apply(none$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum)
hosp_sev <- apply(red_sev$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum)
hosp_dur <- apply(red_dur$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum)
hosp_inf <- apply(red_inf$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum)

plot(hosp_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(hosp_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(hosp_dur, type = "l", col = "#D36135", lwd = 2)
lines(hosp_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "Hospital Occupancy", line = 4, cex.lab = 1.2)


sum(D_none)
sum(D_sev)
sum(D_dur)
sum(D_inf)




# Property 3 - reduces the severity of disease once symptom onset occurs (preferential movement to IMild over ICase - proportion going to IAsymp unaffected)
# drug_3_indic <- user() # binary indicator (0 or 1) specifying whether drug property 3 is activated or not
# drug_3_prop_treat <- user() # the proportion of individuals at the E2 -> I transition receiving the drug
# drug_3_effect_size <- user() # the multiple of the baseline age-specific probability of ICase disease severity that occurs upon infection, protected individuals flow to IMild instead (1 = the same, <1 = protection from severe disease)
# Drug 4 reduces the time which individuals spend in IMild, i.e. they recover more quickly
# drug_4_indic <- user() # binary indicator (0 or 1) specifying whether drug property 4 is activated or not
# drug_4_prop_treat <- user() # proportion of individuals in IMild who receive the drug
# drug_4_effect_size <- user() # the multiple by which the rate of recovery for individuals in IMild has increased by (1 = the same, >1 = increased rate)
# Drug 5 reduces infectivity of individuals in IMild/ICase, reducing the FOI experienced by Susceptible individuals
# drug_5_indic_IMild <- user() # binary indicator (0 or 1) specifying whether drug property 5 is activated or not for IMild individuals
# drug_5_indic_ICase <- user() # binary indicator (0 or 1) specifying whether drug property 5 is activated or not for ICase individuals
# drug_5_prop_treat <- user() # proportion of individuals in IMild or ICase (depending on which indics are activated) who receive the drug
# drug_5_effect_size <- user() # the multiple by which treated individuals are as infectious compared to untreated individuals (1 = the same, <1 = individuals less infectious)


index <- apothecary::odin_index(temp$model)

R <- apply(temp$output[, index$R], 2, max)
D <- apply(temp$output[, index$D], 2, max)

(R + D)/standard_population
