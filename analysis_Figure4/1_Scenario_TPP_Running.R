# Loading Required Libraries
library(tidyverse); library(patchwork)

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
actual_MV_capacity <- round(actual_ICU_beds * 0.5)
time <- 600

none <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                       time_period = time, seeding_cases = 20, day_return = TRUE,
                       hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                       prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                       MV_capacity = actual_MV_capacity)

red_sev <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = time, seeding_cases = 20, day_return = TRUE,
                          drug_3_indic = 1, drug_3_prop_treat = 1, drug_3_effect_size = 0.5,
                          hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                          prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                          MV_capacity = actual_MV_capacity)

red_dur <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = time, seeding_cases = 20, day_return = TRUE,
                          drug_4_indic = 1, drug_4_prop_treat = 1, drug_4_effect_size = 2,
                          hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                          prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                          MV_capacity = actual_MV_capacity)

red_inf <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                          time_period = time, seeding_cases = 20, day_return = TRUE,
                          drug_5_indic_IMild = 1, drug_5_indic_ICase = 1,  drug_5_prop_treat = 1, drug_5_effect_size = 0.5,
                          hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                          prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                          MV_capacity = actual_MV_capacity)

red_dur_hosp <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                               time_period = time, seeding_cases = 20, day_return = TRUE,
                               hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                               prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                               MV_capacity = actual_MV_capacity,
                               drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_prop_treat = 1, drug_8_GetOx_effect_size = 1.45,
                               drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.80)


# Deaths, Healthcare Capacity & Number Appropriately Treated
index <- apothecary::odin_index(none$model)

D_none <- sum(diff(apply(none$output[, index$D], 1, sum)))
D_sev <- sum(diff(apply(red_sev$output[, index$D], 1, sum)))
D_dur <- sum(diff(apply(red_dur$output[, index$D], 1, sum)))
D_inf <- sum(diff(apply(red_inf$output[, index$D], 1, sum)))
D_dur_hosp <- sum(diff(apply(red_dur_hosp$output[, index$D], 1, sum)))

n_req_hosp_none <- sum(none$output[, index$number_req_hosp_bed])
n_req_hosp_sev <- sum(red_sev$output[, index$number_req_hosp_bed])
n_req_hosp_dur <- sum(red_dur$output[, index$number_req_hosp_bed])
n_req_hosp_inf <- sum(red_inf$output[, index$number_req_hosp_bed])
n_req_hosp_dur_hosp <- sum(red_dur_hosp$output[, index$number_req_hosp_bed])

n_hosp_ft_none <- sum(none$output[, index$number_get_hosp_full_treat])
n_hosp_ft_sev <- sum(red_sev$output[, index$number_get_hosp_full_treat])
n_hosp_ft_dur <- sum(red_dur$output[, index$number_get_hosp_full_treat])
n_hosp_ft_inf <- sum(red_inf$output[, index$number_get_hosp_full_treat])
n_hosp_ft_dur_hosp <- sum(red_dur_hosp$output[, index$number_get_hosp_full_treat])

n_hosp_it_none <- sum(none$output[, index$number_get_hosp_incomplete_treat])
n_hosp_it_sev <- sum(red_sev$output[, index$number_get_hosp_incomplete_treat])
n_hosp_it_dur <- sum(red_dur$output[, index$number_get_hosp_incomplete_treat])
n_hosp_it_inf <- sum(red_inf$output[, index$number_get_hosp_incomplete_treat])
n_hosp_it_dur_hosp <- sum(red_dur_hosp$output[, index$number_get_hosp_incomplete_treat])

n_req_ICU_none <- sum(none$output[, index$number_req_ICU_bed])
n_req_ICU_sev <- sum(red_sev$output[, index$number_req_ICU_bed])
n_req_ICU_dur <- sum(red_dur$output[, index$number_req_ICU_bed])
n_req_ICU_inf <- sum(red_inf$output[, index$number_req_ICU_bed])
n_req_ICU_dur_hosp <- sum(red_dur_hosp$output[, index$number_req_ICU_bed])

n_ICU_ft_none <- sum(none$output[, index$number_get_ICU_full_treat])
n_ICU_ft_sev <- sum(red_sev$output[, index$number_get_ICU_full_treat])
n_ICU_ft_dur <- sum(red_dur$output[, index$number_get_ICU_full_treat])
n_ICU_ft_inf <- sum(red_inf$output[, index$number_get_ICU_full_treat])
n_ICU_ft_dur_hosp <- sum(red_dur_hosp$output[, index$number_get_ICU_full_treat])

n_ICU_it_none <- sum(none$output[, index$number_get_ICU_incomplete_treat])
n_ICU_it_sev <- sum(red_sev$output[, index$number_get_ICU_incomplete_treat])
n_ICU_it_dur <- sum(red_dur$output[, index$number_get_ICU_incomplete_treat])
n_ICU_it_inf <- sum(red_inf$output[, index$number_get_ICU_incomplete_treat])
n_ICU_it_dur_hosp <- sum(red_dur_hosp$output[, index$number_get_ICU_incomplete_treat])

max_occ_none <- max(apply(none$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum))
max_occ_sev <- max(apply(red_sev$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum))
max_occ_dur <- max(apply(red_dur$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum))
max_occ_inf <- max(apply(red_inf$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum))
max_occ_dur_hosp <- max(apply(red_dur_hosp$output[, c(index$overall_hosp_occ, index$overall_ICU_occ)], 1, sum))

x <- data.frame(scenario = c("none", "red_sev", "red_dur", "red_inf", "red_dur\nhosp"),
                deaths = c(D_none, D_sev, D_dur, D_inf, D_dur_hosp),
                req_hosp = c(n_req_hosp_none, n_req_hosp_sev, n_req_hosp_dur, n_req_hosp_inf, n_req_hosp_dur_hosp),
                ft_hosp = c(n_hosp_ft_none, n_hosp_ft_sev, n_hosp_ft_dur, n_hosp_ft_inf, n_hosp_ft_dur_hosp),
                it_hosp = c(n_hosp_it_none, n_hosp_it_sev, n_hosp_it_dur, n_hosp_it_inf, n_hosp_it_dur_hosp),
                req_ICU = c(n_req_ICU_none, n_req_ICU_sev, n_req_ICU_dur, n_req_ICU_inf, n_req_ICU_dur_hosp),
                ft_ICU = c(n_ICU_ft_none, n_ICU_ft_sev, n_ICU_ft_dur, n_ICU_ft_inf, n_ICU_ft_dur_hosp),
                it_ICU = c(n_ICU_it_none, n_ICU_it_sev, n_ICU_it_dur, n_ICU_it_inf, n_ICU_it_dur_hosp),
                max_occ = c(max_occ_none, max_occ_sev, max_occ_dur, max_occ_inf, max_occ_dur_hosp))

a <- ggplot(x, aes(x = scenario, y = deaths, fill = scenario)) +
        geom_bar(stat = "identity") +
        labs(y = "Number of Deaths", x = "") +
        scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
        theme(axis.text.x = element_text(size = 8))
b <- ggplot(x, aes(x = scenario, y = req_hosp, fill = scenario)) +
        geom_bar(stat = "identity") +
        labs(y = "Number Requiring Hospital Bed", x = "") +
        scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
  theme(axis.text.x = element_text(size = 8))
c <- ggplot(x, aes(x = scenario, y = req_ICU, fill = scenario)) +
        geom_bar(stat = "identity") +
        labs(y = "Number Requiring ICU Bed", x = "") +
        scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
  theme(axis.text.x = element_text(size = 8))
d <- ggplot(x, aes(x = scenario, y = ft_hosp/req_hosp, fill = scenario)) +
        geom_bar(stat = "identity") +
        labs(y = "Proportion Hospital Full Treatment", x = "") +
        scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
  theme(axis.text.x = element_text(size = 8))
e <- ggplot(x, aes(x = scenario, y = ft_ICU/req_ICU, fill = scenario)) +
        geom_bar(stat = "identity") +
        labs(y = "Proportion ICU Full Treatment", x = "") +
        scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
  theme(axis.text.x = element_text(size = 8))
f <- ggplot(x, aes(x = scenario, y = max_occ, fill = scenario)) +
  geom_bar(stat = "identity") +
  labs(y = "Max Hospital Occupancy", x = "") +
  scale_fill_manual(values = c("#011627", "#83BCA9", "#D36135", "#B91372", "#9F95E3")) +
  theme(axis.text.x = element_text(size = 8))

a + b + c + d + e + f +
  plot_layout(guides = "collect")


n_ICU_ft_none/n_req_ICU_none
n_ICU_ft_sev/n_req_ICU_sev
n_ICU_ft_dur/n_req_ICU_dur
n_ICU_ft_inf/n_req_ICU_inf

max_hosp_occ_none <- max(none$output[, index$overall_hosp_occ])
max_hosp_occ_sev <- max(red_sev$output[, index$overall_hosp_occ])
max_hosp_occ_dur <- max(red_dur$output[, index$overall_hosp_occ])
max_hosp_occ_inf <- max(red_inf$output[, index$overall_hosp_occ])




hosp_ft_none <- none$output[, index$hosp_bed_full_treat_occ]
hosp_ft_sev <- red_sev$output[, index$hosp_bed_full_treat_occ]
hosp_ft_dur <- red_dur$output[, index$hosp_bed_full_treat_occ]
hosp_ft_inf <- red_inf$output[, index$hosp_bed_full_treat_occ]

plot(hosp_ft_none, type = "l", col = "#011627", lwd = 2)
lines(hosp_ft_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(hosp_ft_dur, type = "l", col = "#D36135", lwd = 2)
lines(hosp_ft_inf, type = "l", col = "#B91372", lwd = 2)

hosp_it_none <- none$output[, index$hosp_bed_incomplete_treat_occ]
hosp_it_sev <- red_sev$output[, index$hosp_bed_incomplete_treat_occ]
hosp_it_dur <- red_dur$output[, index$hosp_bed_incomplete_treat_occ]
hosp_it_inf <- red_inf$output[, index$hosp_bed_incomplete_treat_occ]

plot(hosp_it_none, type = "l", col = "#011627", lwd = 2)
lines(hosp_it_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(hosp_it_dur, type = "l", col = "#D36135", lwd = 2)
lines(hosp_it_inf, type = "l", col = "#B91372", lwd = 2)



output <- data.frame(scenario = c(rep("none", time + 1), rep("red_sev_pre_hosp", time + 1),
                                  rep("red_dur_mild_dis", time + 1), rep("red_inf", time + 1)),
                     deaths = c(D_none, D_sev, D_dur, D_inf))


total <- data.frame(scenario = c("none", "red_sev", "red_dur", "red_inf"),
                    deaths = c(sum(D_none), sum(D_sev), sum(D_dur), sum(D_inf)))


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

# overall_hosp_occ
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

# ICU occupancy
par(mfrow = c(1, 1))
ICU_none <- none$output[, index$overall_ICU_occ]
ICU_sev <- red_sev$output[, index$overall_ICU_occ]
ICU_dur <- red_dur$output[, index$overall_ICU_occ]
ICU_inf <- red_inf$output[, index$overall_ICU_occ]

plot(ICU_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(ICU_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(ICU_dur, type = "l", col = "#D36135", lwd = 2)
lines(ICU_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "ICU Occupancy", line = 4, cex.lab = 1.2)

ICU_none <- none$output[, index$ICU_bed_full_treat_occ]
ICU_sev <- red_sev$output[, index$ICU_bed_full_treat_occ]
ICU_dur <- red_dur$output[, index$ICU_bed_full_treat_occ]
ICU_inf <- red_inf$output[, index$ICU_bed_full_treat_occ]

plot(ICU_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(ICU_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(ICU_dur, type = "l", col = "#D36135", lwd = 2)
lines(ICU_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "ICU Occupancy", line = 4, cex.lab = 1.2)

ICU_none <- none$output[, index$ICU_bed_incomplete_treat_occ]
ICU_sev <- red_sev$output[, index$ICU_bed_incomplete_treat_occ]
ICU_dur <- red_dur$output[, index$ICU_bed_incomplete_treat_occ]
ICU_inf <- red_inf$output[, index$ICU_bed_incomplete_treat_occ]

plot(ICU_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(ICU_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(ICU_dur, type = "l", col = "#D36135", lwd = 2)
lines(ICU_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "ICU Occupancy", line = 4, cex.lab = 1.2)

ICU_none <- none$output[, index$ICU_bed_no_treat_occ]
ICU_sev <- red_sev$output[, index$ICU_bed_no_treat_occ]
ICU_dur <- red_dur$output[, index$ICU_bed_no_treat_occ]
ICU_inf <- red_inf$output[, index$ICU_bed_no_treat_occ]

plot(ICU_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(ICU_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(ICU_dur, type = "l", col = "#D36135", lwd = 2)
lines(ICU_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "ICU Occupancy", line = 4, cex.lab = 1.2)


ICU_none <- none$output[, index$ICU_bed_no_treat_occ]
ICU_sev <- red_sev$output[, index$ICU_bed_no_treat_occ]
ICU_dur <- red_dur$output[, index$ICU_bed_no_treat_occ]
ICU_inf <- red_inf$output[, index$ICU_bed_no_treat_occ]

plot(ICU_none, type = "l", col = "#011627", lwd = 2, ylab = "", las = 1, xlab = "Days Since Seeding")
lines(ICU_sev, type = "l", col = "#83BCA9", lwd = 2)
lines(ICU_dur, type = "l", col = "#D36135", lwd = 2)
lines(ICU_inf, type = "l", col = "#B91372", lwd = 2)
legend(350, 140000, legend = c("No Drugs", "Red. Hosp. 50%", "Red Dur Mild Inf 50%", "Red Infectivity 50%"),
       col = c("#011627", "#83BCA9", "#D36135", "#B91372"), lty = 1, cex = 0.7, lwd = 5)
title(ylab = "ICU Occupancy", line = 4, cex.lab = 1.2)


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
