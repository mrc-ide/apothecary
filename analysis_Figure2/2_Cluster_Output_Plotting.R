# Loading required libraries
library(apothecary); library(tidyverse); library(facetscales); library(scales)

# Functions for Data Preparation and Manipulation
wd <- getwd()
source(paste0(wd, "/analysis_Figure2/Functions/Figure_2_Functions.R"))

# Loading in required files and creating relevant dataframes
filenames <- list.files("analysis_Figure2/Outputs/")

drug_filenames <- filenames[!grepl("notreat", filenames)]
for (i in 1:length(drug_filenames)) {
  if (i == 1) {
    drug_temp <- readRDS(paste0("analysis_Figure2/Outputs/", drug_filenames[i]))
  } else {
    drug_temp <- rbind(drug_temp, readRDS(paste0("analysis_Figure2/Outputs/", drug_filenames[i])))
  }
}
drugs <- drug_temp %>%
  separate(scenario, c("number", "drug","R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  unite("drug_benefit", drug_benefit:drug_benefit2, remove = TRUE) %>%
  mutate(healthcare = factor(healthcare, levels = c("unlimHC", "limMV", "limMVox", "limMVoxbeds", "noHC"))) %>%
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull")))

no_drug_filenames <- filenames[grepl("notreat", filenames)]
for (i in 1:length(no_drug_filenames)) {
  if (i == 1) {
    no_drug_temp <- readRDS(paste0("analysis_Figure2/Outputs/", no_drug_filenames[i]))
  } else {
    no_drug_temp <- rbind(no_drug_temp, readRDS(paste0("analysis_Figure2/Outputs/", no_drug_filenames[i])))
  }
}
no_drugs <- no_drug_temp %>%
  separate(scenario, c("number", "drug","R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  select(IFR, R0, healthcare) %>%
  group_by(R0, healthcare) %>%
  summarise(no_drugs_IFR = mean(IFR))

no_hc_filenames <- filenames[grepl("noHC", filenames)]
for (i in 1:length(no_drug_filenames)) {
  if (i == 1) {
    no_hc_temp <- readRDS(paste0("analysis_Figure2/Outputs/", no_hc_filenames[i]))
  } else {
    no_hc_temp <- rbind(no_hc_temp, readRDS(paste0("analysis_Figure2/Outputs/", no_hc_filenames[i])))
  }
}
no_hc <- no_hc_temp %>%
  separate(scenario, c("number", "drug","R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  select(IFR, R0, healthcare) %>%
  group_by(R0, healthcare) %>%
  summarise(no_hc_IFR = mean(IFR)) %>%
  select(R0, no_hc_IFR)

# Merging Drug and No Drug Dataframes Together and Plotting the IFR
overall <- drugs %>%
  left_join(no_drugs, by = c("R0", "healthcare")) %>%
  left_join(no_hc, by = c("R0")) %>%
  mutate(IFR_diff = no_drugs_IFR - IFR) %>%
  mutate(prop_IFR_red = IFR_diff/no_drugs_IFR) %>%
  mutate(healthcare = factor(healthcare, levels = c("unlimHC", "limMV", "limMVox", "limMVoxbeds", "noHC"))) %>%
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull"))) %>%
  filter(healthcare != "noHC")

# New facet labels for R0 variables
R0.labs <- c("High R0", "Low R0")
names(R0.labs) <- c("highR0", "lowR0")

# New facet label names for Healthcare Scenario variables
healthcare.labs <- c("Impact Only In Treated Patients",
                     "Impact In All Hospitalised Patients - Pess.",
                     "Impact In All Hospitalised Patients - Opti.",
                     "Impact In All Hospitalised Patients - Full")
names(healthcare.labs) <- c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull")

ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(aes(x = healthcare, y = no_drugs_IFR, col = interaction(R0, healthcare)), shape = 19) +
  geom_hline(aes(yintercept = no_hc_IFR), no_hc, linetype = "dashed") +
  facet_grid(R0 ~ drug_benefit,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_fill_manual(values = c("white", "white", "#B7C0EE", "#DBF0CE", "#7067CF",
                               "#82CA59", "#541894", "#549418", "purple", "pink"),
                    name = "fill") +
  scale_x_discrete(labels = c("Unlimited\nHealthcare",
                              "Limited\nARS",
                              "Limited\nARS\n& O2",
                              "Limited\nARS, O2\n& Beds",
                              "No\nHealthcare")) +
  scale_colour_manual(values = rep("black", 10)) +
  theme(legend.position = "none", axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(vjust = +2.5),
        strip.text.x = element_text(size = 8)) +
  labs(x = "", y = "Infection Fatality Ratio (%)") +
  lims(y = c(0, 0.8))

ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(aes(x = healthcare, y = no_drugs_IFR, col = interaction(R0, healthcare)), shape = 19) +
  facet_grid(drug_benefit ~ R0,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_y_continuous(position = "right", limits = c(0, 2.6), breaks = c(0, 1, 2)) +
  scale_fill_manual(values = c("white", "white", "#B7C0EE", "#DBF0CE", "#7067CF",
                               "#82CA59", "#541894", "#549418", "purple", "pink"), name = "fill") +
  scale_x_discrete(labels = c("Unlimited\nHealthcare", "Limited\nARS", "Limited\nARS\n& O2", "Limited\nARS, O2\n& Beds", "No\nHealthcare")) +
  scale_colour_manual(values = rep("black", 10)) +
  theme(legend.position = "none", axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 10),
        axis.title.y.right = element_text(vjust = +3.5), strip.background.y = element_blank(), strip.text.y = element_blank()) +
  labs(x = "", y = "Infection Fatality Ratio (%)")



summary <- overall %>%
  group_by(R0, healthcare, drug_benefit) %>%
  summarise(med_IFR = median(IFR),
            mean_IFR = mean(IFR))


# Plotting Drug Effects
num_draws <- 500
drug_effs <- generate_drug_effect_draws(num_draws = num_draws)
dexy_mod_mort <- drug_effs$dexy_mod_mort
dexy_ICU_mort <- drug_effs$dexy_ICU_mort
rem_mod_mort <- drug_effs$rem_mod_mort
rem_mod_dur <- drug_effs$rem_mod_dur
rem_drug_eff <- c(rep("rem_mod_getox_dur", num_draws), rep("rem_mod_noox_dur", num_draws),
                  rep("rem_mod_getox_mort", num_draws), rep("rem_mod_noox_mort", num_draws))
dexy_drug_eff <- c(rep("dexy_mod_getox_mort", num_draws), rep("dexy_mod_noox_mort", num_draws),
                   rep("dexy_sev_getox_mort", num_draws), rep("dexy_sev_noox_mort", num_draws),
                   rep("dexy_crit_getox_getmv_mort", num_draws), rep("dexy_crit_getox_nomv_mort", num_draws), rep("dexy_crit_noox_nomv_mort", num_draws))
x <- data.frame(drug = c(rep("Remdesivir", 4 * length(rem_drug_eff)), rep("Dexamethasone", 4 * length(dexy_drug_eff))),
                eff = c(rep(rem_drug_eff, 4), rep(dexy_drug_eff, 4)),
                eff_2 = c(rep(rep(c("dur", "dur", "mort", "mort"), each = num_draws), 4), rep("mort", 4 * length(dexy_drug_eff))),
                scen = c(rep(c("t_bf", "ah_bc", "ah_bo", "ah_bf"), each = length(rem_drug_eff)),
                         rep(c("t_bf", "ah_bc", "ah_bo", "ah_bf"), each = length(dexy_drug_eff))),
                RR = c(
                  c(rem_mod_dur, rep(1, num_draws), rem_mod_mort, rep(1, num_draws)),
                  c(rem_mod_dur, 1 + 0.3333 * (rem_mod_dur - 1), rem_mod_mort, rem_mod_mort + 0.6666 * (1 - rem_mod_mort)),
                  c(rem_mod_dur, 1 + 0.6666 * (rem_mod_dur - 1), rem_mod_mort, rem_mod_mort + 0.3333 * (1 - rem_mod_mort)),
                  c(rem_mod_dur, rem_mod_dur, rem_mod_mort, rem_mod_mort),
                  c(dexy_mod_mort, rep(1, num_draws), dexy_ICU_mort, rep(1, num_draws), dexy_ICU_mort, rep(1, num_draws), rep(1, num_draws)),
                  c(dexy_mod_mort, dexy_mod_mort + 0.5 * (1 - dexy_mod_mort), dexy_ICU_mort, dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort),
                    dexy_ICU_mort, rep(1, num_draws), rep(1, num_draws)),
                  c(dexy_mod_mort, dexy_mod_mort + 0.25 * (1 - dexy_mod_mort), dexy_ICU_mort, dexy_ICU_mort + 0.25 * (1 - dexy_ICU_mort),
                    dexy_ICU_mort, dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort), dexy_ICU_mort + 0.5 * (1 - dexy_ICU_mort)),
                  c(dexy_mod_mort, dexy_mod_mort, dexy_ICU_mort, dexy_ICU_mort,
                    dexy_ICU_mort, dexy_ICU_mort, dexy_ICU_mort)))
x <- x %>%
  filter(eff_2 == "mort") %>%
  mutate(scen = factor(scen, levels = c("t_bf", "ah_bc", "ah_bo", "ah_bf")))

ggplot(x) +
  geom_bar(aes(x = eff, y = RR, fill = scen, group = interaction(eff, scen)),
           stat = "summary", fun.y = "mean.se", position = "dodge") +
  scale_y_continuous(limits = c(0, 1.05), breaks = c(0, 0.5, 1)) +
  facet_grid(scen~.)
  #            ,
  #            labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  # scale_fill_manual(values = c("white", "white", "#B7C0EE", "#DBF0CE", "#7067CF",
  #                              "#82CA59", "#541894", "#549418", "purple", "pink"), name = "fill") +
  # scale_x_discrete(labels = c("Unlimited\nHealthcare", "Limited\nARS", "Limited\nARS\n& O2", "Limited\nARS, O2\n& Beds", "No\nHealthcare")) +
  # scale_colour_manual(values = rep("black", 10)) +
  # theme(legend.position = "none", axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 10),
  #       axis.title.y.right = element_text(vjust = +3.5), strip.background.y = element_blank(), strip.text.y = element_blank()) +
  # labs(x = "", y = "Infection Fatality Ratio (%)")
  #

# Generating Epidemic Curves for Each of the High/Low R0 Scenarios
country <- "Grenada"
standard_population <- round(rep(50000000/17, 17))
standard_matrix <- matrix(1, 16, 16)
standard_matrix[,16] <- 2
demog_pars_highR0 <- list(R0 = 2, country = country, population = standard_population,
                           matrix = standard_matrix, time_period = 150, seeding_cases = 1000)
demog_pars_lowR0 <- list(R0 = 1.3, country = country, population = standard_population,
                          matrix = standard_matrix, time_period = 300, seeding_cases = 1000)

lowR0 <- run_apothecary(country = demog_pars_lowR0$country, R0 = demog_pars_lowR0$R0,
                        population = demog_pars_lowR0$population, contact_matrix_set = demog_pars_lowR0$matrix,
                        time_period = 365, seeding_cases = demog_pars_lowR0$seeding_cases,
                        day_return = TRUE,
                        hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                        prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000)
highR0 <- run_apothecary(country = demog_pars_highR0$country, R0 = demog_pars_highR0$R0,
                         population = demog_pars_highR0$population, contact_matrix_set = demog_pars_highR0$matrix,
                         time_period = 365, seeding_cases = demog_pars_highR0$seeding_cases,
                         day_return = TRUE,
                         hosp_bed_capacity = 10000000000, ICU_bed_capacity = 10000000000,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000)
index <- apothecary::odin_index(lowR0$model)

hosp_capacity <- 130000
ICU_capacity <- 20000
num_inter <- 4000
hosp_occ <- data.frame(t = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$x,
                       hosp_occ = c(approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$y, approx(highR0$output[, index$overall_hosp_occ], n = num_inter)$y),
                       scenario = c(rep("low", num_inter), rep("high", num_inter)))
ggplot() +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = interaction(scenario, ifelse(hosp_occ > hosp_capacity, NA, TRUE)))) +
  scale_fill_manual(values = alpha(c("#3FA7D6", "#F79D84"), 0.5), name = "fill") +
  geom_line(data = hosp_occ, aes(x = t, y = hosp_occ, colour = scenario, linetype = ifelse(hosp_occ > hosp_capacity, "solid", "dashed")), size = 1) +
  scale_colour_manual(values = c("#3FA7D6", "#F79D84")) +
  geom_hline(yintercept = hosp_capacity, col = "#3FA7D6", linetype = "dashed", size = 1) +
  theme(legend.position = "none")

ICU_occ <- data.frame(t = approx(lowR0$output[, index$overall_ICU_occ], n = num_inter)$x,
                      hosp_occ = c(approx(lowR0$output[, index$overall_ICU_occ], n = num_inter)$y, approx(highR0$output[, index$overall_ICU_occ], n = num_inter)$y),
                      scenario = c(rep("low", num_inter), rep("high", num_inter)))
ggplot() +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = interaction(scenario, ifelse(hosp_occ > ICU_capacity, NA, TRUE)))) +
  theme(legend.position = "none") +
  scale_fill_manual(values = alpha(c("#3FA7D6", "#F79D84"), 0.5), name = "fill") +
  geom_line(data = ICU_occ, aes(x = t, y = hosp_occ, colour = scenario, linetype = ifelse(hosp_occ > ICU_capacity, "solid", "dashed")), size = 1) +
  scale_colour_manual(values = c("#3FA7D6", "#F79D84")) +
  geom_hline(yintercept = ICU_capacity, col = "#3FA7D6", linetype = "dashed", size = 1) +
  theme(legend.position = "none")

lowR0 <- run_apothecary(country = demog_pars_lowR0$country, R0 = demog_pars_lowR0$R0,
                        population = demog_pars_lowR0$population, contact_matrix_set = demog_pars_lowR0$matrix,
                        time_period = 365, seeding_cases = demog_pars_lowR0$seeding_cases,
                        day_return = TRUE,
                        hosp_bed_capacity = hosp_capacity, ICU_bed_capacity = ICU_capacity,
                        prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000)
highR0 <- run_apothecary(country = demog_pars_highR0$country, R0 = demog_pars_highR0$R0,
                         population = demog_pars_highR0$population, contact_matrix_set = demog_pars_highR0$matrix,
                         time_period = 365, seeding_cases = demog_pars_highR0$seeding_cases,
                         day_return = TRUE,
                         hosp_bed_capacity = hosp_capacity, ICU_bed_capacity = ICU_capacity,
                         prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 10000000000)

low_R0_hosp <- sum(lowR0$output[, index$number_get_hosp_any_treat])/sum(lowR0$output[, index$number_req_hosp_bed])
low_R0_ICU <- sum(lowR0$output[, index$number_get_ICU_any_treat])/sum(lowR0$output[, index$number_req_ICU_bed])

high_R0_hosp <-  sum(highR0$output[, index$number_get_hosp_any_treat])/sum(highR0$output[, index$number_req_hosp_bed])
high_R0_ICU <- sum(highR0$output[, index$number_get_ICU_any_treat])/sum(highR0$output[, index$number_req_ICU_bed])

capacity <- data.frame(metric = c("hospital", "ICU", "hospital", "ICU"),
                       R0 = c("low", "low", "high", "high"),
                       value = c(low_R0_hosp, low_R0_ICU, high_R0_hosp, high_R0_ICU))
ggplot() +
  geom_bar(data = capacity, aes(x = R0, y = value, fill = R0), stat = "identity") +
  facet_grid(~metric)

hosp_occ <- data.frame(t = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$x,
                       check = c(approx(highR0$output[, index$overall_hosp_occ], n = num_inter)$y, approx(highR0$output[, index$overall_ICU_occ], n = num_inter)$y,
                                 approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$y, approx(lowR0$output[, index$overall_ICU_occ], n = num_inter)$y),
                       scenario = c(rep("Hospital", num_inter), rep("ICU", num_inter), rep("Hospital", num_inter), rep("ICU", num_inter)),
                       R0 = c(rep("High", num_inter * 2), rep("Low", num_inter * 2)))

x <- data.frame(scenario = c("Hospital", "ICU"), value = c(hosp_capacity, ICU_capacity))

ggplot() +
  geom_line(data = hosp_occ, aes(x = t, y = check, colour = interaction(R0, scenario),
                                 linetype = ifelse((scenario == "Hospital" & check > hosp_capacity) |
                                                     (scenario == "ICU" & check > ICU_capacity), "solid", "dashed")), size = 1) +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = check, colour = interaction(R0, scenario),
                                   fill = interaction(R0, ifelse((scenario == "Hospital" & check > hosp_capacity) |
                                                 (scenario == "ICU" & check > ICU_capacity), NA, TRUE)))) +
  theme(legend.position = "none") +
  scale_fill_manual(values = alpha(c("#F79D84", "#3FA7D6"), 0.5), name = "fill") +
  scale_colour_manual(values = c("#F79D84", "#3FA7D6", "#F79D84", "#3FA7D6")) +
  facet_wrap(~scenario, scales = "free_y") +
  labs(y = "Demand", x = "Time (Days)") +
  geom_hline(data = x, aes(yintercept = value), linetype = "dashed", size = 1)







dummy <- data.frame(check = c(0, 300000, 0, 50000, 0, 300000, 0, 50000),
                    R0 = c("High", "High", "High", "High", "Low", "Low", "Low", "Low"),
                    scenario = c("Hospital", "Hospital", "ICU", "ICU"),
                    t = rep(0, 8))

scales_y <- list(
  Hospital = scale_y_continuous(limits = c(0, 200000)),
  ICU = scale_y_continuous(limits = c(0, 50000))
)

ggplot() +
  geom_line(data = hosp_occ, aes(x = t, y = check, colour = interaction(R0, scenario),
                                 linetype = ifelse((scenario == "Hospital" & check > hosp_capacity) |
                                                   (scenario == "ICU" & check > ICU_capacity), "solid", "dashed")), size = 1) +
  facet_grid_sc(rows = vars(R0),
                cols = vars(scenario))
scales = list(y = scales_y))


mydf <- data.frame(
  Subject = rep(c("A", "B", "C", "D"), each = 3),
  Magnitude = rep(c("SomeValue", "Percent", "Scientific"), times = 4),
  Magnitude2 = rep(c("x", "y", "z"), times = 4),
  Value=c(c(170,0.6,2.7E-4),
          c(180, 0.8, 2.5E-4),
          c(160, 0.71, 3.2E-4),
          c(159, 0.62, 3E-4)))

scales_y <- list(
  Percent = scale_y_continuous(limits = c(0.5, 1)),
  SomeValue = scale_y_continuous(limits = c(170, 180)),
  Scientific = scale_y_continuous(limits = c(0, 0.001))
)

ggplot(mydf) +
  geom_point(aes(x=Subject, y=Value)) +
  facet_grid_sc(rows = vars(Magnitude),
                col = vars(Magnitude2),
                scales = list(y = scales_y))





cyl4 <- quantile(mtcars$hp[mtcars$cyl == 4], 0.9)
cyl6 <- quantile(mtcars$hp[mtcars$cyl == 6], 0.9)
cyl8 <- quantile(mtcars$hp[mtcars$cyl == 8], 0.9)

dummy <- data.frame(hp = c(0, cyl4, 0, cyl6, 0, cyl8), cyl = c(4, 4, 6, 6, 8, 8),
                    mpg = rep(0, 6))

ggplot(mtcars, aes(x=mpg, y=hp)) +
  geom_point() +
  facet_grid(cyl ~., scales = "free_y") +
  geom_blank(data = dummy)






ggplot() +
  geom_line(data = hosp_occ, aes(x = t, y = check, colour = scenario, linetype = ifelse(check > hosp_capacity, "solid", "dashed")), size = 1) +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = check,
                                   fill = interaction(scenario, ifelse(scenario == "Hospital", ifelse(check > hosp_capacity, NA, TRUE),
                                                                       ifelse(check > ICU_capacity, NA, TRUE))))) +
  scale_fill_manual(values = alpha(c("#F79D84", "#3FA7D6"), 0.5), name = "fill") +
  scale_colour_manual(values = c("#F79D84", "#3FA7D6")) +
  facet_wrap(.~scenario, scales = "free_y") +
  labs(y = "Demand", x = "Time (Days)") +
  theme(legend.position = "none") +
  geom_hline(data = x, aes(yintercept = value, col = scenario), linetype = "dashed", size = 1)






hosp_occ <- data.frame(t = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$x,
                       hosp_occ = c(approx(highR0$output[, index$overall_hosp_occ], n = num_inter)$y,
                                    approx(highR0$output[, index$overall_ICU_occ], n = num_inter)$y),
                       scenario = c(rep("hosp", num_inter), rep("ICU", num_inter)))

ggplot() +
  geom_line(data = hosp_occ, aes(x = t, y = check, colour = scenario, linetype = ifelse(check > hosp_capacity, "solid", "dashed")), size = 1) +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = check, fill = interaction(scenario, ifelse(check > hosp_capacity, NA, TRUE)))) +
  scale_fill_manual(values = alpha(c("#F79D84", "#3FA7D6"), 0.5), name = "fill") +
  scale_colour_manual(values = c("#F79D84", "#3FA7D6")) +
  facet_wrap(.~scenario, scales = "free_y") +
  theme(legend.position = "none") +
  geom_hline(data = x, aes(yintercept = value, col = scenario), linetype = "dashed", size = 1)

low_hosp_occ <- data.frame(t = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$x,
                           hosp_occ = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$y)
high_hosp_occ <- data.frame(t = approx(lowR0$output[, index$overall_hosp_occ], n = num_inter)$x,
                            hosp_occ = approx(highR0$output[, index$overall_hosp_occ], n = num_inter)$y)
ggplot() +
  geom_ribbon(data = high_hosp_occ, aes(x = t, ymin = hosp_capacity, ymax = hosp_occ, fill = ifelse(hosp_occ > hosp_capacity, TRUE, NA))) +
  scale_fill_manual(values = c("dark green"), name = "fill") +
  geom_ribbon(data = low_hosp_occ, aes(x = t, ymin = hosp_capacity, ymax = hosp_occ, fill = ifelse(hosp_occ > hosp_capacity, TRUE, NA))) +
  geom_line(data = high_hosp_occ, aes(x = t, y = hosp_occ), colour = "dark green") +
  geom_line(data = low_hosp_occ, aes(x = t, y = hosp_occ), colour = "dark red") +
  geom_hline(yintercept = hosp_capacity)

ggplot() +
  geom_ribbon(data = high_hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = ifelse(hosp_occ > hosp_capacity, NA, TRUE))) +
  scale_fill_manual(values = c("dark green"), name = "fill") +
  geom_ribbon(data = low_hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = ifelse(hosp_occ > hosp_capacity, NA, TRUE))) +
  geom_line(data = high_hosp_occ, aes(x = t, y = hosp_occ, linetype = ifelse(hosp_occ > hosp_capacity, "solid", "dashed"))) +
  geom_line(data = low_hosp_occ, aes(x = t, y = hosp_occ, linetype = ifelse(hosp_occ > hosp_capacity, "solid", "dashed")))


# Generate data (level2 == level1)
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron), level2 = as.vector(LakeHuron))

# Change Level2
huron[1:50,2] <- huron[1:50,2]+100
huron[50:90,2] <- huron[50:90,2]-100
h <- ggplot(huron, aes(year))

h +
  geom_ribbon(aes(ymin = level, ymax = level2), fill = "grey80") +
  geom_line(aes(y = level)) + geom_line(aes(y=level2))

h +
  geom_ribbon(aes(ymin = level, ymax = level2, fill = ifelse(level > level2, TRUE, NA))) +
  geom_line(aes(y = level)) + geom_line(aes(y=level2)) +
  scale_fill_manual(values=c("green"), name="fill")




ggplot(y) +
  geom_boxplot(aes(x = healthcare, y = prop_IFR_red, fill = healthcare), outlier.shape = NA) +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)


test <- overall %>%
  filter(R0 == "highR0") %>%
  group_by(healthcare, drug_benefit) %>%
  summarise(mean = mean(IFR))

ggplot(test, aes(x = healthcare, y = mean, group = drug_benefit, col = drug_benefit)) +
  geom_path()

ggplot(overall, aes(x = healthcare, y = hosp_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)
ggplot(overall, aes(x = healthcare, y = hosp_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)
