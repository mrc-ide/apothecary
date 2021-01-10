# Loading required libraries
library(tidyverse); library(facetscales); library(scales); library(cowplot)

# Loading most recent version of apothecary
devtools::load_all()

# Functions for Data Preparation and Manipulation
wd <- getwd()
source(paste0(wd, "/analysis_Figure2/Functions/Figure_2_Functions.R"))

# Defining Standard Parameters
country <- "Bhutan"
raw_pop <- squire::population[squire::population$country == country, ]
standard_population <- round(raw_pop$n/sum(raw_pop$n) * 50000000)
standard_population_old_agg <- standard_population
standard_population_old_agg[16] <- standard_population_old_agg[16] + standard_population_old_agg[17]
standard_population_old_agg <- standard_population_old_agg[-17]
prop_pop <- standard_population_old_agg/sum(standard_population_old_agg)
standard_matrix <- matrix(rep(prop_pop, 16), ncol = 16, byrow = TRUE)
demog_pars_highR0 <- list(R0 = 2.2, country = country, population = standard_population, matrix = standard_matrix,
                           time_period = 150, seeding_cases = 1000)
demog_pars_lowR0 <- list(R0 = 1.35, country = country, population = standard_population, matrix = standard_matrix,
                          time_period = 300, seeding_cases = 1000)
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.6
actual_prop_ox_ICU_beds <- 0.8
actual_MV_capacity <- round(actual_ICU_beds * 0.5)

# Figure 2A - Epidemic Curves of Hospital & ICU Bed Demand for Each of the High/Low R0 Scenarios
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

hosp_occ <- data.frame(t = seq(1, length(lowR0$output[, index$overall_hosp_occ])),
                       hosp_occ = c(lowR0$output[, index$overall_hosp_occ], highR0$output[, index$overall_hosp_occ]),
                       scenario = c(rep("low", length(lowR0$output[, index$overall_hosp_occ])), rep("high", length(lowR0$output[, index$overall_hosp_occ]))))
hosp_bed_occ <- ggplot() +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = scenario)) +
  scale_fill_manual(values = alpha(c("#E4521B", "#8AB13C"), 0.5), name = "fill") +
  geom_line(data = hosp_occ, aes(x = t, y = hosp_occ, colour = scenario), linetype = "solid", size = 1) +
  scale_colour_manual(values = c("#E4521B", "#8AB13C")) +
  lims(x = c(0, 320)) +
  labs(x = "Time (Days)", y = "Hospital Bed Demand") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none", axis.title = element_text(size = 10), axis.text = element_text(size = 10)) +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))

# "#42B3D5", "#DCEDC8" # blue and lime
# "#E4521B", "#FEEB65" # orange and yellow
# "#E4521B", "#8AB13C # orange and green

ICU_occ <- data.frame(t = seq(1, length(lowR0$output[, index$overall_ICU_occ])),
                      ICU_occ = c(lowR0$output[, index$overall_ICU_occ], highR0$output[, index$overall_ICU_occ]),
                      scenario = c(rep("low", length(lowR0$output[, index$overall_ICU_occ])), rep("high", length(lowR0$output[, index$overall_ICU_occ]))))
ICU_bed_occ <- ggplot() +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = ICU_occ, fill = scenario)) +
  scale_fill_manual(values = alpha(c("#E4521B", "#8AB13C"), 0.5), name = "fill") +
  geom_line(data = ICU_occ, aes(x = t, y = ICU_occ, colour = scenario), linetype = "solid", size = 1) +
  scale_colour_manual(values = c("#E4521B", "#8AB13C")) +
  lims(x = c(0, 320)) +
  labs(x = "Time (Days)", y = "") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none", axis.title = element_text(size = 10), axis.text = element_text(size = 10)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0, 0), "cm"))

# Figure 2B - Proportion of Patients Receiving Beds, Oxygen & MV For Hospital/ICU Bed Patients
lowR0 <- run_apothecary(country = demog_pars_lowR0$country, R0 = demog_pars_lowR0$R0,
                        population = demog_pars_lowR0$population, contact_matrix_set = demog_pars_lowR0$matrix,
                        time_period = 365, seeding_cases = demog_pars_lowR0$seeding_cases,
                        day_return = TRUE,
                        hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                        prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                        MV_capacity = actual_MV_capacity)
highR0 <- run_apothecary(country = demog_pars_highR0$country, R0 = demog_pars_highR0$R0,
                         population = demog_pars_highR0$population, contact_matrix_set = demog_pars_highR0$matrix,
                         time_period = 365, seeding_cases = demog_pars_highR0$seeding_cases,
                         day_return = TRUE,
                         hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                         prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds,
                         MV_capacity = actual_MV_capacity)

low_R0_hosp_bed <- sum(lowR0$output[, index$number_get_hosp_any_treat])/sum(lowR0$output[, index$number_req_hosp_bed])
low_R0_ICU_bed <- sum(lowR0$output[, index$number_get_ICU_any_treat])/sum(lowR0$output[, index$number_req_ICU_bed])
low_R0_hosp_full <- sum(lowR0$output[, index$number_get_hosp_full_treat])/sum(lowR0$output[, index$number_req_hosp_bed])
low_R0_ICU_full <- sum(lowR0$output[, index$number_get_ICU_full_treat])/sum(lowR0$output[, index$number_req_ICU_bed])

high_R0_hosp_bed <-  sum(highR0$output[, index$number_get_hosp_any_treat])/sum(highR0$output[, index$number_req_hosp_bed])
high_R0_ICU_bed <- sum(highR0$output[, index$number_get_ICU_any_treat])/sum(highR0$output[, index$number_req_ICU_bed])
high_R0_hosp_full <-  sum(highR0$output[, index$number_get_hosp_full_treat])/sum(highR0$output[, index$number_req_hosp_bed])
high_R0_ICU_full <- sum(highR0$output[, index$number_get_ICU_full_treat])/sum(highR0$output[, index$number_req_ICU_bed])

treat_prop <- data.frame(metric = rep(c("Hospital", "ICU"), 4),
                         R0 = rep(c("Low R0", "High R0"), each = 4),
                         treat_extent = rep(c("Bed Only", "Bed Only", "Complete\nHealthcare", "Complete\nHealthcare"), 2),
                         value = c(low_R0_hosp_bed - low_R0_hosp_full, low_R0_ICU_bed - low_R0_ICU_full, low_R0_hosp_full, low_R0_ICU_full,
                                   high_R0_hosp_bed - high_R0_hosp_full, high_R0_ICU_bed - high_R0_ICU_full, high_R0_hosp_full, high_R0_ICU_full))
treat_prop$R0 <- factor(treat_prop$R0, levels = c("High R0", "Low R0"))
tret_prop_plot <- ggplot() +
  geom_bar(data = treat_prop, aes(x = R0, y = value, fill = interaction(R0, treat_extent), group = treat_extent),
           stat = "identity") +
  facet_grid(~metric) +
  scale_fill_manual(values = c(alpha(c("#E4521B", "#8AB13C"), alpha = 0.4), "#E4521B", "#8AB13C")) +
  labs(y = "% Patients Receiving Healthcare") +
  theme(axis.title.x = element_blank(), legend.position = "none", legend.title = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
tret_prop_plot

# Figure 2C - IFR from the Different Drug Effect/R0/Health Resource Scenarios

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
  tidyr::separate(scenario, c("number", "drug","R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
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
  filter(healthcare != "noHC") %>%
  filter(drug_benefit != "allhosp_benfull")
no_drug_data <- overall %>%
  group_by(healthcare, R0) %>%
  distinct(no_drugs_IFR)

# New facet labels for R0 variables
R0.labs <- c("High R0", "Low R0")
names(R0.labs) <- c("highR0", "lowR0")

# New facet label names for Healthcare Scenario variables
healthcare.labs <- c("Impact Only In Treated Patients",
                     "Impact In All Hospitalised Patients - Pess.",
                     "Impact In All Hospitalised Patients - Opti.")
                     #"Impact In All Hospitalised Patients - Full")
names(healthcare.labs) <- c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti")# "allhosp_benfull")

IFRplot1 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(aes(x = healthcare, y = no_drugs_IFR, col = interaction(R0, healthcare)), shape = 19) +
  geom_hline(aes(yintercept = no_hc_IFR), no_hc, linetype = "dashed") +
  facet_grid(R0 ~ drug_benefit,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs), switch = "y") +
  scale_fill_manual(values = c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                               "#82CA59", "#9E3813", "#549418", "purple", "pink"),
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
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "", y = "Infection Fatality Ratio (%)") +
  lims(y = c(0, 0.55)) #+
  #scale_y_continuous(position="right")

#c("#F6A78A", "#E4521B", "#9E3813")

IFRplot2 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(data = no_drug_data, aes(x = healthcare, y = no_drugs_IFR, group = interaction(R0, healthcare)),
             shape = 19, position = position_dodge(width = 0.75)) +
  geom_hline(aes(yintercept = no_hc_IFR), no_hc, linetype = "dashed") +
  facet_grid(. ~ drug_benefit,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_fill_manual(values = c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                               "#82CA59", "#9E3813", "#549418", "purple", "pink"),
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
        strip.background.x = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "", y = "Infection Fatality Ratio (%)") +
  lims(y = c(0, 0.55))

# Figure 2D - Proportion of Potential Dexmethasone Benefit Realised
unlimcHC_drug_benefit <- overall %>%
  filter(healthcare == "unlimHC") %>%
  mutate(abs_unlimitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_unlimitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, no_drugs_IFR, IFR,
         abs_unlimitedHC_drug_IFR_reduction, prop_unlimitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_unlimitedHC_drug_IFR_reduction = mean(abs_unlimitedHC_drug_IFR_reduction),
            prop_unlimitedHC_drug_IFR_reduction = mean(prop_unlimitedHC_drug_IFR_reduction))

limHC_drug_benefit <- overall %>%
  filter(healthcare != "unlimHC") %>%
  mutate(abs_limitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_limitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, healthcare, no_drugs_IFR, IFR,
         abs_limitedHC_drug_IFR_reduction, prop_limitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit, healthcare) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_limitedHC_drug_IFR_reduction = mean(abs_limitedHC_drug_IFR_reduction),
            prop_limitedHC_drug_IFR_reduction = mean(prop_limitedHC_drug_IFR_reduction))

overall_new <- limHC_drug_benefit %>%
  left_join(unlimcHC_drug_benefit, by = c("R0", "drug_benefit")) %>%
  mutate(prop_benefit_gained = prop_limitedHC_drug_IFR_reduction/prop_unlimitedHC_drug_IFR_reduction) %>%
  filter(healthcare == "limMVoxbeds", drug_benefit != "allhosp_benfull")
ben <- ggplot(overall_new) +
  geom_bar(aes(x = drug_benefit, y = prop_benefit_gained, fill = R0, group = interaction(healthcare, R0)),
           stat = "identity", position = "dodge") +
  facet_grid(R0 ~ ., labeller = labeller(R0 = R0.labs)) +
  scale_fill_manual(values = c("#9E3813", "#549418")) +
  scale_y_continuous(position="right", limits = c(0, 1)) +
  theme(legend.position = "none", axis.title = element_blank(),
        axis.text.x = element_text(size = 2),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
ben2 <- ggplot(overall_new) +
  geom_bar(aes(x = drug_benefit, y = prop_benefit_gained, fill = R0, group = interaction(healthcare, R0)),
           stat = "identity", position = "dodge") +
  facet_grid(. ~ R0, labeller = labeller(R0 = R0.labs)) +
  scale_y_continuous(position="right", limits = c(0, 1)) +
  scale_fill_manual(values = c("#9E3813", "#549418")) +
  theme(legend.position = "none", axis.title = element_blank(),
        axis.text.x = element_text(size = 2),
        strip.background.x = element_blank(),
        strip.text.x = element_blank())

# 12 wide x 10 high for first one
# Unaligned
# partAB <- plot_grid(hosp_bed_occ, ICU_bed_occ, tret_prop_plot, rel_widths = c(0.75, 0.75, 1), axis = "b", align = "h", nrow = 1)
# partC1 <- plot_grid(IFRplot1, ben, nrow = 1, rel_widths = c(4, 1), align = "h", axis = "b")
# plot_grid(partAB, partC1, ncol = 1, rel_heights = c(1.2, 2)) +
#     draw_plot_label(
#       c("A", "B", "C", "D"),
#       c(0, 0.6, 0, 0.75),
#       c(1, 1, 0.7, 0.7),
#       size = 30)
#
# partAB <- plot_grid(hosp_bed_occ, ICU_bed_occ, tret_prop_plot, rel_widths = c(0.75, 0.75, 1), axis = "b", align = "h", nrow = 1)
# partC2 <- plot_grid(IFRplot2, ben2, nrow = 1, rel_widths = c(3, 1.3), align = "h", axis = "b")
# plot_grid(partAB, partC2, ncol = 1, rel_heights = c(1.3, 2)) +
#   draw_plot_label(
#     c("A", "B", "C", "D"),
#     c(0, 0.6, 0, 0.65),
#     c(1, 1, 0.65, 0.65),
#     size = 30)


# Alternative Figure 2
overall <- overall %>%
  mutate(drug_benefit = as.character(drug_benefit)) %>%
  mutate(drug_benefit = case_when(drug_benefit == "treatonly_benfull" ~ "atreatonly_benfull",
                                  drug_benefit == "allhosp_gradbencons" ~ "ballhosp_gradbencons",
                                  drug_benefit == "allhosp_gradbenopti" ~ "callhosp_gradbenopti",
                                  TRUE ~ drug_benefit))
overall$drug_benefit <- factor(overall$drug_benefit, levels = c("atreatonly_benfull", "ballhosp_gradbencons", "callhosp_gradbenopti"))
healthcare.labs <- c("Impact Only In Treated Patients",
                     "Impact In All Hospitalised Patients - Pess.",
                     "Impact In All Hospitalised Patients - Opti.")
names(healthcare.labs) <- c("atreatonly_benfull", "ballhosp_gradbencons", "callhosp_gradbenopti")

IFRplot2 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(data = no_drug_data, aes(x = healthcare, y = no_drugs_IFR, group = interaction(R0, healthcare)),
             shape = 19, position = position_dodge(width = 0.75)) +
  geom_hline(aes(yintercept = no_hc_IFR), no_hc, linetype = "dashed") +
  facet_grid(. ~ drug_benefit,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_fill_manual(values = c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                               "#82CA59", "#9E3813", "#549418", "purple", "pink"),
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
        strip.background.x = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "", y = "Infection Fatality Ratio (%)") +
  lims(y = c(0, 0.55))

unlimcHC_drug_benefit <- overall %>%
  filter(healthcare == "unlimHC") %>%
  mutate(abs_unlimitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_unlimitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, no_drugs_IFR, IFR,
         abs_unlimitedHC_drug_IFR_reduction, prop_unlimitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_unlimitedHC_drug_IFR_reduction = mean(abs_unlimitedHC_drug_IFR_reduction),
            prop_unlimitedHC_drug_IFR_reduction = mean(prop_unlimitedHC_drug_IFR_reduction))

limHC_drug_benefit <- overall %>%
  mutate(abs_limitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_limitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, healthcare, no_drugs_IFR, IFR,
         abs_limitedHC_drug_IFR_reduction, prop_limitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit, healthcare) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_limitedHC_drug_IFR_reduction = mean(abs_limitedHC_drug_IFR_reduction),
            prop_limitedHC_drug_IFR_reduction = mean(prop_limitedHC_drug_IFR_reduction))

overall_new <- limHC_drug_benefit %>%
  left_join(unlimcHC_drug_benefit, by = c("R0", "drug_benefit")) %>%
  mutate(prop_benefit_gained = prop_limitedHC_drug_IFR_reduction/prop_unlimitedHC_drug_IFR_reduction) %>%
  filter(drug_benefit != "allhosp_benfull")

treatonly_benfull <- overall_new %>%
  filter(drug_benefit == "atreatonly_benfull")
treatonly_benfull <- ggplotGrob(ggplot(treatonly_benfull) +
                                  geom_bar(aes(x = R0, y = 100 *prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                                           stat = "identity", position = "dodge", col = "black") +
                                  scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE",
                                                                     "#E4521B", "#82CA59", "#9E3813", "#549418"),
                                                                   alpha = c(1, 1, 1, 1, 1, 1, 1, 1)), name = "fill") +
                                  scale_x_discrete(labels = c("High R0", "Low R0")) +
                                  scale_y_continuous(position="left", limits = c(0, 100)) +
                                  theme(legend.position = "none", axis.title = element_blank(),
                                        strip.background.y = element_blank(),
                                        strip.text.y = element_blank(),
                                        plot.background = element_rect(colour = "black")))

allhosp_gradbencons <- overall_new %>%
  filter(drug_benefit == "ballhosp_gradbencons")
allhosp_gradbencons <- ggplotGrob(ggplot(allhosp_gradbencons) +
                                    geom_bar(aes(x = R0, y = 100 * prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                                             stat = "identity", position = "dodge", col = "black") +
                                    scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE",
                                                                       "#E4521B", "#82CA59", "#9E3813", "#549418"),
                                                                     alpha = c(1, 1, 1, 1, 1, 1, 1, 1)), name = "fill") +
                                    scale_x_discrete(labels = c("High R0", "Low R0")) +
                                    scale_y_continuous(position="left", limits = c(0, 100)) +
                                    theme(legend.position = "none", axis.title = element_blank(),
                                          strip.background.y = element_blank(),
                                          strip.text.y = element_blank(),
                                          plot.background = element_rect(colour = "black")))

allhosp_gradbenopti <- overall_new %>%
  filter(drug_benefit == "callhosp_gradbenopti")
allhosp_gradbenopti <- ggplotGrob(ggplot(allhosp_gradbenopti) +
                                    geom_bar(aes(x = R0, y = 100 * prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                                             stat = "identity", position = "dodge", col = "black") +
                                    scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE",
                                                                       "#E4521B", "#82CA59", "#9E3813", "#549418"),
                                                                     alpha = c(1, 1, 1, 1, 1, 1, 1, 1)), name = "fill") +
                                    scale_x_discrete(labels = c("High R0", "Low R0")) +
                                    scale_y_continuous(position="left", limits = c(0, 100)) +
                                    theme(legend.position = "none", axis.title = element_blank(),
                                          strip.background.y = element_blank(),
                                          strip.text.y = element_blank(),
                                          plot.background = element_rect(colour = "black")))


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity,
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob,
                                          xmin = xmin, xmax = xmax,
                                          ymin = ymin, ymax = ymax))
}

insetplot <- IFRplot2 +
  annotation_custom2(grob = treatonly_benfull, data = data.frame(drug_benefit="atreatonly_benfull"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5) +
  annotation_custom2(grob = allhosp_gradbencons, data = data.frame(drug_benefit="ballhosp_gradbencons"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5) +
  annotation_custom2(grob = allhosp_gradbenopti, data = data.frame(drug_benefit="callhosp_gradbenopti"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5)


# go with 12 wide and 8 high
partAB <- plot_grid(hosp_bed_occ, ICU_bed_occ, tret_prop_plot, rel_widths = c(0.75, 0.75, 1), axis = "b", align = "h", nrow = 1)
plot_grid(partAB, insetplot, ncol = 1, rel_heights = c(1.3, 2)) +
  draw_plot_label(
    c("A", "B", "C"),
    c(0, 0.6, 0),
    c(1, 1, 0.65),
    size = 30)

# Left Aligned
# plots <- align_plots(hosp_bed_occ, IFRplot1, align = 'v', axis = 'l')
# top_row <- plot_grid(plots[[1]], ICU_bed_occ, tret_prop_plot, rel_widths = c(1, 1, 1), axis = "b", align = "h", nrow = 1)
# plot_grid(top_row, plots[[2]], ncol = 1, rel_heights = c(1.2, 2))

# Right Aligned
# plots <- align_plots(tret_prop_plot, IFRplot1, align = 'v', axis = 'r')
# top_row <- plot_grid(hosp_bed_occ, ICU_bed_occ, plots[[1]], rel_widths = c(1, 1, 1), axis = "b", align = "h", nrow = 1)
# plot_grid(top_row, plots[[2]], ncol = 1, rel_heights = c(1.2, 2))

# Unaligned
# partAB <- plot_grid(hosp_bed_occ, ICU_bed_occ, tret_prop_plot, rel_widths = c(1, 1, 1), axis = "b", align = "h", nrow = 1)
# partC1 <- plot_grid(IFRplot2, label_size = 25, nrow = 1)
# plot_grid(partAB, partC1, label_size = 25, ncol = 1, rel_heights = c(1.2, 2))

# Left Aligned
# plots <- align_plots(hosp_bed_occ, IFRplot2, align = 'v', axis = 'l')
# top_row <- plot_grid(plots[[1]], ICU_bed_occ, tret_prop_plot, rel_widths = c(1, 1, 1), axis = "b", align = "h", nrow = 1)
# plot_grid(top_row, plots[[2]], ncol = 1, rel_heights = c(1.2, 2))

# Right Aligned
# plots <- align_plots(tret_prop_plot, IFRplot1, align = 'v', axis = 'r')
# top_row <- plot_grid(hosp_bed_occ, ICU_bed_occ, plots[[1]], rel_widths = c(1, 1, 1), axis = "b", align = "h", nrow = 1)
# plot_grid(top_row, plots[[2]], ncol = 1, rel_heights = c(1.2, 2))

# ggplot() +
#   geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = interaction(scenario, ifelse(hosp_occ > actual_hosp_beds, NA, TRUE)))) +
#   scale_fill_manual(values = alpha(c("#3FA7D6", "#F79D84"), 0.5), name = "fill") +
#   geom_line(data = hosp_occ, aes(x = t, y = hosp_occ, colour = scenario, linetype = ifelse(hosp_occ > actual_hosp_beds, "solid", "dashed")), size = 1) +
#   scale_colour_manual(values = c("#3FA7D6", "#F79D84")) +
#   geom_hline(yintercept = actual_hosp_beds, col = "#3FA7D6", linetype = "dashed", size = 1) +
#   theme(legend.position = "none")
# middle_set <- plot_grid(c, d, labels = c('', ''), label_size = 30, ncol = 1)
# final <- plot_grid(e, labels = c(''), label_size = 30, ncol = 1)
# combined <- plot_grid(middle_set, final, label_size = 30, ncol = 2)
# plot_grid(top_row, combined, label_size = 30, ncol = 1, rel_heights = c(1, 2))  +
#   draw_plot_label(
#     c("A", "B", "C", "D", "E"),
#     c(0, 0.5, 0, 0, 0.5),
#     c(1, 1, 0.7, 0.38, 0.7),
#     size = 30)
# ggsave("Figures/Figure 2 - Global Regions/Figure_2.pdf", plot = last_plot(), device = NULL, path = NULL,
#        scale = 1, width = 10, height = 9, units = c("in", "cm", "mm"),
#        dpi = 300, useDingbats = FALSE)
#
# low_prop_ben <- overall_new %>%
#   filter(R0 == "lowR0", drug_benefit != "allhosp_benfull")
# high_prop_ben <- overall_new %>%
#   filter(R0 == "highR0", drug_benefit != "allhosp_benfull")
#
# low_ben <- ggplot(low_prop_ben) +
#   geom_bar(aes(x = drug_benefit, y = prop_benefit_gained, fill = R0, group = interaction(healthcare, R0)),
#            stat = "identity", position = "dodge") +
#   scale_y_continuous(position="right", limits = c(0, 1)) +
#   theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_blank())
# high_ben <- ggplot(high_prop_ben) +
#   geom_bar(aes(x = drug_benefit, y = prop_benefit_gained, fill = R0, group = interaction(healthcare, R0)),
#            stat = "identity", position = "dodge") +
#   scale_y_continuous(position="right", limits = c(0, 1)) +
#   theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_blank())
