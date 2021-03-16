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
demog_pars_highR0 <- list(R0 = 2, country = country, population = standard_population, matrix = standard_matrix,
                           time_period = 150, seeding_cases = 1000)
demog_pars_lowR0 <- list(R0 = 1.35, country = country, population = standard_population, matrix = standard_matrix,
                          time_period = 300, seeding_cases = 1000)
actual_hosp_beds <- round(squire::get_healthcare_capacity(country)$hosp_beds * sum(standard_population)/1000)
actual_ICU_beds <- round(squire::get_healthcare_capacity(country)$ICU_beds * sum(standard_population)/1000)
actual_prop_ox_hosp_beds <- 0.4 #0.6
actual_prop_ox_ICU_beds <- 0.4 #0.8
actual_MV_capacity <- round(actual_ICU_beds * 0.4) #0.5

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
                       scenario = c(rep("low", length(lowR0$output[, index$overall_hosp_occ])), rep("high", length(lowR0$output[, index$overall_hosp_occ])))) %>%
  mutate(hosp_occ_2 = ifelse(hosp_occ > actual_hosp_beds, actual_hosp_beds, hosp_occ)) %>%
  mutate(hosp_occ_3 = ifelse(hosp_occ > actual_hosp_beds * actual_prop_ox_hosp_beds, actual_hosp_beds * actual_prop_ox_hosp_beds, hosp_occ))

hosp_bed_occ <- ggplot() +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ, fill = scenario), alpha = 0.25) +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ_2, fill = scenario), alpha = 0.5) +
  geom_ribbon(data = hosp_occ, aes(x = t, ymin = 0, ymax = hosp_occ_3, fill = scenario), alpha = 1) +
  scale_fill_manual(values = c("#E4521B", "#8AB13C"), name = "fill") +
  theme(legend.position = "none", axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        plot.margin = unit(c(0.5, 0, 0, 0), "cm")) +
  scale_y_continuous(labels = comma) +
  geom_hline(yintercept = actual_hosp_beds, linetype = 'dashed') +
  geom_hline(yintercept = actual_hosp_beds * actual_prop_ox_hosp_beds, linetype = 'dashed') +
  lims(x = c(0, 320)) +
  labs(x = "Time (Days)", y = "Hospital Bed Demand")

ICU_occ <- data.frame(t = seq(1, length(lowR0$output[, index$overall_ICU_occ])),
                      ICU_occ = c(lowR0$output[, index$overall_ICU_occ], highR0$output[, index$overall_ICU_occ]),
                      scenario = c(rep("low", length(lowR0$output[, index$overall_ICU_occ])), rep("high", length(lowR0$output[, index$overall_ICU_occ])))) %>%
  mutate(ICU_occ_2 = ifelse(ICU_occ > actual_ICU_beds, actual_ICU_beds, ICU_occ)) %>%
  mutate(ICU_occ_3 = ifelse(ICU_occ > actual_ICU_beds * actual_prop_ox_ICU_beds, actual_ICU_beds * actual_prop_ox_ICU_beds, ICU_occ)) %>%
  mutate(ICU_occ_4 = ifelse(ICU_occ > actual_MV_capacity, actual_MV_capacity, ICU_occ))

ICU_bed_occ <- ggplot() +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = ICU_occ, fill = scenario), alpha = 0.15) +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = ICU_occ_2, fill = scenario), alpha = 0.25) +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = ICU_occ_3, fill = scenario), alpha = 0.5) +
  geom_ribbon(data = ICU_occ, aes(x = t, ymin = 0, ymax = ICU_occ_4, fill = scenario), alpha = 1) +
  scale_fill_manual(values = c("#E4521B", "#8AB13C"), name = "fill") +
  theme(legend.position = "none", axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        plot.margin = unit(c(0.5, 0, 0, 0), "cm")) +
  scale_y_continuous(labels = comma) +
  geom_hline(yintercept = actual_ICU_beds, linetype = 'dashed') +
  geom_hline(yintercept = actual_MV_capacity, linetype = 'dashed') +
  geom_hline(yintercept = actual_ICU_beds * actual_prop_ox_ICU_beds, linetype = 'dashed') +
  lims(x = c(0, 320)) +
  labs(x = "Time (Days)", y = "ICU Bed Demand")

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

treat_prop <- data.frame(metric = rep(c("Hospital", "ICU"), 6),
                         R0 = rep(c("Low R0", "High R0"), each = 6),
                         treat_extent = rep(c("Bed Only", "Bed Only", "None", "None", "Complete\nHealthcare", "Complete\nHealthcare"), 2),
                         value = c(low_R0_hosp_bed - low_R0_hosp_full, low_R0_ICU_bed - low_R0_ICU_full,
                                   1 - low_R0_hosp_bed, 1 - low_R0_ICU_bed,
                                   low_R0_hosp_full, low_R0_ICU_full,
                                   high_R0_hosp_bed - high_R0_hosp_full, high_R0_ICU_bed - high_R0_ICU_full,
                                   1 - high_R0_hosp_bed, 1 - high_R0_ICU_bed,
                                   high_R0_hosp_full, high_R0_ICU_full))
treat_prop$R0 <- factor(treat_prop$R0, levels = c("High R0", "Low R0"))
treat_prop$treat_extent <- factor(treat_prop$treat_extent, levels = c("None", "Bed Only", "Complete\nHealthcare"))

treat_prop_hosp <- treat_prop %>%
  filter(metric == "Hospital")
tret_prop_plot_hosp <- ggplot() +
  geom_bar(data = treat_prop_hosp, aes(x = R0, y = 100 * value, fill = interaction(R0, treat_extent), group = treat_extent),
           stat = "identity") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = c(alpha("#E4521B", alpha = 0.25), alpha("#8AB13C", alpha = 0.1),
                               alpha("#E4521B", alpha = 0.5), alpha("#8AB13C", alpha = 0.65),
                               "#E4521B", "#8AB13C")) +
  labs(y = "% Receiving Required Healthcare") +
  theme(axis.title.x = element_blank(), legend.position = "none", legend.title = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0, 0, 0.25), "cm"))

treat_prop_ICU <- treat_prop %>%
  filter(metric == "ICU")
tret_prop_plot_ICU <- ggplot() +
  geom_bar(data = treat_prop_ICU, aes(x = R0, y = 100 * value, fill = interaction(R0, treat_extent), group = treat_extent),
           stat = "identity") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = c(alpha("#E4521B", alpha = 0.25), alpha("#8AB13C", alpha = 0.1),
                               alpha("#E4521B", alpha = 0.5), alpha("#8AB13C", alpha = 0.65),
                               "#E4521B", "#8AB13C")) +
  labs(y = "% Receiving Required Healthcare") +
  theme(axis.title.x = element_blank(), legend.position = "none", legend.title = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0, 0, 0.25), "cm"))

# Figure 2D - IFR from the Different Drug Effect/R0/Health Resource Scenarios

# Loading in required files and creating relevant dataframes
filenames <- list.files("analysis_Figure2/Outputs/")

# Loading in outputs where Dexamethasone is present (and varying healthcare constraints and R)
drug_filenames <- filenames[grepl("allhosp_gradbencons", filenames)]
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
  mutate(drug_benefit = factor(drug_benefit))

# Loading in outputs where Dexamethasone is NOT present (and varying healthcare constraints and R)
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

# Merging Drug and No Drug Dataframes Together and Plotting the IFR
overall <- drugs %>%
  left_join(no_drugs, by = c("R0", "healthcare")) %>%
  mutate(IFR_diff = no_drugs_IFR - IFR) %>%
  mutate(prop_IFR_red = IFR_diff/no_drugs_IFR) %>%
  mutate(healthcare = factor(healthcare, levels = c("unlimHC", "limMV", "limMVox", "limMVoxbeds", "noHC"))) %>%
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull"))) %>%
  filter(healthcare != "noHC") %>%
  filter(drug_benefit != "allhosp_benfull" & drug_benefit != "treatonly_benfull" & drug_benefit != "allhosp_gradbenopti")
no_drug_data <- overall %>%
  group_by(healthcare, R0) %>%
  distinct(no_drugs_IFR)

IFRplot2 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(data = no_drug_data, aes(x = healthcare, y = no_drugs_IFR, group = interaction(R0, healthcare)),
             shape = 19, position = position_dodge(width = 0.75)) +
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

# Figure 2E - the percentage of Dexamethasone's maximum potential impact that is achieved
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

prob_ben_plot <- ggplot(overall_new) +
  geom_bar(aes(x = R0, y = 100 * prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
           stat = "identity", position = "dodge", col = "black") +
  scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE",
                                     "#E4521B", "#82CA59", "#9E3813", "#549418"),
                                   alpha = c(1, 1, 1, 1, 1, 1, 1, 1)), name = "fill") +
  scale_x_discrete(labels = c("High R0", "Low R0")) +
  scale_y_continuous(position="left", limits = c(0, 100)) +
  labs(y = "% Max Drug Impact") +
  theme(legend.position = "none", axis.title.x = element_blank())


# go with 12 wide and 8 high
partAB <- plot_grid(hosp_bed_occ, tret_prop_plot_hosp,
                    ICU_bed_occ, tret_prop_plot_ICU, rel_widths = c(1.25, 0.75, 1.25, 0.75), axis = "b", align = "h", nrow = 1)
partC <- plot_grid(NULL, IFRplot2, prob_ben_plot, rel_widths = c(0.7, 1.25, 0.85), axis = "b", align = "h", nrow = 1)
fig2 <- plot_grid(partAB, partC, ncol = 1, rel_heights = c(1.3, 2)) +
  draw_plot_label(
    c("A", "B", "C"),
    c(0, 0.48, 0),
    c(1.02, 1.02, 0.65),
    size = 30)
fig2
ggsave2("analysis_Figure2/Figure_2.pdf", fig2, dpi = 400, width = 10, height = 8)
