# Loading required libraries
library(tidyverse); library(facetscales); library(scales); library(cowplot); library(magrittr)

# Loading most recent version of apothecary
devtools::load_all()

# Functions for Data Preparation and Manipulation
wd <- getwd()
source(paste0(wd, "/analysis_Figure2/Functions/Figure_2_Functions.R"))

# Supplementary Figure 2 - IFR from the Different Drug Effect/R0/Health Resource Scenarios
#                          for Various Assumptions About Dexamethasone's Impact

# Loading in outputs where Dexamethasone is present (and varying healthcare constraints and R)
filenames <- list.files("analysis_Figure2/Outputs/")
drug_filenames <- filenames[!grepl("notreat", filenames) & !grepl("allhosp_gradbencons", filenames)]
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
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbenopti", "allhosp_benfull"))) %>%
  filter(healthcare != "noHC")
no_drug_data <- overall %>%
  group_by(healthcare, R0) %>%
  distinct(no_drugs_IFR)

# New facet labels for R0 variables
R0.labs <- c("High R0", "Low R0")
names(R0.labs) <- c("highR0", "lowR0")

# Alternative Figure 2
overall <- overall %>%
  mutate(drug_benefit = as.character(drug_benefit)) %>%
  mutate(drug_benefit = case_when(drug_benefit == "treatonly_benfull" ~ "atreatonly_benfull",
                                  drug_benefit == "allhosp_gradbenopti" ~ "ballhosp_gradbenopti",
                                  drug_benefit == "allhosp_benfull" ~ "callhosp_benfull",
                                  TRUE ~ drug_benefit))
overall$drug_benefit <- factor(overall$drug_benefit,
                               levels = c("atreatonly_benfull", "ballhosp_gradbenopti", "callhosp_benfull"))
healthcare.labs <- c("Impact Only In Treated Patients",
                     "Impact In All Hospitalised Patients",
                     "Drug Given to Non-Hospitalised")
names(healthcare.labs) <- c("treatonly_benfull", "allhosp_gradbenopti", "allhosp_benfull")

IFRplot2 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(data = no_drug_data, aes(x = healthcare, y = no_drugs_IFR, group = interaction(R0, healthcare)),
             shape = 19, position = position_dodge(width = 0.75)) +
  facet_grid(drug_benefit ~ .,
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
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing = unit(2, "lines")) +
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

prob_ben_plot <- ggplot(overall_new) +
  geom_bar(aes(x = R0, y = 100 * prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
           stat = "identity", position = "dodge", col = "black") +
  facet_grid(drug_benefit ~ .,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE",
                                     "#E4521B", "#82CA59", "#9E3813", "#549418"),
                                   alpha = c(1, 1, 1, 1, 1, 1, 1, 1)), name = "fill") +
  scale_x_discrete(labels = c("High R0", "Low R0")) +
  scale_y_continuous(position="left", limits = c(0, 100)) +
  labs(y = "% Max Drug Impact") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing = unit(2, "lines"))

fig2 <- plot_grid(IFRplot2, prob_ben_plot, nrow = 1, align = "h") +
  draw_plot_label(
    c("A", "B", "C", "D", "E", "F"),
    c(0, 0.48, 0, 0.48, 0, 0.48),
    c(1.04, 1.04, 0.73, 0.73, 0.41, 0.41),
    size = 30)
fig2
ggsave2("analysis_Figure2/Supp_Figure_2.pdf", fig2, dpi = 400, width = 8, height = 11)
