# Loading required libraries
library(ggplot2); library(tidyverse); library(dplyr); library(cowplot); library(scales)

# Loading apothecary
devtools::load_all()

# Sourcing required functions
source("analysis_Figure4/Functions/metric_helper_functions.R")

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

# Loading in Effectiveness Scan Results
df <- readRDS("analysis_Figure4/Outputs/effectiveness_scan.rds")
AB <- ggplot(df, aes(x = effectiveness, y = 1-averted, col = drug)) +
  geom_path(size =1) +
  scale_y_continuous(position = "left") +
  facet_grid(R0~.) +
  labs(x = "Drug Effectiveness", y = "Proportion of Deaths Averted") +
  scale_colour_manual(values = c("#f32bb1", "#8b82f8", "#f76c07", "#98cb66", "#009bfd")) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 12))

# Loading in Drug Effect Results
none_low <- readRDS("analysis_Figure4/Outputs/none_low_df.rds")
none_high <- readRDS("analysis_Figure4/Outputs/none_high_df.rds")
dexy_low <- readRDS("analysis_Figure4/Outputs/dexy_low_df.rds")
dexy_high <- readRDS("analysis_Figure4/Outputs/dexy_high_df.rds")
rem_low <- readRDS("analysis_Figure4/Outputs/rem_low_df.rds")
rem_high <- readRDS("analysis_Figure4/Outputs/rem_high_df.rds")
ivm_low <- readRDS("analysis_Figure4/Outputs/ivm_low_df.rds")
ivm_high <- readRDS("analysis_Figure4/Outputs/ivm_high_df.rds")
mab_low <- readRDS("analysis_Figure4/Outputs/mab_low_df.rds")
mab_high <- readRDS("analysis_Figure4/Outputs/mab_high_df.rds")

ivm_low$drug <- "Ivermectin"
ivm_high$drug <- "Ivermectin"
drugs_df <- rbind(none_low, none_high, dexy_low, dexy_high, rem_low, rem_high, ivm_low, ivm_high, mab_low, mab_high) %>%
  filter(drug != "None")
drugs_df$drug <- factor(drugs_df$drug, levels = c("Dexamethasone", "Ivermectin", "Remdesivir", "mAb"))
deaths_df <- drugs_df %>%
  select(drug, R0, direct_deaths_averted, indirect_deaths_averted_transmission, indirect_deaths_averted_healthcare) %>%
  pivot_longer(cols = c(-drug, -R0), names_to = "effect_type", values_to = "deaths_averted")

deaths_df$effect_type <- factor(deaths_df$effect_type, levels = c("indirect_deaths_averted_transmission",
                                                                  "indirect_deaths_averted_healthcare",
                                                                  "direct_deaths_averted"))
deaths_df$drug <- factor(deaths_df$drug, levels = c("Dexamethasone", "Ivermectin", "Remdesivir", "mAb"))
C <- ggplot(deaths_df) +
  geom_bar(width=0.9, aes(x = drug, y = deaths_averted,
                          fill = interaction(drug, effect_type, R0)), stat = "identity") +
  facet_wrap(R0~., nrow = 2) +
  labs(x = "", y = "Total Deaths Averted") +
  scale_x_discrete(labels = c("Dexamethasone", "Ivermectin(?)", "Remdesivir", "mAbs")) +
  scale_y_continuous(position = "left") +
  theme(panel.margin = grid::unit(0.2, "lines"),
        legend.position = "none", axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        strip.background = element_blank(), strip.text = element_blank()) +
  scale_fill_manual(values = c("pink", "pink", "pink", "#DCF4F3",           # indirect transmission
                               "pink", "#BCB6FE", "#FF9595", "#9BE8E3",     # indirect healthcare
                               "#f32bb1", "#8b82f8", "#F22C2C", "#02D2C4",  # direc
                               "pink", "pink", "pink", "#DCF4F3",
                               "pink", "#BCB6FE", "#FF9595", "#9BE8E3",
                               "#f32bb1", "#8b82f8", "#F22C2C", "#02D2C4"))

# Overall Figure Plotting
temp <- plot_grid(AB, C, ncol = 2, rel_widths = c(1, 1))
fig4 <- temp +
  draw_plot_label(
    c("A", "B", "C", "D"),
    c(-0.02, -0.02, 0.48, 0.48),
    c(1.02, 0.58, 1.02, 0.58),
    size = 30)
ggsave2(file = "analysis_Figure4/new_Figure_4.pdf", fig4, dpi = 300,
        width = 9, height = 8)
