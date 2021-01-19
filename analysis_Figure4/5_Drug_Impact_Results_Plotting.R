# practice plotting
library(ggplot2); library(tidyverse); library(dplyr); library(cowplot); library(scales)

# load in data
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
deaths_df_plot <- ggplot(deaths_df) +
  geom_bar(width=0.9, aes(x = R0, y = deaths_averted,
               fill = interaction(drug, effect_type, R0)), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  labs(x = "", y = "Total Deaths Averted") +
  scale_x_discrete(labels = c("High R0", "Low R0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        legend.position = "none", axis.text.x = element_text(size = 12)) +
  scale_fill_manual(values = c("pink", "pink", "pink", "#E4EFF1", "pink", "#D9D3D6",
                               "#C2E4FF", "#CADFE3", "#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6",
                               "pink", "pink", "pink", "#E4EFF1", "pink", "#D9D3D6",
                               "#C2E4FF", "#CADFE3", "#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6"))

IFR <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = IFR, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  lims(y = c(0, 0.5)) +
  labs(x = "", y = "IFR") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

AR <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = attack_rate, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Attack Rate") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

prop_full_hosp <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = prop_full_hosp, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Prop. Full Hosp") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

prop_full_ICU <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = prop_full_ICU, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Prop. Full ICU") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

doses <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = doses, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  labs(x = "", y = "Doses") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

deaths_per_dose <- ggplot(drugs_df) +
  geom_bar(aes(x = R0, y = total_deaths_averted/doses, fill = drug), stat = "identity") +
  facet_wrap(~drug, nrow = 1) +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#A5D8FF", "#AFD0D6")) +
  labs(x = "", y = "Doses") +
  scale_x_discrete(labels = c("High\nR0", "Low\nR0")) +
  theme(panel.margin = grid::unit(0.2, "lines"),
        strip.text = element_text(size = 8),
        legend.position = "none", axis.text.x = element_text(size = 8))

one <- plot_grid(deaths_df_plot, labels = c(''), ncol = 1)
two <- plot_grid(IFR, AR, ncol = 1)
three <- plot_grid(prop_full_hosp, prop_full_ICU, deaths_per_dose, nrow = 1)

four <- plot_grid(one, two, nrow = 1, rel_widths = c(2, 1))
five <- plot_grid(four, three, ncol = 1, rel_heights = c(2, 1))
fig4 <- five +
  draw_plot_label(
    c("A", "B", "C", "D", "E", "F"),
    c(0.0, 0.65, 0.65, 0, 0.32, 0.65),
    c(1.02, 1.02, 0.70, 0.38, 0.38, 0.38),
    size = 30)
ggsave2(file = "analysis_Figure4/Figure_4.pdf", fig4, dpi = 300,
        width = 12.55, height = 8.5)
# 11 x 7.5
