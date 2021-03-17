# practice plotting
library(ggplot2); library(tidyverse); library(dplyr); library(cowplot); library(scales)

# load in data
none <- readRDS("analysis_Figure4/Outputs/none_df.rds")
dexy <- readRDS("analysis_Figure4/Outputs/dexy_df.rds")
rem <- readRDS("analysis_Figure4/Outputs/rem_df.rds")
ivm <- readRDS("analysis_Figure4/Outputs/ivm_df.rds")
mabs <- readRDS("analysis_Figure4/Outputs/mab_df.rds")

drugs_df <- rbind(none, dexy, rem, ivm, mabs) %>%
  filter(drug != "None")

deaths_df <- drugs_df %>%
  select(drug, direct_deaths_averted, indirect_deaths_averted_transmission, indirect_deaths_averted_healthcare) %>%
  pivot_longer(cols = -drug, names_to = "effect_type", values_to = "deaths_averted")

deaths_df$effect_type <- factor(deaths_df$effect_type, levels = c("indirect_deaths_averted_transmission",
                                                                  "indirect_deaths_averted_healthcare",
                                                                  "direct_deaths_averted"))
deaths_df_plot <-
  ggplot(deaths_df) +
  geom_bar(aes(x = drug, y = deaths_averted, fill = interaction(effect_type, drug)), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#C49799", "#C49799", "#BFB6BB", "#D9D3D6", "#BFB6BB",
                               "#E4EFF1", "#CADFE3", "#AFD0D6", "#C2E4FF", "#C2E4FF", "#A5D8FF"),
                    labels = c("Indirect - Transmission", "Indirect - Healthcare", "Direct")) +
  theme(legend.position = "none", legend.title = element_blank(),
        axis.text.x = element_text(size = 12)) +
  labs(x = "", y = "Deaths") +
  guides(fill=guide_legend(ncol=2))

#plot(seq(1:10), col = hue_pal()(10), pch = 20, cex = 3.5)

IFR <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = IFR, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  lims(y = c(0, 0.5)) +
  labs(x = "", y = "IFR") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

AR <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = attack_rate, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Attack Rate") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

prop_full_hosp <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = prop_full_hosp, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Prop. Full Healthcare\nGeneral Hospital") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

prop_full_ICU <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = prop_full_ICU, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  lims(y = c(0, 1)) +
  labs(x = "", y = "Prop. Full\nHealthcare ICU") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

doses <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = doses, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  labs(x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

deaths_per_dose <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = total_deaths_averted/doses, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#C49799", "#BFB6BB", "#AFD0D6", "#A5D8FF")) +
  labs(x = "", y = "Deaths Averted Per Dose") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

one <- plot_grid(deaths_df_plot, labels = c(''), ncol = 1)
two <- plot_grid(IFR, AR, ncol = 1)
three <- plot_grid(prop_full_hosp, prop_full_ICU, deaths_per_dose, nrow = 1)

four <- plot_grid(one, two, nrow = 1, rel_widths = c(2, 1))
five <- plot_grid(four, three, ncol = 1, rel_heights = c(2, 1))
five +
  draw_plot_label(
    c("A", "B", "C", "D", "E", "F"),
    c(0.0, 0.65, 0.65, 0, 0.32, 0.65),
    c(1.02, 1.02, 0.70, 0.38, 0.38, 0.38),
    size = 30)
