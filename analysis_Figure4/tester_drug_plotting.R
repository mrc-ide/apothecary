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
deaths_df <- ggplot(deaths_df) +
  geom_bar(aes(x = drug, y = deaths_averted, fill = effect_type), stat = "identity") +
  scale_fill_manual(values = c("#FFC914", "#17BEBB", "#E4572E"),
                               labels = c("Indirect - Transmission", "Indirect - Healthcare", "Direct")) +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(fill=guide_legend(ncol=2))

plot(seq(1:10), col = hue_pal()(10), pch = 20, cex = 3.5)

IFR <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = IFR, fill = drug), stat = "identity") +
  scale_fill_manual(values = hue_pal()(4)) +
  lims(y = c(0, 0.5)) +
  theme(legend.position = "none")

AR <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = attack_rate, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

prop_full_hosp <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = prop_full_hosp, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

prop_full_ICU <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = prop_full_ICU, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

doses <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = doses, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  theme(legend.position = "none")

deaths_per_dose <- ggplot(drugs_df) +
  geom_bar(aes(x = drug, y = total_deaths_averted/doses, fill = drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  theme(legend.position = "none")

one <- plot_grid(deaths_df, labels = c(''), ncol = 1)
two <- plot_grid(IFR, AR, ncol = 1)
three <- plot_grid(prop_full_hosp, prop_full_ICU, deaths_per_dose, nrow = 1)

four <- plot_grid(one, two, nrow = 1, rel_widths = c(2, 1))
five <- plot_grid(four, three, ncol = 1, rel_heights = c(2, 1))
five

row <- plot_grid(IFR, AR, prop_full_hosp, prop_full_ICU, doses, labels = c('', '', ''), ncol = 2)
plot_grid(middle_set, row, label_size = 30, ncol = 2)

row <- plot_grid(IFR, AR, prop_full_hosp, prop_full_ICU, doses, labels = c('', '', ''), ncol = 2)
middle_set <- plot_grid(deaths_df, labels = c(''), ncol = 1)
plot_grid(middle_set, row, label_size = 30, ncol = 2)

