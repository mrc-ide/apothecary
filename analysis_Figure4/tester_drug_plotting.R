# practice plotting
library(ggplot2); library(tidyverse); library(dplyr); library(cowplot)

# load in data
x <- read.csv("tester_drug_effects.csv") %>%
  select(Drug, Direct_Deaths_Averted, Indirect_Deaths_Averted_Healthcare, Indirect_Deaths_Averted_Transmission) %>%
  filter(Drug != "None") %>%
  pivot_longer(cols = Direct_Deaths_Averted:Indirect_Deaths_Averted_Transmission, names_to = "effect_type", values_to = "deaths_averted")

a <- ggplot(x) +
  geom_bar(aes(x = Drug, y = deaths_averted, fill = effect_type), stat = "identity") +
  scale_fill_manual(values = c("#CD99EF", "#DD7596", "#6A94B7")) +
  scale_fill_manual(values = c("#f3eec6", "#fd9800", "#cdd84b")) +
  scale_fill_manual(values = c("#ede4e5", "#e8d3b4", "#ff896b")) +
  scale_fill_manual(values = c("#C3DDCB", "#9FCAB7", "#96C28D"),
                    labels = c("Direct", "Indirect - Healthcare", "Indirect - Transmission") )  +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill=guide_legend(ncol=2))

y <- read.csv("tester_drug_effects.csv") %>%
  select(-Direct_Deaths_Averted, -Indirect_Deaths_Averted_Healthcare,
         -Indirect_Deaths_Averted_Transmission, -Total_Deaths_Averted, -Deaths) %>%
  filter(Drug != "None")

colnames(y)
b <- ggplot(y) +
  geom_bar(aes(x = Drug, y = IFR, fill = Drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

c <- ggplot(y) +
  geom_bar(aes(x = Drug, y = Prop_full_hosp, fill = Drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

d <- ggplot(y) +
  geom_bar(aes(x = Drug, y = Prop_full_ICU, fill = Drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

e <- ggplot(y) +
  geom_bar(aes(x = Drug, y = AR, fill = Drug), stat = "identity") +
  scale_fill_manual(values = c("#FF784F", "#DD7596", "#5299D3", "#DBC453")) +
  lims(y = c(0, 1)) +
  theme(legend.position = "none")

row <- plot_grid(b, c, d, e, labels = c('', '', ''), ncol = 2)
middle_set <- plot_grid(a, labels = c(''), ncol = 1)
plot_grid(middle_set, row, label_size = 30, ncol = 2)

