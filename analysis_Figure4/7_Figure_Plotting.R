# Loading required libraries
library(ggplot2); library(tidyverse); library(dplyr); library(cowplot); library(scales); library(viridis)

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

# Run Model Runs
none_low <- run_apothecary(country = "Bhutan", R0 = 1.35, population = standard_population, contact_matrix_set = standard_matrix,
                           time_period = time, seeding_cases = 20, day_return = TRUE,
                           hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                           prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
index <- apothecary:::odin_index(none_low$model)
none_low_deaths <- max(apply(none_low$output[, index$D], 1, sum))
none_high_deaths <- max(apply(none_high$output[, index$D], 1, sum))

# Plotting Figure 4B
type1 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_low.rds")
type1_relevant <- type1[6:10, 19:21]
type1min <- none_low_deaths - type1_relevant[1, 1]
type1_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type1_low_df.rds")$total_deaths_averted
type1_indirect_hc <- 0
type1_indirect_trans <- 0
type1_central <- type1_direct + type1_indirect_hc + type1_indirect_trans
type1max <- none_low_deaths - type1_relevant[5, 3]

type2 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_low.rds")
type2_relevant <- type2[6:10, 19:21]
type2min <- none_low_deaths - type2_relevant[1, 1]
type2_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$direct_deaths_averted
type2_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$indirect_deaths_averted_healthcare
type2_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$indirect_deaths_averted_transmission
type2_central <- type2_direct + type2_indirect_hc + type2_indirect_trans
type2max <- none_low_deaths - type2_relevant[5, 3]

type3 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_low.rds")
type3_relevant <- type3[11:17, 19:21]
type3min <- none_low_deaths - type3_relevant[7, 1]
type3_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$direct_deaths_averted
type3_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$indirect_deaths_averted_healthcare
type3_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$indirect_deaths_averted_transmission
type3_central <- type3_direct + type3_indirect_hc + type3_indirect_trans
type3max <- none_low_deaths - type3_relevant[1, 3]

type4 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_low.rds")
type4_relevant <- type4[8:14, 8:14]
type4min <- none_low_deaths - type4_relevant[1, 1]
type4_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$direct_deaths_averted
type4_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$indirect_deaths_averted_healthcare
type4_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$indirect_deaths_averted_transmission
type4_central <- type4_direct + type4_indirect_hc + type4_indirect_trans
type4max <- none_low_deaths - type4_relevant[7, 7]

duration_prop <- seq(0.01, 1, length.out = 21)
type3_effectiveness <- 1/duration_prop
duration_prop[13:17]
type3_effectiveness[13:17]

type5a <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_low.rds")
type5a_relevant <- type5a[13:17, 8:13]
type5amin <- none_low_deaths - type5a_relevant[7, 1]
type5a_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$direct_deaths_averted
type5a_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$indirect_deaths_averted_healthcare
type5a_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$indirect_deaths_averted_transmission
type5a_central <- type5a_direct + type5a_indirect_hc + type5a_indirect_trans
type5amax <- none_low_deaths - type5a_relevant[1, 6]

type5b <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_low.rds")
type5b_relevant <- type5b[13:17, 8:13]
type5bmin <- none_low_deaths - type5b_relevant[7, 1]
type5b_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$direct_deaths_averted
type5b_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$indirect_deaths_averted_healthcare
type5b_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$indirect_deaths_averted_transmission
type5b_central <- type5b_direct + type5b_indirect_hc + type5b_indirect_trans
type5bmax <- none_low_deaths - type5b_relevant[1, 6]

drug_type <- c(rep(paste0("Type", seq(1:4)), each = 3), rep("Type5a", 3), rep("Type5b", 3))
estimate_type <- rep(c("min", "central", "max"), 6)
deaths_averted <- c(type1min, type1_central, type1max,
                    type2min, type2_central, type2max,
                    type3min, type3_central, type3max,
                    type4min, type4_central, type4max,
                    type5amin, type5a_central, type5amax,
                    type5bmin, type5b_central, type5bmax)
deaths_averted <- deaths_averted/none_low_deaths
df1 <- data.frame(drug = drug_type, estimate = estimate_type, deaths_averted = deaths_averted) %>%
  pivot_wider(names_from = estimate, values_from = deaths_averted)

ggplot(df1, aes(x = drug, y = central, fill = drug)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3) +
  labs(x = "", y = "Deaths Averted") +
  theme(legend.position = "none")

drug_type <- c(rep(paste0("Type", seq(1:4)), each = 3), rep("Type5a", 3), rep("Type5b", 3))
estimate_type <- rep(c("direct", "indirect_hc", "indirect_trans"), 6)
deaths_averted <- c(type1_direct, type1_indirect_hc, type1_indirect_trans,
                    type2_direct, type2_indirect_hc, type2_indirect_trans,
                    type3_direct, type3_indirect_hc, type3_indirect_trans,
                    type4_direct, type4_indirect_hc, type4_indirect_trans,
                    type5a_direct, type5a_indirect_hc, type5a_indirect_trans,
                    type5b_direct, type5b_indirect_hc, type5b_indirect_trans)
deaths_averted <- deaths_averted/none_low_deaths
df2 <- data.frame(drug = drug_type, estimate = estimate_type, deaths_averted = deaths_averted)
df2$estimate <- factor(df2$estimate, levels = c("indirect_trans", "indirect_hc", "direct"))
ggplot() +
  geom_bar(data = df2, aes(x = drug, y = deaths_averted, fill = estimate), stat = "identity") +
  geom_errorbar(data = df1, aes(x = drug, ymin = min, ymax = max), width = 0.3) +
  labs(x = "", y = "Proportio of Deaths Averted") +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right", limits = c(0, 1))

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
colc_low <- readRDS("analysis_Figure4/Outputs/colchicine_low_df.rds")
colc_high <- readRDS("analysis_Figure4/Outputs/colchicine_high_df.rds")
mab_symp_low <- readRDS("analysis_Figure4/Outputs/mab_post_symptom_low_df.rds")
mab_symp_high <- readRDS("analysis_Figure4/Outputs/mab_post_symptom_high_df.rds")
mab_exp_low <- readRDS("analysis_Figure4/Outputs/mab_post_exposure_low_df.rds")
mab_exp_high <- readRDS("analysis_Figure4/Outputs/mab_post_exposure_high_df.rds")

ivm_low$drug <- "Ivermectin"
ivm_high$drug <- "Ivermectin"
mab_symp_low$drug <- "mAb Post-Sympt"
mab_symp_high$drug <- "mAb Post-Sympt"
mab_exp_low$drug <- "mAb Post-Exp"
mab_exp_high$drug <- "mAb Post-Exp"
colc_low$drug <- "Colchicine"
colc_high$drug <- "Colchicine"

drugs_df <- rbind(none_low, none_high, dexy_low, dexy_high, rem_low, rem_high, ivm_low, ivm_high,
                  colc_low, colc_high, mab_symp_low, mab_symp_high, mab_exp_low, mab_exp_high) %>%
  filter(drug != "None")
drugs_df$drug <- factor(drugs_df$drug, levels = c("Dexamethasone", "Ivermectin", "Remdesivir", "Colchicine", "mAb Post-Sympt", "mAb Post-Exp"))

deaths_df <- drugs_df %>%
  select(drug, R0, direct_deaths_averted, indirect_deaths_averted_transmission, indirect_deaths_averted_healthcare) %>%
  pivot_longer(cols = c(-drug, -R0), names_to = "effect_type", values_to = "deaths_averted")
deaths_df$effect_type <- factor(deaths_df$effect_type, levels = c("indirect_deaths_averted_transmission",
                                                                  "indirect_deaths_averted_healthcare",
                                                                  "direct_deaths_averted"))

lowR0_deaths <- deaths_df %>%
  filter(R0 == "low")
lowR0_deaths_plot <- ggplot(lowR0_deaths) +
  geom_bar(width=0.9, aes(x = drug, y = deaths_averted,
                          fill = interaction(drug, effect_type, R0)), stat = "identity") +
  labs(x = "", y = "Total Deaths Averted") +
  # scale_x_discrete(labels = c("Dexamethasone", "Ivermectin(?)", "Remdesivir", "mAbs")) +
  scale_y_continuous(position = "left") +
  theme(panel.margin = grid::unit(0.2, "lines"),
        legend.position = "none", axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        strip.background = element_blank(), strip.text = element_blank()) +
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "red", # R0 indirect transmission
                               "pink", "#E4EFF1", "pink", "pink", "green", "green", # R0 indirect hosp
                               "pink", "#D9D3D6", "#C2E4FF", "#CADFE3", "blue", "blue")) # R0 direct

highR0_deaths <- deaths_df %>%
  filter(R0 == "high")
highR0_deaths_plot <- ggplot(highR0_deaths) +
  geom_bar(width=0.9, aes(x = drug, y = deaths_averted,
                          fill = interaction(drug, effect_type, R0)), stat = "identity") +
  labs(x = "", y = "Total Deaths Averted") +
  # scale_x_discrete(labels = c("Dexamethasone", "Ivermectin(?)", "Remdesivir", "mAbs")) +
  scale_y_continuous(position = "left") +
  theme(panel.margin = grid::unit(0.2, "lines"),
        legend.position = "none", axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        strip.background = element_blank(), strip.text = element_blank()) +
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "red", # R0 indirect transmission
                               "pink", "#E4EFF1", "pink", "pink", "green", "green", # R0 indirect hosp
                               "pink", "#D9D3D6", "#C2E4FF", "#CADFE3", "blue", "blue")) # R0 direct

# Overall Figure Plotting
temp1 <- plot_grid(lowR0_deaths_plot, highR0_deaths_plot, ncol = 1, rel_heights = c(1, 1))
temp2 <- plot_grid(AB, temp1, ncol = 2, rel_widths = c(1, 1))
fig4 <- temp2 +
  draw_plot_label(
    c("A", "B", "C", "D"),
    c(-0.02, -0.02, 0.48, 0.48),
    c(1.02, 0.58, 1.02, 0.58),
    size = 30)
ggsave2(file = "analysis_Figure4/new_Figure_4.pdf", fig4, dpi = 300,
        width = 9, height = 8)



type1_low <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_low.rds")
type1_low <- type1_low/none_low_deaths
colnames(type1_low) <- seq(0, 1, length.out = 21)
row.names(type1_low) <- seq(0, 1, length.out = 21)
type1_low <- reshape2::melt(type1_low)
ggplot() +
  geom_tile(data = type1_low, aes(Var1, Var2, fill = 1-value)) +
  scale_x_continuous(limits = c(-0.1, 1.1), expand = c(0.04, 0)) +
  scale_y_continuous(limits = c(-0.1, 1.1), expand = c(0.04, 0)) +
  scale_fill_gradientn(colours = c("#C5A2A6", "#BFB6BB", "#B7C3C9",
                                   "#AFD0D6", "#AAD4EB", "#D1E7F3"),
                       limits = c(-0.01, 1)) +
  # scale_fill_viridis(option="magma", limits = c(-0.01, 1), breaks = 0.5, direction = 1) +
  # scale_fill_gradient2(limits = c(-0.01, 1),
  #                      low="navy", mid="white", high="red",
  #                      midpoint=0.25) +
  theme_cowplot() +
  labs(x = "Drug Coverage", y = "Drug Effectiveness") +
  theme(axis.line=element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(vjust = 8),
        axis.text.y = element_blank(), axis.title.y = element_text(vjust = -0.5),
        axis.ticks = element_blank()) +
  annotate("text", x = -0.05, y = seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
           hjust = 1, size = 4.5) +
  annotate("text", y = -0.05, x = seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
           hjust = 1, size = 4.5)

heatmap_plot <- function(matrix, deaths, reverse) {
  matrix <- matrix/deaths
  colnames(matrix) <- seq(0, 1, length.out = 21)
  if (reverse) {
    row.names(matrix) <- seq(1, 0, length.out = 21)
  } else {
    row.names(matrix) <- seq(0, 1, length.out = 21)
  }
  matrix <- reshape2::melt(matrix)
  plot <- ggplot() +
    geom_tile(data = matrix, aes(Var1, Var2, fill = 1-value)) +
    #geom_raster(data = matrix, aes(Var1, Var2, fill = 1-value), interpolate = TRUE) +
    scale_x_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    #scale_fill_viridis(option="magma", limits = c(-0.01, 1), direction = 1) +
    # scale_fill_gradient2(limits = c(-0.01, 1),
    #                      low="navy", mid="white", high="red",
    #                      midpoint=0.25) +
    scale_fill_gradientn(colours = c("#C5A2A6", "#BFB6BB", "#B7C3C9",
                                     "#AFD0D6", "#AAD4EB", "#D1E7F3"),
                         limits = c(-0.01, 1)) +
    theme_cowplot() +
    labs(x = "", y = "") +
    theme(axis.line=element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(vjust = 8),
          axis.text.y = element_blank(), axis.title.y = element_text(vjust = -0.5),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
          #plot.margin = margin(1, 1, 1, 1)) +
    annotate("text", x = -0.03, y = seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
             hjust = 1, size = 4.5) +
    annotate("text", y = -0.05, x = 0.05 +seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
             hjust = 1, size = 4.5)
  return(plot)
}

a <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_low.rds"), none_low_deaths, FALSE)
c <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_low.rds"), none_low_deaths, FALSE)
e <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_low.rds"), none_low_deaths, TRUE)
g <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_low.rds"), none_low_deaths, FALSE)
i <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_low.rds"), none_low_deaths, TRUE)
k <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_low.rds"), none_low_deaths, TRUE)
plot_grid(a, c, e, g, i, k)



# b <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds"), none_high_deaths, FALSE)
# t <- plot_grid(a, b, align = 'vh',  labels = c("Low Type 1", "High Type 1"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(t, legend_b, ncol = 1, rel_heights = c(1, .1))
#
#
# d <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_high.rds"), none_high_deaths, FALSE)
# u <- plot_grid(c, d, align = 'vh',  labels = c("Low Type 2", "High Type 2"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(u, legend_b, ncol = 1, rel_heights = c(1, .1))
#
# f <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_high.rds"), none_high_deaths, TRUE)
# v <- plot_grid(e, f, align = 'vh',  labels = c("Low Type 3", "High Type 3"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(v, legend_b, ncol = 1, rel_heights = c(1, .1))
#
# h <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_high.rds"), none_high_deaths, FALSE)
# w <- plot_grid(g, h, align = 'vh',  labels = c("Low Type 4", "High Type 4"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(w, legend_b, ncol = 1, rel_heights = c(1, .1))
#
# j <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_high.rds"), none_high_deaths, TRUE)
# x <- plot_grid(i, j, align = 'vh',  labels = c("Low Type 5", "High Type 5"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(x, legend_b, ncol = 1, rel_heights = c(1, .1))
#
# l <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_high.rds"), none_high_deaths, TRUE)
# y <- plot_grid(k, l, align = 'vh',  labels = c("Low Type 6", "High Type 6"), hjust = -1, nrow = 1)
# legend_b <- get_legend(
#   a +
#     guides(fill = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# plot_grid(y, legend_b, ncol = 1, rel_heights = c(1, .1))
#
#
# type1_high <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds")
# type1_high <- type1_high/none_high_deaths
# colnames(type1_high) <- paste0("cov_", seq(1:19))
# row.names(type1_high) <- paste0("eff_", seq(1:19))
# type1_high <- reshape2::melt(type1_high)
# ggplot(type1_high, aes(Var1, Var2, fill = 1-value)) +
#   geom_raster(interpolate = TRUE) +
#   scale_fill_gradient2(limits = c(-0.01, 1),
#                       low="navy", mid="white", high="red",
#                       midpoint=0.25) +
#   theme_classic()
#
#
# type1_high <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds")
