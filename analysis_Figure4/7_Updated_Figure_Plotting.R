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
none_high <- run_apothecary(country = "Bhutan", R0 = 2, population = standard_population, contact_matrix_set = standard_matrix,
                           time_period = time, seeding_cases = 20, day_return = TRUE,
                           hosp_bed_capacity = actual_hosp_beds, ICU_bed_capacity = actual_ICU_beds,
                           prop_ox_hosp_beds = actual_prop_ox_hosp_beds, prop_ox_ICU_beds = actual_prop_ox_ICU_beds, MV_capacity = actual_MV_capacity)
index <- apothecary:::odin_index(none_low$model)
none_low_deaths <- max(apply(none_low$output[, index$D], 1, sum))
none_high_deaths <- max(apply(none_high$output[, index$D], 1, sum))

# Plotting Figure 4B for Low R0
type1 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_low.rds")
type1_relevant <- type1[5:10, 19:21]
type1min <- none_low_deaths - type1_relevant[1, 1]
type1_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type1_low_df.rds")$total_deaths_averted
type1_indirect_hc <- 0
type1_indirect_trans <- 0
type1_central <- type1_direct + type1_indirect_hc + type1_indirect_trans
type1max <- none_low_deaths - type1_relevant[6, 3]

type2 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_low.rds")
type2_relevant <- type2[5:10, 19:21]
type2min <- none_low_deaths - type2_relevant[1, 1]
type2_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$direct_deaths_averted
type2_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$indirect_deaths_averted_healthcare
type2_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_low_df.rds")$indirect_deaths_averted_transmission
type2_central <- type2_direct + type2_indirect_hc + type2_indirect_trans
type2max <- none_low_deaths - type2_relevant[6, 3]

type3 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_low.rds")
type3_relevant <- type3[12:17, 19:21]
type3min <- none_low_deaths - type3_relevant[6, 1]
type3_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$direct_deaths_averted
type3_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$indirect_deaths_averted_healthcare
type3_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_low_df.rds")$indirect_deaths_averted_transmission
type3_central <- type3_direct + type3_indirect_hc + type3_indirect_trans
type3max <- none_low_deaths - type3_relevant[1, 3]

type4 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_low.rds")
type4_relevant <- type4[6:16, 6:11]
type4min <- none_low_deaths - type4_relevant[1, 1]
type4_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$direct_deaths_averted
type4_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$indirect_deaths_averted_healthcare
type4_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_low_df.rds")$indirect_deaths_averted_transmission
type4_central <- type4_direct + type4_indirect_hc + type4_indirect_trans
type4max <- none_low_deaths - type4_relevant[11, 6]

type5a <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_low.rds")
type5a_relevant <- type5a[6:16, 6:11]
type5amin <- none_low_deaths - type5a_relevant[11, 1]
type5a_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$direct_deaths_averted
type5a_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$indirect_deaths_averted_healthcare
type5a_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_low_df.rds")$indirect_deaths_averted_transmission
type5a_central <- type5a_direct + type5a_indirect_hc + type5a_indirect_trans
type5amax <- none_low_deaths - type5a_relevant[1, 6]

type5b <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_low.rds")
type5b_relevant <- type5b[6:16, 4:7]
type5bmin <- none_low_deaths - type5b_relevant[11, 1]
type5b_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$direct_deaths_averted
type5b_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$indirect_deaths_averted_healthcare
type5b_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_low_df.rds")$indirect_deaths_averted_transmission
type5b_central <- type5b_direct + type5b_indirect_hc + type5b_indirect_trans
type5bmax <- none_low_deaths - type5b_relevant[1, 4]

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

low1 <- ggplot(df1, aes(x = drug, y = central, fill = drug)) +
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
low2 <- ggplot() +
  geom_bar(data = df2, aes(x = drug, y = deaths_averted, fill = estimate), stat = "identity") +
  geom_errorbar(data = df1, aes(x = drug, ymin = min, ymax = max), width = 0.3) +
  labs(x = "", y = "Proportio of Deaths Averted") +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right", limits = c(0, 1))

heatmap_plot <- function(matrix, deaths, reverse) {
  matrix <- matrix/deaths
  colnames(matrix) <- seq(0, 1, length.out = 21)
  if (reverse) {
    row.names(matrix) <- seq(1, 0, length.out = 21)
  } else {
    row.names(matrix) <- seq(0, 1, length.out = 21)
  }
  matrix <- reshape2::melt(matrix)

  # cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee",
  #                              "#0099dc", "#4ab04a", "#ffd73e"))(40),
  #           colorRampPalette(c("#eec73a", "#e29421", "#e29421",
  #                              "#f05336","#ce472e"), bias=2)(60))
  cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#25A593",
                               "#38AB6F", "#4ab04a", "#A5C444", "#D2CE41", "#ffd73e"))(50),
            colorRampPalette(c("#ffd73e", "#eec73a", "#E8AE2E", "#e29421", "#E9742C",
                               "#f05336", "#ce472e", "#A62E18"))(50))

  plot <- ggplot() +
    geom_tile(data = matrix, aes(Var1, Var2, fill = 1-value)) +
    scale_x_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    scale_fill_gradientn(colours=cols, limits=c(-0.01, 1),
                         breaks=seq(-0.1, 1, by=0.1),
                         guide=guide_colourbar(ticks=T, nbin=50,
                                               barheight=.5, label=T,
                                               barwidth=10)) +
    # scale_fill_gradient2(limits = c(-0.01, 1),
    #                      low="white", mid="grey", high="black",
    #                      midpoint=0.35) +
    theme_cowplot() +
    labs(x = "", y = "") +
    theme(axis.line=element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(vjust = 8),
          axis.text.y = element_blank(), axis.title.y = element_text(vjust = -0.5),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
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
x <- plot_grid(a, c, e, g, i, k)
low <- plot_grid(x, low2, rel_widths = c(2, 1))

# Plotting Figure 4B for High R0
type1 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds")
type1_relevant <- type1[5:10, 19:21]
type1min <- none_high_deaths - type1_relevant[1, 1]
type1_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type1_high_df.rds")$total_deaths_averted
type1_indirect_hc <- 0
type1_indirect_trans <- 0
type1_central <- type1_direct + type1_indirect_hc + type1_indirect_trans
type1max <- none_high_deaths - type1_relevant[6, 3]

type2 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_high.rds")
type2_relevant <- type2[5:10, 19:21]
type2min <- none_high_deaths - type2_relevant[1, 1]
type2_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_high_df.rds")$direct_deaths_averted
type2_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_high_df.rds")$indirect_deaths_averted_healthcare
type2_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type2_high_df.rds")$indirect_deaths_averted_transmission
type2_central <- type2_direct + type2_indirect_hc + type2_indirect_trans
type2max <- none_high_deaths - type2_relevant[6, 3]

type3 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_high.rds")
type3_relevant <- type3[12:17, 19:21]
type3min <- none_high_deaths - type3_relevant[6, 1]
type3_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_high_df.rds")$direct_deaths_averted
type3_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_high_df.rds")$indirect_deaths_averted_healthcare
type3_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type3_high_df.rds")$indirect_deaths_averted_transmission
type3_central <- type3_direct + type3_indirect_hc + type3_indirect_trans
type3max <- none_high_deaths - type3_relevant[1, 3]

type4 <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_high.rds")
type4_relevant <- type4[6:16, 6:11]
type4min <- none_high_deaths - type4_relevant[1, 1]
type4_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_high_df.rds")$direct_deaths_averted
type4_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_high_df.rds")$indirect_deaths_averted_healthcare
type4_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type4_high_df.rds")$indirect_deaths_averted_transmission
type4_central <- type4_direct + type4_indirect_hc + type4_indirect_trans
type4max <- none_high_deaths - type4_relevant[11, 6]

type5a <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_high.rds")
type5a_relevant <- type5a[6:16, 6:11]
type5amin <- none_high_deaths - type5a_relevant[11, 1]
type5a_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_high_df.rds")$direct_deaths_averted
type5a_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_high_df.rds")$indirect_deaths_averted_healthcare
type5a_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5a_high_df.rds")$indirect_deaths_averted_transmission
type5a_central <- type5a_direct + type5a_indirect_hc + type5a_indirect_trans
type5amax <- none_high_deaths - type5a_relevant[1, 6]

type5b <- readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_high.rds")
type5b_relevant <- type5b[6:16, 4:7]
type5bmin <- none_high_deaths - type5b_relevant[11, 1]
type5b_direct <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_high_df.rds")$direct_deaths_averted
type5b_indirect_hc <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_high_df.rds")$indirect_deaths_averted_healthcare
type5b_indirect_trans <- readRDS("analysis_Figure4/Outputs/figure4b_central_estimates/type5b_high_df.rds")$indirect_deaths_averted_transmission
type5b_central <- type5b_direct + type5b_indirect_hc + type5b_indirect_trans
type5bmax <- none_high_deaths - type5b_relevant[1, 4]

drug_type <- c(rep(paste0("Type", seq(1:4)), each = 3), rep("Type5a", 3), rep("Type5b", 3))
estimate_type <- rep(c("min", "central", "max"), 6)
deaths_averted <- c(type1min, type1_central, type1max,
                    type2min, type2_central, type2max,
                    type3min, type3_central, type3max,
                    type4min, type4_central, type4max,
                    type5amin, type5a_central, type5amax,
                    type5bmin, type5b_central, type5bmax)
deaths_averted <- deaths_averted/none_high_deaths
df1 <- data.frame(drug = drug_type, estimate = estimate_type, deaths_averted = deaths_averted) %>%
  pivot_wider(names_from = estimate, values_from = deaths_averted)

high1 <- ggplot(df1, aes(x = drug, y = central, fill = drug)) +
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
deaths_averted <- deaths_averted/none_high_deaths
df2 <- data.frame(drug = drug_type, estimate = estimate_type, deaths_averted = deaths_averted)
df2$estimate <- factor(df2$estimate, levels = c("indirect_trans", "indirect_hc", "direct"))
high2 <- ggplot() +
  geom_bar(data = df2, aes(x = drug, y = deaths_averted, fill = estimate), stat = "identity") +
  geom_errorbar(data = df1, aes(x = drug, ymin = min, ymax = max), width = 0.3) +
  labs(x = "", y = "Proportion of Deaths Averted") +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right", limits = c(0, 1))

a <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type1_high.rds"), none_high_deaths, FALSE)
c <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type2_high.rds"), none_high_deaths, FALSE)
e <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type3_high.rds"), none_high_deaths, TRUE)
g <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type4_high.rds"), none_high_deaths, FALSE)
i <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type5_high.rds"), none_high_deaths, TRUE)
k <- heatmap_plot(readRDS("analysis_Figure4/Outputs/sensitivity_analysis/type6_high.rds"), none_high_deaths, TRUE)
x <- plot_grid(a, c, e, g, i, k)
high <- plot_grid(x, high2, rel_widths = c(2, 1))

plot_grid(low, high, nrow = 2)
