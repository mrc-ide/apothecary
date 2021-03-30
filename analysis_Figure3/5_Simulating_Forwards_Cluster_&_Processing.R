# Loading required libraries
library(tidyverse); library(qpdf); library(stringi); library(rnaturalearth); library(ggplot2); library(dplyr);
library(rgdal); library(rgeos); library(sf); library(rnaturalearthdata); library(patchwork); library(rnaturalearthhires)
library(mapproj); library(cowplot)

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster =  "fi--didemrchnb", #"fi--dideclusthn",#
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")

# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/MCMC_cluster_function.R",
             "N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/Cluster_Output_Projection_Functions.R")

additional_identifier <- "yooo_kloop_bloop_comeon_try_newbie"
context_name <- paste0("N:/Charlie/apothecary_runs_", Sys.Date(), additional_identifier)
ctx <- context::context_save(path = context_name,
                             sources = sources,
                             packages = packages,
                             package_sources = provisionr::package_sources(local =
                                                                             c("N:/Charlie/apothecary_fitting/apothecary_0.1.0.zip",
                                                                               "N:/Charlie/apothecary_fitting/dde_1.0.3.zip",
                                                                               "N:/Charlie/apothecary_fitting/odin_1.1.8.zip",
                                                                               "N:/Charlie/apothecary_fitting/squire_0.6.4.zip")))
# Configure the Queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# Loading Up Tasks
filenames <- list.files("N:/Charlie/apothecary_fitting/apothecary_run_results/")
filenames <- filenames[!grepl("Smaller", filenames)]
iso <- str_split(filenames, "_")
iso <- unlist(lapply(iso, `[[`, 1))
countries <- countrycode::countrycode(iso, origin = "iso3c", destination = "country.name")
countries <- str_replace(countries, "&", "and")
misspell <- c("Côte d’Ivoire", "Congo - Brazzaville", "Cape Verde", "Kyrgyzstan", "Palestinian Territories", "São Tomé and Príncipe")
indices <- which(countries %in% misspell)
countries[indices] <- c("Cote d'Ivoire", "Republic of the Congo", "Cabo Verde", "Kyrgyz Republic", "State of Palestine", "Sao Tome and Principe")
wb_income <- read.csv("analysis_Figure3/Inputs/gdp_income_group.csv") %>%
  select(country_code, income_group)

for (i in 1:length(iso)) {

  # Getting income strata for each country
  income_strata_spec <- wb_income$income_group[wb_income$country_code == iso[i]]

  # Loading in and processing MCMC results for each country
  temp <- filenames[grep(iso[i], filenames)]
  pmcmc_res <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", temp))
  index <- squire:::odin_index(pmcmc_res$model)
  past_t <- pmcmc_res$output[, "t", ]
  past_deaths <- apply(pmcmc_res$output[, index$D, ], c(1, 3), sum)

  # Saving smaller version of MCMC results to pass to the cluster
  dims <- dim(pmcmc_res$output)[1]
  pmcmc_res$output <- pmcmc_res$output[(dims-2):dims, , ]
  saveRDS(list(pmcmc_res = pmcmc_res,
               past_t = past_t,
               past_deaths = past_deaths),
          paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/Smaller_Versions/small_", temp))

  # Running the projections of drug impact
  test <- run$enqueue(cluster_projections(pmcmc_res = pmcmc_res, iso = iso[i],
                                          country = countries[i], income_strata = income_strata_spec,
                                          number_replicates = 500, timepoints = past_t, cumulative_deaths_data = past_deaths))
  print(i)
}

# running the missing ones
projection_outputs <- list.files("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Outputs/Projections/")
projection_iso <- str_split(projection_outputs, "_")
projection_iso <- unlist(lapply(projection_iso, `[[`, 1))
missing <- iso[!(iso %in% projection_iso)]
rerun_indices <- which(iso %in% missing)

rerun_indices <- rerun_indices[c(1, 2, 4)]
countries <- c("Brunei Darussalam", "Republic of the Congo", "Philippines")
counter <- 1
for (i in rerun_indices) {

  # Getting income strata for each country
  income_strata_spec <- wb_income$income_group[wb_income$country_code == iso[i]]

  # Loading in and processing MCMC results for each country
  temp <- filenames[grep(iso[i], filenames)]
  pmcmc_res <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", temp))
  index <- squire:::odin_index(pmcmc_res$model)
  past_t <- pmcmc_res$output[, "t", ]
  past_deaths <- apply(pmcmc_res$output[, index$D, ], c(1, 3), sum)

  # Saving smaller version of MCMC results to pass to the cluster
  dims <- dim(pmcmc_res$output)[1]
  pmcmc_res$output <- pmcmc_res$output[(dims-2):dims, , ]
  saveRDS(list(pmcmc_res = pmcmc_res,
               past_t = past_t,
               past_deaths = past_deaths),
          paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/Smaller_Versions/small_", temp))

  # Running the projections of drug impact
  test <- run$enqueue(cluster_projections(pmcmc_res = pmcmc_res, iso = iso[i],
                                          country = countries[counter], income_strata = income_strata_spec,
                                          number_replicates = 500, timepoints = past_t, cumulative_deaths_data = past_deaths))
  counter <- counter + 1
  print(i, counter)
}

run$task_status()

# Creating Overall Dataframe
output <- list.files("analysis_Figure3/Outputs/Projections/")
for (i in 1:length(output)) {
  temp <- readRDS(paste0("analysis_Figure3/Outputs/Projections/", output[i]))
  if (i == 1) {
    overall <- temp
  } else {
    overall <- rbind(overall, temp)
  }
}
wb <- read.csv("analysis_Figure3/Inputs/gdp_income_group.csv") %>%
  select(country_code, region, income_group) %>%
  rename(iso = country_code)

overall <- overall %>%
  left_join(wb, by = c("country" = "iso")) %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  mutate(low = (1 - (low_drugs_limited/low_nodrugs_limited))/(1 - (low_drugs_unlimited/low_nodrugs_unlimited)),
         high = (1 - (high_drugs_limited/high_nodrugs_limited))/(1 - (high_drugs_unlimited/high_nodrugs_unlimited)))

deaths_averted <- overall %>%
  select(country, income_group, low_drugs_limited, low_nodrugs_limited) %>%
  mutate(prop_averted = 1 - low_drugs_limited/low_nodrugs_limited) %>%
  group_by(income_group) %>%
  summarise(mean = mean(prop_averted))

summary <- overall %>%
  group_by(income_group) %>%
  summarise(mean_low = median(low),
            mean_high = median(high),
            n = n())

red <- overall %>%
  dplyr::select(country, low, high)

world <- ne_countries(scale = "small", returnclass = "sf")
world <- world %>%
  left_join(red, by = c("iso_a3" = "country"))

# crs available from: https://proj.org/operations/projections/
ggplot(data = world) +
  geom_sf(aes(fill = low)) +
  coord_sf(crs = "+proj=eck4") +
  scale_fill_viridis_c(option = "magma", breaks = c(0, 1), limits = c(0, 1), direction = -1) +
  cowplot::theme_minimal_grid()

ggplot(data = world) +
  geom_sf(aes(fill = low)) +
  coord_sf(crs = "+proj=eck4") +
  scale_fill_viridis_c(option = "magma", breaks = c(0, 1), limits = c(0, 1), direction = -1) +
  theme(legend.position = "left",
        #panel.background = element_rect(fill = "light grey"),
        panel.grid = element_line(color = "white"))

a <- ggplot(data = world) +
  geom_sf(aes(fill = low)) +
  scale_fill_viridis_c(option = "magma", breaks = seq(0, 1, 0.25), limits = c(0, 1), direction = -1) +
  theme(legend.position = "left",
        legend.key.width = unit(0.05, "npc"),
        legend.key.height = unit(0.08, "npc"),
        legend.title = element_blank())
b <- ggplot(data = world) +
  geom_sf(aes(fill = high)) +
  scale_fill_viridis_c(option = "magma", breaks = seq(0, 1, 0.25), limits = c(0, 1), direction = -1) +
  theme(legend.position = "left",
        legend.key.width = unit(0.05, "npc"),
        legend.key.height = unit(0.08, "npc"),
        legend.title = element_blank())

c <- ggplot(data = overall, aes(x = income_group, y = 100 * low, col = income_group)) +
  geom_boxplot(outlier.alpha = 0, size = 1) +
  geom_jitter(data = overall, aes(x = income_group, y = 100 * low, fill = income_group),
              pch = 21, size = 3, width = 0.2) +
  labs(y = "", x = "") +
  scale_x_discrete(labels = c("Low income" = "LIC", "Upper middle income" = "UMIC",
                              "Lower middle income" = "LMIC", "High income" = "HIC")) +
  scale_colour_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) +
  scale_fill_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) +
  labs(y = "% Potential Benefit") +
  scale_y_continuous(position = "right", limits = c(0, 100)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14))
d <- ggplot(data = overall, aes(x = income_group, y = 100 * high, col = income_group)) +
  geom_boxplot(outlier.alpha = 0, size = 1) +
  geom_jitter(data = overall, aes(x = income_group, y = 100 * high, fill = income_group),
              pch = 21, size = 3, width = 0.2) +
  labs(y = "", x = "") +
  scale_x_discrete(labels = c("Low income" = "LIC", "Upper middle income" = "UMIC",
                              "Lower middle income" = "LMIC", "High income" = "HIC")) +
  scale_colour_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) +
  scale_fill_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) +
  labs(y = "% Potential Benefit") +
  scale_y_continuous(position = "right", limits = c(0, 100)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# 10.5 width x 7.5 height
fig3 <- plot_grid(a, c, b, d, ncol = 2, rel_widths = c(2.5, 1), align = 'h', axis = 'tb') +
  draw_plot_label(
    c("A", "B", "C", "D"),
    c(0.01, 0.71, 0.01, 0.71),
    c(1.02, 1.02, 0.55, 0.55),
    size = 30)
ggsave2(file = "analysis_Figure3/Figure_3.pdf", fig3, dpi = 300,
        width = 10.5, height = 7.5)
