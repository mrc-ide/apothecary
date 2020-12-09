# Loading required libraries
library(tidyverse); library(qpdf); library(stringi); library(rnaturalearth); library(ggplot2); library(dplyr);
library(rgdal); library(rgeos); library(sf); library(rnaturalearthdata); library(patchwork); library(rnaturalearthhires)
library(mapproj)

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster =  "fi--dideclusthn",#"fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")

# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/MCMC_cluster_function.R",
             "N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/Cluster_Output_Projectiong_Functions.R")

additional_identifier <- "new"
context_name <- paste0("N:/Charlie/apothecary_runs_", Sys.Date(), additional_identifier)
ctx <- context::context_save(path = context_name,
                             sources = sources,
                             packages = packages,
                             package_sources = provisionr::package_sources(local =
                                                                             c("N:/Charlie/apothecary_fitting/apothecary_0.1.0.zip",
                                                                               "N:/Charlie/apothecary_fitting/dde_1.0.2.zip",
                                                                               "N:/Charlie/apothecary_fitting/odin_1.0.6.zip",
                                                                               "N:/Charlie/apothecary_fitting/squire_0.5.6.zip")))
# Configure the Queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# Loading Up Tasks
filenames <- list.files("N:/Charlie/apothecary_fitting/apothecary_run_results/")
iso <- str_split(filenames, "_")
iso <- unlist(lapply(iso, `[[`, 1))
countries <- countrycode::countrycode(iso, origin = "iso3c", destination = "country.name")
countries <- str_replace(countries, "&", "and")
misspell <- c("Côte d’Ivoire", "Congo - Brazzaville", "Cape Verde", "Kyrgyzstan", "Myanmar (Burma)", "Palestinian Territories", "São Tomé and Príncipe")
indices <- which(countries %in% misspell)
countries[indices] <- c("Cote d'Ivoire", "Republic of the Congo", "Cabo Verde", "Kyrgyz Republic", "Myanmar", "State of Palestine", "Sao Tome and Principe")

for (i in 1:length(iso)) {
  temp <- filenames[grep(iso[i], filenames)]
  pmcmc_res <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", temp))
  index <- squire:::odin_index(pmcmc_res$model)
  past_t <- pmcmc_res$output[, "t", ]
  past_deaths <- apply(pmcmc_res$output[, index$D, ], c(1, 3), sum)
  dims <- dim(pmcmc_res$output)[1]
  pmcmc_res$output <- pmcmc_res$output[(dims-2):dims, , ]
  test <- run$enqueue(cluster_projections(pmcmc_res = pmcmc_res, iso = iso[i], country = countries[i],
                                          number_replicates = 500, timepoints = past_t, cumulative_deaths_data = past_deaths))
  print(i)
}

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
wb <- read.csv("analysis_Figure3/Inputs/World_Bank_Country_Metadata.csv") %>%
  select(ï..country_code, region, income_group) %>%
  rename(iso = ï..country_code)
overall <- overall %>%
  left_join(wb, by = c("country" = "iso")) %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  mutate(low = (1 - (low_drugs_limited/low_nodrugs_limited))/(1 - (low_drugs_unlimited/low_nodrugs_unlimited)),
         high = (1 - (high_drugs_limited/high_nodrugs_limited))/(1 - (high_drugs_unlimited/high_nodrugs_unlimited)))

summary <- overall %>%
  group_by(income_group) %>%
  summarise(mean_low = mean(low),
            mean_high = mean(high),
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
  scale_fill_viridis_c(option = "magma", breaks = c(0, 1), limits = c(0, 1), direction = -1) +
  theme(legend.position = "left")
b <- ggplot(data = world) +
  geom_sf(aes(fill = high)) +
  scale_fill_viridis_c(option = "magma", breaks = c(0, 1), limits = c(0, 1), direction = -1) +
  theme(legend.position = "left")
c <- ggplot(data = overall, aes(x = income_group, y = low, fill = income_group)) +
  geom_boxplot(outlier.alpha = 0) +
  labs(y = "", x = "") +
  scale_x_discrete(labels = c("Low income" = "LIC", "Upper middle income" = "UMIC",
                              "Lower middle income" = "LMIC", "High income" = "HIC")) +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right")
d <- ggplot(data = overall, aes(x = income_group, y = high, fill = income_group)) +
  geom_boxplot(outlier.alpha = 0) +
  labs(y = "", x = "") +
  scale_x_discrete(labels = c("Low income" = "LIC", "Upper middle income" = "UMIC",
                              "Lower middle income" = "LMIC", "High income" = "HIC")) +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right")

a + c + b + d +
  plot_layout(guides = 'auto', widths = c(3, 1))

