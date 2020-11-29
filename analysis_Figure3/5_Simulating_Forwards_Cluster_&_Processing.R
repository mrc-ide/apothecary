# Loading required libraries
library(tidyverse); library(qpdf)

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster =  "fi--dideclusthn",#"fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")


# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/MCMC_cluster_function.R",
             "N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/Cluster_Output_Projectiong_Functions.R")

additional_identifier <- ""
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



# Loading apothecary
devtools::load_all()

# Sourcing required functions
source("analysis_Figure3/Functions/Cluster_Output_Plotting_Functions.R")
source("analysis_Figure3/Functions/Cluster_Output_Projectiong_Functions.R")

# Loading in country dataframe
country <- "France"
iso3c <- "FRA"
filenames <- list.files("N:/Charlie/apothecary_fitting/apothecary_run_results/")
filename <- filenames[grep(iso3c, filenames)]
pmcmc_res <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", filename))
data <- pmcmc_res$pmcmc_results$inputs$data

# Plotting Model Output & Data
deaths <- deaths_extraction(pmcmc_res)
daily_death_summary <- deaths %>%
  group_by(date) %>%
  summarise(median_deaths = median(deaths, na.rm = TRUE),
            lower_deaths = quantile(deaths, 0.05, na.rm = TRUE),
            upper_deaths = quantile(deaths, 0.95, na.rm = TRUE))
ggplot() +
  geom_line(data = deaths, aes(x = as.Date(date), y = deaths, group = replicate), alpha = 0.2, col = "grey") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = median_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = lower_deaths), col = "black") +
  geom_line(data = daily_death_summary, aes(x = as.Date(date), y = upper_deaths), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths, col = "red"))

# Comparing Counterfactual Drug Availability at Outset
all_deaths <- past_assess(pmcmc_res,
                          drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                          drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                          drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64)
death_summary <- all_deaths %>%
  group_by(date, scenario) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95))
ggplot() +
  geom_line(data = all_deaths, aes(x = as.Date(date), y = value, col = scenario, group = interaction(replicate, scenario)), alpha = 0.2) +
  geom_line(data = death_summary, aes(x = as.Date(date), y = median, group = scenario), col = "black") +
  geom_line(data = death_summary, aes(x = as.Date(date), y = lower, group = scenario), col = "black") +
  geom_line(data = death_summary, aes(x = as.Date(date), y = upper, group = scenario), col = "black") +
  theme(legend.position = "none") +
  geom_point(data = data, aes(x = date, y = deaths), col = "black")

# Projecting Forwards Under Various Scenarios
actual_hosp <- get_hosp_bed_capacity(country)
actual_ICU <- get_ICU_bed_capacity(country)
end_date <- names(pmcmc_res$output[, 1, 1])[length(names(pmcmc_res$output[, 1, 1]))]

# Low R0 Scenarios
lowR0_no_drugs_unlimited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

lowR0_drugs_unlimited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

lowR0_no_drugs_limited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

lowR0_drugs_limited <- future_projection(pmcmc_res, 300, R0 = 1.35, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

a <- lowR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
b <- lowR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
c <- lowR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
d <- lowR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

b/a
d/c

# High R0 Scenarios
highR0_no_drugs_unlimited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_drugs_unlimited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = 100000000, ICU_bed_capacity = 100000000,
                                           drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                           drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                           drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_no_drugs_limited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

highR0_drugs_limited <- future_projection(pmcmc_res, 200, R0 = 2, hosp_bed_capacity = actual_hosp, ICU_bed_capacity = actual_ICU,
                                         drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_prop_treat = 1, drug_11_GetOx_effect_size = 0.82,
                                         drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_prop_treat = 1, drug_12_GetOx_effect_size = 0.64,
                                         drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_prop_treat = 1, drug_13_GetOx_GetMV_effect_size = 0.64) %>%
  mutate(projection_deaths = ifelse(is.na(projection_deaths), 0, projection_deaths))

e <- highR0_no_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
f <- highR0_drugs_unlimited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
g <- highR0_no_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))
h <- highR0_drugs_limited %>% filter(date > end_date) %>% group_by(replicate) %>% summarise(total = sum(projection_deaths)) %>% ungroup() %>% summarise(mean = mean(total))

f/e
h/g

