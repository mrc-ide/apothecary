# Initial Setup (Run Once)
# drat:::add("mrc-ide")
# source("https://mrc-ide.github.io/didehpc/install")
# install.packages("didehpc")
# remotes::install_github("mrc-ide/provisionr@dev", upgrade = FALSE)
# devtools::install_github("mrc-ide/buildr", upgrade=FALSE)
# countries <- c("AFG", "ARG", "CAN", "DOM", "ETH", "GTM", "HND", "IDN",  "PAK", "ROU", "RUS", "UKR")

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster =  "fi--dideclusthn",#"fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "tidyr", "odin", "squire", "apothecary", "dde")


# Creating a Context
sources <- c("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Functions/MCMC_cluster_function.R")
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

# Testing the Running Locally
pars_init <- readRDS("analysis_Figure3/Inputs/pars_init.rds")
ecdc <- readRDS("analysis_Figure3/Inputs/ecdc_all.rds")
worldometer <- readRDS("analysis_Figure3/Inputs/worldometers_all.rds")
mortality_data <- list(ecdc = ecdc, worldometer = worldometer)
interventions <- readRDS("analysis_Figure3/Inputs/google_brt.rds")

# Establishing Which Countries to Run (and Track Missing Ones)
countries <- names(unlist(lapply(interventions, length))[unlist(lapply(interventions, length)) != 0]) # countries for which we have mobility data
inits_countries <- names(pars_init)
countries <- countries[countries %in% inits_countries]
yes_death_countries <- ecdc %>%
  dplyr::group_by(Region, countryterritoryCode) %>%
  summarise(total_deaths = sum(deaths)) %>%
  filter(total_deaths != 0)
countries <- countries[countries %in% yes_death_countries$countryterritoryCode]

# Running Countries Locally to Check They Work
for (i in 1:length(countries)) {
  test <- run_apothecary_MCMC(country = countries[i], date = "2020-11-16", pars_init = pars_init,
                              mortality_data = mortality_data, interventions = interventions,
                              n_mcmc = 2, replicates = 2, healthcare = "unlimited", n_chains = 1, gibbs = TRUE)
  print(i)
}

# Running the Fitting for Every Country
for (i in 1:length(countries)) {
  test <- run$enqueue(run_apothecary_MCMC(country = countries[i], date = "2020-11-16", pars_init = pars_init,
                                          mortality_data = mortality_data, interventions = interventions,
                                          n_mcmc = 20000, replicates = 500, healthcare = "unlimited", n_chains = 1, gibbs = TRUE))
  print(i)
}
table(unname(run$task_status()), useNA = "ifany")


# Rerunning the initial failures
x <- run$task_times()

reruns <- which(unname(run$task_status() == "PENDING"))[33:42] - 132
for (i in reruns) {
  test <- run$enqueue(run_apothecary_MCMC(country = countries[i], date = "2020-11-16", pars_init = pars_init,
                                          mortality_data = mortality_data, interventions = interventions,
                                          n_mcmc = 20000, replicates = 500, healthcare = "unlimited", n_chains = 1, gibbs = TRUE))
  print(i)
}
table(unname(run$task_status()), useNA = "ifany")

reruns <- c("ATG", "CHE", "COD", "IND", "ITA", "JAM", "JPN", "PAN", "TTO")
for (i in seq_along(reruns)) {
  test <- run$enqueue(run_apothecary_MCMC(country = reruns[i], date = "2020-11-16", pars_init = pars_init,
                                          mortality_data = mortality_data, interventions = interventions,
                                          n_mcmc = 50000, replicates = 500, healthcare = "unlimited", n_chains = 1))
  print(i)
}
table(unname(run$task_status()), useNA = "ifany")

# Finding which ones just haven't run then rerunning
successful_countries <- substring(list.files("N:/Charlie/apothecary_run_results/"), first = 1, last = 3)
table(successful_countries)
failed_countries <- included_countries[!(included_countries %in% successful_countries)]
for (i in 1:length(failed_countries)) {
  test <- run$enqueue(run_apothecary_MCMC(country = failed_countries[i], pars_init = pars_init, ecdc = ecdc,
                                          interventions = interventions, n_mcmc = 50000, run_identifier = 1))
  print(failed_countries[i])
}

table(unname(run$task_status()))



x <- run$task_times()


non_cluster_included_countries <- included_countries[which(is.na(x$started))]
non_cluster_ids <- x$task_id[which(is.na(x$started))]
run$task_status(non_cluster_ids)
for (i in 1:length(included_countries)) {
  test <- run_apothecary_MCMC(country = included_countries[i], pars_init = pars_init, ecdc = ecdc,
                              interventions = interventions, n_mcmc = 4, run_identifier = 1)
  print(i)
}

# Running the Fitting for Every Country
for (i in 1:length(included_countries)) {
  test <- run$enqueue(run_apothecary_MCMC(country = included_countries[i], pars_init = pars_init, ecdc = ecdc,
                                          interventions = interventions, n_mcmc = 50000, run_identifier = 2))
  print(i)
}





non_cluster_included_countries <- included_countries[which(is.na(x$started))]
non_cluster_ids <- x$task_id[which(is.na(x$started))]
run$task_status(non_cluster_ids)
table(unname(run$task_status()), useNA = "ifany")

which(unname(run$task_status() == "ERROR"))
included_countries[78]
run$task_status(run$task_list()[78])
run$task_result(run$task_list()[78])


x <- which(unname(run$task_status() == "PENDING"))
run$task_status(run$task_list()[x])



x <- run$task_times()
sum(!is.na(x$started))
x <- run$task_list()
run$task_status()


run$task_status("ba08bb572912d521c5234b1972991d9f")
run$task_result("ba08bb572912d521c5234b1972991d9f")

