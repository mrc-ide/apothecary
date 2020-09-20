# Initial Setup (Run Once)
# drat:::add("mrc-ide")
# source("https://mrc-ide.github.io/didehpc/install")
# install.packages("didehpc")
# remotes::install_github("mrc-ide/provisionr@dev", upgrade = FALSE)
# devtools::install_github("mrc-ide/buildr", upgrade=FALSE)
# countries <- c("AFG", "ARG", "CAN", "DOM", "ETH", "GTM", "HND", "IDN",  "PAK", "ROU", "RUS", "UKR")

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "M:", "//fi--didef3.dide.ic.ac.uk/malaria", "M:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb",
                                  parallel = FALSE, rtools = TRUE)
packages <- c("lubridate", "dplyr", "plyr", "tidyr", "odin", "squire", "apothecary", "dde")


# Creating a Context
sources <- c("MCMC_cluster_function.R")
additional_identifier <- "new"
context_name <- paste0("M:/Charlie/apothecary_runs_", Sys.Date(), additional_identifier)
ctx <- context::context_save(path = context_name,
                             sources = sources,
                             packages = packages,
                             package_sources = provisionr::package_sources(local =
                                                                             c("M:/Charlie/apothecary_0.1.0.zip",
                                                                               "M:/Charlie/dde_1.0.2.zip",
                                                                               "M:/Charlie/odin_1.0.6.zip",
                                                                               "M:/Charlie/odin.js_0.1.8.zip",
                                                                               "M:/Charlie/squire_0.4.35.zip")))

# Configure the Queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# Testing the Running Locally
missing_ISOs <- countries[!(countries %in% names(pars_init))]
missing_countries <- squire::population$country[match(missing_ISOs, squire::population$iso3c)]
included_countries <- countries[countries %in% names(pars_init)]
pars_init <- readRDS("pars_init.rds")
ecdc <- readRDS("ecdc_all.rds")
interventions <- readRDS("google_brt.rds")
countries <- unique(squire::population$iso3c)
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

