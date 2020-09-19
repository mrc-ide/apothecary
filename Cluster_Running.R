# Initial Setup (Run Once)
# drat:::add("mrc-ide")
# source("https://mrc-ide.github.io/didehpc/install")
# install.packages("didehpc")
# remotes::install_github("mrc-ide/provisionr@dev", upgrade = FALSE)
# devtools::install_github("mrc-ide/buildr", upgrade=FALSE)

# Setting Up Cluster
loc <- didehpc::path_mapping("location", "M:", "//fi--didef3.dide.ic.ac.uk/malaria", "M:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb",
                                  parallel = FALSE, template = "12Core", cores = 12, rtools = TRUE)
packages <- c("lubridate", "dplyr", "plyr", "tidyr", "odin", "squire", "apothecary", "dde")


# Creating a Context
sources <- c("MCMC_cluster_function.R")
additional_identifier <- ""
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

# Tester
pars_init <- readRDS("pars_init.rds")
ecdc <- readRDS("ecdc_all.rds")
interventions <- readRDS("google_brt.rds")
test <- run$enqueue(run_apothecary_MCMC(pars_init, ecdc, interventions, 250, 1))

test$context_id()
test$times()
test$log()
test$status()
x <- test$result()

dim(x$output)

