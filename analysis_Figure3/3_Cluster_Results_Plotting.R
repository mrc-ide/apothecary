# Loading required libraries
library(tidyverse)

# Sourcing required functions
source("analysis_Figure3/Functions/Cluster_Output_Plotting_Functions.R")

# Load MCMC output
out <- readRDS("N:/Charlie/apothecary_fitting/apothecary_run_results/ISL_2020-11-16_20000_iterations.rds")
country <- "Iceland"
iso3c <- "ISL"
date <- as.Date("2020-11-16")
data <- out$pmcmc_results$inputs$data
interventions <- readRDS("analysis_Figure3/Inputs/google_brt.rds")

# Plotting MCMC output
pdf(file = "analysis_Figure3/pdf.pdf", height = 8, width = 16)
overall_plot(out)
overall_plot(out)
dev.off()
