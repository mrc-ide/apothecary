# Loading required libraries
library(tidyverse); library(qpdf)

# Sourcing required functions
source("analysis_Figure3/Functions/Cluster_Output_Plotting_Functions.R")

# Loading in required files and creating relevant dataframes
filenames <- list.files("N:/Charlie/apothecary_fitting/apothecary_run_results/")

# Plotting MCMC output
date <- as.Date("2021-05-03")
interventions <- readRDS("analysis_Figure3/Inputs/google_brt.rds")
pdf_names <- c()
for (i in 1:length(filenames)) {
  out <- readRDS(paste0("N:/Charlie/apothecary_fitting/apothecary_run_results/", filenames[i]))
  data <- out$pmcmc_results$inputs$data
  iso3c <- strsplit(filenames[i], "_")[[1]][1]
  country <- countrycode::countrycode(iso3c, origin = "iso3c", destination = "country.name")
  temp_name <- paste0(country, "_", date, ".pdf")
  pdf_names <- c(pdf_names, temp_name)
  x <- overall_plot(out)
  ggsave(paste0("analysis_Figure3/Outputs/", temp_name), width=20, height=8, x)
  print(i)
  rm(out)
}

# -28 is to remove sao tome which has weird letters and doesn't combine nicely
big_pdf <- qpdf::pdf_combine(paste0("analysis_Figure3/Outputs/", pdf_names[-c(33, 137)])) #, output = "analysis_Figure3/Outputs/combined_big_boy.pdf"))

lapply(out, object.size)
lapply(out$pmcmc_results, object.size)


out$replicate_parameters
