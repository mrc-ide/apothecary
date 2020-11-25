# Loading required libraries
library(tidyverse)

# Sourcing required functions
source("analysis_Figure3/Functions/Cluster_Output_Plotting_Functions.R")

# Load MCMC output
out <- readRDS("N:/Charlie/apothecary_fitting/apothecary_run_results/BRA_2020-11-16_50000_iterations.rds")
country <- "Brazil"
iso3c <- "BRA"
date <- as.Date("2020-11-16")
data <- out$pmcmc_results$inputs$data
interventions <- readRDS("analysis_Figure3/Inputs/google_brt.rds")

# Plotting MCMC output
g1 <- simple_pmcmc_plot(out) +
  theme(text = element_text(size = 2))

title <- cowplot::ggdraw() +
  cowplot::draw_label(
    paste0(country, ",", iso3c),
    fontface = 'bold',
    x = 0.5
  )

line <- ggplot() + cowplot::draw_line(x = 0:10, y=1) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

line_v <- ggplot() + cowplot::draw_line(y = 0:10, x=1) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

header <- cowplot::plot_grid(title, line, ncol = 1)

index <- squire:::odin_index(out$model)
forecast <- 0
date_0 <- date

out$parameters <- out$raw_parameters
out$parameters$day_return <- TRUE
out$parameters$replicates <- 1

suppressMessages(suppressWarnings(
  d <- deaths_plot_single(
    out = out, data = data, date = date,
    date_0 = date_0, forecast = forecast,
    single = TRUE, full = TRUE) +
    theme(legend.position = "none")))

date_Meff_change <- out$pmcmc_results$inputs$Rt_args$date_Meff_change
Rt_rw_duration <- out$pmcmc_results$inputs$Rt_args$Rt_rw_duration
rw_needed <- length(grep("Rt_rw", names(out$pmcmc_results$results)))

intervention <- intervention_plot_google(
  interventions[[iso3c]], date, data,
  forecast,
  start_date = min(as.Date(out$replicate_parameters$start_date))) +
  geom_vline(xintercept = as.Date(date_Meff_change) + seq(Rt_rw_duration, Rt_rw_duration*rw_needed, by = Rt_rw_duration),
             linetype = "dashed") + xlab("") + theme(axis.text.x = element_text(angle=45, vjust = 0.5))

rtp <- rt_plot(out)$plot

date_range <- as.Date(c(min(as.Date(out$replicate_parameters$start_date)),date_0))
suppressMessages(suppressWarnings(
  bottom <- cowplot::plot_grid(
    intervention + scale_x_date(date_breaks = "1 month", date_labels = "%b" ,limits = date_range),
    d + scale_x_date(date_breaks = "1 month", date_labels = "%b" ,limits = date_range),
    rtp + scale_x_date(date_breaks = "1 month", date_labels = "%b" ,limits = date_range),
    ncol=1,
    rel_heights = c(0.4,0.6,0.6, 0.4))
))

combined <- cowplot::plot_grid(header,
                               cowplot::plot_grid(g1, line_v, bottom, ncol = 3, rel_widths = c(3,0.1,1)),
                               ncol=1, rel_heights = c(1,15))
combined
