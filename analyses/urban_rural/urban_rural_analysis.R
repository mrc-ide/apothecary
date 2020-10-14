library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)

devtools::load_all()

#Read in data
urban_rural_oxy_data <- read.csv("data-raw/urban_rural_oxygen_availability_split.csv", stringsAsFactors = T)
urban_rural_oxy_data$setting_subcategory <- factor(urban_rural_oxy_data$setting_subcategory,
                                                   levels = c("Rural", "All", "Urban"))
urban_rural_oxy_data <- urban_rural_oxy_data[order(urban_rural_oxy_data$setting_subcategory), ]

median_oxy <- subset(urban_rural_oxy_data, q == 0.5)

#Set up scenarios
oxygen_scenario <- c("No O2",
                     "Rural O2",
                     "Median O2",
                     "Urban O2",
                     "Unlimited oxygen")

scenario_df <- data.frame(scenario = oxygen_scenario,
                          R0 = 2.5,#c(2.5, 2, 2.5, 3, 2.5),
                          prop_ox_hosp_beds = c(0, median_oxy$oxygen_availability/100, 1),
                          prop_ox_ICU_beds = c(0, median_oxy$oxygen_availability/100, 1))


all_apothecary_runs <- do.call(rbind, sapply(1:nrow(scenario_df), function(x){

  print(x)

  scenario_go <- run_apothecary(country = "Nigeria",
                 R0 = scenario_df$R0[x],
                 # hosp_bed_capacity = 10000000000,
                 # ICU_bed_capacity = 10000000000,
                 prop_ox_hosp_beds = scenario_df$prop_ox_hosp_beds[x],
                 prop_ox_ICU_beds = scenario_df$prop_ox_ICU_beds[x],
                 # MV_capacity = 10000000000,
                 model = "deterministic",
                 time_period = 365,
                 day_return = TRUE)

  index <- squire:::odin_index(scenario_go$model)
  deaths_inc <- rowSums(scenario_go$output[, unlist(index[names(index)[grepl("Die", names(index))]])])
  total_deaths <- sum(deaths_inc)

  data.frame(scenario_df[x, ], time = scenario_go$output[, 1],
             inc_deaths = deaths_inc, total_deaths = total_deaths)

}, simplify = FALSE))

all_apothecary_runs$scenario <- factor(all_apothecary_runs$scenario, levels = oxygen_scenario)


line_deaths <- ggplot(data = all_apothecary_runs) + geom_line(aes(x = time, y = inc_deaths, color = scenario)) +
  theme_minimal() + labs(x = "Time", y = "Deaths", color = "Scenario")


bar_deaths <- ggplot(data = all_apothecary_runs) + geom_bar(aes(x = scenario, y = total_deaths, fill = scenario),
                                              stat = "identity") +
  theme_minimal() + labs(x = "Scenario", y = "Total deaths", fill = "Scenario")

ggarrange(line_deaths, bar_deaths, nrow = 1, common.legend = T, legend = "bottom")

