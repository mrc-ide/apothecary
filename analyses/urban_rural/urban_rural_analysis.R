options(scipen = 999999)

library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(viridis)

devtools::load_all()

#Read in data
urban_rural_oxy_data <- read.csv("data-raw/urban_rural_oxygen_availability_split.csv", stringsAsFactors = T)
urban_rural_oxy_data$setting_subcategory <- factor(urban_rural_oxy_data$setting_subcategory,
                                                   levels = c("Rural", "All", "Urban"))
urban_rural_oxy_data <- urban_rural_oxy_data[order(urban_rural_oxy_data$setting_subcategory), ]

median_oxy <- subset(urban_rural_oxy_data, q == 0.5)

urban_rural_healthcare_facilities <- read.csv("data-raw/urban_rural_oxygen_facility_availability.csv", stringsAsFactors = T)
urban_rural_healthcare_facilities$setting_subcategory <- factor(urban_rural_healthcare_facilities$setting_subcategory,
                                                                levels = c("Rural", "All", "Urban"))
urban_rural_healthcare_facilities <- urban_rural_healthcare_facilities[order(urban_rural_healthcare_facilities$setting_subcategory), ]

#Set up scenarios
oxygen_scenario <- c("No O2",
                     "Rural O2",
                     "Median O2",
                     "Urban O2",
                     "Unlimited O2")

scenario_df <- data.frame(scenario = oxygen_scenario,
                          R0 = c(2, 1.75, 1, 2.25, 2),
                          healthcare_modifier = c(1,
                                                  subset(urban_rural_healthcare_facilities, setting_subcategory == "Rural")$proportion_healthcare_facilities,
                                                  1,
                                                  subset(urban_rural_healthcare_facilities, setting_subcategory == "Urban")$proportion_healthcare_facilities,
                                                  1000000000000000000000000000000),
                          prop_ox_hosp_beds = c(0, median_oxy$oxygen_availability/100, 1),
                          prop_ox_ICU_beds = c(0, median_oxy$oxygen_availability/100, 1))

#Get healthcare capacity
NGA_hospital <- get_healthcare_capacity("Nigeria")$hosp_beds * (sum(get_population("Nigeria")$n) / 1000)
NGA_ICU <- get_healthcare_capacity("Nigeria")$ICU_beds * (sum(get_population("Nigeria")$n) / 1000)


all_apothecary_runs <- do.call(rbind, sapply(1:nrow(scenario_df), function(x){

  print(x)

  scenario_go <- run_apothecary(country = "Nigeria",
                 R0 = scenario_df$R0[x],
                 hosp_bed_capacity = NGA_hospital * scenario_df$healthcare_modifier[x],
                 ICU_bed_capacity = NGA_ICU * scenario_df$healthcare_modifier[x],
                 prop_ox_hosp_beds = scenario_df$prop_ox_hosp_beds[x],
                 prop_ox_ICU_beds = scenario_df$prop_ox_ICU_beds[x],
                 MV_capacity = NGA_ICU * scenario_df$healthcare_modifier[x],
                 model = "deterministic",
                 time_period = 300,
                 day_return = TRUE)

  index <- squire:::odin_index(scenario_go$model)
  deaths_inc <- rowSums(scenario_go$output[, unlist(index[names(index)[grepl("Die", names(index))]])])
  all_S <- rowSums(scenario_go$output[, index$S])
  all_inf <- max(all_S) - all_S

  total_deaths <- sum(deaths_inc)/max(all_inf)

  data.frame(scenario_df[x, ], time = scenario_go$output[, 1],
             inc_deaths = deaths_inc, total_deaths = total_deaths)

}, simplify = FALSE))

all_apothecary_runs$scenario <- factor(all_apothecary_runs$scenario, levels = oxygen_scenario)


line_deaths <- ggplot(data = all_apothecary_runs) + geom_line(aes(x = time, y = inc_deaths, color = scenario)) +
  theme_minimal() + labs(x = "Time", y = "Deaths", color = "Scenario") #+ coord_cartesian(xlim = c(0, 175))


bar_deaths <- ggplot(data = subset(all_apothecary_runs, time == 0)) + geom_bar(aes(x = scenario, y = total_deaths, fill = scenario),
                                              stat = "identity") +
  theme_minimal() + labs(x = "Scenario", y = "IFR", fill = "Scenario")

ggarrange(line_deaths, bar_deaths, nrow = 1, common.legend = T, legend = "bottom")


#Go for a heatmap
all_apothecary_runs_R0 <- do.call(rbind, sapply(seq(1.5, 3, by = 0.1), function(y){

  do.call(rbind, sapply(1:nrow(scenario_df), function(x){

    print(x)

    scenario_go <- run_apothecary(country = "Nigeria",
                                  R0 = y,
                                  hosp_bed_capacity = NGA_hospital * scenario_df$healthcare_modifier[x],
                                  ICU_bed_capacity = NGA_ICU * scenario_df$healthcare_modifier[x],
                                  prop_ox_hosp_beds = scenario_df$prop_ox_hosp_beds[x],
                                  prop_ox_ICU_beds = scenario_df$prop_ox_ICU_beds[x],
                                  MV_capacity = NGA_ICU * scenario_df$healthcare_modifier[x],
                                  model = "deterministic",
                                  time_period = 300,
                                  day_return = TRUE)

    index <- squire:::odin_index(scenario_go$model)
    deaths_inc <- rowSums(scenario_go$output[, unlist(index[names(index)[grepl("Die", names(index))]])])
    all_S <- rowSums(scenario_go$output[, index$S])
    all_inf <- max(all_S) - all_S

    out_of_capacity <- rowSums(scenario_go$output[, unlist(index[names(index)[grepl("NoOx", names(index))]])])

    total_deaths <- sum(deaths_inc)/max(all_inf)

    data.frame(scenario_df[x, ], R0_input = y, time = scenario_go$output[, 1],
               inc_deaths = deaths_inc, total_deaths = total_deaths)

  }, simplify = FALSE))

}, simplify = FALSE))

all_apothecary_runs_R0$scenario <- factor(all_apothecary_runs_R0$scenario, levels = oxygen_scenario)


ggplot() + geom_tile(data = subset(all_apothecary_runs_R0, time == 0),
                     aes(x = R0_input, y = scenario, fill = total_deaths)) +
  theme_minimal() + labs(x = "R0", y = "Scenario", fill = "IFR") + scale_fill_viridis(option = "B", direction = -1)


ggplot(data = subset(all_apothecary_runs_R0, scenario == "Median O2")) +
  geom_line(aes(x = time, y = inc_deaths, group = R0_input, color = R0_input)) +
  theme_minimal() + labs(x = "Time", y = "Deaths", color = "R0")

