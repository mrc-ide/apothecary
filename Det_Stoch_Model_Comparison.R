# Loading required libraries
library(dplyr); library(scales); library(countrycode); library(ggplot2); library(tidyr)

# Loading apothecary and associated functions
devtools::load_all()

# Loading required functions
source("plot_output_function.R")

# Testing Baseline Deterministic vs Stochastic
base_check <- c("S", "E1", "E2", "IMild", "IAsymp", "ICase1", "ICase2", "R", "D_Hospital", "D_Community", "IRec1", "IRec2",
                "IMod_GetHosp_GetOx_Surv1", "IMod_GetHosp_GetOx_Surv2", "IMod_GetHosp_GetOx_Die1", "IMod_GetHosp_GetOx_Die2",
                "IMod_GetHosp_NoOx_Surv1", "IMod_GetHosp_NoOx_Surv2", "IMod_GetHosp_NoOx_Die1", "IMod_GetHosp_NoOx_Die2",
                "IMod_NoHosp_NoOx_Surv1", "IMod_NoHosp_NoOx_Surv2", "IMod_NoHosp_NoOx_Die1", "IMod_NoHosp_NoOx_Die2",
                "ISev_GetICU_GetOx_Surv1", "ISev_GetICU_GetOx_Surv2", "ISev_GetICU_GetOx_Die1", "ISev_GetICU_GetOx_Die2",
                "ISev_GetICU_NoOx_Surv1", "ISev_GetICU_NoOx_Surv2", "ISev_GetICU_NoOx_Die1", "ISev_GetICU_NoOx_Die2",
                "ISev_NoICU_NoOx_Surv1", "ISev_NoICU_NoOx_Surv2", "ISev_NoICU_NoOx_Die1", "ISev_NoICU_NoOx_Die2",
                "ICrit_GetICU_GetOx_GetMV_Surv1", "ICrit_GetICU_GetOx_GetMV_Surv2", "ICrit_GetICU_GetOx_GetMV_Die1", "ICrit_GetICU_GetOx_GetMV_Die2",
                "ICrit_GetICU_GetOx_NoMV_Surv1", "ICrit_GetICU_GetOx_NoMV_Surv2", "ICrit_GetICU_GetOx_NoMV_Die1", "ICrit_GetICU_GetOx_NoMV_Die2",
                "ICrit_GetICU_NoOx_NoMV_Surv1", "ICrit_GetICU_NoOx_NoMV_Surv2", "ICrit_GetICU_NoOx_NoMV_Die1", "ICrit_GetICU_NoOx_NoMV_Die2",
                "ICrit_NoICU_NoOx_NoMV_Surv1", "ICrit_NoICU_NoOx_NoMV_Surv2", "ICrit_NoICU_NoOx_NoMV_Die1", "ICrit_NoICU_NoOx_NoMV_Die2",
                "hosp_occ", "ICU_occ", "MV_occ", "oxygen_needed_overall", "oxygen_used")

dt <- 0.01

# No Healthcare Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 100000000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 100000000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

# Bed Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 0, ICU_bed_capacity = 0,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 100000000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 0, ICU_bed_capacity = 0,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 100000000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

# Oxygen Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 100000000000,
                    oxygen_availability_0 = 0, input_oxygen_supply = 0, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 100000000000,
                    oxygen_availability_0 = 0, input_oxygen_supply = 0, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

# MV Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 100000000000,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 1000000000, input_baseline_oxygen_demand = 0, MV_capacity = 0)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 100000000000, ICU_bed_capacity = 100000000000,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 1000000000, input_baseline_oxygen_demand = 0, MV_capacity = 0)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

# Bed and Oxygen Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 1000000, ICU_bed_capacity = 250000,
                    oxygen_availability_0 = 0, input_oxygen_supply = 3000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 1000000, ICU_bed_capacity = 250000,
                    oxygen_availability_0 = 0, input_oxygen_supply = 3000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
                    oxygen_availability_0 = 0, input_oxygen_supply = 1000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
                    oxygen_availability_0 = 0, input_oxygen_supply = 1200, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)



# Bed and MV Constraints
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 1000000000, input_baseline_oxygen_demand = 0, MV_capacity = 500)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
                    oxygen_availability_0 = 1000000000, input_oxygen_supply = 1000000000, input_baseline_oxygen_demand = 0, MV_capacity = 500)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)




# Old and Scrap Tester
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 100, dt = 1, model = "deterministic",
                    hosp_bed_capacity = 10000000, ICU_bed_capacity = 250000,
                    oxygen_availability_0 = 1000, input_oxygen_supply = 1000, input_baseline_oxygen_demand = 0, MV_capacity = 10000000)
index <- squire:::odin_index(y$model)
plot(y$output[, index$time], apply(y$output[, index$S], 1, sum), col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], apply(y$output[, index$IMod_GetHosp_GetOx_Die1], 1, sum), col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], apply(y$output[, index$IMod_GetHosp_NoOx_Die1], 1, sum), col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], y$output[, index$oxygen_needed_overall], col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], y$output[, index$oxygen_supply], col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], y$output[, index$oxygen_used], col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], apply(y$output[, index$D_Hospital], 1, sum), col = "black", lwd = 2, type = "l")
plot(y$output[, index$time], apply(y$output[, index$D_Community], 1, sum), col = "black", lwd = 2, type = "l")

