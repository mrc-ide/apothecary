# Loading required libraries
library(dplyr); library(scales); library(countrycode); library(ggplot2); library(tidyr)

# Loading apothecary and associated functions
devtools::load_all()

# Loading required functions
source("run_rep_settings_function.R")

# Checking Drug Property 1
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_1_indic = 1, drug_1_effect_size = 0.75,
                    prophylactic_drug_timing_1 = 150,
                    prophylactic_drug_timing_2 = 200,
                    prophylactic_prop_treat = 0.5,
                    prophylactic_drug_wane = 0.01, model = "stochastic")

y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_bed_capacity = 100000000000, ICU_bed_capacity = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_1_indic = 1, drug_1_effect_size = 0.75,
                    prophylactic_drug_timing_1 = 150,
                    prophylactic_drug_timing_2 = 151,
                    prophylactic_prop_treat = 0.5,
                    prophylactic_drug_wane = 0.01, model = "deterministic", dt = 1)

x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)

plot(y$output[, y_index$x], type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_S_PS, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$n_S_PS], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$S, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$S], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$PS, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$PS], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_PS_S, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$n_PS_S], 1, sum), col = "red", type = "l")

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_PS_PE1, 1], 1, sum), col = "black", type = "l")
lines(apply(y$output[, y_index$n_PS_PE1], 1, sum), col = "red", type = "l")


plot(apply(y$output[, y_index$n_PS_PE1], 1, sum), col = "red", type = "l")

plot(apply(y$output[, y_index$S], 1, sum))

plot(apply(y$output[, y_index$PS], 1, sum), col = "red")

y$output[, y_index$time]
y$output[, y_index$t]

x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot(apply(x$output[, x_index$PS, 1], 1, sum), type = "l")
lines(apply(y$output[, y_index$PS], 1, sum), col = "red")

plot(apply(x$output[, index$n_S_PS, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_leave_PS, 1], 1, sum), type = "l")
lines(apply(x$output[, index$n_PS_S, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_PS_PE1, 1], 1, sum), type = "l", col = "blue")
plot(apply(x$output[, index$n_PE1_PE2, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_PE2_I, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_I, 1], 1, sum), type = "l")


# Checking Drug Property 2
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_2_indic = 1, drug_2_effect_size = 0.5,
                    prophylactic_drug_timing_1 = 150,
                    prophylactic_drug_timing_2 = 250,
                    prophylactic_prop_treat = 0.75,
                    prophylactic_drug_wane = 0.05)

index <- squire:::odin_index(x$model)
plot(apply(x$output[, index$PS, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_S_PS, 1], 1, sum))

plot(apply(x$output[, index$n_leave_PS, 1], 1, sum))
plot(apply(x$output[, index$n_PS_S, 1], 1, sum))
plot(apply(x$output[, index$n_PS_PE1, 1], 1, sum))
plot(apply(x$output[, index$n_PE1_PE2, 1], 1, sum))
plot(apply(x$output[, index$n_PE2_I, 1], 1, sum))
plot(apply(x$output[, index$n_PE2_ICase1_initial, 1], 1, sum))
plot(apply(x$output[, index$n_PE2_ICase1, 1], 1, sum))
plot(apply(x$output[, index$n_PE2_IMild, 1], 1, sum))
plot(apply(x$output[, index$n_PE2_ICase1, 1], 1, sum)/apply(x$output[, index$n_PE2_ICase1_initial, 1], 1, sum),
     pch = 20, cex = log(apply(x$output[, index$n_PE2_ICase1_initial, 1], 1, sum) + 1))
plot(apply(x$output[, index$n_E2_I, 1], 1, sum), type = "l")
lines(apply(x$output[, index$n_PE2_I, 1], 1, sum))
plot(apply(x$output[, index$n_E2_I, 1], 1, sum) + apply(x$output[, index$n_PE2_I, 1], 1, sum), type = "l")


# Checking Drug Property 3
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_3_indic = 1, drug_3_effect_size = 0.75,
                    drug_3_prop_treat = 0.66)

plot(apply(x$output[, index$n_E2_I, 1], 1, sum))
plot(apply(x$output[, index$n_E2_ICase1_initial, 1], 1, sum))
plot(apply(x$output[, index$n_E2_ICase1, 1], 1, sum))
plot(apply(x$output[, index$n_E2_IMild, 1], 1, sum), type = "l")
lines(apply(x$output[, index$n_E2_IAsym, 1], 1, sum))

# Checking Drug Property 4 - APPEARS TO BE WORKING FINE, BUT EFFECT_SIZE > 2 REALLY REDUCES
# THE EPIDEMIC - EXPONENTIAL DIST FOR IMILD CURRENTLY *AND* ONLY A SHORT PERIOD OF TIME INFECTIOUS...
set.seed(10)
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100)
plot(apply(x$output[, index$n_E2_IMild, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_IMild_R, 1], 1, sum), type = "l")
x$output[, index$gamma_IMild, 1][1]
x$output[, index$gamma_IMild_Drug_4, 1][1]
x$output[, index$p_IMild_R, 1][1]

set.seed(10)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_4_indic = 1, drug_4_effect_size = 2,
                    drug_4_prop_treat = 1)
lines(apply(y$output[, index$n_IMild_R, 1], 1, sum), type = "l", col = "red")
y$output[, index$gamma_IMild, 1][1]
y$output[, index$gamma_IMild_Drug_4, 1][1]
y$output[, index$p_IMild_R, 1][1]


# Checking Drug Property 5
x <- run_apothecary(country = "France", R0 = 2.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_5_indic_IMild = 1, drug_5_indic_ICase = 1, drug_5_effect_size = 0.2,
                    drug_5_prop_treat = 0.5)

plot(apply(x$output[, index$n_E2_IMild, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_IMild_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_IMild_No_Drug_5, 1], 1, sum), type = "l")

plot(apply(x$output[, index$n_E2_ICase1, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_ICase1_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_ICase1_No_Drug_5, 1], 1, sum), type = "l")

x <- run_apothecary(country = "France", R0 = 2.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_5_indic_IMild = 1, drug_5_indic_ICase = 1, drug_5_effect_size = 0.2,
                    drug_5_prop_treat = 0.5,
                    drug_2_indic = 1, drug_2_effect_size = 0.5,
                    prophylactic_drug_timing_1 = 50,
                    prophylactic_drug_timing_2 = 250,
                    prophylactic_prop_treat = 1,
                    prophylactic_drug_wane = 0)

plot(apply(x$output[, index$PS, 1], 1, sum), type = "l")

plot(apply(x$output[, index$n_E2_IMild, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_IMild_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_IMild_No_Drug_5, 1], 1, sum), type = "l")

plot(apply(x$output[, index$n_E2_ICase1, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_ICase1_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_E2_ICase1_No_Drug_5, 1], 1, sum), type = "l")

plot(apply(x$output[, index$n_PE2_IMild, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_PE2_IMild_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_PE2_IMild_No_Drug_5, 1], 1, sum), type = "l")

plot(apply(x$output[, index$n_PE2_ICase1, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_PE2_ICase1_Drug_5, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_PE2_ICase1_No_Drug_5, 1], 1, sum), type = "l")


# Checking Drug Property 6
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_6_indic = 1, drug_6_effect_size = 0.5,
                    drug_6_prop_treat = 1)

plot(apply(x$output[, index$number_req_ICU_initial, 1], 1, sum), type = "l")
plot(apply(x$output[, index$number_req_ICU, 1], 1, sum), type = "l")
plot(x$output[, index$total_req_ICU, 1], type = "l")

plot(apply(x$output[, index$number_req_Hosp, 1], 1, sum), type = "l")
plot(x$output[, index$total_req_Hosp, 1], type = "l")


# Checking Drug Property 7
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_7_indic = 1, drug_7_effect_size = 0.5,
                    drug_7_prop_treat = 1)

plot(apply(x$output[, index$number_req_ICU_MV_initial, 1], 1, sum), type = "l")
plot(apply(x$output[, index$number_req_ICU_MV, 1], 1, sum), type = "l")
plot(x$output[, index$total_req_ICU_MV, 1], type = "l")
plot(x$output[, index$total_req_ICU_Ox, 1], type = "l")


# Checking Drug Property 8
set.seed(10)
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1,
                    drug_8_prop_treat = 1,
                    drug_8_GetOx_effect_size = 1, drug_8_NoOx_effect_size = 1)

plot(apply(x$output[, index$IMod_GetHosp_GetOx_Surv1, 1], 1, sum), type = "l")
plot(apply(x$output[, index$IMod_GetHosp_NoOx_Surv1, 1], 1, sum), type = "l")
sum(x$output[, index$total_GetHosp, 1])

x$output[, index$p_IMod_GetHosp_GetOx_Surv, 1][1]
x$output[, index$p_IMod_GetHosp_NoOx_Surv, 1][1]

x$output[, index$gamma_IMod_GetHosp_GetOx_Surv, 1][1]
x$output[, index$gamma_IMod_GetHosp_GetOx_Surv_Drug_8, 1][1]
x$output[, index$gamma_IMod_GetHosp_NoOx_Surv, 1][1]
x$output[, index$gamma_IMod_GetHosp_NoOx_Surv_Drug_8, 1][1]


x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    oxygen_availability_0 = 0, input_oxygen_supply = 0,
                    drug_8_indic_IMod_GetHosp_GetOx = 1, drug_8_indic_IMod_GetHosp_NoOx = 1,
                    drug_8_prop_treat = 1,
                    drug_8_GetOx_effect_size = 1, drug_8_NoOx_effect_size = 2)

plot(apply(x$output[, index$IMod_GetHosp_GetOx_Surv1, 1], 1, sum), type = "l")
plot(apply(x$output[, index$IMod_GetHosp_NoOx_Surv1, 1], 1, sum), type = "l")
sum(x$output[, index$total_GetHosp, 1])

x$output[, index$p_IMod_GetHosp_GetOx_Surv, 1][1]
x$output[, index$p_IMod_GetHosp_NoOx_Surv, 1][1]

x$output[, index$gamma_IMod_GetHosp_GetOx_Surv, 1][1]
x$output[, index$gamma_IMod_GetHosp_GetOx_Surv_Drug_8, 1][1]
x$output[, index$gamma_IMod_GetHosp_NoOx_Surv, 1][1]
x$output[, index$gamma_IMod_GetHosp_NoOx_Surv_Drug_8, 1][1]


# Checking Drug Property 9 - NOTE DOESN'T APPEAR TO CHANGE OCCUPANCY OF IREC FOR SOME REASON... IS THAT TO BE EXPECTED?
set.seed(15)
x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1,
                    drug_9_prop_treat = 1,
                    drug_9_GetOx_effect_size = 1, drug_9_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

set.seed(15)
y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_9_indic_ISev_GetICU_GetOx = 1, drug_9_indic_ISev_GetICU_NoOx = 1,
                    drug_9_prop_treat = 1,
                    drug_9_GetOx_effect_size = 3, drug_9_NoOx_effect_size = 1)

x$output[, index$gamma_ISev_GetICU_GetOx_Surv, 1][1]
x$output[, index$gamma_ISev_GetICU_GetOx_Surv_Drug_9, 1][1]
x$output[, index$gamma_ISev_GetICU_NoOx_Surv, 1][1]
x$output[, index$gamma_ISev_GetICU_NoOx_Surv_Drug_9, 1][1]
x$output[, index$p_ISev_GetICU_GetOx_Surv, 1][1]
x$output[, index$p_ISev_GetICU_NoOx_Surv, 1][1]

y$output[, index$gamma_ISev_GetICU_GetOx_Surv, 1][1]
y$output[, index$gamma_ISev_GetICU_GetOx_Surv_Drug_9, 1][1]
y$output[, index$gamma_ISev_GetICU_NoOx_Surv, 1][1]
y$output[, index$gamma_ISev_GetICU_NoOx_Surv_Drug_9, 1][1]
y$output[, index$p_ISev_GetICU_GetOx_Surv, 1][1]
y$output[, index$p_ISev_GetICU_NoOx_Surv, 1][1]

plot(apply(x$output[, index$number_req_ICU, 1], 1, sum), type = "l")
lines(apply(y$output[, index$number_req_ICU, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$number_req_ICU, 1])
sum(y$output[, index$number_req_ICU, 1])

plot(apply(x$output[, index$number_GetICU_GetOx, 1], 1, sum), type = "l")
lines(apply(y$output[, index$number_GetICU_GetOx, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$number_GetICU_GetOx, 1])
sum(y$output[, index$number_GetICU_GetOx, 1])

plot(apply(x$output[, index$number_GetICU_GetOx_GetMV, 1], 1, sum), type = "l")
lines(apply(y$output[, index$number_GetICU_GetOx_GetMV, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$number_GetICU_GetOx_GetMV, 1])
sum(y$output[, index$number_GetICU_GetOx_GetMV, 1])

plot(apply(x$output[, index$n_ISev_GetICU_GetOx_Die1, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_ISev_GetICU_GetOx_Die1, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$n_ISev_GetICU_GetOx_Die1, 1])
sum(y$output[, index$n_ISev_GetICU_GetOx_Die1, 1])

plot(apply(x$output[, index$n_ISev_GetICU_GetOx_Surv1, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_ISev_GetICU_GetOx_Surv1, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$n_ISev_GetICU_GetOx_Surv1, 1])
sum(y$output[, index$n_ISev_GetICU_GetOx_Surv1, 1])

plot(apply(y$output[, index$n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2, 1], 1, sum), type = "l")
sum(x$output[, index$n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2, 1])
sum(y$output[, index$n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2, 1])

plot(apply(x$output[, index$ISev_GetICU_GetOx_Surv1, 1], 1, sum), type = "l")
lines(apply(y$output[, index$ISev_GetICU_GetOx_Surv1, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$ISev_GetICU_GetOx_Surv1, 1])/(10 * 1/x$raw_parameters$gamma_ISev_GetICU_GetOx_Surv)
sum(y$output[, index$ISev_GetICU_GetOx_Surv1, 1])/(10 * 1/(3 * y$raw_parameters$gamma_ISev_GetICU_GetOx_Surv))

plot(apply(x$output[, index$ISev_GetICU_GetOx_Surv2, 1], 1, sum), type = "l")
lines(apply(y$output[, index$ISev_GetICU_GetOx_Surv2, 1], 1, sum), type = "l", col = "red")

plot(apply(y$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1], 1, sum), type = "l")
sum(x$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1])
sum(y$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1])

plot(apply(x$output[, index$IRec1, 1], 1, sum), type = "l")
lines(apply(y$output[, index$IRec1, 1], 1, sum), type = "l", col = "red")

plot(apply(x$output[, index$IRec2, 1], 1, sum), type = "l")
lines(apply(y$output[, index$IRec2, 1], 1, sum), type = "l", col = "red")

plot(apply(x$output[, index$R, 1], 1, sum), type = "l")
lines(apply(y$output[, index$R, 1], 1, sum), type = "l", col = "red")


# Checking Drug Property 10
set.seed(15)
x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1,
                    drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1,
                    drug_10_prop_treat = 1,
                    drug_10_GetOx_GetMV_effect_size = 1, drug_10_GetOx_NoMV_effect_size = 1,
                    drug_10_NoOx_NoMV_effect_size = 1,
                    prob_critical = rep(1, 17))

index <- squire:::odin_index(x$model)

set.seed(15)
y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_10_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 1,
                    drug_10_indic_ICrit_GetICU_NoOx_NoMV = 1,
                    drug_10_prop_treat = 1,
                    drug_10_GetOx_GetMV_effect_size = 3, drug_10_GetOx_NoMV_effect_size = 1,
                    drug_10_NoOx_NoMV_effect_size = 1,
                    prob_critical = rep(1, 17))

x$output[, index$gamma_ICrit_GetICU_GetOx_GetMV_Surv, 1][1]
x$output[, index$gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10, 1][1]
x$output[, index$gamma_ICrit_GetICU_GetOx_NoMV_Surv, 1][1]
x$output[, index$gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10, 1][1]
x$output[, index$gamma_ICrit_GetICU_NoOx_NoMV_Surv, 1][1]
x$output[, index$gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10, 1][1]
x$output[, index$p_ICrit_GetICU_GetOx_GetMV_Surv, 1][1]

y$output[, index$gamma_ICrit_GetICU_GetOx_GetMV_Surv, 1][1]
y$output[, index$gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10, 1][1]
y$output[, index$gamma_ICrit_GetICU_GetOx_NoMV_Surv, 1][1]
y$output[, index$gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10, 1][1]
y$output[, index$gamma_ICrit_GetICU_NoOx_NoMV_Surv, 1][1]
y$output[, index$gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10, 1][1]
y$output[, index$p_ICrit_GetICU_GetOx_GetMV_Surv, 1][1]

plot(apply(x$output[, index$number_req_ICU, 1], 1, sum), type = "l")
lines(apply(y$output[, index$number_req_ICU, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$number_req_ICU, 1])
sum(y$output[, index$number_req_ICU, 1])

plot(apply(x$output[, index$number_GetICU_GetOx_GetMV, 1], 1, sum), type = "l")
lines(apply(y$output[, index$number_GetICU_GetOx_GetMV, 1], 1, sum), type = "l", col = "red")
sum(x$output[, index$number_GetICU_GetOx_GetMV, 1])
sum(y$output[, index$number_GetICU_GetOx_GetMV, 1])

plot(apply(x$output[, index$ICrit_GetICU_GetOx_GetMV_Surv1, 1], 1, sum), type = "l")
lines(apply(y$output[, index$ICrit_GetICU_GetOx_GetMV_Surv1, 1], 1, sum), type = "l", col = "red")

plot(apply(x$output[, index$ICrit_GetICU_GetOx_GetMV_Surv2, 1], 1, sum), type = "l")
lines(apply(y$output[, index$ICrit_GetICU_GetOx_GetMV_Surv2, 1], 1, sum), type = "l", col = "red")

plot(apply(y$output[, index$IRec1, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$IRec1, 1], 1, sum), type = "l")

plot(apply(y$output[, index$IRec2, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$IRec2, 1], 1, sum), type = "l")

plot(apply(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1, 1], 1, sum), type = "l")
sum(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1, 1])
sum(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1, 1])

plot(apply(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2, 1], 1, sum), type = "l")
sum(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2, 1])
sum(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2, 1])

plot(apply(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec, 1], 1, sum), type = "l", col = "red")
lines(apply(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec, 1], 1, sum), type = "l")
sum(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec, 1])
sum(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec, 1])


plot(apply(x$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_ISev_GetICU_NoOx_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(x$output[, index$n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec, 1], 1, sum), type = "l")

plot(apply(y$output[, index$n_ISev_GetICU_GetOx_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(y$output[, index$n_ISev_GetICU_NoOx_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(y$output[, index$n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec, 1], 1, sum), type = "l")
plot(apply(y$output[, index$n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec, 1], 1, sum), type = "l")


plot(apply(x$output[, index$ICrit_GetICU_GetOx_NoMV_Surv2, 1], 1, sum), type = "l")
plot(apply(x$output[, index$ICrit_GetICU_NoOx_NoMV_Surv2, 1], 1, sum), type = "l")
plot(apply(x$output[, index$ICrit_NoICU_NoOx_NoMV_Surv2, 1], 1, sum), type = "l")

plot(apply(y$output[, index$ICrit_GetICU_GetOx_NoMV_Surv2, 1], 1, sum), type = "l")
plot(apply(y$output[, index$ICrit_GetICU_NoOx_NoMV_Surv2, 1], 1, sum), type = "l")
plot(apply(y$output[, index$ICrit_NoICU_NoOx_NoMV_Surv2, 1], 1, sum), type = "l")

# Checking Drug Property 11
set.seed(15)
x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                    drug_11_prop_treat = 1,
                    drug_11_GetOx_effect_size = 1, drug_11_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                    drug_11_prop_treat = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5)

plot(apply(x$output[, index$n_IMod_GetHosp_GetOx_Die2_D_Hospital, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_IMod_GetHosp_GetOx_Die2_D_Hospital, 1], 1, sum), type = "l", col = "red")

x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100, input_oxygen_supply = 0, oxygen_availability_0 = 0,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                    drug_11_prop_treat = 1,
                    drug_11_GetOx_effect_size = 1, drug_11_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100, input_oxygen_supply = 0, oxygen_availability_0 = 0,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1,
                    drug_11_prop_treat = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5)

plot(apply(x$output[, index$n_IMod_GetHosp_NoOx_Die2_D_Hospital, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_IMod_GetHosp_NoOx_Die2_D_Hospital, 1], 1, sum), type = "l", col = "red")


# Checking Drug Property 12
set.seed(15)
x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                    drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 1, drug_12_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                    drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5)

plot(apply(x$output[, index$n_ISev_GetICU_GetOx_Die2_D_Hospital, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_ISev_GetICU_GetOx_Die2_D_Hospital, 1], 1, sum), type = "l", col = "red")

x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100, input_oxygen_supply = 0, oxygen_availability_0 = 0,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                    drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 1, drug_12_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100, input_oxygen_supply = 0, oxygen_availability_0 = 0,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                    drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5)

plot(apply(x$output[, index$n_ISev_GetICU_NoOx_Die2_D_Hospital, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_ISev_GetICU_NoOx_Die2_D_Hospital, 1], 1, sum), type = "l", col = "red")


# Checking Drug Property 13
set.seed(15)
x <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1,
                    drug_12_prop_treat = 1,
                    drug_12_GetOx_effect_size = 1, drug_12_NoOx_effect_size = 1)

index <- squire:::odin_index(x$model)

y <- run_apothecary(country = "France", R0 = 3, tt_R0 = 0, hosp_beds = 100000000000, ICU_beds = 10000000000,
                    day_return = FALSE, seeding_cases = 100,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1,
                    drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1,
                    drug_13_prop_treat = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5,
                    drug_13_NoOx_NoMV_effect_size = 0.5)

plot(apply(x$output[, index$n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital, 1], 1, sum), type = "l")
lines(apply(y$output[, index$n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital, 1], 1, sum), type = "l", col = "red")

