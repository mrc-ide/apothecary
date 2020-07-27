# Load the squire package
devtools::load_all(".")

# Loading Mixing Matrix, Population Etc
cpm <- squire:::parse_country_population_mixing_matrix(country = "United Kingdom", population = NULL, contact_matrix_set = NULL)
country <- cpm$country
contact_matrix_set <- list(cpm$contact_matrix_set)
matrices_set <- squire:::matrix_set_explicit(contact_matrix_set, cpm$population)
baseline_matrix <- squire:::process_contact_matrix_scaled_age(contact_matrix_set[[1]], cpm$population)

# List of Initial Conditions
inits <- list(S_0 = cpm$population, E1_0 = rep(100, 17), E2_0 = rep(0, 17), IAsymp_0 = rep(0, 17), IMild_0 = rep(0, 17), ICase1_0 = rep(0, 17),
              ICase2_0 = rep(0, 17), IRec1_0 = rep(0, 17), IRec2_0 = rep(0, 17), R_0 = rep(0, 17), D_Community_0 = rep(0, 17), D_Hospital_0 = rep(0, 17),
              PS_0 = rep(0, 17), PE1_0 = rep(0, 17), PE2_0 = rep(0, 17), IMild_Drug_5_0 = rep(0, 17), ICase1_Drug_5_0 = rep(0, 17), ICase2_Drug_5_0 = rep(0, 17),
              IMod_GetHosp_GetOx_Surv1_0 = rep(0, 17), IMod_GetHosp_GetOx_Surv2_0 = rep(0, 17), IMod_GetHosp_GetOx_Die1_0 = rep(0, 17), IMod_GetHosp_GetOx_Die2_0 = rep(0, 17),
              IMod_GetHosp_NoOx_Surv1_0 = rep(0, 17), IMod_GetHosp_NoOx_Surv2_0 = rep(0, 17), IMod_GetHosp_NoOx_Die1_0 = rep(0, 17), IMod_GetHosp_NoOx_Die2_0 = rep(0, 17),
              IMod_NoHosp_NoOx_Surv1_0 = rep(0, 17), IMod_NoHosp_NoOx_Surv2_0 = rep(0, 17), IMod_NoHosp_NoOx_Die1_0 = rep(0, 17), IMod_NoHosp_NoOx_Die2_0 = rep(0, 17),
              ISev_GetICU_GetOx_Surv1_0 = rep(0, 17), ISev_GetICU_GetOx_Surv2_0 = rep(0, 17), ISev_GetICU_GetOx_Die1_0 = rep(0, 17), ISev_GetICU_GetOx_Die2_0 = rep(0, 17),
              ISev_GetICU_NoOx_Surv1_0 = rep(0, 17), ISev_GetICU_NoOx_Surv2_0 = rep(0, 17), ISev_GetICU_NoOx_Die1_0 = rep(0, 17), ISev_GetICU_NoOx_Die2_0 = rep(0, 17),
              ISev_NoICU_NoOx_Surv1_0 = rep(0, 17), ISev_NoICU_NoOx_Surv2_0 = rep(0, 17), ISev_NoICU_NoOx_Die1_0 = rep(0, 17), ISev_NoICU_NoOx_Die2_0 = rep(0, 17),
              ICrit_GetICU_GetOx_GetMV_Surv1_0 = rep(0, 17), ICrit_GetICU_GetOx_GetMV_Surv2_0 = rep(0, 17), ICrit_GetICU_GetOx_GetMV_Die1_0 = rep(0, 17),
              ICrit_GetICU_GetOx_GetMV_Die2_0 = rep(0, 17), ICrit_GetICU_GetOx_NoMV_Surv1_0 = rep(0, 17), ICrit_GetICU_GetOx_NoMV_Surv2_0 = rep(0, 17),
              ICrit_GetICU_GetOx_NoMV_Die1_0 = rep(0, 17), ICrit_GetICU_GetOx_NoMV_Die2_0 = rep(0, 17), ICrit_GetICU_NoOx_NoMV_Surv1_0 = rep(0, 17),
              ICrit_GetICU_NoOx_NoMV_Surv2_0 = rep(0, 17), ICrit_GetICU_NoOx_NoMV_Die1_0 = rep(0, 17), ICrit_GetICU_NoOx_NoMV_Die2_0 = rep(0, 17),
              ICrit_NoICU_NoOx_NoMV_Surv1_0 = rep(0, 17), ICrit_NoICU_NoOx_NoMV_Surv2_0 = rep(0, 17), ICrit_NoICU_NoOx_NoMV_Die1_0 = rep(0, 17),
              ICrit_NoICU_NoOx_NoMV_Die2_0 = rep(0, 17))

# List of Drug Effects and Properties
drugs <- list(prophylactic_drug_timing_1 = 100000, prophylactic_drug_timing_2 = 100000, prophylactic_prop_treat = 0, prophylactic_drug_wane = 1000000,
              drug_1_indic = 0, drug_1_effect_size = 0,
              drug_2_indic = 0, drug_2_effect_size = 0,
              drug_3_indic = 0, drug_3_prop_treat = 0, drug_3_effect_size = 0,
              drug_4_indic = 0, drug_4_prop_treat = 0, drug_4_effect_size = 0,
              drug_5_indic_IMild = 0, drug_5_indic_ICase = 0, drug_5_prop_treat = 0, drug_5_effect_size = 0,
              drug_6_indic = 0, drug_6_prop_treat = 0, drug_6_effect_size = 0,
              drug_7_indic = 0, drug_7_prop_treat = 0, drug_7_effect_size = 0,
              drug_8_indic_IMod_GetHosp_GetOx = 0, drug_8_indic_IMod_GetHosp_NoOx = 0, drug_8_prop_treat = 0, drug_8_GetOx_effect_size = 0, drug_8_NoOx_effect_size = 0,
              drug_9_indic_ISev_GetICU_GetOx = 0, drug_9_indic_ISev_GetICU_NoOx = 0, drug_9_prop_treat = 0, drug_9_GetOx_effect_size = 0, drug_9_NoOx_effect_size = 0,
              drug_10_indic_ICrit_GetICU_GetOx_GetMV = 0, drug_10_indic_ICrit_GetICU_GetOx_NoMV = 0, drug_10_indic_ICrit_GetICU_NoOx_NoMV = 0,
              drug_10_prop_treat = 0, drug_10_GetOx_GetMV_effect_size = 0, drug_10_GetOx_NoMV_effect_size = 0, drug_10_NoOx_NoMV_effect_size = 0,
              drug_11_indic_IMod_GetHosp_GetOx = 0, drug_11_indic_IMod_GetHosp_NoOx = 0, drug_11_prop_treat = 0, drug_11_GetOx_effect_size = 0, drug_11_NoOx_effect_size = 0,
              drug_12_indic_ISev_GetICU_GetOx = 0, drug_12_indic_ISev_GetICU_NoOx = 0, drug_12_prop_treat = 0, drug_12_GetOx_effect_size = 0, drug_12_NoOx_effect_size = 0,
              drug_13_indic_ICrit_GetICU_GetOx_GetMV = 0, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 0, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 0,
              drug_13_prop_treat = 0, drug_13_GetOx_GetMV_effect_size = 0, drug_13_GetOx_NoMV_effect_size = 0, drug_13_NoOx_NoMV_effect_size = 0)

# List of Hospital/ICU/Oxygen/Mechnical Ventilation Related Parameters
ox_mv <- list(tt_hosp_beds = 0, hosp_beds = 100000000,
              tt_ICU_beds = 0, ICU_beds = 100000000,
              tt_oxygen_supply = 0, input_oxygen_supply = 100000000, tt_baseline_oxygen_demand = 0, input_baseline_oxygen_demand = 1000,
              oxygen_availability_0 = 100000000, max_leftover = 1000000,
              severe_critical_case_oxygen_consumption_multiplier = 1.5,
              MV_capacity = 20000000)

# List of Additional Parameters
other <- list(dt = 0.1,
              N_age = 17,
              tt_matrix = 0,
              mix_mat_set = matrices_set,
              tt_beta = 0,
              beta_set = 0.06,
              rel_inf_asymp = 1,
              rel_inf_mild = 1)

# List of Rates
rates <- list(gamma_E = 2 * 1/4.6, gamma_IAsymp = 1/2.1, gamma_IMild = 1/2.1, gamma_ICase = 2 * 1/4.5, gamma_rec = 2 * 1/3.4,
              gamma_IMod_GetHosp_GetOx_Surv = 2 * 1/9.6, gamma_IMod_GetHosp_GetOx_Die = 2 * 1/7.6, gamma_IMod_GetHosp_NoOx_Surv =  2 * 1/(9.6/2),
              gamma_IMod_GetHosp_NoOx_Die = 2 * 1/(7.6/2), gamma_IMod_NoHosp_NoOx_Surv = 2 * 1/(9.6/2), gamma_IMod_NoHosp_NoOx_Die = 2 * 1/(7.6/2),
              gamma_ISev_GetICU_GetOx_Surv = 2 * 1/11.3, gamma_ISev_GetICU_GetOx_Die = 2 * 1/10.1, gamma_ISev_GetICU_NoOx_Surv = 2 * 1/(11.3/2),
              gamma_ISev_GetICU_NoOx_Die = 2 * 1/(10.1/2), gamma_ISev_NoICU_NoOx_Surv = 2 * 1/(11.3/2), gamma_ISev_NoICU_NoOx_Die = 2 * 1/(10.1/2),
              gamma_ICrit_GetICU_GetOx_GetMV_Surv = 2 * 1/11.3, gamma_ICrit_GetICU_GetOx_GetMV_Die = 2 * 1/10.1, gamma_ICrit_GetICU_GetOx_NoMV_Surv = 2 * 1/(11.3/2),
              gamma_ICrit_GetICU_GetOx_NoMV_Die = 2 * 1/(10.1/2), gamma_ICrit_GetICU_NoOx_NoMV_Surv = 2 * 1/(11.3/2), gamma_ICrit_GetICU_NoOx_NoMV_Die = 2 * 1/(10.1/2),
              gamma_ICrit_NoICU_NoOx_NoMV_Surv = 2 * 1/(11.3/2), gamma_ICrit_NoICU_NoOx_NoMV_Die = 2 * 1/(10.1/2))

# List of Probabilities
probs <- list(prob_asymp = rep(0.5, 17),
              prob_hosp = c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
                            0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
                            0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
                            0.176634654 ,0.180000000),
              prob_severe = c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
                              0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
                              0.103612417, 0.149427991, 0.223777304,	0.306985918,
                              0.385779555, 0.461217861, 0.709444444),
              prob_critical = rep(0.8, 17),
              prob_moderate_death_get_hosp_get_ox_baseline = c(0.0125702,	0.0125702,	0.0125702,	0.0125702, 0.0125702,
                                                               0.0125702,	0.0125702,	0.013361147, 0.015104687,	0.019164124,
                                                               0.027477519,	0.041762108, 0.068531658,	0.105302319, 0.149305732,
                                                               0.20349534,	0.5804312),
              prob_moderate_death_get_hosp_no_ox_baseline = rep(0.6, 17),
              prob_moderate_death_no_hosp_no_ox = rep(0.6, 17),
              prob_severe_death_get_ICU_get_ox_baseline = rep(0.5, 17),
              prob_severe_death_get_ICU_no_ox_baseline = rep(0.95, 17),
              prob_severe_death_no_ICU_no_ox = rep(0.95, 17),
              prob_critical_death_get_ICU_get_ox_get_MV_baseline = rep(0.5, 17),
              prob_critical_death_get_ICU_get_ox_no_MV_baseline = rep(0.95, 17),
              prob_critical_death_get_ICU_no_ox_no_MV_baseline = rep(0.95, 17),
              prob_critical_death_no_ICU_no_ox_no_MV = rep(0.95, 17))

overall <- c(inits, drugs, ox_mv, other, rates, probs)


# don't forget to eventually change this to include IAsymp
beta_set <- squire::beta_est_explicit(dur_IMild = 1/(overall$gamma_IMild),
                                      dur_ICase =  2/(overall$gamma_ICase),
                                      prob_hosp = overall$prob_hosp,
                                      mixing_matrix = baseline_matrix,
                                      R0 = 3)
overall$beta_set <- beta_set

# Initialising the Model
x <- odin::odin("inst/odin/thera_healthcare_ext_SEIR.R")

# Bed Related Parameters
overall$hosp_beds <- 10000000
overall$ICU_beds <- 10000000

# Oxygen Related Parameters
overall$input_oxygen_supply <- 1000000000
overall$input_baseline_oxygen_demand <- 100
overall$oxygen_availability_0 <- 1000000000
overall$max_leftover <- 1000000

# MV Related Parameters
overall$MV_capacity <- 1000000000

# Running the Model
set.seed(100)
overall$dt <- 0.1
mod <- apothecary_SEIR(user = overall)
t <- seq(from = 0, to = 250/0.1)
results <- mod$run(t, replicate = 1)
index <- squire:::odin_index(mod)

plot(apply(results[, index$S, 1], 1, sum))
plot(apply(results[, index$IMild, 1], 1, sum))
plot(apply(results[, index$ICase1, 1], 1, sum))
plot(apply(results[, index$ICase2, 1], 1, sum))
plot(apply(results[, index$R, 1], 1, sum))
plot(apply(results[, index$D_Community, 1], 1, sum))
plot(apply(results[, index$D_Hospital, 1], 1, sum))


# Checking Hospital Occupancy
plot(apply(results[, c(index$IMod_GetHosp_GetOx_Die1, index$IMod_GetHosp_GetOx_Die2,
                       index$IMod_GetHosp_GetOx_Surv1, index$IMod_GetHosp_GetOx_Surv2,
                       index$IMod_GetHosp_NoOx_Die1, index$IMod_GetHosp_NoOx_Die2,
                       index$IMod_GetHosp_NoOx_Surv1, index$IMod_GetHosp_NoOx_Surv2,
                       index$IRec1, index$IRec2), 1], 1, sum), type = "l", ylab = "b")

plot(apply(results[, c(index$IMod_NoHosp_NoOx_Die1, index$IMod_NoHosp_NoOx_Die2,
                       index$IMod_NoHosp_NoOx_Surv1, index$IMod_NoHosp_NoOx_Surv2), 1], 1, sum),
     type = "l", ylab = "b")

# Checking ICU Occupancy
plot(apply(results[, c(index$ISev_GetICU_GetOx_Die1, index$ISev_GetICU_GetOx_Die2,
                       index$ISev_GetICU_GetOx_Surv1, index$ISev_GetICU_GetOx_Surv2,
                       index$ISev_GetICU_NoOx_Die1, index$ISev_GetICU_NoOx_Die2,
                       index$ISev_GetICU_NoOx_Surv1, index$ISev_GetICU_NoOx_Surv2), 1], 1, sum),
     type = "l", ylab = "b")

plot(apply(results[, c(index$ICrit_GetICU_GetOx_GetMV_Die1, index$ICrit_GetICU_GetOx_GetMV_Die2,
                       index$ICrit_GetICU_GetOx_GetMV_Surv1, index$ICrit_GetICU_GetOx_GetMV_Surv2,
                       index$ICrit_GetICU_GetOx_NoMV_Die1, index$ICrit_GetICU_GetOx_NoMV_Die2,
                       index$ICrit_GetICU_GetOx_NoMV_Surv1, index$ICrit_GetICU_GetOx_NoMV_Surv2,
                       index$ICrit_GetICU_NoOx_NoMV_Die1, index$ICrit_GetICU_NoOx_NoMV_Die2,
                       index$ICrit_GetICU_NoOx_NoMV_Surv1, index$ICrit_GetICU_NoOx_NoMV_Surv2), 1], 1, sum),
     type = "l", ylab = "b")

plot(apply(results[, c(index$ISev_NoICU_NoOx_Die1, index$ISev_NoICU_NoOx_Die2,
                       index$ISev_NoICU_NoOx_Surv1, index$ISev_NoICU_NoOx_Surv2,
                       index$ICrit_NoICU_NoOx_NoMV_Die1, index$ICrit_NoICU_NoOx_NoMV_Die2,
                       index$ICrit_NoICU_NoOx_NoMV_Surv1, index$ICrit_NoICU_NoOx_NoMV_Surv2), 1], 1, sum),
     type = "l", ylab = "b")



index_leaving <- c(index$n_IMod_GetHosp_GetOx_Die2_D_Hospital, index$n_IMod_GetHosp_GetOx_Surv2_R,
                   index$n_IMod_GetHosp_NoOx_Die2_D_Hospital, index$n_IMod_GetHosp_NoOx_Surv2_R)
index_entering <- c(index$n_ISev_GetICU_GetOx_Surv2_Rec, index$n_ISev_GetICU_NoOx_Surv2_Rec,
                    index$n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec, index$n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec,
                    index$n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec)

plot(apply(results[, index_leaving, 1], 1, sum), type = "l", ylab = "b")
plot(apply(results[, index_entering, 1], 1, sum), type = "l", ylab = "b")

plot(apply(results[, c(index$IRec1, index$IRec2), 1], 1, sum), type = "l", ylab = "b")



plot(apply(results[, index$IMod_GetHosp_GetOx_Die1, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_GetOx_Die2, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_GetOx_Surv1, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_GetOx_Surv2, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_NoOx_Die1, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_NoOx_Die2, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_NoOx_Surv1, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IMod_GetHosp_NoOx_Surv2, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IRec1, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$IRec2, 1], 1, sum), type = "l", col = "red")








deaths <- apply(results[, index$D_Community, 1], 1, sum) + apply(results[, index$D_Hospital, 1], 1, sum)
max(deaths)

y <- run_explicit_SEEIR_model(country = "United Kingdom", R0 = 3, hosp_bed_capacity = 10000000, ICU_bed_capacity = 10000000,
                              replicates = 1)
y_d <- format_output(y, var_select = "deaths")
sum(y_d$y)

lines(deaths)


oxygen_supply + leftover - baseline_oxygen_demand

results[, index$oxygen_supply, 1]
results[, index$baseline_oxygen_demand, 1]
results[, index$leftover, 1]
results[, index$temp_leftover, 1]
sum(results[, index$leftover, 1] == results[, index$temp_leftover, 1]) # don't match at the upper end where max_leftover takes over

plot(results[, index$oxygen_availability, 1], type = "l")

results[, index$leftover, 1]

pars$input_oxygen_supply - pars$input_baseline_oxygen_demand

a <- apply(results[, index$number_GetHosp_Ox, 1], 1, sum)
b <- apply(results[, index$number_GetICU_GetOx_NeedMV, 1], 1, sum) * pars$severe_critical_case_oxygen_consumption_multiplier
c <- apply(results[, index$number_GetICU_GetOx, 1], 1, sum) * pars$severe_critical_case_oxygen_consumption_multiplier
sum(results[, index$oxygen_used, 1] == a + b + c)

plot(results[, index$oxygen_supply, 1] - results[, index$baseline_oxygen_demand, 1], type = "l")
plot(a + b + c, type = "l")



plot(results[, index$oxygen_supply, 1] - results[, index$baseline_oxygen_demand, 1] - (a + b + c), type = "l")

results[, index$oxygen_availability, 1] - results[, index$oxygen_used, 1]


# Checking We Don't Lose Infections Anywhere
infected <- sum(apply(results[, index$n_E2_I, 1], 1, sum))
D_Community <- max(apply(results[, index$D_Community, 1], 1, sum))
D_Hospital <- max(apply(results[, index$D_Hospital, 1], 1, sum))
recovered <- max(apply(results[, index$R, 1], 1, sum))

# Checking all infected individuals get assigned to be mild or cases as appropriate - checks out!
infected
sum(apply(results[, index$n_E2_IMild, 1], 1, sum)) + sum(apply(results[, index$n_E2_ICase1, 1], 1, sum)) + sum(apply(results[, index$n_E2_IAsymp, 1], 1, sum))

# Check that all mild infections end up in recoveries
sum(results[, index$n_E2_IMild, 1])
sum(results[, index$n_IMild_R, 1])
sum(results[, index$n_S_E1, 1])

sum(results[, index$n_E1_E2, 1])


infected - (D_Community + D_Hospital + recovered)

sum(results[, index$n_E2_IMild, 1]) - sum(results[, index$n_IMild_R, 1])



# Checking Number of Admitted to Hospital/ICU Beds
plot(apply(results[, index$n_ICase2_Hosp, 1], 1, sum), type = "l")
lines(apply(results[, index$number_GetHosp, 1], 1, sum) +
        apply(results[, index$number_GetICU, 1], 1, sum) +
        apply(results[, index$number_NotHosp, 1], 1, sum) +
        apply(results[, index$number_NotICU, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$number_GetHosp, 1], 1, sum), type = "l")
plot(apply(results[, index$number_GetICU, 1], 1, sum), type = "l")
plot(apply(results[, index$number_NotHosp, 1], 1, sum), type = "l")
plot(apply(results[, index$number_NotICU, 1], 1, sum), type = "l")

# Checking Overall Oxygen Assignment
## need to do something like number receiving oxygen at each timepoin

# Checking Number of Hospital Bed Occupying Individuals Who Get Oxygen
plot(results[, index$available_oxygen_for_hosp_beds, 1], type = "l")
lines(apply(results[, index$number_GetHosp_Ox, 1], 1, sum), type = "l", col = "red")
plot(apply(results[, index$number_GetHosp_NoOx, 1], 1, sum), type = "l", col = "red")

# Checking Number of ICU Bed Occupying Individuals Who Get Oxygen (and MVs if Required)
plot(results[, index$available_oxygen_for_ICU_beds, 1], type = "l")
lines(results[, index$available_oxygen_for_ICU_Ox, 1] +
        results[, index$available_oxygen_for_ICU_MV, 1], type = "l", col = "red")
plot(apply(results[, index$number_GetICU, 1], 1, sum), type = "l")
lines(apply(results[, index$number_GetICU_GetOx, 1], 1, sum) +
        apply(results[, index$number_GetICU_NoOx, 1], 1, sum) +
        apply(results[, index$number_GetICU_GetOx_NeedMV, 1], 1, sum) +
        apply(results[, index$number_GetICU_NoOx_NeedMV, 1], 1, sum), type = "l", col = "red")

plot(apply(results[, index$number_GetICU_GetOx_NeedMV, 1], 1, sum), type = "l")
lines(apply(results[, index$number_GetICU_GetOx_GetMV, 1], 1, sum) +
        apply(results[, index$number_GetICU_GetOx_NoMV, 1], 1, sum), type = "l", col = "red")

plot(apply(results[, index$number_GetHosp_NoOx, 1], 1, sum))

plot(results[, index$hosp_occ, 1], type = "l")
plot(results[, index$oxygen_used, 1], type = "l")

max(apply(results[, index$number_GetICU_GetOx_NeedMV, 1], 1, sum)) * pars$severe_critical_case_oxygen_consumption_multiplier
max(apply(results[, index$number_GetICU_GetOx, 1], 1, sum)) * pars$severe_critical_case_oxygen_consumption_multiplier
max(apply(results[, index$number_GetHosp_Ox, 1], 1, sum))



plot(results[, "time", 1], apply(results[, index$n_E2_I, 1], 1, sum), type = "l")
plot(results[, "time", 1], results[, index$oxygen_availability, 1], type = "l",
     ylim = c(0, max(results[, index$oxygen_availability, 1]))) ## oxygen availability drops to -21 - what's up with that?
























overall <- max(apply(results[, index$R, 1], 1, sum) + apply(results[, index$D_Community, 1], 1, sum) +
                 apply(results[, index$D_Hospital, 1], 1, sum))

infections <- max(apply(results[, index$R, 1], 1, sum))
deaths <- max(apply(results[, index$D_Community, 1], 1, sum) + apply(results[, index$D_Hospital, 1], 1, sum))

sum(apply(results[, index$n_E2_I, 1], 1, sum))

100*sum(apply(results[, index$n_E2_I, 1], 1, sum))/sum(cpm$population)




odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial(0))
  model$transform_variables(seq_len(1L + n_state + n_out))
}

results[, "n_S_E1[15]", 1]
results[, "E2[15]", 1]
results[, "n_E1_E2[15]", 1]
results[, "n_E2_I[15]", 1]
results[, "n_E2_IMild[15]", 1]
results[, "n_E2_ICase1[15]", 1]

results[, "n_ICase2_Hosp[15]", 1]

results[, "n_E2_IMild[15]", 1]

results[, "n_ICase2_Hosp[15]", 1]
results[, "IMod_GetHosp_GetOx_Surv1[1]", 1]

results[, "ICase2[17]", 1]

results[, "number_req_Hosp[1]", 1]
results[, "total_req_Hosp", 1]
results[, "hosp_occ", 1]
results[, "current_free_hosp", 1]

results[, "total_GetHosp", 1]
results[, "number_GetHosp[1]", 1]
results[, "number_NotHosp[1]", 1]

results[, "oxygen_supply", 1]
results[, "leftover", 1]
results[, "oxygen_demand", 1]

results[, "prop_ox_hosp_beds", 1]
results[, "available_oxygen_for_hosp_beds", 1]
results[, "available_oxygen_for_ICU_beds", 1]
results[, "total_GetHosp_GetOx", 1]

results[, "oxygen_availability", 1]

results[, "number_GetHosp_Ox[1]", 1]
results[, "number_GetHosp_NoOx[1]", 1]

results[, "total_GetICU_GetOx", 1]
results[, "number_GetICU_GetOx_overall[1]", 1]

x <- run_explicit_SEEIR_model(R0 = 3, country = "United Kingdom")
x$parameters$dur_IMild
x$parameters$dur_ICase
x$parameters$prob_hosp
x$parameters$contact_matrix_set
x$parameters$beta_set


results[, "number_req_ICU_MV[1]", 1]
results[, "number_req_ICU_Ox[1]", 1]

results[, "total_req_ICU_MV", 1]
results[, "total_req_ICU_Ox", 1]

results[, "MV_occ", 1]
results[, "current_free_MV", 1]

results[, "available_oxygen_for_ICU_Ox", 1]
results[, "available_oxygen_for_ICU_MV", 1]

results[, "number_GetICU_GetOx_NeedMV[5]", 1]
results[, "total_GetICU_GetOx_Need_MV", 1]
results[, "total_GetICU_GetOx_GetMV", 1]

x <- odin::odin("inst/odin/updated_healthcare_rep_SEIR.R")
x <- updated_healthcare_rep_SEIR
mod <- x(user = pars, unused_user_action = "ignore")
t <- seq(from = 1, to = 100/0.1)
t <- round(seq(1/0.1, length(t)+(1/0.1), by=1/0.1))
set.seed(1)
results <- mod$run(t, replicate = 1)
process_contact_matrix_scaled_age(x$parameters$contact_matrix_set[[1]], x$parameters$population)

x <- run_explicit_SEEIR_model(R0 = 3, country = "United Kingdom", hosp_bed_capacity = 1000000000,
                              ICU_bed_capacity = 1000000000, replicates = 1)

index <- squire::odin_index(x$model)
sum(x$output[, index$n_E2_IMild, 1])

deaths <- format_output(x, var_select = "D")
deaths <- max(deaths$y)

recovered <- format_output(x, var_select = "R")
recovered <- max(recovered$y)

100 * (deaths+recovered)/sum(cpm$population)


