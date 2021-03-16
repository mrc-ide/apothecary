# Unlimited Healthcare No Drugs
w <- run_apothecary(country = "France",
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000)

# Unlimited Healthcare, Drugs in Hospitalised Only
x <- run_apothecary(country = "France",
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                    drug_11_prop_treat = 1,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_indic_IMod_NoHosp_NoOx = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5, drug_11_NoHosp_effect_size = 1,
                    drug_12_prop_treat = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_indic_ISev_NoICU_NoOx = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5, drug_12_NoICU_effect_size = 1,
                    drug_13_prop_treat = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_indic_ICrit_NoICU_NoOx_NoMV = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_NoICU_effect_size = 1)

# Unlimited Healthcare, Drugs to Everyone
y <- run_apothecary(country = "France",
                    hosp_bed_capacity = 100000000, ICU_bed_capacity = 10000000, prop_ox_hosp_beds = 1, prop_ox_ICU_beds = 1, MV_capacity = 100000000,
                    drug_11_prop_treat = 1,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_indic_IMod_NoHosp_NoOx = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5, drug_11_NoHosp_effect_size = 0.5,
                    drug_12_prop_treat = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_indic_ISev_NoICU_NoOx = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5, drug_12_NoICU_effect_size = 0.5,
                    drug_13_prop_treat = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_indic_ICrit_NoICU_NoOx_NoMV = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_NoICU_effect_size = 0.5)

index <- apothecary:::odin_index(x$model)
max(apply(w$output[, index$D], 1, sum))
max(apply(x$output[, index$D], 1, sum))
max(apply(y$output[, index$D], 1, sum))


# Limited Healthcare, No Drugs
w <- run_apothecary(country = "France")

# Limited Healthcare, Drugs in Hospitalised Only
x <- run_apothecary(country = "France",
                    drug_11_prop_treat = 1,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_indic_IMod_NoHosp_NoOx = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5, drug_11_NoHosp_effect_size = 1,
                    drug_12_prop_treat = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_indic_ISev_NoICU_NoOx = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5, drug_12_NoICU_effect_size = 1,
                    drug_13_prop_treat = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_indic_ICrit_NoICU_NoOx_NoMV = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_NoICU_effect_size = 1)

# Limited Healthcare, Drugs to Everyone
y <- run_apothecary(country = "France",
                    drug_11_prop_treat = 1,
                    drug_11_indic_IMod_GetHosp_GetOx = 1, drug_11_indic_IMod_GetHosp_NoOx = 1, drug_11_indic_IMod_NoHosp_NoOx = 1,
                    drug_11_GetOx_effect_size = 0.5, drug_11_NoOx_effect_size = 0.5, drug_11_NoHosp_effect_size = 0.5,
                    drug_12_prop_treat = 1,
                    drug_12_indic_ISev_GetICU_GetOx = 1, drug_12_indic_ISev_GetICU_NoOx = 1, drug_12_indic_ISev_NoICU_NoOx = 1,
                    drug_12_GetOx_effect_size = 0.5, drug_12_NoOx_effect_size = 0.5, drug_12_NoICU_effect_size = 0.5,
                    drug_13_prop_treat = 1,
                    drug_13_indic_ICrit_GetICU_GetOx_GetMV = 1, drug_13_indic_ICrit_GetICU_GetOx_NoMV = 1, drug_13_indic_ICrit_GetICU_NoOx_NoMV = 1, drug_13_indic_ICrit_NoICU_NoOx_NoMV = 1,
                    drug_13_GetOx_GetMV_effect_size = 0.5, drug_13_GetOx_NoMV_effect_size = 0.5, drug_13_NoOx_NoMV_effect_size = 0.5, drug_13_NoICU_effect_size = 0.5)

index <- apothecary:::odin_index(y$model)

max(apply(w$output[, index$D], 1, sum))
max(apply(x$output[, index$D], 1, sum))
max(apply(y$output[, index$D], 1, sum))

plot(apply(w$output[, index$D], 1, sum), type = "l")
lines(apply(y$output[, index$D], 1, sum), type = "l")

probs$prob_moderate_death_no_hosp_no_ox_baseline
unname(y$output[1, index$prob_moderate_death_no_hosp_no_ox_baseline])

probs$prob_severe

unname(y$output[1, index$prob_moderate_death_get_hosp_get_ox_baseline])/unname(y$output[1, index$prob_moderate_death_get_hosp_get_ox])
unname(y$output[1, index$prob_moderate_death_get_hosp_no_ox_baseline])/unname(y$output[1, index$prob_moderate_death_get_hosp_no_ox])
unname(y$output[1, index$prob_moderate_death_no_hosp_no_ox_baseline])/unname(y$output[1, index$prob_moderate_death_no_hosp_no_ox])

unname(y$output[1, index$prob_severe_death_get_ICU_get_ox_baseline])/unname(y$output[1, index$prob_severe_death_get_ICU_get_ox])
unname(y$output[1, index$prob_severe_death_get_ICU_no_ox_baseline])/unname(y$output[1, index$prob_severe_death_get_ICU_no_ox])
unname(y$output[1, index$prob_severe_death_no_ICU_no_ox_baseline])/unname(y$output[1, index$prob_severe_death_no_ICU_no_ox])

unname(y$output[1, index$prob_critical_death_get_ICU_get_ox_get_MV_baseline])/unname(y$output[1, index$prob_critical_death_get_ICU_get_ox_get_MV])
unname(y$output[1, index$prob_critical_death_get_ICU_get_ox_no_MV_baseline])/unname(y$output[1, index$prob_critical_death_get_ICU_get_ox_no_MV])
unname(y$output[1, index$prob_critical_death_get_ICU_no_ox_no_MV_baseline])/unname(y$output[1, index$prob_critical_death_get_ICU_no_ox_no_MV])
unname(y$output[1, index$prob_critical_death_no_ICU_no_ox_no_MV_baseline])/unname(y$output[1, index$prob_critical_death_no_ICU_no_ox_no_MV])


y$parameters$prob_moderate_death_get_hosp_get_ox_Drug_11

y$parameters$prob_moderate_death_no_hosp_no_ox_baseline


prob_moderate_death_get_hosp_get_ox_baseline[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed AND oxygen)
prob_moderate_death_get_hosp_get_ox_Drug_11[] <- ((1 - drug_11_prop_treat) * prob_moderate_death_get_hosp_get_ox_baseline[i]) + (drug_11_prop_treat * drug_11_GetOx_effect_size * prob_moderate_death_get_hosp_get_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 11 properties
prob_moderate_death_get_hosp_get_ox[] <- if (drug_11_indic_IMod_GetHosp_GetOx == 1) prob_moderate_death_get_hosp_get_ox_Drug_11[i] else prob_moderate_death_get_hosp_get_ox_baseline[i]

prob_moderate_death_get_hosp_no_ox_baseline[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed BUT no oxygen)
prob_moderate_death_get_hosp_no_ox_Drug_11[] <- ((1 - drug_11_prop_treat) * prob_moderate_death_get_hosp_no_ox_baseline[i]) + (drug_11_prop_treat * drug_11_NoOx_effect_size * prob_moderate_death_get_hosp_no_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 11 properties
prob_moderate_death_get_hosp_no_ox[] <- if (drug_11_indic_IMod_GetHosp_NoOx == 1) prob_moderate_death_get_hosp_no_ox_Drug_11[i] else prob_moderate_death_get_hosp_no_ox_baseline[i]

prob_moderate_death_no_hosp_no_ox_baseline[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you do NOT receive a hospital bed and you do NOT receive oxygen
prob_moderate_death_no_hosp_no_ox_Drug_11[] <- ((1 - drug_11_prop_treat) * prob_moderate_death_no_hosp_no_ox_baseline[i]) + (drug_11_prop_treat * drug_11_NoHosp_effect_size * prob_moderate_death_no_hosp_no_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 11 properties
prob_moderate_death_no_hosp_no_ox[] <- if (drug_11_indic_IMod_NoHosp_NoOx == 1) prob_moderate_death_no_hosp_no_ox_Drug_11[i] else prob_moderate_death_no_hosp_no_ox_baseline[i]



z$parameters$prob_moderate_death_get_hosp_get_ox_baseline
z$parameters$prob_moderate_death_get_hosp_get_ox_baseline

index <- apothecary:::odin_index(x$model)


