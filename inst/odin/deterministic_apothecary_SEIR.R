## TIMESTEP RELATED PARAMETERS
##------------------------------------------------------------------------------
time <- t
output(time) <- TRUE
N_age <- user()

## DRUG RELATED PARAMETERS AND EFFECTS
##------------------------------------------------------------------------------

## Drugs Taken Prophylactically - Includes Properties 1 & 2
##---------------------------------------------------------

# Property 1 - protects Susceptible individuals from infection (individuals flow to P and stay there until drug wears off)
drug_1_indic <- user() # binary indicator (0 or 1) specifying whether drug property 1 is activated or not
drug_1_effect_size <- user() # the multiple of the FOI experienced by the individuals in the P compartment (0 means drug completely protective, 1 means no protection)

# Property 2 - reduces the severity of disease that occurs if symptom onset occurs whilst still protected (preferential movement to IMild over ICase - proportion going to IAsymp unaffected)
drug_2_indic <- user() # binary indicator (0 or 1) specifying whether drug property 2 is activated or not
drug_2_effect_size <- user() # the multiple of the baseline age-specific probability of ICase disease severity that occurs upon infection, protected individuals flow to IMild instead (1 = the same, <1 = protection from severe disease)

# General Properties of the Prophylactic Drug (Independent of the Specific Pharmacological Properties Below)
prophylactic_drug_timing_1 <- user() # timing of the first round of mass administration to currently Susceptible individuals
prophylactic_drug_timing_2 <- user() # timing of the second round of mass administration to currently Susceptible individuals
prophylactic_prop_treat <- user() # proportion of individuals in S who receive the drug and move to compartment P (where P = individuals with active drug in their bodies)
prophylactic_drug_wane <- user() # proportion of individuals at each timestep for whom the drug wears off and who move back to S/E (depending on which properties are activated)

## Drugs Taken Whilst Infected But Pre-Hospital - Includes Properties 3, 4 & 5
##----------------------------------------------------------------------------

# Property 3 - reduces the severity of disease once symptom onset occurs (preferential movement to IMild over ICase - proportion going to IAsymp unaffected)
drug_3_indic <- user() # binary indicator (0 or 1) specifying whether drug property 3 is activated or not
drug_3_prop_treat <- user() # the proportion of individuals at the E2 -> I transition receiving the drug
drug_3_effect_size <- user() # the multiple of the baseline age-specific probability of ICase disease severity that occurs upon infection, protected individuals flow to IMild instead (1 = the same, <1 = protection from severe disease)

# Drug 4 reduces the time which individuals spend in IMild, i.e. they recover more quickly
drug_4_indic <- user() # binary indicator (0 or 1) specifying whether drug property 4 is activated or not
drug_4_prop_treat <- user() # proportion of individuals in IMild who receive the drug
drug_4_effect_size <- user() # the multiple by which the rate of recovery for individuals in IMild has increased by (1 = the same, >1 = increased rate)

# Drug 5 reduces infectivity of individuals in IMild/ICase, reducing the FOI experienced by Susceptible individuals
drug_5_indic_IMild <- user() # binary indicator (0 or 1) specifying whether drug property 5 is activated or not for IMild individuals
drug_5_indic_ICase <- user() # binary indicator (0 or 1) specifying whether drug property 5 is activated or not for ICase individuals
drug_5_prop_treat <- user() # proportion of individuals in IMild or ICase (depending on which indics are activated) who receive the drug
drug_5_effect_size <- user() # the multiple by which treated individuals are as infectious compared to untreated individuals (1 = the same, <1 = individuals less infectious)

## Drugs Taken In Hospital - Includes Properties 6 & 7 (reduce disease severity),
## Properties 8, 9 & 10 (reduce duration of stay), and Properties 11, 12 and 13 (reduce mortality)
##---------------------------------------------------------------------------------------------------------------------

# Drug 6 reduces the severity of disease in hospital, leading to a greater proportion of individuals flowing to IMod over ISev/ICrit
drug_6_indic <- user() # binary indicator (0 or 1) specifying whether drug property 6 is activated or not
drug_6_prop_treat <- user() # proportion of individuals at the ICase2 -> IHosp transition who receive the drug
drug_6_effect_size <- user() # the proportion of individual flowing into IMod who would have otherwise flowed into ISev or ICrit (0 = baseline, 1 = all severe/critical disease prevented)

# Drug 7 reduces the severity of disease in hospital, leading to a greater proportion of individuals flowing to ISev over ICrit
drug_7_indic <- user() # binary indicator (0 or 1) specifying whether drug property 7 is activated or not
drug_7_prop_treat <- user() # proportion of individuals at the ICase2 -> IHosp transition who receive the drug
drug_7_effect_size <- user() # the proportion of individual flowing into ISev who would have otherwise flowed into ICrit (0 = baseline, 1 = all critical disease prevented)

# Drug 8 reduces the duration of stay in hospital for IMod Patients who survive - can be dependent on whether receiving other appropriate treatment (Oxygen) or not
drug_8_indic_IMod_GetHosp_GetOx <- user() # binary indicator (0 or 1) specifying whether drug property 8 is activated or not for IMod who get Hosp Bed and Oxygen
drug_8_indic_IMod_GetHosp_NoOx <- user() # binary indicator (0 or 1) specifying whether drug property 8 is activated or not for IMod who get Hosp Bed BUT NO Oxygen
drug_8_prop_treat <- user() # proportion of individuals in relevant IMod compartments (hospital based ones) who receive the drug
drug_8_GetOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in IMod who receive a hospital bed and oxygen (1 = baseline, >1 = increase rate of recovery)
drug_8_NoOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in IMod who receive a hospital bed BUT NO oxygen (1 = baseline, >1 = increase rate of recovery)

# Drug 9 reduces the duration of stay in hospital for ISev Patients who survive - can be dependent on whether receiving other appropriate treatment (Oxygen) or not
# NOTE CHANGE CURRENTLY NOT APPLIED TO IREC COMPARTMENTS - PROBABLY SHOULD BE APPLIED?? OR NOT?? POTENTIAL WEIRD BEHAVIOUR WITH HOSPITAL BED OCCUPANCY POSSIBLY IF NOT APPLIED...
drug_9_indic_ISev_GetICU_GetOx <- user() # binary indicator (0 or 1) specifying whether drug property 9 is activated or not for ISev who get ICU Bed and Oxygen
drug_9_indic_ISev_GetICU_NoOx <- user() # binary indicator (0 or 1) specifying whether drug property 9 is activated or not for ISev who get ICU Bed BUT NO Oxygen
drug_9_prop_treat <- user() # proportion of individuals in relevant ISev compartments (hospital based ones) who receive the drug
drug_9_GetOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ISev who receive an ICU bed and oxygen (1 = baseline, >1 = increase rate of recovery)
drug_9_NoOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ISev who receive an ICU bed BUT NO oxygen (1 = baseline, >1 = increase rate of recovery)

# Drug 10 reduces the duration of stay in hospital for ICrit Patients - can be dependent on whether receiving other appropriate treatment (Oxygen and MV) or not
# NOTE CHANGE CURRENTLY NOT APPLIED TO IREC COMPARTMENTS - PROBABLY SHOULD BE APPLIED?? OR NOT?? POTENTIAL WEIRD BEHAVIOUR WITH HOSPITAL BED OCCUPANCY POSSIBLY IF NOT APPLIED...
drug_10_indic_ICrit_GetICU_GetOx_GetMV <- user() # binary indicator (0 or 1) specifying whether drug property 10 is activated or not for ICrit who get ICU Bed, Oxygen and MV
drug_10_indic_ICrit_GetICU_GetOx_NoMV <- user() # binary indicator (0 or 1) specifying whether drug property 10 is activated or not for ICrit who get ICU Bed, Oxygen BUT NOT MV
drug_10_indic_ICrit_GetICU_NoOx_NoMV <- user() # binary indicator (0 or 1) specifying whether drug property 10 is activated or not for ICrit who get ICU Bed BUT NO Oxygen AND NO MV
drug_10_prop_treat <- user() # proportion of individuals in relevant ICrit compartments (hospital based ones) who receive the drug
drug_10_GetOx_GetMV_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ICrit who receive an ICU bed, oxygen and MV (1 = baseline, >1 = increase rate of recovery)
drug_10_GetOx_NoMV_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ICrit who receive an ICU bed, oxygen BUT NO MV (1 = baseline, >1 = increase rate of recovery)
drug_10_NoOx_NoMV_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ICrit who receive an ICU bed, BUT NO oxygen OR MV (1 = baseline, >1 = increase rate of recovery)

# Drug 11 reduces mortality in IMod Patients - can be dependent on receiving other appropriate treatment (Oxygen) or not
drug_11_indic_IMod_GetHosp_GetOx <- user() # binary indicator (0 or 1) specifying whether drug property 11 is activated or not for IMod who get Hosp Bed and Oxygen
drug_11_indic_IMod_GetHosp_NoOx <- user() # binary indicator (0 or 1) specifying whether drug property 11 is activated or not for IMod who get Hosp Bed BUT NOT Oxygen
drug_11_prop_treat <- user() # Proportion of individuals in relevant IMod compartments who receive the drug
drug_11_GetOx_effect_size <- user() # the multiple of the baseline age-specific probability of IMod death treated individuals who receive Hosp Bed and Oxygen experience (1 = the same as untreated, <1 = mortality reduction)
drug_11_NoOx_effect_size <- user() # the multiple of the baseline age-specific probability of IMod death treated individuals who receive Hosp Bed BUT NOT Oxygen experience (1 = the same as untreated, <1 = mortality reduction)

# Drug 12 reduces mortality in ISev Patients - can be dependent on receiving other appropriate treatment (Oxygen) or not
drug_12_indic_ISev_GetICU_GetOx <- user() # binary indicator (0 or 1) specifying whether drug property 12 is activated or not for ISev who get ICU Bed and Oxygen
drug_12_indic_ISev_GetICU_NoOx <- user() # binary indicator (0 or 1) specifying whether drug property 12 is activated or not for ISev who get ICU Bed BUT NOT Oxygen
drug_12_prop_treat <- user() # proportion of individuals in relevant ISev compartments who receive the drug
drug_12_GetOx_effect_size <- user() # the multiple of the baseline age-specific probability of ISev death treated individuals who receive ICU Bed and Oxygen experience (1 = the same as untreated, <1 = mortality reduction)
drug_12_NoOx_effect_size <- user() # the multiple of the baseline age-specific probability of ISev death treated individuals who receive ICU Bed BUT NO Oxygen experience (1 = the same as untreated, <1 = mortality reduction)

# Drug 13 reduces mortality in ICrit Patients - can be dependent on receiving other appropriate treatment (Oxygen and MV) or not
drug_13_indic_ICrit_GetICU_GetOx_GetMV <- user() # binary indicator (0 or 1) specifying whether drug property 13 is activated or not for ICrit who get ICU Bed, Oxygen and MV
drug_13_indic_ICrit_GetICU_GetOx_NoMV <- user() # binary indicator (0 or 1) specifying whether drug property 13 is activated or not for ICrit who get ICU Bed, Oxygen BUT NOT MV
drug_13_indic_ICrit_GetICU_NoOx_NoMV <- user() # binary indicator (0 or 1) specifying whether drug property 13 is activated or not for ICrit who get ICU Bed BUT NOT Oxygen AND NOT MV
drug_13_prop_treat <- user() # proportion of individuals in relevant ICrit compartments who receive the drug
drug_13_GetOx_GetMV_effect_size <- user() # the multiple of the baseline age-specific probability of ICrit death treated individuals who receive ICU Bed, Oxygen and MV experience (1 = the same as untreated, <1 = mortality reduction)
drug_13_GetOx_NoMV_effect_size <- user() # the multiple of the baseline age-specific probability of ICrit death treated individuals who receive ICU Bed, Oxygen BUT NOT MV experience (1 = the same as untreated, <1 = mortality reduction)
drug_13_NoOx_NoMV_effect_size <- user() # the multiple of the baseline age-specific probability of ICrit death treated individuals who receive ICU Bed BUT NOT Oxygen AND NOT MV experience (1 = the same as untreated, <1 = mortality reduction)


## RATES
##------------------------------------------------------------------------------
gamma_E <- user() # passage through latent infection
gamma_IAsymp <- user() # asymptomatic infection to recovery
gamma_IMild <- user() # mild infection to recovery
gamma_IMild_Drug_4 <- ((1 - drug_4_prop_treat) * gamma_IMild) + (drug_4_prop_treat * drug_4_effect_size * gamma_IMild) # weighted sum of recovery rates for treated/untreated depending on Drug 4 properties
gamma_ICase <- user() # symptom onset to requiring hospitalisation
gamma_rec <- user() # rate of progression through post-ICU recovery compartment

# Rates Related to Requiring Hospital Bed and Oxygen, Incorporating Effects of Drug 8 If Relevant
gamma_IMod_GetHosp_GetOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and surviving
gamma_IMod_GetHosp_GetOx_Surv_Drug_8 <- (((1 - drug_8_prop_treat) * gamma_IMod_GetHosp_GetOx_Surv) + (drug_8_prop_treat * drug_8_GetOx_effect_size * gamma_IMod_GetHosp_GetOx_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 8 properties
gamma_IMod_GetHosp_GetOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and dying

gamma_IMod_GetHosp_NoOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed but NOT oxygen and surviving
gamma_IMod_GetHosp_NoOx_Surv_Drug_8 <- (((1 - drug_8_prop_treat) * gamma_IMod_GetHosp_NoOx_Surv) + (drug_8_prop_treat * drug_8_NoOx_effect_size * gamma_IMod_GetHosp_NoOx_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 8 properties
gamma_IMod_GetHosp_NoOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on getting  hosp bed but NOT oxygen and dying

gamma_IMod_NoHosp_NoOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and surviving
gamma_IMod_NoHosp_NoOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and dying

# Rates Related to Requiring ICU Bed and Oxygen, Incorporating Effects of Drug 9 If Relevant
gamma_ISev_GetICU_GetOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and surviving
gamma_ISev_GetICU_GetOx_Surv_Drug_9 <- (((1 - drug_9_prop_treat) * gamma_ISev_GetICU_GetOx_Surv) + (drug_9_prop_treat * drug_9_GetOx_effect_size * gamma_ISev_GetICU_GetOx_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 9 properties
gamma_ISev_GetICU_GetOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and dying

gamma_ISev_GetICU_NoOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and surviving
gamma_ISev_GetICU_NoOx_Surv_Drug_9 <- (((1 - drug_9_prop_treat) * gamma_ISev_GetICU_NoOx_Surv) + (drug_9_prop_treat * drug_9_NoOx_effect_size * gamma_ISev_GetICU_NoOx_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 9 properties
gamma_ISev_GetICU_NoOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and dying

gamma_ISev_NoICU_NoOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and surviving
gamma_ISev_NoICU_NoOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and dying

# Rates Related to Requiring ICU Bed, Oxygen and Mechanical Ventilation, Incorporating Effects of Drug 10 If Relevant
gamma_ICrit_GetICU_GetOx_GetMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and surviving
gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10 <- (((1 - drug_10_prop_treat) * gamma_ICrit_GetICU_GetOx_GetMV_Surv) + (drug_10_prop_treat * drug_10_GetOx_GetMV_effect_size * gamma_ICrit_GetICU_GetOx_GetMV_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 10 properties
gamma_ICrit_GetICU_GetOx_GetMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and dying

gamma_ICrit_GetICU_GetOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and surviving
gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10 <- (((1 - drug_10_prop_treat) * gamma_ICrit_GetICU_GetOx_NoMV_Surv) + (drug_10_prop_treat * drug_10_GetOx_NoMV_effect_size * gamma_ICrit_GetICU_GetOx_NoMV_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 10 properties
gamma_ICrit_GetICU_GetOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and dying

gamma_ICrit_GetICU_NoOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and surviving
gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10 <- (((1 - drug_10_prop_treat) * gamma_ICrit_GetICU_NoOx_NoMV_Surv) + (drug_10_prop_treat * drug_10_NoOx_NoMV_effect_size * gamma_ICrit_GetICU_NoOx_NoMV_Surv)) # weighted sum of recovery rates for treated/untreated depending on Drug 10 properties
gamma_ICrit_GetICU_NoOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and dying

gamma_ICrit_NoICU_NoOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and surviving
gamma_ICrit_NoICU_NoOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and dying

## PROBABILITIES
##------------------------------------------------------------------------------
prob_asymp[] <- user() # probability of being asymptomatic conditional on being subclinical, by age
prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring ICU bed) by age
prob_critical[] <- user() # probability of critical disease (requiring ICU bed AND MV) by age, conditional on having severe disease

# Probabilities of Death Related to Requiring Hospital Bed and Oxygen, Incorporating Effects of Drug 11 If Relevant
prob_moderate_death_get_hosp_get_ox_baseline[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed AND oxygen)
prob_moderate_death_get_hosp_get_ox_Drug_11[] <- ((1 - drug_11_prop_treat) * prob_moderate_death_get_hosp_get_ox_baseline[i]) + (drug_11_prop_treat * drug_11_GetOx_effect_size * prob_moderate_death_get_hosp_get_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 11 properties
prob_moderate_death_get_hosp_get_ox[] <- if (drug_11_indic_IMod_GetHosp_GetOx == 1) prob_moderate_death_get_hosp_get_ox_Drug_11[i] else prob_moderate_death_get_hosp_get_ox_baseline[i]

prob_moderate_death_get_hosp_no_ox_baseline[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed BUT no oxygen)
prob_moderate_death_get_hosp_no_ox_Drug_11[] <- ((1 - drug_11_prop_treat) * prob_moderate_death_get_hosp_no_ox_baseline[i]) + (drug_11_prop_treat * drug_11_NoOx_effect_size * prob_moderate_death_get_hosp_no_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 11 properties
prob_moderate_death_get_hosp_no_ox[] <- if (drug_11_indic_IMod_GetHosp_NoOx == 1) prob_moderate_death_get_hosp_no_ox_Drug_11[i] else prob_moderate_death_get_hosp_no_ox_baseline[i]

prob_moderate_death_no_hosp_no_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you do NOT receive a hospital bed and you do NOT receive oxygen

# Probabilities of Death Related to Requiring ICU Bed and Oxygen, Incorporating Effects of Drug 12 If Relevant
prob_severe_death_get_ICU_get_ox_baseline[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed AND oxygen)
prob_severe_death_get_ICU_get_ox_Drug_12[] <- ((1 - drug_12_prop_treat) * prob_severe_death_get_ICU_get_ox_baseline[i]) + (drug_12_prop_treat * drug_12_GetOx_effect_size * prob_severe_death_get_ICU_get_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 12 properties
prob_severe_death_get_ICU_get_ox[] <- if (drug_12_indic_ISev_GetICU_GetOx == 1) prob_severe_death_get_ICU_get_ox_Drug_12[i] else prob_severe_death_get_ICU_get_ox_baseline[i]

prob_severe_death_get_ICU_no_ox_baseline[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed BUT no oxygen)
prob_severe_death_get_ICU_no_ox_Drug_12[] <- ((1 - drug_12_prop_treat) * prob_severe_death_get_ICU_no_ox_baseline[i]) + (drug_12_prop_treat * drug_12_NoOx_effect_size * prob_severe_death_get_ICU_no_ox_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 12 properties
prob_severe_death_get_ICU_no_ox[] <- if (drug_12_indic_ISev_GetICU_NoOx == 1) prob_severe_death_get_ICU_no_ox_Drug_12[i] else prob_severe_death_get_ICU_no_ox_baseline[i]

prob_severe_death_no_ICU_no_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you do NOT receive an ICU bed and you do NOT receive oxygen

# Probabilities of Death Related to Requiring ICU Bed, Oxygen and Mechanical Ventilation, Incorporating Effects of Drug 13 If Relevant
prob_critical_death_get_ICU_get_ox_get_MV_baseline[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen AND MV)
prob_critical_death_get_ICU_get_ox_get_MV_Drug_13[] <- ((1 - drug_13_prop_treat) * prob_critical_death_get_ICU_get_ox_get_MV_baseline[i]) + (drug_13_prop_treat * drug_13_GetOx_GetMV_effect_size * prob_critical_death_get_ICU_get_ox_get_MV_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 13 properties
prob_critical_death_get_ICU_get_ox_get_MV[] <- if (drug_13_indic_ICrit_GetICU_GetOx_GetMV == 1) prob_critical_death_get_ICU_get_ox_get_MV_Drug_13[i] else prob_critical_death_get_ICU_get_ox_get_MV_baseline[i]

prob_critical_death_get_ICU_get_ox_no_MV_baseline[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen BUT no MV)
prob_critical_death_get_ICU_get_ox_no_MV_Drug_13[] <- ((1 - drug_13_prop_treat) * prob_critical_death_get_ICU_get_ox_no_MV_baseline[i]) + (drug_13_prop_treat * drug_13_GetOx_NoMV_effect_size * prob_critical_death_get_ICU_get_ox_no_MV_baseline[i]) # weighted sum of death probabilities for treated/untreated depending on Drug 13 properties
prob_critical_death_get_ICU_get_ox_no_MV[] <- if (drug_13_indic_ICrit_GetICU_GetOx_NoMV == 1) prob_critical_death_get_ICU_get_ox_no_MV_Drug_13[i] else prob_critical_death_get_ICU_get_ox_no_MV_baseline[i]

prob_critical_death_get_ICU_no_ox_no_MV_baseline[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed BUT no oxygen and you do NOT receive MV
prob_critical_death_get_ICU_no_ox_no_MV_Drug_13[] <- ((1 - drug_13_prop_treat) * prob_critical_death_get_ICU_no_ox_no_MV_baseline[i]) + (drug_13_prop_treat * drug_13_NoOx_NoMV_effect_size * prob_critical_death_get_ICU_no_ox_no_MV_baseline[i]) # weighted sum of death probabilities depending on Drug 13 properties
prob_critical_death_get_ICU_no_ox_no_MV[] <- if (drug_13_indic_ICrit_GetICU_NoOx_NoMV == 1) prob_critical_death_get_ICU_no_ox_no_MV_Drug_13[i] else prob_critical_death_get_ICU_no_ox_no_MV_baseline[i]

prob_critical_death_no_ICU_no_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you do NOT receive an ICU bed, you do NOT receive oxygen, and you do NOT receive MV


## CALCULATING THE NUMBER OF INDIVIDUALS MOVING OUT OF NON-HEALTHCARE RELATED COMPARTMENTS
##----------------------------------------------------------------------------------------

# Terms describing individual flows out of compartments
n_S_E1[] <- lambda[i] * S[i]
n_E1_E2[] <- gamma_E * E1[i]
n_E2_I[] <- gamma_E * E2[i]

n_E2_ICase1_initial[] <- n_E2_I[i] * prob_hosp[i]
n_E2_ICase1[] <- if (drug_3_indic == 1) n_E2_ICase1_initial[i] * (1 - drug_3_prop_treat * drug_3_effect_size) else  n_E2_ICase1_initial[i]
n_E2_ICase1_Drug_5[] <- if (drug_5_indic_ICase == 1) n_E2_ICase1[i] * drug_5_prop_treat else 0
n_E2_ICase1_No_Drug_5[] <- if (drug_5_indic_ICase == 1) n_E2_ICase1[i] * (1 - drug_5_prop_treat) else n_E2_ICase1[i]

n_E2_IMild_or_IAsymp[] <- n_E2_I[i] - n_E2_ICase1[i]
n_E2_IAsymp[] <- n_E2_IMild_or_IAsymp[i] * prob_asymp[i]
n_E2_IMild[] <- (n_E2_IMild_or_IAsymp[i] - n_E2_IAsymp[i]) + (n_E2_ICase1_initial[i] - n_E2_ICase1[i])
n_E2_IMild_Drug_5[] <- if (drug_5_indic_IMild == 1) n_E2_IMild[i] * drug_5_prop_treat else 0
n_E2_IMild_No_Drug_5[] <- if (drug_5_indic_IMild == 1) n_E2_IMild[i] * (1 - drug_5_prop_treat) else n_E2_IMild[i]

x <- if (((time > prophylactic_drug_timing_1  && time <= prophylactic_drug_timing_1 + 1) || (time > prophylactic_drug_timing_2  && time <= prophylactic_drug_timing_2 + 1)) && (drug_1_indic == 1|| drug_2_indic == 1)) 1 else 0
output(x) <- TRUE

n_S_PS[] <- if (((time > prophylactic_drug_timing_1  && time <= prophylactic_drug_timing_1 + 1) || (time > prophylactic_drug_timing_2  && time <= prophylactic_drug_timing_2 + 1)) && (drug_1_indic == 1|| drug_2_indic == 1)) log(1/(1-prophylactic_prop_treat)) * S[i] else 0
n_PS_S[] <- prophylactic_drug_wane * PS[i]
n_PS_PE1[] <- if (drug_1_indic == 1) lambda[i] * drug_1_effect_size * PS[i] else lambda[i] * PS[i]
n_PE1_PE2[] <-  gamma_E * PE1[i]
n_PE2_I[] <-  gamma_E * PE2[i]

n_PE2_ICase1_initial[] <- n_PE2_I[i] * prob_hosp[i]
n_PE2_ICase1[] <- if (drug_2_indic == 1) n_E2_ICase1_initial[i] * (1 - drug_2_effect_size) else  n_PE2_ICase1_initial[i]
n_PE2_ICase1_Drug_5[] <- if (drug_5_indic_ICase == 1) n_PE2_ICase1[i] * drug_5_prop_treat else 0
n_PE2_ICase1_No_Drug_5[] <- if (drug_5_indic_ICase == 1) n_PE2_ICase1[i] * (1 - drug_5_prop_treat) else n_PE2_ICase1[i]

n_PE2_IMild_or_IAsymp[] <- n_PE2_I[i] - n_PE2_ICase1[i]
n_PE2_IAsymp[] <- n_PE2_IMild_or_IAsymp[i] * prob_asymp[i]
n_PE2_IMild[] <- (n_PE2_IMild_or_IAsymp[i] - n_PE2_IAsymp[i]) + (n_PE2_ICase1_initial[i] - n_PE2_ICase1[i])
n_PE2_IMild_Drug_5[] <- if (drug_5_indic_IMild == 1) n_PE2_IMild[i] * drug_5_prop_treat else 0
n_PE2_IMild_No_Drug_5[] <- if (drug_5_indic_IMild == 1) n_PE2_IMild[i] * (1 - drug_5_prop_treat) else n_PE2_IMild[i]

n_IAsymp_R[] <- gamma_IAsymp * IAsymp[i]
n_IMild_R[] <- if (drug_4_indic == 1) gamma_IMild_Drug_4 * IMild[i]  else gamma_IMild * IMild[i]
n_IMild_Drug_5_R[] <- if (drug_4_indic == 1) gamma_IMild_Drug_4 * IMild_Drug_5[i]  else gamma_IMild * IMild_Drug_5[i]

n_ICase1_Drug_5_ICase2_Drug_5[] <- gamma_ICase * ICase1_Drug_5[i]
n_ICase2_Drug_5_Hosp[] <- gamma_ICase * ICase2_Drug_5[i]
n_ICase1_ICase2[] <- gamma_ICase * ICase1[i]
n_ICase2_Hosp[] <- gamma_ICase * ICase2[i]

n_IRec1_IRec2[] <- gamma_rec * IRec1[i]
n_IRec2_R[] <- gamma_rec * IRec2[i]

## WORKING OUT NUMBER OF ICU BEDS AVAILABILE AND HOW MANY INDIVIDUALS RECEIVE THEM
##--------------------------------------------------------------------------------
number_req_ICU_initial[] <- (n_ICase2_Hosp[i] + n_ICase2_Drug_5_Hosp[i]) * prob_severe[i]
number_req_ICU[] <- if (drug_6_indic == 1) number_req_ICU_initial[i] * (1 - (drug_6_prop_treat * drug_6_effect_size)) else number_req_ICU_initial[i]
total_req_ICU <- sum(number_req_ICU)

# Calculating Current ICU Occupancy
ICU_occ <- sum(ISev_GetICU_GetOx_Surv1) + sum(ISev_GetICU_GetOx_Surv2) + sum(ISev_GetICU_GetOx_Die1) + sum(ISev_GetICU_GetOx_Die2) +
           sum(ISev_GetICU_NoOx_Surv1) + sum(ISev_GetICU_NoOx_Surv2) + sum(ISev_GetICU_NoOx_Die1) + sum(ISev_GetICU_NoOx_Die2) +
           sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) +
           sum(ICrit_GetICU_GetOx_NoMV_Surv1) + sum(ICrit_GetICU_GetOx_NoMV_Surv2) + sum(ICrit_GetICU_GetOx_NoMV_Die1) + sum(ICrit_GetICU_GetOx_NoMV_Die2) +
           sum(ICrit_GetICU_NoOx_NoMV_Surv1) + sum(ICrit_GetICU_NoOx_NoMV_Surv2) + sum(ICrit_GetICU_NoOx_NoMV_Die1) + sum(ICrit_GetICU_NoOx_NoMV_Die2)

# Calculating New Occupancy After Taking Into Account Individuals Leaving ICU Beds This Timestep
current_free_ICU <- current_ICU_bed_capacity +
                    sum(n_ISev_GetICU_GetOx_Surv2_Rec) + sum(n_ISev_GetICU_GetOx_Die2_D_Hospital) +
                    sum(n_ISev_GetICU_NoOx_Surv2_Rec) + sum(n_ISev_GetICU_NoOx_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) - ICU_occ

# Individuals Getting and Not Getting ICU Beds, and Their Associated Disease Severities
total_GetICU <- if(current_free_ICU <= 0) 0 else(if(current_free_ICU - total_req_ICU >= 0) total_req_ICU else(current_free_ICU))
number_GetICU[] <- if (total_req_ICU == 0) 0 else number_req_ICU[i]/sum(number_req_ICU) * total_GetICU

number_req_ICU_MV_initial[] <- number_GetICU[i] * prob_critical[i]
number_req_ICU_MV[] <- if (drug_7_indic == 1) number_req_ICU_MV_initial[i] * (1 - (drug_7_prop_treat * drug_7_effect_size)) else number_req_ICU_MV_initial[i]

number_req_ICU_Ox[] <- number_GetICU[i] - number_req_ICU_MV[i]
total_req_ICU_MV <- sum(number_req_ICU_MV)
total_req_ICU_Ox <- sum(number_req_ICU_Ox)

number_NotICU[] <- number_req_ICU[i] - number_GetICU[i]
number_NotICU_NotOx_NotMV[] <- number_NotICU[i] * prob_critical[i]
number_NotICU_NotOx[] <- number_NotICU[i] - number_NotICU_NotOx_NotMV[i]

## WORKING OUT NUMBER OF HOSPITAL BEDS AVAILABILE AND HOW MANY INDIVIDUALS RECEIVE THEM
##-------------------------------------------------------------------------------------
number_req_Hosp[] <- (n_ICase2_Hosp[i] + n_ICase2_Drug_5_Hosp[i]) - number_req_ICU[i]
total_req_Hosp <- sum(number_req_Hosp)

# Current Hospital Bed Occupancy (Includes Stepdown Hospital Beds for Patient Leaving ICU)
hosp_occ <- sum(IMod_GetHosp_GetOx_Surv1) + sum(IMod_GetHosp_GetOx_Surv2) + sum(IMod_GetHosp_GetOx_Die1) + sum(IMod_GetHosp_GetOx_Die2) +
            sum(IMod_GetHosp_NoOx_Surv1) + sum(IMod_GetHosp_NoOx_Surv2) + sum(IMod_GetHosp_NoOx_Die1) + sum(IMod_GetHosp_NoOx_Die2) +
            sum(IRec1) + sum(IRec2)

# Totting Hospital Bed Occupancy Up After Taking Account of Individuals Leaving Hospital Beds to Recovery and Entering from ICU
current_free_hosp <- current_hosp_bed_capacity +
                     sum(n_IMod_GetHosp_GetOx_Surv2_R) + sum(n_IMod_GetHosp_GetOx_Die2_D_Hospital) +
                     sum(n_IMod_GetHosp_NoOx_Surv2_R) + sum(n_IMod_GetHosp_NoOx_Die2_D_Hospital) +
                     sum(n_IRec2_R) -
                     sum(n_ISev_GetICU_GetOx_Surv2_Rec) - sum(n_ISev_GetICU_NoOx_Surv2_Rec) -
                     sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) - sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) - sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) - hosp_occ

# Individuals Getting and Not Get Hospital Beds
total_GetHosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_req_Hosp >= 0) total_req_Hosp else(current_free_hosp))
number_GetHosp[] <- if (total_req_Hosp == 0) 0 else number_req_Hosp[i]/sum(number_req_Hosp) * total_GetHosp
number_NotHosp[] <- number_req_Hosp[i] - number_GetHosp[i]


## WORKING OUT HOW MUCH OXYGEN IS AVAILABILE AND HOW MANY INDIVIDUALS REQUIRING HOSPITAL/ICU BED RECEIVE IT
##---------------------------------------------------------------------------------------------------------
# Updating Oxyen Availability With New Supply At Each Timestep, Subtract Off Baseline Demand and Add In Any O2 Leftover From Previous Timestep
deriv(oxygen_availability) <- 0#oxygen_supply - baseline_oxygen_demand + leftover

# Working Out Proportion of Oxygen Going to Hospital Beds vs ICU Beds, and Splitting ICU Oxygen Into Amounts for Each Disease Severity Category
prop_ox_hosp_beds <- if (total_GetHosp == 0 && total_GetICU == 0) 0 else (total_GetHosp/(total_GetHosp + total_GetICU * severe_critical_case_oxygen_consumption_multiplier))
available_oxygen_for_hosp_beds <- round(prop_ox_hosp_beds * oxygen_availability)
available_oxygen_for_ICU_beds <- floor((oxygen_availability - available_oxygen_for_hosp_beds)/severe_critical_case_oxygen_consumption_multiplier)
available_oxygen_for_ICU_MV <- if(total_req_ICU_MV == 0 && total_req_ICU_Ox == 0) 0 else (round(available_oxygen_for_ICU_beds * total_req_ICU_MV/(total_req_ICU_MV + total_req_ICU_Ox))) # if these are 0s we get NAs maybe!!!
available_oxygen_for_ICU_Ox <- available_oxygen_for_ICU_beds - available_oxygen_for_ICU_MV

# Number Getting Hospital Beds Who Receive/Don't Receive Oxygen
total_GetHosp_GetOx <- if (available_oxygen_for_hosp_beds <= 0) 0 else(if(available_oxygen_for_hosp_beds - total_GetHosp >= 0) total_GetHosp else(available_oxygen_for_hosp_beds)) # Working out the number of new ICU requiring infections that get a bed
number_GetHosp_Ox[] <- if (total_GetHosp_GetOx == 0) 0 else number_GetHosp[i]/sum(number_GetHosp) * total_GetHosp_GetOx
number_GetHosp_NoOx[] <- number_GetHosp[i] - number_GetHosp_Ox[i]

# Calculating the Number of Severe Cases (no MV required) Who Get Oxygen
total_GetICU_GetOx_Only <- if (available_oxygen_for_ICU_Ox <= 0) 0 else(if(available_oxygen_for_ICU_Ox - total_req_ICU_Ox >= 0) total_req_ICU_Ox else(available_oxygen_for_ICU_Ox))
number_GetICU_GetOx[] <- if (total_GetICU_GetOx_Only == 0) 0 else number_req_ICU_Ox[i]/sum(number_req_ICU_Ox) * total_GetICU_GetOx_Only
number_GetICU_NoOx[] <- number_req_ICU_Ox[i] - number_GetICU_GetOx[i]

# Calculating the Number of Critical Cases (MV requied) Who Get Oxygen
total_GetICU_GetOx_Need_MV <- if (available_oxygen_for_ICU_MV <= 0) 0 else(if(available_oxygen_for_ICU_MV - total_req_ICU_MV >= 0) total_req_ICU_MV else(available_oxygen_for_ICU_MV))
number_GetICU_GetOx_NeedMV[] <- if(total_GetICU_GetOx_Need_MV == 0) 0 else number_req_ICU_MV[i]/sum(number_req_ICU_MV) * total_GetICU_GetOx_Need_MV
number_GetICU_NoOx_NeedMV[] <- number_req_ICU_MV[i] - number_GetICU_GetOx_NeedMV[i]

## WORKING OUT HOW MANY MECHANICAL VENTILATORS ARE AVAILABILE AND HOW MANY INDIVIDUALS REQUIRING THEM RECEIVE THEM
##----------------------------------------------------------------------------------------------------------------
# Calculating the Number of Critical Cases Who Have Been Assigned to Receive Oxygen (Hence Are Eligible to Get MV) Who Also Get MV
MV_occ <- sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2)
current_free_MV <- MV_capacity + sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) - MV_occ
total_GetICU_GetOx_GetMV <- if(current_free_MV <= 0) 0 else(if(current_free_MV - total_GetICU_GetOx_Need_MV >= 0) total_GetICU_GetOx_Need_MV else(current_free_MV))
number_GetICU_GetOx_GetMV[] <-  if(total_GetICU_GetOx_GetMV == 0) 0 else number_GetICU_GetOx_NeedMV[i]/sum(number_GetICU_GetOx_NeedMV) * total_GetICU_GetOx_GetMV
number_GetICU_GetOx_NoMV[] <- number_GetICU_GetOx_NeedMV[i] - number_GetICU_GetOx_GetMV[i]

## TALLYING UP USED AND REMAINING OXYGEN, INCLUDING ANY LEFTOVER, WHICH MAY OR MAY NOT BE CARRIED OVER INTO NEXT TIMESTEP
##-----------------------------------------------------------------------------------------------------------------------
temp_leftover <- oxygen_supply - baseline_oxygen_demand - sum(number_GetHosp_Ox) - (sum(number_GetICU_GetOx_NeedMV) + sum(number_GetICU_GetOx)) * severe_critical_case_oxygen_consumption_multiplier
leftover <- if (temp_leftover < 0) 0 else (if(temp_leftover >= max_leftover) max_leftover else temp_leftover)
oxygen_needed_overall <- sum(number_req_Hosp) + (sum(number_req_ICU_MV) + sum(number_req_ICU_Ox)) * severe_critical_case_oxygen_consumption_multiplier
oxygen_used <- sum(number_GetHosp_Ox) + (sum(number_GetICU_GetOx_NeedMV) + sum(number_GetICU_GetOx)) * severe_critical_case_oxygen_consumption_multiplier

## CALCULATING THE NUMBER OF INDIVIDUALS MOVING OUT OF HOSPITAL/ICU BED RELATED COMPARTMENTS
##------------------------------------------------------------------------------------------

# Numbers changing between hospital bed related compartments
n_IMod_GetHosp_GetOx_Die1[] <- number_GetHosp_Ox[i] * prob_moderate_death_get_hosp_get_ox[i]
n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[] <- IMod_GetHosp_GetOx_Die1[i] * gamma_IMod_GetHosp_GetOx_Die
n_IMod_GetHosp_GetOx_Die2_D_Hospital[] <- IMod_GetHosp_GetOx_Die2[i] * gamma_IMod_GetHosp_GetOx_Die
n_IMod_GetHosp_GetOx_Surv1[] <- number_GetHosp_Ox[i] - n_IMod_GetHosp_GetOx_Die1[i]
n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[] <- if (drug_8_indic_IMod_GetHosp_GetOx == 1) IMod_GetHosp_GetOx_Surv1[i] * gamma_IMod_GetHosp_GetOx_Surv_Drug_8  else IMod_GetHosp_GetOx_Surv1[i] * gamma_IMod_GetHosp_GetOx_Surv
n_IMod_GetHosp_GetOx_Surv2_R[] <- if (drug_8_indic_IMod_GetHosp_GetOx == 1) IMod_GetHosp_GetOx_Surv2[i] * gamma_IMod_GetHosp_GetOx_Surv_Drug_8 else IMod_GetHosp_GetOx_Surv2[i] * gamma_IMod_GetHosp_GetOx_Surv

n_IMod_GetHosp_NoOx_Die1[] <- number_GetHosp_NoOx[i] * prob_moderate_death_get_hosp_no_ox[i]
n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[] <- IMod_GetHosp_NoOx_Die1[i] * gamma_IMod_GetHosp_NoOx_Die
n_IMod_GetHosp_NoOx_Die2_D_Hospital[] <- IMod_GetHosp_NoOx_Die2[i] * gamma_IMod_GetHosp_NoOx_Die
n_IMod_GetHosp_NoOx_Surv1[] <- number_GetHosp_NoOx[i] - n_IMod_GetHosp_NoOx_Die1[i]
n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[] <- if (drug_8_indic_IMod_GetHosp_NoOx == 1) IMod_GetHosp_NoOx_Surv1[i] * gamma_IMod_GetHosp_NoOx_Surv_Drug_8  else IMod_GetHosp_NoOx_Surv1[i] * gamma_IMod_GetHosp_NoOx_Surv
n_IMod_GetHosp_NoOx_Surv2_R[] <- if (drug_8_indic_IMod_GetHosp_NoOx == 1) IMod_GetHosp_NoOx_Surv2[i] * gamma_IMod_GetHosp_NoOx_Surv_Drug_8 else IMod_GetHosp_NoOx_Surv2[i] * gamma_IMod_GetHosp_NoOx_Surv

n_IMod_NoHosp_NoOx_Die1[] <- number_NotHosp[i] * prob_moderate_death_no_hosp_no_ox[i]
n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[] <- IMod_NoHosp_NoOx_Die1[i] * gamma_IMod_NoHosp_NoOx_Die
n_IMod_NoHosp_NoOx_Die2_D_Community[] <- IMod_NoHosp_NoOx_Die2[i] * gamma_IMod_NoHosp_NoOx_Die
n_IMod_NoHosp_NoOx_Surv1[] <- number_NotHosp[i] - n_IMod_NoHosp_NoOx_Die1[i]
n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[] <- IMod_NoHosp_NoOx_Surv1[i] * gamma_IMod_NoHosp_NoOx_Surv
n_IMod_NoHosp_NoOx_Surv2_R[] <- IMod_NoHosp_NoOx_Surv2[i] * gamma_IMod_NoHosp_NoOx_Surv

# Numbers changing between ICU bed/non-mechanical ventilation related compartments
n_ISev_GetICU_GetOx_Die1[] <- number_GetICU_GetOx[i] * prob_severe_death_get_ICU_get_ox[i]
n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[] <- ISev_GetICU_GetOx_Die1[i] * gamma_ISev_GetICU_GetOx_Die
n_ISev_GetICU_GetOx_Die2_D_Hospital[] <- ISev_GetICU_GetOx_Die2[i] * gamma_ISev_GetICU_GetOx_Die
n_ISev_GetICU_GetOx_Surv1[] <- number_GetICU_GetOx[i] - n_ISev_GetICU_GetOx_Die1[i]
n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[] <-if (drug_9_indic_ISev_GetICU_GetOx == 1) ISev_GetICU_GetOx_Surv1[i] * gamma_ISev_GetICU_GetOx_Surv_Drug_9 else ISev_GetICU_GetOx_Surv1[i] * gamma_ISev_GetICU_GetOx_Surv
n_ISev_GetICU_GetOx_Surv2_Rec[] <- if (drug_9_indic_ISev_GetICU_GetOx == 1) ISev_GetICU_GetOx_Surv2[i] * gamma_ISev_GetICU_GetOx_Surv_Drug_9 else ISev_GetICU_GetOx_Surv2[i] * gamma_ISev_GetICU_GetOx_Surv

n_ISev_GetICU_NoOx_Die1[] <- number_GetICU_NoOx[i] * prob_severe_death_get_ICU_no_ox[i]
n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[] <- ISev_GetICU_NoOx_Die1[i]* gamma_ISev_GetICU_NoOx_Die
n_ISev_GetICU_NoOx_Die2_D_Hospital[] <- ISev_GetICU_NoOx_Die2[i] * gamma_ISev_GetICU_NoOx_Die
n_ISev_GetICU_NoOx_Surv1[] <- number_GetICU_NoOx[i] - n_ISev_GetICU_NoOx_Die1[i]
n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[] <- if (drug_9_indic_ISev_GetICU_NoOx == 1) ISev_GetICU_NoOx_Surv1[i] * gamma_ISev_GetICU_NoOx_Surv_Drug_9  else ISev_GetICU_NoOx_Surv1[i] * gamma_ISev_GetICU_NoOx_Surv
n_ISev_GetICU_NoOx_Surv2_Rec[] <- if (drug_9_indic_ISev_GetICU_NoOx == 1) ISev_GetICU_NoOx_Surv2[i] * gamma_ISev_GetICU_NoOx_Surv_Drug_9  else ISev_GetICU_NoOx_Surv2[i] * gamma_ISev_GetICU_NoOx_Surv

n_ISev_NoICU_NoOx_Die1[] <- number_NotICU_NotOx[i] * prob_severe_death_no_ICU_no_ox[i]
n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[] <- ISev_NoICU_NoOx_Die1[i] * gamma_ISev_NoICU_NoOx_Die
n_ISev_NoICU_NoOx_Die2_D_Community[] <- ISev_NoICU_NoOx_Die2[i] * gamma_ISev_NoICU_NoOx_Die
n_ISev_NoICU_NoOx_Surv1[] <- number_NotICU_NotOx[i] - n_ISev_NoICU_NoOx_Die1[i]
n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[] <- ISev_NoICU_NoOx_Surv1[i] * gamma_ISev_NoICU_NoOx_Surv
n_ISev_NoICU_NoOx_Surv2_R[] <- ISev_NoICU_NoOx_Surv2[i] * gamma_ISev_NoICU_NoOx_Surv

# Numbers changing between ICU bed/mechanical ventilation related compartments
n_ICrit_GetICU_GetOx_GetMV_Die1[] <- number_GetICU_GetOx_GetMV[i] * prob_critical_death_get_ICU_get_ox_get_MV[i]
n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[] <- ICrit_GetICU_GetOx_GetMV_Die1[i] * gamma_ICrit_GetICU_GetOx_GetMV_Die
n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[] <- ICrit_GetICU_GetOx_GetMV_Die2[i] * gamma_ICrit_GetICU_GetOx_GetMV_Die
n_ICrit_GetICU_GetOx_GetMV_Surv1[] <- number_GetICU_GetOx_GetMV[i] - n_ICrit_GetICU_GetOx_GetMV_Die1[i]
n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[] <- if (drug_10_indic_ICrit_GetICU_GetOx_GetMV == 1) ICrit_GetICU_GetOx_GetMV_Surv1[i] * gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10  else ICrit_GetICU_GetOx_GetMV_Surv1[i] * gamma_ICrit_GetICU_GetOx_GetMV_Surv
n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[] <- if (drug_10_indic_ICrit_GetICU_GetOx_GetMV == 1) ICrit_GetICU_GetOx_GetMV_Surv2[i] * gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10  else ICrit_GetICU_GetOx_GetMV_Surv2[i] * gamma_ICrit_GetICU_GetOx_GetMV_Surv

n_ICrit_GetICU_GetOx_NoMV_Die1[] <- number_GetICU_GetOx_NoMV[i] * prob_critical_death_get_ICU_get_ox_no_MV[i]
n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[] <- ICrit_GetICU_GetOx_NoMV_Die1[i] * gamma_ICrit_GetICU_GetOx_NoMV_Die
n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[] <- ICrit_GetICU_GetOx_NoMV_Die2[i] * gamma_ICrit_GetICU_GetOx_NoMV_Die
n_ICrit_GetICU_GetOx_NoMV_Surv1[] <- number_GetICU_GetOx_NoMV[i] - n_ICrit_GetICU_GetOx_NoMV_Die1[i]
n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[] <- if (drug_10_indic_ICrit_GetICU_GetOx_NoMV == 1) ICrit_GetICU_GetOx_NoMV_Surv1[i] * gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10  else ICrit_GetICU_GetOx_NoMV_Surv1[i] * gamma_ICrit_GetICU_GetOx_NoMV_Surv
n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[] <- if (drug_10_indic_ICrit_GetICU_GetOx_NoMV == 1) ICrit_GetICU_GetOx_NoMV_Surv2[i] * gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10  else ICrit_GetICU_GetOx_NoMV_Surv2[i] * gamma_ICrit_GetICU_GetOx_NoMV_Surv

n_ICrit_GetICU_NoOx_NoMV_Die1[] <- number_GetICU_NoOx_NeedMV[i] * prob_critical_death_get_ICU_no_ox_no_MV[i]
n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[] <- ICrit_GetICU_NoOx_NoMV_Die1[i] * gamma_ICrit_GetICU_NoOx_NoMV_Die
n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[] <- ICrit_GetICU_NoOx_NoMV_Die2[i] * gamma_ICrit_GetICU_NoOx_NoMV_Die
n_ICrit_GetICU_NoOx_NoMV_Surv1[] <- number_GetICU_NoOx_NeedMV[i] - n_ICrit_GetICU_NoOx_NoMV_Die1[i]
n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[] <- if (drug_10_indic_ICrit_GetICU_NoOx_NoMV == 1) ICrit_GetICU_NoOx_NoMV_Surv1[i] * gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10  else ICrit_GetICU_NoOx_NoMV_Surv1[i] * gamma_ICrit_GetICU_NoOx_NoMV_Surv
n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[] <- if (drug_10_indic_ICrit_GetICU_NoOx_NoMV == 1) ICrit_GetICU_NoOx_NoMV_Surv2[i] * gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10  else ICrit_GetICU_NoOx_NoMV_Surv2[i] * gamma_ICrit_GetICU_NoOx_NoMV_Surv

n_ICrit_NoICU_NoOx_NoMV_Die1[] <- number_NotICU_NotOx_NotMV[i] * prob_critical_death_no_ICU_no_ox_no_MV[i]
n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[] <- ICrit_NoICU_NoOx_NoMV_Die1[i] * gamma_ICrit_NoICU_NoOx_NoMV_Die
n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[] <- ICrit_NoICU_NoOx_NoMV_Die2[i] * gamma_ICrit_NoICU_NoOx_NoMV_Die
n_ICrit_NoICU_NoOx_NoMV_Surv1[] <- number_NotICU_NotOx_NotMV[i] - n_ICrit_NoICU_NoOx_NoMV_Die1[i]
n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[] <- ICrit_NoICU_NoOx_NoMV_Surv1[i] * gamma_ICrit_NoICU_NoOx_NoMV_Surv
n_ICrit_NoICU_NoOx_NoMV_Surv2_R[] <- ICrit_NoICU_NoOx_NoMV_Surv2[i] * gamma_ICrit_NoICU_NoOx_NoMV_Surv


## DERIVATIVES FOR ALL STATE VARIABLES
##------------------------------------------------------------------------------------------

# Passage Through Drug Treated Initial Susceptible/Latent Stages
deriv(S[]) <- -n_S_E1[i] - n_S_PS[i] + n_PS_S[i]
deriv(E1[]) <- n_S_E1[i] - n_E1_E2[i]
deriv(E2[]) <- n_E1_E2[i] - n_E2_I[i]
deriv(IAsymp[]) <- n_E2_IAsymp[i] + n_PE2_IAsymp[i] - n_IAsymp_R[i]
deriv(IMild[]) <- n_E2_IMild_No_Drug_5[i] + n_PE2_IMild_No_Drug_5[i] - n_IMild_R[i]
deriv(ICase1[]) <- n_E2_ICase1_No_Drug_5[i] + n_PE2_ICase1_No_Drug_5[i] - n_ICase1_ICase2[i]
deriv(ICase2[]) <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]
deriv(IMild_Drug_5[]) <- n_E2_IMild_Drug_5[i] + n_PE2_IMild_Drug_5[i] - n_IMild_Drug_5_R[i]
deriv(ICase1_Drug_5[]) <- n_E2_ICase1_Drug_5[i] + n_PE2_ICase1_Drug_5[i] - n_ICase1_Drug_5_ICase2_Drug_5[i]
deriv(ICase2_Drug_5[]) <- n_ICase1_Drug_5_ICase2_Drug_5[i] - n_ICase2_Drug_5_Hosp[i]
deriv(PS[]) <-  n_S_PS[i] - n_PS_S[i] - n_PS_PE1[i]
deriv(PE1[]) <- n_PS_PE1[i] - n_PE1_PE2[i]
deriv(PE2[]) <- n_PE1_PE2[i] - n_PE2_I[i]

# Passage Through Requiring Hospital Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
deriv(IMod_GetHosp_GetOx_Die1[]) <- n_IMod_GetHosp_GetOx_Die1[i] - n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i]
deriv(IMod_GetHosp_GetOx_Die2[]) <- n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i] - n_IMod_GetHosp_GetOx_Die2_D_Hospital[i]
deriv(IMod_GetHosp_GetOx_Surv1[]) <- n_IMod_GetHosp_GetOx_Surv1[i] - n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i]
deriv(IMod_GetHosp_GetOx_Surv2[]) <- n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i] - n_IMod_GetHosp_GetOx_Surv2_R[i]
deriv(IMod_GetHosp_NoOx_Die1[]) <- n_IMod_GetHosp_NoOx_Die1[i] - n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i]
deriv(IMod_GetHosp_NoOx_Die2[]) <- n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i] - n_IMod_GetHosp_NoOx_Die2_D_Hospital[i]
deriv(IMod_GetHosp_NoOx_Surv1[]) <- n_IMod_GetHosp_NoOx_Surv1[i] - n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i]
deriv(IMod_GetHosp_NoOx_Surv2[]) <- n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i] - n_IMod_GetHosp_NoOx_Surv2_R[i]
deriv(IMod_NoHosp_NoOx_Die1[]) <- n_IMod_NoHosp_NoOx_Die1[i] - n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i]
deriv(IMod_NoHosp_NoOx_Die2[]) <- n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i] - n_IMod_NoHosp_NoOx_Die2_D_Community[i]
deriv(IMod_NoHosp_NoOx_Surv1[]) <- n_IMod_NoHosp_NoOx_Surv1[i] - n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i]
deriv(IMod_NoHosp_NoOx_Surv2[]) <- n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i] - n_IMod_NoHosp_NoOx_Surv2_R[i]

# Passage Through Requiring ICU Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
deriv(ISev_GetICU_GetOx_Die1[]) <- n_ISev_GetICU_GetOx_Die1[i] - n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i]
deriv(ISev_GetICU_GetOx_Die2[]) <- n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i] - n_ISev_GetICU_GetOx_Die2_D_Hospital[i]
deriv(ISev_GetICU_GetOx_Surv1[]) <- n_ISev_GetICU_GetOx_Surv1[i] - n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i]
deriv(ISev_GetICU_GetOx_Surv2[]) <- n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i] - n_ISev_GetICU_GetOx_Surv2_Rec[i]
deriv(ISev_GetICU_NoOx_Die1[]) <- n_ISev_GetICU_NoOx_Die1[i] - n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i]
deriv(ISev_GetICU_NoOx_Die2[]) <- n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i] - n_ISev_GetICU_NoOx_Die2_D_Hospital[i]
deriv(ISev_GetICU_NoOx_Surv1[]) <- n_ISev_GetICU_NoOx_Surv1[i] - n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i]
deriv(ISev_GetICU_NoOx_Surv2[]) <- n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i] - n_ISev_GetICU_NoOx_Surv2_Rec[i]
deriv(ISev_NoICU_NoOx_Die1[]) <- n_ISev_NoICU_NoOx_Die1[i] - n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i]
deriv(ISev_NoICU_NoOx_Die2[]) <- n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i] - n_ISev_NoICU_NoOx_Die2_D_Community[i]
deriv(ISev_NoICU_NoOx_Surv1[]) <- n_ISev_NoICU_NoOx_Surv1[i] - n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i]
deriv(ISev_NoICU_NoOx_Surv2[]) <- n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i] - n_ISev_NoICU_NoOx_Surv2_R[i]

# Passage Through Requiring ICU Bed, Oxygen and Mechanical Ventilation, Either Receiving All, ICU Bed and Oxygen, ICU Bed Only or Nothing, and Surviving or Not
deriv(ICrit_GetICU_GetOx_GetMV_Die1[]) <- n_ICrit_GetICU_GetOx_GetMV_Die1[i] - n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i]
deriv(ICrit_GetICU_GetOx_GetMV_Die2[]) <- n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i] - n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i]
deriv(ICrit_GetICU_GetOx_GetMV_Surv1[]) <- n_ICrit_GetICU_GetOx_GetMV_Surv1[i] - n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i]
deriv(ICrit_GetICU_GetOx_GetMV_Surv2[]) <- n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i] - n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]
deriv(ICrit_GetICU_GetOx_NoMV_Die1[]) <- n_ICrit_GetICU_GetOx_NoMV_Die1[i] - n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i]
deriv(ICrit_GetICU_GetOx_NoMV_Die2[]) <- n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i] - n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i]
deriv(ICrit_GetICU_GetOx_NoMV_Surv1[]) <- n_ICrit_GetICU_GetOx_NoMV_Surv1[i] - n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i]
deriv(ICrit_GetICU_GetOx_NoMV_Surv2[]) <- n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i] - n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i]
deriv(ICrit_GetICU_NoOx_NoMV_Die1[]) <- n_ICrit_GetICU_NoOx_NoMV_Die1[i] - n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i]
deriv(ICrit_GetICU_NoOx_NoMV_Die2[]) <- n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i] - n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]
deriv(ICrit_GetICU_NoOx_NoMV_Surv1[]) <- n_ICrit_GetICU_NoOx_NoMV_Surv1[i] - n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i]
deriv(ICrit_GetICU_NoOx_NoMV_Surv2[]) <- n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i] - n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i]
deriv(ICrit_NoICU_NoOx_NoMV_Die1[]) <- n_ICrit_NoICU_NoOx_NoMV_Die1[i] - n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i]
deriv(ICrit_NoICU_NoOx_NoMV_Die2[]) <- n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i] - n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
deriv(ICrit_NoICU_NoOx_NoMV_Surv1[]) <- n_ICrit_NoICU_NoOx_NoMV_Surv1[i] - n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i]
deriv(ICrit_NoICU_NoOx_NoMV_Surv2[]) <- n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i] - n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]

# Passage Through Recovery, from Mild Infection, Requiring Oxygen or From ICU Post-Requiring Mechanical Ventilation
deriv(IRec1[]) <- n_ISev_GetICU_GetOx_Surv2_Rec[i] + n_ISev_GetICU_NoOx_Surv2_Rec[i] +
                  n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]  + n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i] + n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i] -
                  n_IRec1_IRec2[i]
deriv(IRec2[]) <- n_IRec1_IRec2[i] - n_IRec2_R[i]
deriv(D_Community[]) <- n_IMod_NoHosp_NoOx_Die2_D_Community[i] + n_ISev_NoICU_NoOx_Die2_D_Community[i] + n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
deriv(D_Hospital[]) <- n_IMod_GetHosp_GetOx_Die2_D_Hospital[i] + n_IMod_GetHosp_NoOx_Die2_D_Hospital[i] +
                       n_ISev_GetICU_GetOx_Die2_D_Hospital[i] + n_ISev_GetICU_NoOx_Die2_D_Hospital[i] +
                       n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i] + n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i] + n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]
deriv(D[]) <- n_IMod_NoHosp_NoOx_Die2_D_Community[i] + n_ISev_NoICU_NoOx_Die2_D_Community[i] + n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i] +
              n_IMod_GetHosp_GetOx_Die2_D_Hospital[i] + n_IMod_GetHosp_NoOx_Die2_D_Hospital[i] +
              n_ISev_GetICU_GetOx_Die2_D_Hospital[i] + n_ISev_GetICU_NoOx_Die2_D_Hospital[i] +
              n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i] + n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i] + n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]
deriv(R[]) <- n_IAsymp_R[i] + n_IMild_R[i] + n_IMild_Drug_5_R[i] + n_IRec2_R[i] +
              n_IMod_GetHosp_GetOx_Surv2_R[i] + n_IMod_GetHosp_NoOx_Surv2_R[i] + n_IMod_NoHosp_NoOx_Surv2_R[i] +
              n_ISev_NoICU_NoOx_Surv2_R[i] +
              n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]

## COMPUTING THE FORCE OF INFECTION AND INTERPOLATION FOR MIXING MATRIX AND BETA
##------------------------------------------------------------------------------
# Interpolation for Mixing Matrix
m[, ] <- interpolate(tt_matrix, mix_mat_set, "constant")
dim(m) <- c(N_age, N_age)
tt_matrix[] <- user()
mix_mat_set[, ,] <- user()
dim(tt_matrix) <- user()
dim(mix_mat_set) <- c(length(tt_matrix), N_age, N_age)

# Interpolation for beta
beta <- interpolate(tt_beta, beta_set, "constant")
tt_beta[] <- user()
beta_set[] <- user()
dim(tt_beta) <- user()
dim(beta_set) <- length(tt_beta)

# Generating Force of Infection
temp[] <- (rel_inf_asymp * IAsymp[i]) + (rel_inf_mild * IMild[i]) + ICase1[i] + ICase2[i]  +
          (rel_inf_mild * drug_5_effect_size * IMild_Drug_5[i]) + ICase1_Drug_5[i] * drug_5_effect_size  + ICase2_Drug_5[i] * drug_5_effect_size
s_ij[,] <- m[i, j] * temp[j]
lambda[] <- beta * sum(s_ij[i, ])

## INTERPOLATION FOR CHANGING HOSPITAL AND ICU BED CAPACITY OVER TIME
##------------------------------------------------------------------------------
current_hosp_bed_capacity <- interpolate(tt_hosp_beds, hosp_bed_capacity, "constant")
tt_hosp_beds[] <- user()
hosp_bed_capacity[] <- user()
dim(tt_hosp_beds) <- user()
dim(hosp_bed_capacity) <- length(tt_hosp_beds)

current_ICU_bed_capacity <- interpolate(tt_ICU_beds, ICU_bed_capacity, "constant")
tt_ICU_beds[] <- user()
ICU_bed_capacity[] <- user()
dim(tt_ICU_beds) <- user()
dim(ICU_bed_capacity) <- length(tt_ICU_beds)

##  INTERPOLATION FOR PARAMETERS DESCRIBING THE AVAILABILITY OF HEALTHCARE MATERIALS LIKE OXYGEN OR MV
##----------------------------------------------------------------------------------------------------
oxygen_supply <- interpolate(tt_oxygen_supply, input_oxygen_supply, "constant") # rate of resupply of oxygen
tt_oxygen_supply[] <- user()
input_oxygen_supply[] <- user()
dim(tt_oxygen_supply) <- user()
dim(input_oxygen_supply) <- length(tt_oxygen_supply)

baseline_oxygen_demand <- interpolate(tt_baseline_oxygen_demand, input_baseline_oxygen_demand, "constant") # rate of demand of oxygen
tt_baseline_oxygen_demand[] <- user()
input_baseline_oxygen_demand[] <- user()
dim(tt_baseline_oxygen_demand) <- user()
dim(input_baseline_oxygen_demand) <- length(tt_baseline_oxygen_demand)

severe_critical_case_oxygen_consumption_multiplier <- user() # consumption of oxygen for severe/critical covid-19 cases compared to moderate cases
max_leftover <- user()
MV_capacity <- user() # number of mechanical ventilators available


## DEFINING INITIAL STATES
##------------------------------------------------------------------------------
initial(oxygen_availability) <- oxygen_availability_0
initial(S[]) <- S_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
initial(IAsymp[]) <- IAsymp_0[i]
initial(IMild[]) <- IMild_0[i]
initial(ICase1[]) <- ICase1_0[i]
initial(ICase2[]) <- ICase2_0[i]
initial(IMild_Drug_5[]) <- IMild_Drug_5_0[i]
initial(ICase1_Drug_5[]) <- ICase1_Drug_5_0[i]
initial(ICase2_Drug_5[]) <- ICase2_Drug_5_0[i]
initial(IRec1[]) <- IRec1_0[i]
initial(IRec2[]) <- IRec2_0[i]
initial(R[]) <- R_0[i]
initial(D_Community[]) <- D_Community_0[i]
initial(D_Hospital[]) <- D_Hospital_0[i]
initial(D[]) <- 0
initial(PS[]) <- PS_0[i]
initial(PE1[]) <- PE1_0[i]
initial(PE2[]) <- PE2_0[i]

initial(IMod_GetHosp_GetOx_Surv1[]) <- IMod_GetHosp_GetOx_Surv1_0[i]
initial(IMod_GetHosp_GetOx_Surv2[]) <- IMod_GetHosp_GetOx_Surv2_0[i]
initial(IMod_GetHosp_GetOx_Die1[]) <- IMod_GetHosp_GetOx_Die1_0[i]
initial(IMod_GetHosp_GetOx_Die2[]) <- IMod_GetHosp_GetOx_Die2_0[i]
initial(IMod_GetHosp_NoOx_Surv1[]) <- IMod_GetHosp_NoOx_Surv1_0[i]
initial(IMod_GetHosp_NoOx_Surv2[]) <- IMod_GetHosp_NoOx_Surv2_0[i]
initial(IMod_GetHosp_NoOx_Die1[]) <- IMod_GetHosp_NoOx_Die1_0[i]
initial(IMod_GetHosp_NoOx_Die2[]) <- IMod_GetHosp_NoOx_Die2_0[i]
initial(IMod_NoHosp_NoOx_Surv1[]) <- IMod_NoHosp_NoOx_Surv1_0[i]
initial(IMod_NoHosp_NoOx_Surv2[]) <- IMod_NoHosp_NoOx_Surv2_0[i]
initial(IMod_NoHosp_NoOx_Die1[]) <- IMod_NoHosp_NoOx_Die1_0[i]
initial(IMod_NoHosp_NoOx_Die2[]) <- IMod_NoHosp_NoOx_Die2_0[i]

initial(ISev_GetICU_GetOx_Surv1[]) <- ISev_GetICU_GetOx_Surv1_0[i]
initial(ISev_GetICU_GetOx_Surv2[]) <- ISev_GetICU_GetOx_Surv2_0[i]
initial(ISev_GetICU_GetOx_Die1[]) <- ISev_GetICU_GetOx_Die1_0[i]
initial(ISev_GetICU_GetOx_Die2[]) <- ISev_GetICU_GetOx_Die2_0[i]
initial(ISev_GetICU_NoOx_Surv1[]) <- ISev_GetICU_NoOx_Surv1_0[i]
initial(ISev_GetICU_NoOx_Surv2[]) <- ISev_GetICU_NoOx_Surv2_0[i]
initial(ISev_GetICU_NoOx_Die1[]) <- ISev_GetICU_NoOx_Die1_0[i]
initial(ISev_GetICU_NoOx_Die2[]) <- ISev_GetICU_NoOx_Die2_0[i]
initial(ISev_NoICU_NoOx_Surv1[]) <- ISev_NoICU_NoOx_Surv1_0[i]
initial(ISev_NoICU_NoOx_Surv2[]) <- ISev_NoICU_NoOx_Surv2_0[i]
initial(ISev_NoICU_NoOx_Die1[]) <- ISev_NoICU_NoOx_Die1_0[i]
initial(ISev_NoICU_NoOx_Die2[]) <- ISev_NoICU_NoOx_Die2_0[i]

initial(ICrit_GetICU_GetOx_GetMV_Surv1[]) <- ICrit_GetICU_GetOx_GetMV_Surv1_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Surv2[]) <- ICrit_GetICU_GetOx_GetMV_Surv2_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Die1[]) <- ICrit_GetICU_GetOx_GetMV_Die1_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Die2[]) <- ICrit_GetICU_GetOx_GetMV_Die2_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Surv1[]) <- ICrit_GetICU_GetOx_NoMV_Surv1_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Surv2[]) <- ICrit_GetICU_GetOx_NoMV_Surv2_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Die1[]) <- ICrit_GetICU_GetOx_NoMV_Die1_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Die2[]) <- ICrit_GetICU_GetOx_NoMV_Die2_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Surv1[]) <- ICrit_GetICU_NoOx_NoMV_Surv1_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Surv2[]) <- ICrit_GetICU_NoOx_NoMV_Surv2_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Die1[]) <- ICrit_GetICU_NoOx_NoMV_Die1_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Die2[]) <- ICrit_GetICU_NoOx_NoMV_Die2_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Surv1[]) <- ICrit_NoICU_NoOx_NoMV_Surv1_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Surv2[]) <- ICrit_NoICU_NoOx_NoMV_Surv2_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Die1[]) <- ICrit_NoICU_NoOx_NoMV_Die1_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Die2[]) <- ICrit_NoICU_NoOx_NoMV_Die2_0[i]

##Initial vectors
rel_inf_asymp <- user()
rel_inf_mild <- user()
oxygen_availability_0 <- user()
S_0[] <- user()
E1_0[] <- user()
E2_0[] <- user()
IAsymp_0[] <- user()
IMild_0[] <- user()
ICase1_0[] <- user()
ICase2_0[] <- user()
IRec1_0[] <- user()
IRec2_0[] <- user()
R_0[] <- user()
D_Community_0[] <- user()
D_Hospital_0[] <- user()

PS_0[] <- user()
PE1_0[] <- user()
PE2_0[] <- user()
IMild_Drug_5_0[] <- user()
ICase1_Drug_5_0[] <- user()
ICase2_Drug_5_0[] <- user()

IMod_GetHosp_GetOx_Surv1_0[] <- user()
IMod_GetHosp_GetOx_Surv2_0[] <- user()
IMod_GetHosp_GetOx_Die1_0[] <- user()
IMod_GetHosp_GetOx_Die2_0[] <- user()
IMod_GetHosp_NoOx_Surv1_0[] <- user()
IMod_GetHosp_NoOx_Surv2_0[] <- user()
IMod_GetHosp_NoOx_Die1_0[] <- user()
IMod_GetHosp_NoOx_Die2_0[] <- user()
IMod_NoHosp_NoOx_Surv1_0[] <- user()
IMod_NoHosp_NoOx_Surv2_0[] <- user()
IMod_NoHosp_NoOx_Die1_0[] <- user()
IMod_NoHosp_NoOx_Die2_0[] <- user()

ISev_GetICU_GetOx_Surv1_0[] <- user()
ISev_GetICU_GetOx_Surv2_0[] <- user()
ISev_GetICU_GetOx_Die1_0[] <- user()
ISev_GetICU_GetOx_Die2_0[] <- user()
ISev_GetICU_NoOx_Surv1_0[] <- user()
ISev_GetICU_NoOx_Surv2_0[] <- user()
ISev_GetICU_NoOx_Die1_0[] <- user()
ISev_GetICU_NoOx_Die2_0[] <- user()
ISev_NoICU_NoOx_Surv1_0[] <- user()
ISev_NoICU_NoOx_Surv2_0[] <- user()
ISev_NoICU_NoOx_Die1_0[] <- user()
ISev_NoICU_NoOx_Die2_0[] <- user()

ICrit_GetICU_GetOx_GetMV_Surv1_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Surv2_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Die1_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Die2_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Surv1_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Surv2_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Die1_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Die2_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Surv1_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Surv2_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Die1_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Die2_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Surv1_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Surv2_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Die1_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Die2_0[] <- user()

##Dimensions of the different "vectors" used
# For the State Variables
dim(S) <- N_age
dim(E1) <- N_age
dim(E2) <- N_age
dim(IAsymp) <- N_age
dim(IMild) <- N_age
dim(ICase1) <- N_age
dim(ICase2) <- N_age
dim(IRec1) <- N_age
dim(IRec2) <- N_age
dim(R) <- N_age
dim(D_Community) <- N_age
dim(D_Hospital) <- N_age

dim(PS) <- N_age
dim(PE1) <- N_age
dim(PE2) <- N_age
dim(IMild_Drug_5) <- N_age
dim(ICase1_Drug_5) <- N_age
dim(ICase2_Drug_5) <- N_age

dim(IMod_GetHosp_GetOx_Surv1) <- N_age
dim(IMod_GetHosp_GetOx_Surv2) <- N_age
dim(IMod_GetHosp_GetOx_Die1) <- N_age
dim(IMod_GetHosp_GetOx_Die2) <- N_age
dim(IMod_GetHosp_NoOx_Surv1) <- N_age
dim(IMod_GetHosp_NoOx_Surv2) <- N_age
dim(IMod_GetHosp_NoOx_Die1) <- N_age
dim(IMod_GetHosp_NoOx_Die2) <- N_age
dim(IMod_NoHosp_NoOx_Surv1) <- N_age
dim(IMod_NoHosp_NoOx_Surv2) <- N_age
dim(IMod_NoHosp_NoOx_Die1) <- N_age
dim(IMod_NoHosp_NoOx_Die2) <- N_age

dim(ISev_GetICU_GetOx_Surv1) <- N_age
dim(ISev_GetICU_GetOx_Surv2) <- N_age
dim(ISev_GetICU_GetOx_Die1) <- N_age
dim(ISev_GetICU_GetOx_Die2) <- N_age
dim(ISev_GetICU_NoOx_Surv1) <- N_age
dim(ISev_GetICU_NoOx_Surv2) <- N_age
dim(ISev_GetICU_NoOx_Die1) <- N_age
dim(ISev_GetICU_NoOx_Die2) <- N_age
dim(ISev_NoICU_NoOx_Surv1) <- N_age
dim(ISev_NoICU_NoOx_Surv2) <- N_age
dim(ISev_NoICU_NoOx_Die1) <- N_age
dim(ISev_NoICU_NoOx_Die2) <- N_age

dim(ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die2) <- N_age

# For the Initial Values
dim(S_0) <- N_age
dim(E1_0) <- N_age
dim(E2_0) <- N_age
dim(IAsymp_0) <- N_age
dim(IMild_0) <- N_age
dim(ICase1_0) <- N_age
dim(ICase2_0) <- N_age
dim(IRec1_0) <- N_age
dim(IRec2_0) <- N_age
dim(R_0) <- N_age
dim(D_Community_0) <- N_age
dim(D_Hospital_0) <- N_age
dim(D) <- N_age

dim(PS_0) <- N_age
dim(PE1_0) <- N_age
dim(PE2_0) <- N_age
dim(IMild_Drug_5_0) <- N_age
dim(ICase1_Drug_5_0) <- N_age
dim(ICase2_Drug_5_0) <- N_age

dim(IMod_GetHosp_GetOx_Surv1_0) <- N_age
dim(IMod_GetHosp_GetOx_Surv2_0) <- N_age
dim(IMod_GetHosp_GetOx_Die1_0) <- N_age
dim(IMod_GetHosp_GetOx_Die2_0) <- N_age
dim(IMod_GetHosp_NoOx_Surv1_0) <- N_age
dim(IMod_GetHosp_NoOx_Surv2_0) <- N_age
dim(IMod_GetHosp_NoOx_Die1_0) <- N_age
dim(IMod_GetHosp_NoOx_Die2_0) <- N_age
dim(IMod_NoHosp_NoOx_Surv1_0) <- N_age
dim(IMod_NoHosp_NoOx_Surv2_0) <- N_age
dim(IMod_NoHosp_NoOx_Die1_0) <- N_age
dim(IMod_NoHosp_NoOx_Die2_0) <- N_age

dim(ISev_GetICU_GetOx_Surv1_0) <- N_age
dim(ISev_GetICU_GetOx_Surv2_0) <- N_age
dim(ISev_GetICU_GetOx_Die1_0) <- N_age
dim(ISev_GetICU_GetOx_Die2_0) <- N_age
dim(ISev_GetICU_NoOx_Surv1_0) <- N_age
dim(ISev_GetICU_NoOx_Surv2_0) <- N_age
dim(ISev_GetICU_NoOx_Die1_0) <- N_age
dim(ISev_GetICU_NoOx_Die2_0) <- N_age
dim(ISev_NoICU_NoOx_Surv1_0) <- N_age
dim(ISev_NoICU_NoOx_Surv2_0) <- N_age
dim(ISev_NoICU_NoOx_Die1_0) <- N_age
dim(ISev_NoICU_NoOx_Die2_0) <- N_age

dim(ICrit_GetICU_GetOx_GetMV_Surv1_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Surv2_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die1_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die2_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv1_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv2_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die1_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die2_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv1_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv2_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die1_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die2_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv1_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv2_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die1_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die2_0) <- N_age

# For the Number of People Moving In and Out of Compartments
dim(n_E1_E2) <- N_age
dim(n_E2_I) <- N_age
dim(n_E2_ICase1) <- N_age
dim(n_E2_IMild_or_IAsymp) <- N_age
dim(n_E2_IAsymp) <- N_age
dim(n_E2_IMild) <- N_age
dim(n_IAsymp_R) <- N_age
dim(n_IMild_R) <- N_age
dim(n_ICase1_ICase2) <- N_age
dim(n_ICase2_Hosp) <- N_age
dim(n_IRec1_IRec2) <- N_age
dim(n_IRec2_R) <- N_age

dim(n_IMod_GetHosp_GetOx_Die1) <- N_age
dim(n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2) <- N_age
dim(n_IMod_GetHosp_GetOx_Die2_D_Hospital) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv1) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv2_R) <- N_age
dim(n_IMod_GetHosp_NoOx_Die1) <- N_age
dim(n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2) <- N_age
dim(n_IMod_GetHosp_NoOx_Die2_D_Hospital) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv1) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv2_R) <- N_age
dim(n_IMod_NoHosp_NoOx_Die1) <- N_age
dim(n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2) <- N_age
dim(n_IMod_NoHosp_NoOx_Die2_D_Community) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv1) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv2_R) <- N_age

dim(n_ISev_GetICU_GetOx_Die1) <- N_age
dim(n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2) <- N_age
dim(n_ISev_GetICU_GetOx_Die2_D_Hospital) <- N_age
dim(n_ISev_GetICU_GetOx_Surv1) <- N_age
dim(n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2) <- N_age
dim(n_ISev_GetICU_GetOx_Surv2_Rec) <- N_age
dim(n_ISev_GetICU_NoOx_Die1) <- N_age
dim(n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2) <- N_age
dim(n_ISev_GetICU_NoOx_Die2_D_Hospital) <- N_age
dim(n_ISev_GetICU_NoOx_Surv1) <- N_age
dim(n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2) <- N_age
dim(n_ISev_GetICU_NoOx_Surv2_Rec) <- N_age
dim(n_ISev_NoICU_NoOx_Die1) <- N_age
dim(n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2) <- N_age
dim(n_ISev_NoICU_NoOx_Die2_D_Community) <- N_age
dim(n_ISev_NoICU_NoOx_Surv1) <- N_age
dim(n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2) <- N_age
dim(n_ISev_NoICU_NoOx_Surv2_R) <- N_age

dim(n_ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv2_R) <- N_age

dim(number_req_ICU) <- N_age
dim(number_NotICU) <- N_age
dim(number_NotICU_NotOx_NotMV) <- N_age
dim(number_NotICU_NotOx) <- N_age
dim(number_req_Hosp) <- N_age
dim(number_NotHosp) <- N_age
dim(number_GetHosp) <- N_age
dim(number_GetHosp_Ox) <- N_age
dim(number_GetHosp_NoOx) <- N_age
dim(number_req_ICU_MV) <- N_age
dim(number_req_ICU_Ox) <- N_age
dim(number_GetICU) <- N_age
dim(number_GetICU_GetOx_NoMV) <- N_age
dim(number_GetICU_NoOx_NeedMV) <- N_age
dim(number_GetICU_NoOx) <- N_age
dim(number_GetICU_GetOx_GetMV) <- N_age
dim(number_GetICU_GetOx_NeedMV) <- N_age
dim(number_GetICU_GetOx) <- N_age

# Related to Calculating Age-Structured Force of Infection
dim(n_S_E1) <- N_age
dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age

# Severity Parameters
dim(prob_asymp) <- N_age
dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_critical) <- N_age

dim(prob_moderate_death_get_hosp_get_ox_baseline) <- N_age
dim(prob_moderate_death_get_hosp_get_ox_Drug_11) <- N_age
dim(prob_moderate_death_get_hosp_get_ox) <- N_age

dim(prob_moderate_death_get_hosp_no_ox_baseline) <- N_age
dim(prob_moderate_death_get_hosp_no_ox_Drug_11) <- N_age
dim(prob_moderate_death_get_hosp_no_ox) <- N_age

dim(prob_moderate_death_no_hosp_no_ox) <- N_age

dim(prob_severe_death_get_ICU_get_ox_baseline) <- N_age
dim(prob_severe_death_get_ICU_get_ox_Drug_12) <- N_age
dim(prob_severe_death_get_ICU_get_ox) <- N_age

dim(prob_severe_death_get_ICU_no_ox_baseline) <- N_age
dim(prob_severe_death_get_ICU_no_ox_Drug_12) <- N_age
dim(prob_severe_death_get_ICU_no_ox) <- N_age

dim(prob_severe_death_no_ICU_no_ox) <- N_age

dim(prob_critical_death_get_ICU_get_ox_get_MV_baseline) <- N_age
dim(prob_critical_death_get_ICU_get_ox_get_MV_Drug_13) <- N_age
dim(prob_critical_death_get_ICU_get_ox_get_MV) <- N_age

dim(prob_critical_death_get_ICU_get_ox_no_MV_baseline) <- N_age
dim(prob_critical_death_get_ICU_get_ox_no_MV_Drug_13) <- N_age
dim(prob_critical_death_get_ICU_get_ox_no_MV) <- N_age

dim(prob_critical_death_get_ICU_no_ox_no_MV_baseline) <- N_age
dim(prob_critical_death_get_ICU_no_ox_no_MV_Drug_13) <- N_age
dim(prob_critical_death_get_ICU_no_ox_no_MV) <- N_age

dim(prob_critical_death_no_ICU_no_ox_no_MV) <- N_age

dim(n_S_PS) <- N_age
dim(n_PS_PE1) <- N_age
dim(n_PS_S) <- N_age
dim(n_PE1_PE2) <- N_age
dim(n_PE2_I) <- N_age
dim(n_PE2_ICase1_initial) <- N_age
dim(n_PE2_ICase1) <- N_age
dim(n_PE2_ICase1_Drug_5) <- N_age
dim(n_PE2_ICase1_No_Drug_5) <- N_age
dim(n_E2_IMild_No_Drug_5) <- N_age
dim(number_req_ICU_initial) <- N_age
dim(number_req_ICU_MV_initial) <- N_age
dim(n_PE2_IMild_or_IAsymp) <- N_age
dim(n_PE2_IAsymp) <- N_age
dim(n_PE2_IMild) <- N_age
dim(n_PE2_IMild_Drug_5) <- N_age
dim(n_PE2_IMild_No_Drug_5) <- N_age
dim(n_E2_ICase1_initial) <- N_age
dim(n_E2_ICase1_Drug_5) <- N_age
dim(n_E2_ICase1_No_Drug_5) <- N_age
dim(n_E2_IMild_Drug_5) <- N_age
dim(n_IMild_Drug_5_R) <- N_age
dim(n_ICase1_Drug_5_ICase2_Drug_5) <- N_age
dim(n_ICase2_Drug_5_Hosp) <- N_age

## MODEL OUTPUTS
##------------------------------------------------------------------------------
pop <- sum(S) + sum(E1) + sum(E2) + sum(IAsymp) + sum(IMild) + sum(ICase1) + sum(ICase2) + sum(IRec1) + sum(IRec2) + sum(R) +
  sum(D_Community) + sum(D_Hospital) + sum(PS) + sum(PE1) + sum(PE2) + sum(IMild_Drug_5) + sum(ICase1_Drug_5) + sum(ICase2_Drug_5) +
  sum(IMod_GetHosp_GetOx_Surv1) + sum(IMod_GetHosp_GetOx_Surv2) + sum(IMod_GetHosp_GetOx_Die1) + sum(IMod_GetHosp_GetOx_Die2) +
  sum(IMod_GetHosp_NoOx_Surv1) + sum(IMod_GetHosp_NoOx_Surv2) + sum(IMod_GetHosp_NoOx_Die1) + sum(IMod_GetHosp_NoOx_Die2) +
  sum(IMod_NoHosp_NoOx_Surv1) + sum(IMod_NoHosp_NoOx_Surv2) + sum(IMod_NoHosp_NoOx_Die1) + sum(IMod_NoHosp_NoOx_Die2) +
  sum(ISev_GetICU_GetOx_Surv1) + sum(ISev_GetICU_GetOx_Surv2) + sum(ISev_GetICU_GetOx_Die1) + sum(ISev_GetICU_GetOx_Die2) +
  sum(ISev_GetICU_NoOx_Surv1) + sum(ISev_GetICU_NoOx_Surv2) + sum(ISev_GetICU_NoOx_Die1) + sum(ISev_GetICU_NoOx_Die2) +
  sum(ISev_NoICU_NoOx_Surv1) + sum(ISev_NoICU_NoOx_Surv2) + sum(ISev_NoICU_NoOx_Die1) + sum(ISev_NoICU_NoOx_Die2) +
  sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) +
  sum(ICrit_GetICU_GetOx_NoMV_Surv1) + sum(ICrit_GetICU_GetOx_NoMV_Surv2) + sum(ICrit_GetICU_GetOx_NoMV_Die1) + sum(ICrit_GetICU_GetOx_NoMV_Die2) +
  sum(ICrit_GetICU_NoOx_NoMV_Surv1) + sum(ICrit_GetICU_NoOx_NoMV_Surv2) + sum(ICrit_GetICU_NoOx_NoMV_Die1) + sum(ICrit_GetICU_NoOx_NoMV_Die2) +
  sum(ICrit_NoICU_NoOx_NoMV_Surv1) + sum(ICrit_NoICU_NoOx_NoMV_Surv2) + sum(ICrit_NoICU_NoOx_NoMV_Die1) + sum(ICrit_NoICU_NoOx_NoMV_Die2)
output(pop) <- TRUE

# Variables to Check for Drug 1 and Drug 2
output(n_S_PS) <- TRUE
output(n_PS_PE1) <- TRUE
output(n_PS_S) <- TRUE
output(n_PE1_PE2) <- TRUE
output(n_PE2_I) <- TRUE
output(n_PE2_ICase1_initial) <- TRUE
output(n_PE2_ICase1) <- TRUE

# Variables to Check for Drug 3
output(n_E2_ICase1_initial) <- TRUE
output(n_E2_ICase1) <- TRUE
output(n_E2_IAsymp) <- TRUE

# Variables to Check for Drug 4
output(gamma_IMild) <- TRUE
output(gamma_IMild_Drug_4) <- TRUE

# Variables to Check for Drug 5
output(n_E2_IMild) <- TRUE
output(n_E2_IMild_Drug_5) <- TRUE
output(n_E2_IMild_No_Drug_5) <- TRUE
output(n_PE2_IMild) <- TRUE
output(n_PE2_IMild_Drug_5) <- TRUE
output(n_PE2_IMild_No_Drug_5) <- TRUE
output(n_E2_ICase1_Drug_5) <- TRUE
output(n_E2_ICase1_No_Drug_5) <- TRUE
output(n_PE2_ICase1_Drug_5) <- TRUE
output(n_PE2_ICase1_No_Drug_5) <- TRUE

# Variables to Check for Drug 6
output(number_req_ICU_initial) <- TRUE
output(number_req_ICU) <- TRUE

# Variables to Check for Drug 7
output(number_req_ICU_MV_initial) <- TRUE
output(number_req_ICU_MV) <- TRUE

# Variables to Check for Drug 8
output(gamma_IMod_GetHosp_GetOx_Surv) <- TRUE
output(gamma_IMod_GetHosp_GetOx_Surv_Drug_8) <- TRUE
output(gamma_IMod_GetHosp_NoOx_Surv) <- TRUE
output(gamma_IMod_GetHosp_NoOx_Surv_Drug_8) <- TRUE

# Variables to Check for Drug 9
output(gamma_ISev_GetICU_GetOx_Surv) <- TRUE
output(gamma_ISev_GetICU_GetOx_Surv_Drug_9) <- TRUE
output(gamma_ISev_GetICU_NoOx_Surv) <- TRUE
output(gamma_ISev_GetICU_NoOx_Surv_Drug_9) <- TRUE
output(n_ISev_GetICU_GetOx_Die1) <- TRUE
output(n_ISev_GetICU_GetOx_Surv1) <- TRUE
output(n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2) <- TRUE

# Variables to Check for Drug 10
output(gamma_ICrit_GetICU_GetOx_GetMV_Surv) <- TRUE
output(gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10) <- TRUE
output(gamma_ICrit_GetICU_GetOx_NoMV_Surv) <- TRUE
output(gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10) <- TRUE
output(gamma_ICrit_GetICU_NoOx_NoMV_Surv) <- TRUE
output(gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10) <- TRUE

# Variables to Check for Drug 11
output(prob_moderate_death_get_hosp_get_ox_baseline) <- TRUE
output(prob_moderate_death_get_hosp_get_ox_Drug_11) <- TRUE
output(prob_moderate_death_get_hosp_get_ox) <- TRUE
output(prob_moderate_death_get_hosp_no_ox_baseline) <- TRUE
output(prob_moderate_death_get_hosp_no_ox_Drug_11) <- TRUE
output(prob_moderate_death_get_hosp_no_ox) <- TRUE

# Variables to Check for Drug 12
output(prob_severe_death_get_ICU_get_ox_baseline) <- TRUE
output(prob_severe_death_get_ICU_get_ox_Drug_12) <- TRUE
output(prob_severe_death_get_ICU_get_ox) <- TRUE
output(prob_severe_death_get_ICU_no_ox_baseline) <- TRUE
output(prob_severe_death_get_ICU_no_ox_Drug_12) <- TRUE
output(prob_severe_death_get_ICU_no_ox) <- TRUE

# Variables to Check for Drug 13
output(prob_critical_death_get_ICU_get_ox_get_MV_baseline) <- TRUE
output(prob_critical_death_get_ICU_get_ox_get_MV_Drug_13) <- TRUE
output(prob_critical_death_get_ICU_get_ox_get_MV) <- TRUE
output(prob_critical_death_get_ICU_get_ox_no_MV_baseline) <- TRUE
output(prob_critical_death_get_ICU_get_ox_no_MV_Drug_13) <- TRUE
output(prob_critical_death_get_ICU_get_ox_no_MV) <- TRUE
output(prob_critical_death_get_ICU_no_ox_no_MV_baseline) <- TRUE
output(prob_critical_death_get_ICU_no_ox_no_MV_Drug_13) <- TRUE
output(prob_critical_death_get_ICU_no_ox_no_MV) <- TRUE

# Extra Non-State Variables Outputted by the Model
output(n_S_E1) <- TRUE
output(n_E1_E2) <- TRUE
output(n_E2_I) <- TRUE
output(n_IMild_R) <- TRUE
output(n_ICase1_ICase2) <- TRUE
output(n_ICase2_Hosp) <- TRUE
output(n_IRec1_IRec2) <- TRUE
output(n_IRec2_R) <- TRUE
output(total_req_ICU) <- TRUE
output(ICU_occ) <- TRUE
output(current_free_ICU) <- TRUE
output(total_GetICU) <- TRUE
output(number_GetICU) <- TRUE
output(number_NotICU) <- TRUE
output(number_NotICU_NotOx_NotMV) <- TRUE
output(number_NotICU_NotOx) <- TRUE
output(number_req_Hosp) <- TRUE
output(total_req_Hosp) <- TRUE
output(hosp_occ) <- TRUE
output(current_free_hosp) <- TRUE
output(total_GetHosp) <- TRUE
output(number_GetHosp) <- TRUE
output(number_NotHosp) <- TRUE
output(oxygen_supply) <- TRUE
output(leftover) <- TRUE
output(baseline_oxygen_demand) <- TRUE
output(prop_ox_hosp_beds) <- TRUE
output(available_oxygen_for_hosp_beds) <- TRUE
output(available_oxygen_for_ICU_beds) <- TRUE
output(total_GetHosp_GetOx) <- TRUE
output(number_GetHosp_Ox) <- TRUE
output(number_GetHosp_NoOx) <- TRUE
output(number_req_ICU_Ox) <- TRUE
output(total_req_ICU_MV) <- TRUE
output(total_req_ICU_Ox) <- TRUE
output(MV_occ) <- TRUE
output(current_free_MV) <- TRUE
output(available_oxygen_for_ICU_MV) <- TRUE
output(available_oxygen_for_ICU_Ox) <- TRUE
output(number_GetICU_GetOx_NeedMV) <-TRUE
output(total_GetICU_GetOx_Need_MV) <- TRUE
output(total_GetICU_GetOx_GetMV) <- TRUE
output(number_GetICU_NoOx) <- TRUE
output(number_GetICU_NoOx_NeedMV) <- TRUE
output(number_GetICU_GetOx_GetMV) <- TRUE
output(number_GetICU_GetOx_NoMV) <- TRUE
output(oxygen_used) <- TRUE
output(number_GetICU_GetOx) <- TRUE
output(oxygen_needed_overall) <- TRUE
output(temp_leftover) <- TRUE
output(n_IMod_GetHosp_GetOx_Die2_D_Hospital) <- TRUE
output(n_IMod_GetHosp_GetOx_Surv2_R) <- TRUE
output(n_IMod_GetHosp_NoOx_Die2_D_Hospital) <- TRUE
output(n_IMod_GetHosp_NoOx_Surv2_R) <- TRUE
output(n_ISev_GetICU_GetOx_Die2_D_Hospital) <- TRUE
output(n_ISev_GetICU_GetOx_Surv2_Rec) <- TRUE
output(n_ISev_GetICU_NoOx_Die2_D_Hospital) <- TRUE
output(n_ISev_GetICU_NoOx_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) <- TRUE
