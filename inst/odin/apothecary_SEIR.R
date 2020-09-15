# NOTE - DO WE NEED TO ACCOUNT FOR E GETTING ERRONEOUSLY TREATED BUT NO EFFECT OR EFFECT WHEN CONSIDERING DRUG PROPERTY 2??? POSS IASYMP AS WELL???
# NOTE- NOTE NEED TO DECIDE WHETHER WE APPLY THE INCREASED RATES OF RECOVERY TO THE REC COMPARTMENTS AS WELL - THINK WE NEED TO, BUT NEED TO WORK OUT HOW TO APPLY DIFF RATESTO 1 COMPARTMENT
# NOTE - DO WE WANT TO APPLY ANY OF THE DRUG PROPERTIES E.G. RATE CHANGING TO PEOPLE ASSIGNED TO DIE IN HOSPITALS.
# NOTE - FORMULATION ASSUMES RANDOM ASSIGNMENT OF OXYGEN, BUT COULD TOY WITH SEVERITY SPECIFIC TRIAGING
# NOTE - CHECK WHETHER IREC SHOULD BE OXYGEN BEDS OR NOT (PROB NOT?) AND WHETHER APPLY DRUG 9 AND 10 PROPERTIES (PROB NOT?)

## TIMESTEP RELATED PARAMETERS
##------------------------------------------------------------------------------
dt <- user() # Specified timestep
time <- step * dt # Tracking actual time
output(time) <- TRUE
N_age <- user() # Number of age groups
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
drug_9_indic_ISev_GetICU_GetOx <- user() # binary indicator (0 or 1) specifying whether drug property 9 is activated or not for ISev who get ICU Bed and Oxygen
drug_9_indic_ISev_GetICU_NoOx <- user() # binary indicator (0 or 1) specifying whether drug property 9 is activated or not for ISev who get ICU Bed BUT NO Oxygen
drug_9_prop_treat <- user() # proportion of individuals in relevant ISev compartments (hospital based ones) who receive the drug
drug_9_GetOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ISev who receive an ICU bed and oxygen (1 = baseline, >1 = increase rate of recovery)
drug_9_NoOx_effect_size <- user() # the multiple by which the rate of recovery increases for individuals in ISev who receive an ICU bed BUT NO oxygen (1 = baseline, >1 = increase rate of recovery)

# Drug 10 reduces the duration of stay in hospital for ICrit Patients - can be dependent on whether receiving other appropriate treatment (Oxygen and MV) or not
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

## INDIVIDUAL PROBABILITIES OF TRANSITION BETWEEN COMPARTMENTS
##------------------------------------------------------------------------------
# Transition Probabilities Up Until Hospitalisation/Recovery from Mild Infection
p_S_E1[] <- 1 - exp(-lambda[i] * dt) # Infection - age dependent FOI based on mixing patterns
p_E1_E2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_E2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
p_IMild_R <- if (drug_4_indic == 1) 1 - exp(-gamma_IMild_Drug_4 * dt) else 1 - exp(-gamma_IMild * dt) # Recovery from mild disease taking into account proportion of people receiving drug with property 4
p_IAsymp_R <- 1 - exp(-gamma_IAsymp * dt) # Recovery from mild disease
p_ICase1_ICase2 <- 1 - exp(-gamma_ICase * dt) # Delay between symptom onset and requiring hospitalisation
p_ICase2_Hosp <- 1 - exp(-gamma_ICase * dt) # Progression to requiring hospitalisation. Number split between I_Oxygen and I_MV

# Transition Probabilities for Those Recovering from ICU
p_Rec1_Rec2 <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)
p_Rec2_R <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)

# Transition Probabilities for Those Requiring Hospital Bed and Oxygen -> Recovery/Death, Incorporating Effects of Drug 8 if Relevant
p_IMod_GetHosp_GetOx_Surv <- if (drug_8_indic_IMod_GetHosp_GetOx == 1) 1 - exp(-gamma_IMod_GetHosp_GetOx_Surv_Drug_8 * dt) else 1 - exp(-gamma_IMod_GetHosp_GetOx_Surv * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Recovery
p_IMod_GetHosp_GetOx_Die <- 1 - exp(-gamma_IMod_GetHosp_GetOx_Die * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Dying
p_IMod_GetHosp_NoOx_Surv <- if (drug_8_indic_IMod_GetHosp_NoOx == 1) 1 - exp(-gamma_IMod_GetHosp_NoOx_Surv_Drug_8 * dt) else 1 - exp(-gamma_IMod_GetHosp_NoOx_Surv * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
p_IMod_GetHosp_NoOx_Die <- 1 - exp(-gamma_IMod_GetHosp_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
p_IMod_NoHosp_NoOx_Surv <- 1 - exp(-gamma_IMod_NoHosp_NoOx_Surv * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Recovery
p_IMod_NoHosp_NoOx_Die <- 1 - exp(-gamma_IMod_NoHosp_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed and Oxygen -> Recovery/Death, Incorporating Effects of Drug 9 if Relevant
p_ISev_GetICU_GetOx_Surv <- if (drug_9_indic_ISev_GetICU_GetOx == 1) 1 - exp(-gamma_ISev_GetICU_GetOx_Surv_Drug_9 * dt) else 1 - exp(-gamma_ISev_GetICU_GetOx_Surv * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Recovery
p_ISev_GetICU_GetOx_Die <- 1 - exp(-gamma_ISev_GetICU_GetOx_Die * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Dying
p_ISev_GetICU_NoOx_Surv <- if (drug_9_indic_ISev_GetICU_NoOx == 1) 1 - exp(-gamma_ISev_GetICU_NoOx_Surv_Drug_9 * dt) else 1 - exp(-gamma_ISev_GetICU_NoOx_Surv * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Recovery
p_ISev_GetICU_NoOx_Die <- 1 - exp(-gamma_ISev_GetICU_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
p_ISev_NoICU_NoOx_Surv <- 1 - exp(-gamma_ISev_NoICU_NoOx_Surv * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Recovery
p_ISev_NoICU_NoOx_Die <- 1 - exp(-gamma_ISev_NoICU_NoOx_Die * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed, Oxygen and Mechanical Ventilation -> Recovery/Death, Incorporating Effects of Drug 10 if Relevant
p_ICrit_GetICU_GetOx_GetMV_Surv <- if (drug_10_indic_ICrit_GetICU_GetOx_GetMV == 1) 1 - exp(-gamma_ICrit_GetICU_GetOx_GetMV_Surv_Drug_10 * dt) else 1 - exp(-gamma_ICrit_GetICU_GetOx_GetMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
p_ICrit_GetICU_GetOx_GetMV_Die <- 1 - exp(-gamma_ICrit_GetICU_GetOx_GetMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Dying
p_ICrit_GetICU_GetOx_NoMV_Surv <- if (drug_10_indic_ICrit_GetICU_GetOx_NoMV == 1) 1 - exp(-gamma_ICrit_GetICU_GetOx_NoMV_Surv_Drug_10 * dt) else 1 - exp(-gamma_ICrit_GetICU_GetOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
p_ICrit_GetICU_GetOx_NoMV_Die <- 1 - exp(-gamma_ICrit_GetICU_GetOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
p_ICrit_GetICU_NoOx_NoMV_Surv <- if (drug_10_indic_ICrit_GetICU_NoOx_NoMV) 1 - exp(-gamma_ICrit_GetICU_NoOx_NoMV_Surv_Drug_10 * dt) else 1 - exp(-gamma_ICrit_GetICU_NoOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
p_ICrit_GetICU_NoOx_NoMV_Die <- 1 - exp(-gamma_ICrit_GetICU_NoOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
p_ICrit_NoICU_NoOx_NoMV_Surv <- 1 - exp(-gamma_ICrit_NoICU_NoOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
p_ICrit_NoICU_NoOx_NoMV_Die <- 1 - exp(-gamma_ICrit_NoICU_NoOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Dying

## NUMBER OF INDIVIDUALS LEAVING DIFFERENT COMPARTMENTS
##------------------------------------------------------------------------------

## DRAWS FOR NUMBER OF INDIVIDUALS MOVING BETWEEN NON-HOSPITAL/ICU BED RELATED COMPARTMENTS
##-----------------------------------------------------------------------------------------

# For those treated with the prophylactic drugs (properties 1 and 2)
drug_lambda[] <- if (drug_1_indic == 1) lambda[i] * drug_1_effect_size else lambda[i] # FOI experienced by individuals who've taken Drug 1
n_S_PS[] <- if ((time == prophylactic_drug_timing_1 || time == prophylactic_drug_timing_2) && (drug_1_indic == 1|| drug_2_indic == 1)) rbinom(S[i], prophylactic_prop_treat) else 0 # number treated in mass community campaign with prophylactic drug
p_leave_PS[] <- if (drug_1_indic == 1) 1 - exp(-(prophylactic_drug_wane + drug_lambda[i]) * dt) else 1 - exp(-(prophylactic_drug_wane + lambda[i]) * dt) # total of competing hazards for leaving PS (drug wane -> S and infection -> PE1). Latter depends on whether drug 1 is active or not.
n_leave_PS[] <- rbinom(PS[i], p_leave_PS[i]) # total number of people leaving PS to either S or PE1
n_PS_PE1[] <- if (prophylactic_drug_wane != 0) rbinom(n_leave_PS[i], (drug_lambda[i])/(drug_lambda[i] + prophylactic_drug_wane)) else n_leave_PS[i] # number of people leaving PS who flow to PE1. Depends on whether drug 1 is active or not.
n_PS_S[] <- n_leave_PS[i] - n_PS_PE1[i] # number of people leaving PS who flow back to S (due to drug wearing off)
n_PE1_PE2[] <- rbinom(PE1[i], p_E1_E2) # number of people leaving PE1 who flow to PE2.
n_PE2_I[] <-  rbinom(PE2[i], p_E2_I) # number of people leaving PE2 who flow to one of the I compartments

n_PE2_ICase1_initial[] <- rbinom(n_PE2_I[i], prob_hosp[i]) # initial number of people moving from PE2 -> ICase1 (subsequently modified by Drug 2's effects, with some of these flowing to IMild instead)
n_PE2_ICase1_initial_part_2[] <- if (drug_3_indic == 1) rbinom(n_PE2_ICase1_initial[i], 1 - (drug_3_prop_treat * drug_3_effect_size)) else n_PE2_ICase1_initial[i]
n_PE2_ICase1[] <- if (drug_2_indic == 1) rbinom(n_PE2_ICase1_initial_part_2[i], (1 - drug_2_effect_size)) else n_PE2_ICase1_initial_part_2[i] # number of people who flow to ICase that remain after the effects of Drug 2 have been taken into account
n_PE2_ICase1_Drug_5[] <- if (drug_5_indic_ICase == 1) rbinom(n_PE2_ICase1[i], drug_5_prop_treat) else 0 # number of people flowing to ICase that receive Drug 5 and move to reduced infectivity compartment
n_PE2_ICase1_No_Drug_5[] <- n_PE2_ICase1[i] - n_PE2_ICase1_Drug_5[i] # number of people flowing to ICase that DO NOT receive Drug 5

n_PE2_IMild_or_IAsymp[] <- n_PE2_I[i] - n_PE2_ICase1[i] # number of individuals with mild disease or are asymptomatic
n_PE2_IAsymp[] <- rbinom(n_PE2_IMild_or_IAsymp[i], prob_asymp[i]) # number of individuals who are asymptomatic
n_PE2_IMild[] <- n_PE2_IMild_or_IAsymp[i] - n_PE2_IAsymp[i] + (n_PE2_ICase1_initial[i] - n_PE2_ICase1[i]) # number of mild disease individuals taking into account (n_PE2_ICase1_initial[i] - n_PE2_ICase1[i]) which is the extra IMilds produced through Drug 2 effects
n_PE2_IMild_Drug_5[] <- if (drug_5_indic_IMild == 1) rbinom(n_PE2_IMild[i], drug_5_prop_treat) else 0 # number of people flowing to IMild that receive Drug 5 and move to reduced infectivity compartment
n_PE2_IMild_No_Drug_5[] <- n_PE2_IMild[i] - n_PE2_IMild_Drug_5[i] # number of people flowing to IMild that DO NOT receive Drug 5

# For those not treated with the prophylactic drugs (properties 1 and 2)
n_S_E1[] <- rbinom(S[i] - n_S_PS[i], p_S_E1[i]) # Number of newly infected individuals
n_E1_E2[] <- rbinom(E1[i], p_E1_E2) # Number progressing through latent compartments
n_E2_I[] <- rbinom(E2[i], p_E2_I) # Number of new symptom onsets

n_E2_ICase1_initial[] <- rbinom(n_E2_I[i], prob_hosp[i]) # Initial number of people moving from PE2 -> ICase1 (subsequently modified by Drug 3's effects, with some of these flowing to IMild instead)
n_E2_ICase1[] <- if (drug_3_indic == 1) rbinom(n_E2_ICase1_initial[i], 1 - (drug_3_prop_treat * drug_3_effect_size)) else n_E2_ICase1_initial[i]
n_E2_ICase1_Drug_5[] <- if (drug_5_indic_ICase == 1) rbinom(n_E2_ICase1[i], drug_5_prop_treat) else 0 # number of people flowing to ICase that receive Drug 5 and move to reduced infectivity compartment
n_E2_ICase1_No_Drug_5[] <- n_E2_ICase1[i] - n_E2_ICase1_Drug_5[i] # number of people flowing to ICase that DO NOT receive Drug 5

n_E2_IMild_or_IAsymp[] <- n_E2_I[i] - n_E2_ICase1[i] # number of individuals with mild disease or are asymptomatic
n_E2_IAsymp[] <- rbinom(n_E2_IMild_or_IAsymp[i], prob_asymp[i]) # number of individuals who are asymptomatic
n_E2_IMild[] <- n_E2_IMild_or_IAsymp[i] - n_E2_IAsymp[i] + (n_E2_ICase1_initial[i] - n_E2_ICase1[i])  # number of mild disease individuals taking into account (n_E2_ICase1_initial[i] - n_E2_ICase1[i]) which is the extra IMilds produced through Drug 3 effects
n_E2_IMild_Drug_5[] <- if (drug_5_indic_IMild == 1) rbinom(n_E2_IMild[i], drug_5_prop_treat) else 0 # number of people flowing to IMild that receive Drug 5 and move to reduced infectivity compartment
n_E2_IMild_No_Drug_5[] <- n_E2_IMild[i] - n_E2_IMild_Drug_5[i] # number of people flowing to IMild that DO NOT receive the drug

# Individuals moving either to Recovery or the Hospital
n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # Number of mild infections NOT receiving drug 5 recovering, taking into account proportion receiving drug 4 and its effect to hasten recovery
n_IMild_Drug_5_R[] <- rbinom(IMild_Drug_5[i], p_IMild_R) # Number of mild receibing drug 5 infections recovering, taking into account proportion receiving drug 4 and its effect to hasten recovery
n_IAsymp_R[] <- rbinom(IAsymp[i], p_IAsymp_R) # Number of asymptomatic infections recovering
n_ICase1_Drug_5_ICase2_Drug_5[] <- rbinom(ICase1_Drug_5[i], p_ICase1_ICase2) # Number of individuals in ICase1 who receive Drug 5 progressing to ICase2
n_ICase2_Drug_5_Hosp[] <- rbinom(ICase2_Drug_5[i], p_ICase2_Hosp) # Number of individuals in ICase2 who receive Drug 5 progressing to IHosp
n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # Number progressing through the onset but not hospitalised compartment
n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # Number progressing to requiring hospitalisation
n_IRec1_IRec2[] <- rbinom(IRec1[i], p_Rec1_Rec2) # Number progressing through ICU recovery compartment
n_IRec2_R[] <- rbinom(IRec2[i], p_Rec2_R) # Number recovering completely NOTE, CHANGE: DOES P_REC NEEDS TO INCORPORATE DRUG EFFECT (9 & 10 THAT MODIFY SEV/CRIT DURATION OF STAY, THOR NOT??)

## WORKING OUT NUMBER OF HOSPITAL BEDS AVAILABILE AND HOW MANY INDIVIDUALS RECEIVE THEM
##-------------------------------------------------------------------------------------
# Calculating Number of Individuals Requiring An ICU Bed
number_req_Hosp[] <- (n_ICase2_Hosp[i] + n_ICase2_Drug_5_Hosp[i]) - number_req_ICU[i]  # Number of new hospitalisations that are going to require a hospital bed and oxygen (i.e. IMod)
total_req_Hosp <- sum(number_req_Hosp) # Totalling number newly requiring a hospital bed and oxygen (i.e. IMod) over age groups

# Current Hospital Bed Occupancy (Includes Stepdown Hospital Beds for Patient Leaving ICU)
hosp_bed_ox_occ <- sum(IMod_GetHosp_GetOx_Surv1) + sum(IMod_GetHosp_GetOx_Surv2) + sum(IMod_GetHosp_GetOx_Die1) + sum(IMod_GetHosp_GetOx_Die2)
hosp_bed_no_ox_occ <- sum(IMod_GetHosp_NoOx_Surv1) + sum(IMod_GetHosp_NoOx_Surv2) + sum(IMod_GetHosp_NoOx_Die1) + sum(IMod_GetHosp_NoOx_Die2) +
                      sum(IRec1) + sum(IRec2)
hosp_bed_no_ox_occ_without_IRec <- sum(IMod_GetHosp_NoOx_Surv1) + sum(IMod_GetHosp_NoOx_Surv2) + sum(IMod_GetHosp_NoOx_Die1) + sum(IMod_GetHosp_NoOx_Die2)
overall_hosp_occ <- hosp_bed_ox_occ + hosp_bed_no_ox_occ
output(hosp_bed_ox_occ) <- TRUE
output(hosp_bed_no_ox_occ) <- TRUE
output(hosp_bed_no_ox_occ_without_IRec) <- TRUE
output(overall_hosp_occ) <- TRUE

# Totting Hospital Bed Occupancy Up After Taking Account of Individuals Leaving Hospital Beds to Recovery and Entering from ICU
current_free_hosp_bed_ox <- round(current_prop_ox_hosp_beds * current_hosp_bed_capacity) +
                            sum(n_IMod_GetHosp_GetOx_Surv2_R) + sum(n_IMod_GetHosp_GetOx_Die2_D_Hospital) - hosp_bed_ox_occ
current_free_hosp_bed_no_ox <- (current_hosp_bed_capacity - round(current_prop_ox_hosp_beds * current_hosp_bed_capacity))  +
                               sum(n_IMod_GetHosp_NoOx_Surv2_R) + sum(n_IMod_GetHosp_NoOx_Die2_D_Hospital) +
                               sum(n_IRec2_R) -
                               sum(n_ISev_GetICU_GetOx_Surv2_Rec) - sum(n_ISev_GetICU_NoOx_Surv2_Rec) -
                               sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) - sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) - sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) - hosp_bed_no_ox_occ
output(current_free_hosp_bed_ox) <- TRUE
output(current_free_hosp_bed_no_ox) <- TRUE

# Calculating Individuals Getting A Hospital Bed With Oxygen
total_IMod_GetHosp_GetOx <- if (current_free_hosp_bed_ox <= 0) 0 else (if(current_free_hosp_bed_ox - total_req_Hosp >= 0) total_req_Hosp else(current_free_hosp_bed_ox)) # Working out the number of new hospital bed requiring infections that get a bed
number_IMod_GetHosp_GetOx[] <- rmhyper(total_IMod_GetHosp_GetOx, number_req_Hosp) # sampling without replacment total_GetHosp beds' worth of individuals from the N_age long vector number_req_Hosp
number_req_hosp_remaining[] <- number_req_Hosp[i] - number_IMod_GetHosp_GetOx[i]

# Calculating Individuals Getting A Hospital Bed Without Oxygen
total_IMod_GetHosp_NoOx <- if (current_free_hosp_bed_no_ox <= 0) 0 else (if(current_free_hosp_bed_no_ox - (total_req_Hosp - sum(number_IMod_GetHosp_GetOx)) >= 0) (total_req_Hosp - sum(number_IMod_GetHosp_GetOx)) else(current_free_hosp_bed_no_ox)) # Working out the number of new hospital bed requiring infections that get a bed
number_IMod_GetHosp_NoOx[] <- rmhyper(total_IMod_GetHosp_NoOx, number_req_hosp_remaining) # sampling without replacment total_GetHosp beds' worth of individuals from the N_age long vector number_req_Hosp

# Calculating Individuals Not Receiving A Hospital Bed
number_IMod_NoHosp_NoOx[] <- number_req_Hosp[i] - number_IMod_GetHosp_GetOx[i] - number_IMod_GetHosp_NoOx[i] # Number of individuals who require hospital bed and oxygen who do not receive a hospital bed (and hence also do no receive oxygen)

## WORKING OUT NUMBER OF ICU BEDS AVAILABILE HOW MANY INDIVIDUALS RECEIVE THEM, OXYGEN AND MECHANCAL VENTILATION
##--------------------------------------------------------------------------------------------------------------
# Calculating Number of Individuals Requiring An ICU Bed
number_req_ICU_initial[] <- rbinom(n_ICase2_Hosp[i] + n_ICase2_Drug_5_Hosp[i], prob_severe[i]) # Initial number of new hospitalisations that are going to require an ICU bed (either with or w/o mechanical ventilation)
number_req_ICU[] <- if (drug_6_indic == 1) rbinom(number_req_ICU_initial[i], 1 - (drug_6_prop_treat * drug_6_effect_size)) else number_req_ICU_initial[i] # Number of new hospitalisations initially requiring an ICU bed (either with or w/o mechanical ventilation) that flow to IMod instead due to Drug 6 effects
total_req_ICU <- sum(number_req_ICU) # Totalling number newly requiring an ICU bed over age groups

# Calculating Current ICU Occupancy - Total, But Also Specifically for Beds With and Without Oxygen
ICU_bed_ox_occ <- sum(ISev_GetICU_GetOx_Surv1) + sum(ISev_GetICU_GetOx_Surv2) + sum(ISev_GetICU_GetOx_Die1) + sum(ISev_GetICU_GetOx_Die2) +
                  sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) +
                  sum(ICrit_GetICU_GetOx_NoMV_Surv1) + sum(ICrit_GetICU_GetOx_NoMV_Surv2) + sum(ICrit_GetICU_GetOx_NoMV_Die1) + sum(ICrit_GetICU_GetOx_NoMV_Die2)
ICU_bed_no_ox_occ <- sum(ISev_GetICU_NoOx_Surv1) + sum(ISev_GetICU_NoOx_Surv2) + sum(ISev_GetICU_NoOx_Die1) + sum(ISev_GetICU_NoOx_Die2) +
                     sum(ICrit_GetICU_NoOx_NoMV_Surv1) + sum(ICrit_GetICU_NoOx_NoMV_Surv2) + sum(ICrit_GetICU_NoOx_NoMV_Die1) + sum(ICrit_GetICU_NoOx_NoMV_Die2)
overall_ICU_occ <- ICU_bed_ox_occ + ICU_bed_no_ox_occ
output(ICU_bed_ox_occ) <- TRUE
output(ICU_bed_no_ox_occ) <- TRUE
output(overall_ICU_occ) <- TRUE

# Calculating New Occupancy After Taking Into Account Individuals Leaving ICU Beds This Timestep
current_free_ICU_bed_ox <- round(current_prop_ox_ICU_beds * current_ICU_bed_capacity) +
                       sum(n_ISev_GetICU_GetOx_Surv2_Rec) + sum(n_ISev_GetICU_GetOx_Die2_D_Hospital) +
                       sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) +
                       sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) - ICU_bed_ox_occ
current_free_ICU_bed_no_ox <- (current_ICU_bed_capacity - round(current_prop_ox_ICU_beds * current_ICU_bed_capacity)) +
                          sum(n_ISev_GetICU_NoOx_Surv2_Rec) + sum(n_ISev_GetICU_NoOx_Die2_D_Hospital) +
                          sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) - ICU_bed_no_ox_occ
output(current_free_ICU_bed_ox) <- TRUE
output(current_free_ICU_bed_no_ox) <- TRUE

# Individuals Getting ICU Beds With Oxygen And Their Associated Disease Severities
total_GetICU_GetOx <- if(current_free_ICU_bed_ox <= 0) 0 else(if(current_free_ICU_bed_ox - total_req_ICU >= 0) total_req_ICU else(current_free_ICU_bed_ox)) # Working out the number of new ICU requiring infections that get a bed
number_GetICU_GetOx[] <- rmhyper(total_GetICU_GetOx, number_req_ICU) # sampling without replacment total_GetICU beds' worth of individuals from the N_age long vector number_req_ICU
number_req_ICU_remaining[] <- number_req_ICU[i] - number_GetICU_GetOx[i]

number_ICrit_GetICU_GetOx_initial[] <- rbinom(number_GetICU_GetOx[i], prob_critical[i]) # Initial number of new ICU Bed admissions that are going to have Critical Disease (i.e. require oxygen AND MV)
number_ICrit_GetICU_GetOx[] <- if (drug_7_indic == 1) rbinom(number_ICrit_GetICU_GetOx_initial[i], 1 - (drug_7_prop_treat * drug_7_effect_size)) else number_ICrit_GetICU_GetOx_initial[i] # Number of new ICU Bed admissions that are going to have Critical Disease (i.e. require oxygen AND MV) after taking Drug 7 effects into account
number_ISev_GetICU_Bed_GetOx[] <- number_GetICU_GetOx[i] - number_ICrit_GetICU_GetOx[i] # Number of new ICU admissions that going to require oxygen only

# Calculating Whether the Critical Cases In ICU Beds With Oxygen Get A Mechanical Ventilator Or Not
MV_occ <- sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) # Current Mechanical Ventilator Usage
current_free_MV <- MV_capacity + sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) - MV_occ # Number of mechanical ventilators that are currently free after taking into account flows out of ICrit
output(MV_occ) <- TRUE
output(current_free_MV) <- TRUE
total_ICrit_GetICU_GetOx_GetMV <- if(current_free_MV <= 0) 0 else(if(current_free_MV - sum(number_ICrit_GetICU_GetOx) >= 0) sum(number_ICrit_GetICU_GetOx) else(current_free_MV))
number_ICrit_GetICU_GetOx_GetMV[] <-  rmhyper(total_ICrit_GetICU_GetOx_GetMV, number_ICrit_GetICU_GetOx)
number_ICrit_GetICU_GetOx_NoMV[] <- number_ICrit_GetICU_GetOx[i] - number_ICrit_GetICU_GetOx_GetMV[i]

# Individuals Getting ICU Beds Without Oxygen And Their Associated Disease Severities
total_GetICU_NoOx <- if(current_free_ICU_bed_no_ox <= 0) 0 else(if(current_free_ICU_bed_no_ox - (total_req_ICU - total_GetICU_GetOx)) total_req_ICU - total_GetICU_GetOx else (current_free_ICU_bed_no_ox))
number_GetICU_NoOx[] <- rmhyper(total_GetICU_NoOx, number_req_ICU_remaining)

number_ICrit_GetICU_NoOx_NoMV_initial[] <- rbinom(number_GetICU_NoOx[i], prob_critical[i]) # Initial number of new ICU Bed admissions that are going to have Critical Disease (i.e. require oxygen AND MV)
number_ICrit_GetICU_NoOx_NoMV[] <- if (drug_7_indic == 1) rbinom(number_ICrit_GetICU_NoOx_NoMV_initial[i], 1 - (drug_7_prop_treat * drug_7_effect_size)) else number_ICrit_GetICU_NoOx_NoMV_initial[i] # Number of new ICU Bed admissions that are going to have Critical Disease (i.e. require oxygen AND MV) after taking Drug 7 effects into account
number_ISev_GetICU_NoOx[] <- number_GetICU_NoOx[i] - number_ICrit_GetICU_NoOx_NoMV[i] # Number of new ICU admissions that going to require oxygen only

# Individuals Not Receiving ICU Beds And Their Associated Disease Severities
number_NoICU[] <- number_req_ICU[i] - number_GetICU_GetOx[i] - number_GetICU_NoOx[i] # number who do not get an ICU bed
number_ICrit_NoICU_NoOx_NoMV[] <- rbinom(number_NoICU[i], prob_critical[i]) # number who do not get an ICU bed and who require both oxygen and mechanical ventilation (i.e. ICrit)
number_ISev_NoICU_NoOx[] <- number_NoICU[i] - number_ICrit_NoICU_NoOx_NoMV[i] # number who do not get an ICU bed and who require oxygen only (i.e. ISev)

## CALCULATING THE NUMBER OF INDIVIDUALS MOVING OUT OF HOSPITAL/ICU BED RELATED COMPARTMENTS
##------------------------------------------------------------------------------------------
# Numbers changing between hospital bed related compartments
n_IMod_GetHosp_GetOx_Die1[] <- rbinom(number_IMod_GetHosp_GetOx[i], prob_moderate_death_get_hosp_get_ox[i])
n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[] <- rbinom(IMod_GetHosp_GetOx_Die1[i], p_IMod_GetHosp_GetOx_Die) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IMod_GetHosp_GetOx_Die2_D_Hospital[] <- rbinom(IMod_GetHosp_GetOx_Die2[i], p_IMod_GetHosp_GetOx_Die) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IMod_GetHosp_GetOx_Surv1[] <- number_IMod_GetHosp_GetOx[i] - n_IMod_GetHosp_GetOx_Die1[i]
n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[] <- rbinom(IMod_GetHosp_GetOx_Surv1[i], p_IMod_GetHosp_GetOx_Surv) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery
n_IMod_GetHosp_GetOx_Surv2_R[] <- rbinom(IMod_GetHosp_GetOx_Surv2[i], p_IMod_GetHosp_GetOx_Surv) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery

n_IMod_GetHosp_NoOx_Die1[] <- rbinom(number_IMod_GetHosp_NoOx[i], prob_moderate_death_get_hosp_no_ox[i])
n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[] <- rbinom(IMod_GetHosp_NoOx_Die1[i], p_IMod_GetHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IMod_GetHosp_NoOx_Die2_D_Hospital[] <- rbinom(IMod_GetHosp_NoOx_Die2[i], p_IMod_GetHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IMod_GetHosp_NoOx_Surv1[] <- number_IMod_GetHosp_NoOx[i] - n_IMod_GetHosp_NoOx_Die1[i]
n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[] <- rbinom(IMod_GetHosp_NoOx_Surv1[i], p_IMod_GetHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
n_IMod_GetHosp_NoOx_Surv2_R[] <- rbinom(IMod_GetHosp_NoOx_Surv2[i], p_IMod_GetHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery

n_IMod_NoHosp_NoOx_Die1[] <- rbinom(number_IMod_NoHosp_NoOx[i], prob_moderate_death_no_hosp_no_ox[i])
n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[] <- rbinom(IMod_NoHosp_NoOx_Die1[i], p_IMod_NoHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IMod_NoHosp_NoOx_Die2_D_Community[] <- rbinom(IMod_NoHosp_NoOx_Die2[i], p_IMod_NoHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IMod_NoHosp_NoOx_Surv1[] <- number_IMod_NoHosp_NoOx[i] - n_IMod_NoHosp_NoOx_Die1[i]
n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[] <- rbinom(IMod_NoHosp_NoOx_Surv1[i], p_IMod_NoHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery
n_IMod_NoHosp_NoOx_Surv2_R[] <- rbinom(IMod_NoHosp_NoOx_Surv2[i], p_IMod_NoHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery

# Numbers changing between ICU bed/non-mechanical ventilation related compartments
n_ISev_GetICU_GetOx_Die1[] <- rbinom(number_ISev_GetICU_Bed_GetOx[i], prob_severe_death_get_ICU_get_ox[i])
n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[] <- rbinom(ISev_GetICU_GetOx_Die1[i], p_ISev_GetICU_GetOx_Die) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying
n_ISev_GetICU_GetOx_Die2_D_Hospital[] <- rbinom(ISev_GetICU_GetOx_Die2[i], p_ISev_GetICU_GetOx_Die) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying
n_ISev_GetICU_GetOx_Surv1[] <- number_ISev_GetICU_Bed_GetOx[i] - n_ISev_GetICU_GetOx_Die1[i]
n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[] <- rbinom(ISev_GetICU_GetOx_Surv1[i], p_ISev_GetICU_GetOx_Surv) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery
n_ISev_GetICU_GetOx_Surv2_Rec[] <- rbinom(ISev_GetICU_GetOx_Surv2[i], p_ISev_GetICU_GetOx_Surv) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery

n_ISev_GetICU_NoOx_Die1[] <-  rbinom(number_ISev_GetICU_NoOx[i], prob_severe_death_get_ICU_no_ox[i])
n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[] <- rbinom(ISev_GetICU_NoOx_Die1[i], p_ISev_GetICU_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
n_ISev_GetICU_NoOx_Die2_D_Hospital[] <- rbinom(ISev_GetICU_NoOx_Die2[i], p_ISev_GetICU_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
n_ISev_GetICU_NoOx_Surv1[] <- number_ISev_GetICU_NoOx[i] - n_ISev_GetICU_NoOx_Die1[i]
n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[] <- rbinom(ISev_GetICU_NoOx_Surv1[i], p_ISev_GetICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
n_ISev_GetICU_NoOx_Surv2_Rec[] <- rbinom(ISev_GetICU_NoOx_Surv2[i], p_ISev_GetICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery

n_ISev_NoICU_NoOx_Die1[] <- rbinom(number_ISev_NoICU_NoOx[i], prob_severe_death_no_ICU_no_ox[i])
n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[] <- rbinom(ISev_NoICU_NoOx_Die1[i], p_ISev_NoICU_NoOx_Die) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying
n_ISev_NoICU_NoOx_Die2_D_Community[] <- rbinom(ISev_NoICU_NoOx_Die2[i], p_ISev_NoICU_NoOx_Die) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying
n_ISev_NoICU_NoOx_Surv1[] <- number_ISev_NoICU_NoOx[i] - n_ISev_NoICU_NoOx_Die1[i]
n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[] <- rbinom(ISev_NoICU_NoOx_Surv1[i], p_ISev_NoICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery
n_ISev_NoICU_NoOx_Surv2_R[] <- rbinom(ISev_NoICU_NoOx_Surv2[i], p_ISev_NoICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery

# Numbers changing between ICU bed/mechanical ventilation related compartments
n_ICrit_GetICU_GetOx_GetMV_Die1[] <- rbinom(number_ICrit_GetICU_GetOx_GetMV[i], prob_critical_death_get_ICU_get_ox_get_MV[i])
n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Die1[i], p_ICrit_GetICU_GetOx_GetMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Dying
n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Die2[i], p_ICrit_GetICU_GetOx_GetMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICrit_GetICU_GetOx_GetMV_Surv1[] <- number_ICrit_GetICU_GetOx_GetMV[i] - n_ICrit_GetICU_GetOx_GetMV_Die1[i]
n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Surv1[i], p_ICrit_GetICU_GetOx_GetMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Surv2[i], p_ICrit_GetICU_GetOx_GetMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery

n_ICrit_GetICU_GetOx_NoMV_Die1[] <- rbinom(number_ICrit_GetICU_GetOx_NoMV[i], prob_critical_death_get_ICU_get_ox_no_MV[i])
n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Die1[i], p_ICrit_GetICU_GetOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Die2[i], p_ICrit_GetICU_GetOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICrit_GetICU_GetOx_NoMV_Surv1[] <- number_ICrit_GetICU_GetOx_NoMV[i] - n_ICrit_GetICU_GetOx_NoMV_Die1[i]
n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Surv1[i], p_ICrit_GetICU_GetOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Surv2[i], p_ICrit_GetICU_GetOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery

n_ICrit_GetICU_NoOx_NoMV_Die1[] <- rbinom(number_ICrit_GetICU_NoOx_NoMV[i], prob_critical_death_get_ICU_no_ox_no_MV[i])
n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Die1[i], p_ICrit_GetICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Die2[i], p_ICrit_GetICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICrit_GetICU_NoOx_NoMV_Surv1[] <- number_ICrit_GetICU_NoOx_NoMV[i] - n_ICrit_GetICU_NoOx_NoMV_Die1[i]
n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Surv1[i], p_ICrit_GetICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Surv2[i], p_ICrit_GetICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery

n_ICrit_NoICU_NoOx_NoMV_Die1[] <- rbinom(number_ICrit_NoICU_NoOx_NoMV[i], prob_critical_death_no_ICU_no_ox_no_MV[i])
n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Die1[i], p_ICrit_NoICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Die2[i], p_ICrit_NoICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
n_ICrit_NoICU_NoOx_NoMV_Surv1[] <- number_ICrit_NoICU_NoOx_NoMV[i] - n_ICrit_NoICU_NoOx_NoMV_Die1[i]
n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Surv1[i], p_ICrit_NoICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
n_ICrit_NoICU_NoOx_NoMV_Surv2_R[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Surv2[i], p_ICrit_NoICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery

## TOTALLING UP THE FLOWS IN AND OUT OF EACH COMPARTMENT
##------------------------------------------------------

# Non-Hospital/ICU Bed Related Compartments
delta_S[] <- - n_S_E1[i] - n_S_PS[i] + n_PS_S[i]
delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]
delta_IAsymp[] <- n_E2_IAsymp[i] + n_PE2_IAsymp[i] - n_IAsymp_R[i]

delta_IMild[] <- n_E2_IMild_No_Drug_5[i] + n_PE2_IMild_No_Drug_5[i] - n_IMild_R[i]
delta_ICase1[] <- n_E2_ICase1_No_Drug_5[i] + n_PE2_ICase1_No_Drug_5[i] - n_ICase1_ICase2[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]

delta_IMild_Drug_5[] <- n_E2_IMild_Drug_5[i] + n_PE2_IMild_Drug_5[i] - n_IMild_Drug_5_R[i]
delta_ICase1_Drug_5[] <- n_E2_ICase1_Drug_5[i] + n_PE2_ICase1_Drug_5[i] - n_ICase1_Drug_5_ICase2_Drug_5[i]
delta_ICase2_Drug_5[] <- n_ICase1_Drug_5_ICase2_Drug_5[i] - n_ICase2_Drug_5_Hosp[i]

delta_PS[] <- n_S_PS[i] - n_leave_PS[i]
delta_PE1[] <- n_PS_PE1[i] - n_PE1_PE2[i]
delta_PE2[] <- n_PE1_PE2[i] - n_PE2_I[i]

# Stepdown Bed, Recovery and Death Related Compartments
delta_IRec1[] <- n_ISev_GetICU_GetOx_Surv2_Rec[i] + n_ISev_GetICU_NoOx_Surv2_Rec[i] +
                 n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]  + n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i] + n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i] -
                 n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]
delta_R[] <- n_IMild_R[i] + n_IMild_Drug_5_R[i] +
             n_IAsymp_R[i] +
             n_IRec2_R[i] +
             n_IMod_GetHosp_GetOx_Surv2_R[i] + n_IMod_GetHosp_NoOx_Surv2_R[i] + n_IMod_NoHosp_NoOx_Surv2_R[i] +
             n_ISev_NoICU_NoOx_Surv2_R[i] +
             n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]
delta_D_Community[] <- n_IMod_NoHosp_NoOx_Die2_D_Community[i] + n_ISev_NoICU_NoOx_Die2_D_Community[i] + n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
delta_D_Hospital[] <- n_IMod_GetHosp_GetOx_Die2_D_Hospital[i] + n_IMod_GetHosp_NoOx_Die2_D_Hospital[i] +
                      n_ISev_GetICU_GetOx_Die2_D_Hospital[i] + n_ISev_GetICU_NoOx_Die2_D_Hospital[i] +
                      n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i] + n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i] + n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]

# Flows In and Out of Hospital Bed Related Compartments
delta_IMod_GetHosp_GetOx_Die1[] <- n_IMod_GetHosp_GetOx_Die1[i] - n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i]
delta_IMod_GetHosp_GetOx_Die2[] <- n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i] - n_IMod_GetHosp_GetOx_Die2_D_Hospital[i]
delta_IMod_GetHosp_GetOx_Surv1[] <- n_IMod_GetHosp_GetOx_Surv1[i] - n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i]
delta_IMod_GetHosp_GetOx_Surv2[] <- n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i] - n_IMod_GetHosp_GetOx_Surv2_R[i]
delta_IMod_GetHosp_NoOx_Die1[] <- n_IMod_GetHosp_NoOx_Die1[i] - n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i]
delta_IMod_GetHosp_NoOx_Die2[] <- n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i] - n_IMod_GetHosp_NoOx_Die2_D_Hospital[i]
delta_IMod_GetHosp_NoOx_Surv1[] <- n_IMod_GetHosp_NoOx_Surv1[i] - n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i]
delta_IMod_GetHosp_NoOx_Surv2[] <- n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i] - n_IMod_GetHosp_NoOx_Surv2_R[i]
delta_IMod_NoHosp_NoOx_Die1[] <- n_IMod_NoHosp_NoOx_Die1[i] - n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i]
delta_IMod_NoHosp_NoOx_Die2[] <- n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i] - n_IMod_NoHosp_NoOx_Die2_D_Community[i]
delta_IMod_NoHosp_NoOx_Surv1[] <- n_IMod_NoHosp_NoOx_Surv1[i] - n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i]
delta_IMod_NoHosp_NoOx_Surv2[] <- n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i] - n_IMod_NoHosp_NoOx_Surv2_R[i]

# Flows In and Out of ICU Bed & Oxygen Only Related Compartments
delta_ISev_GetICU_GetOx_Die1[] <- n_ISev_GetICU_GetOx_Die1[i] - n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i]
delta_ISev_GetICU_GetOx_Die2[] <- n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i] - n_ISev_GetICU_GetOx_Die2_D_Hospital[i]
delta_ISev_GetICU_GetOx_Surv1[] <- n_ISev_GetICU_GetOx_Surv1[i] - n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i]
delta_ISev_GetICU_GetOx_Surv2[] <- n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i] - n_ISev_GetICU_GetOx_Surv2_Rec[i]
delta_ISev_GetICU_NoOx_Die1[] <- n_ISev_GetICU_NoOx_Die1[i] - n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i]
delta_ISev_GetICU_NoOx_Die2[] <- n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i] - n_ISev_GetICU_NoOx_Die2_D_Hospital[i]
delta_ISev_GetICU_NoOx_Surv1[] <- n_ISev_GetICU_NoOx_Surv1[i] - n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i]
delta_ISev_GetICU_NoOx_Surv2[] <- n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i] - n_ISev_GetICU_NoOx_Surv2_Rec[i]
delta_ISev_NoICU_NoOx_Die1[] <- n_ISev_NoICU_NoOx_Die1[i] - n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i]
delta_ISev_NoICU_NoOx_Die2[] <- n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i] - n_ISev_NoICU_NoOx_Die2_D_Community[i]
delta_ISev_NoICU_NoOx_Surv1[] <- n_ISev_NoICU_NoOx_Surv1[i] - n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i]
delta_ISev_NoICU_NoOx_Surv2[] <- n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i] - n_ISev_NoICU_NoOx_Surv2_R[i]

# Flows In and Out of ICU Bed, Oxygen and Mechanical Ventilation Related Compartments
delta_ICrit_GetICU_GetOx_GetMV_Die1[] <- n_ICrit_GetICU_GetOx_GetMV_Die1[i] - n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i]
delta_ICrit_GetICU_GetOx_GetMV_Die2[] <- n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i] - n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_GetOx_GetMV_Surv1[] <- n_ICrit_GetICU_GetOx_GetMV_Surv1[i] - n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i]
delta_ICrit_GetICU_GetOx_GetMV_Surv2[] <- n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i] - n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]
delta_ICrit_GetICU_GetOx_NoMV_Die1[] <- n_ICrit_GetICU_GetOx_NoMV_Die1[i] - n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i]
delta_ICrit_GetICU_GetOx_NoMV_Die2[] <- n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i] - n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_GetOx_NoMV_Surv1[] <- n_ICrit_GetICU_GetOx_NoMV_Surv1[i] - n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i]
delta_ICrit_GetICU_GetOx_NoMV_Surv2[] <- n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i] - n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i]
delta_ICrit_GetICU_NoOx_NoMV_Die1[] <- n_ICrit_GetICU_NoOx_NoMV_Die1[i] - n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i]
delta_ICrit_GetICU_NoOx_NoMV_Die2[] <- n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i] - n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_NoOx_NoMV_Surv1[] <- n_ICrit_GetICU_NoOx_NoMV_Surv1[i] - n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i]
delta_ICrit_GetICU_NoOx_NoMV_Surv2[] <- n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i] - n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i]
delta_ICrit_NoICU_NoOx_NoMV_Die1[] <- n_ICrit_NoICU_NoOx_NoMV_Die1[i] - n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i]
delta_ICrit_NoICU_NoOx_NoMV_Die2[] <- n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i] - n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
delta_ICrit_NoICU_NoOx_NoMV_Surv1[] <- n_ICrit_NoICU_NoOx_NoMV_Surv1[i] - n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i]
delta_ICrit_NoICU_NoOx_NoMV_Surv2[] <- n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i] - n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]

## UPDATING STATE VARIABLES WITH THE OVERALL TRANSITIONS IN AND OUT OF EACH COMPARTMENT
##-------------------------------------------------------------------------------------
# Passage Through Initial Latent and Infection Stages
update(S[]) <- S[i] + delta_S[i] # Susceptibles (1 comp)
update(E1[]) <- E1[i] + delta_E1[i] # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
update(IAsymp[]) <- IAsymp[i] + delta_IAsymp[i] # Asymptomatic infections (1 comp)
update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)
update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)

update(IMild_Drug_5[]) <- IMild_Drug_5[i] + delta_IMild_Drug_5[i]
update(ICase1_Drug_5[]) <- ICase1_Drug_5[i] + delta_ICase1_Drug_5[i]
update(ICase2_Drug_5[]) <- ICase2_Drug_5[i] + delta_ICase2_Drug_5[i]

# Passage Through Drug Treated Initial Susceptible/Latent Stages
update(PS[]) <- PS[i] + delta_PS[i]
update(PE1[]) <- PE1[i] + delta_PE1[i]
update(PE2[]) <- PE2[i] + delta_PE2[i]

# Passage Through Requiring Hospital Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(IMod_GetHosp_GetOx_Die1[]) <- IMod_GetHosp_GetOx_Die1[i] + delta_IMod_GetHosp_GetOx_Die1[i] # Require hosp bed and oxygen, get both, die (1st)
update(IMod_GetHosp_GetOx_Die2[]) <- IMod_GetHosp_GetOx_Die2[i] + delta_IMod_GetHosp_GetOx_Die2[i] # Require hosp bed and oxygen, get both, die (2nd)
update(IMod_GetHosp_GetOx_Surv1[]) <- IMod_GetHosp_GetOx_Surv1[i] + delta_IMod_GetHosp_GetOx_Surv1[i] # Require hosp bed and oxygen, get both, survive (1st)
update(IMod_GetHosp_GetOx_Surv2[]) <- IMod_GetHosp_GetOx_Surv2[i] + delta_IMod_GetHosp_GetOx_Surv2[i] # Require hosp bed and oxygen, get both, survive (2nd)
update(IMod_GetHosp_NoOx_Die1[]) <- IMod_GetHosp_NoOx_Die1[i] + delta_IMod_GetHosp_NoOx_Die1[i] # Require hosp bed and oxygen, get bed only, die (1st)
update(IMod_GetHosp_NoOx_Die2[]) <- IMod_GetHosp_NoOx_Die2[i] + delta_IMod_GetHosp_NoOx_Die2[i] # Require hosp bed and oxygen, get bed only, die (2nd)
update(IMod_GetHosp_NoOx_Surv1[]) <- IMod_GetHosp_NoOx_Surv1[i] + delta_IMod_GetHosp_NoOx_Surv1[i] # Require hosp bed and oxygen, get bed only, survive (1st)
update(IMod_GetHosp_NoOx_Surv2[]) <- IMod_GetHosp_NoOx_Surv2[i] + delta_IMod_GetHosp_NoOx_Surv2[i] # Require hosp bed and oxygen, get bed only, survive (2nd)
update(IMod_NoHosp_NoOx_Die1[]) <- IMod_NoHosp_NoOx_Die1[i] + delta_IMod_NoHosp_NoOx_Die1[i] # Require hosp bed and oxygen, get neither, die (1st)
update(IMod_NoHosp_NoOx_Die2[]) <- IMod_NoHosp_NoOx_Die2[i] + delta_IMod_NoHosp_NoOx_Die2[i] # Require hosp bed and oxygen, get neither, survive (2nd)
update(IMod_NoHosp_NoOx_Surv1[]) <- IMod_NoHosp_NoOx_Surv1[i] + delta_IMod_NoHosp_NoOx_Surv1[i] # Require hosp bed and oxygen, get neither, survive (1st)
update(IMod_NoHosp_NoOx_Surv2[]) <- IMod_NoHosp_NoOx_Surv2[i] + delta_IMod_NoHosp_NoOx_Surv2[i] # Require hosp bed and oxygen, get neither, survive (2nd)

# Passage Through Requiring ICU Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(ISev_GetICU_GetOx_Die1[]) <- ISev_GetICU_GetOx_Die1[i] + delta_ISev_GetICU_GetOx_Die1[i] # Require ICU bed and oxygen, get both, die (1st)
update(ISev_GetICU_GetOx_Die2[]) <- ISev_GetICU_GetOx_Die2[i] + delta_ISev_GetICU_GetOx_Die2[i] # Require ICU bed and oxygen, get both, die (2nd)
update(ISev_GetICU_GetOx_Surv1[]) <- ISev_GetICU_GetOx_Surv1[i] + delta_ISev_GetICU_GetOx_Surv1[i] # Require ICU bed and oxygen, get both, survive (1st)
update(ISev_GetICU_GetOx_Surv2[]) <- ISev_GetICU_GetOx_Surv2[i] + delta_ISev_GetICU_GetOx_Surv2[i] # Require ICU bed and oxygen, get both, survive (2nd)
update(ISev_GetICU_NoOx_Die1[]) <- ISev_GetICU_NoOx_Die1[i] + delta_ISev_GetICU_NoOx_Die1[i] # Require ICU bed and oxygen, get ICU bed only, die (1st)
update(ISev_GetICU_NoOx_Die2[]) <- ISev_GetICU_NoOx_Die2[i] + delta_ISev_GetICU_NoOx_Die2[i] # Require ICU bed and oxygen, get ICU bed only, die (2nd)
update(ISev_GetICU_NoOx_Surv1[]) <- ISev_GetICU_NoOx_Surv1[i] + delta_ISev_GetICU_NoOx_Surv1[i] # Require ICU bed and oxygen, get ICU bed only, survive (1st)
update(ISev_GetICU_NoOx_Surv2[]) <- ISev_GetICU_NoOx_Surv2[i] + delta_ISev_GetICU_NoOx_Surv2[i] # Require ICU bed and oxygen, get ICU bed only, survive (2nd)
update(ISev_NoICU_NoOx_Die1[]) <- ISev_NoICU_NoOx_Die1[i] + delta_ISev_NoICU_NoOx_Die1[i] # Require ICU bed and oxygen, get neither, die (1st)
update(ISev_NoICU_NoOx_Die2[]) <- ISev_NoICU_NoOx_Die2[i] + delta_ISev_NoICU_NoOx_Die2[i] # Require ICU bed and oxygen, get neither, die (2nd)
update(ISev_NoICU_NoOx_Surv1[]) <- ISev_NoICU_NoOx_Surv1[i] + delta_ISev_NoICU_NoOx_Surv1[i] # Require ICU bed and oxygen, get neither, suvive (1st)
update(ISev_NoICU_NoOx_Surv2[]) <- ISev_NoICU_NoOx_Surv2[i] + delta_ISev_NoICU_NoOx_Surv2[i] # Require ICU bed and oxygen, get neither, suvive (2nd)

# Passage Through Requiring ICU Bed, Oxygen and Mechanical Ventilation, Either Receiving All, ICU Bed and Oxygen, ICU Bed Only or Nothing, and Surviving or Not
update(ICrit_GetICU_GetOx_GetMV_Die1[]) <- ICrit_GetICU_GetOx_GetMV_Die1[i] + delta_ICrit_GetICU_GetOx_GetMV_Die1[i] # Require ICU bed, oxygen and MV, get all, die (1st)
update(ICrit_GetICU_GetOx_GetMV_Die2[]) <- ICrit_GetICU_GetOx_GetMV_Die2[i] + delta_ICrit_GetICU_GetOx_GetMV_Die2[i] # Require ICU bed, oxygen and MV, get all, die (2nd)
update(ICrit_GetICU_GetOx_GetMV_Surv1[]) <- ICrit_GetICU_GetOx_GetMV_Surv1[i] + delta_ICrit_GetICU_GetOx_GetMV_Surv1[i] # Require ICU bed, oxygen and MV, get all, survive (1st)
update(ICrit_GetICU_GetOx_GetMV_Surv2[]) <- ICrit_GetICU_GetOx_GetMV_Surv2[i] + delta_ICrit_GetICU_GetOx_GetMV_Surv2[i] # Require ICU bed, oxygen and MV, get all, survive (2nd)
update(ICrit_GetICU_GetOx_NoMV_Die1[]) <- ICrit_GetICU_GetOx_NoMV_Die1[i] + delta_ICrit_GetICU_GetOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (1st)
update(ICrit_GetICU_GetOx_NoMV_Die2[]) <- ICrit_GetICU_GetOx_NoMV_Die2[i] + delta_ICrit_GetICU_GetOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (2nd)
update(ICrit_GetICU_GetOx_NoMV_Surv1[]) <- ICrit_GetICU_GetOx_NoMV_Surv1[i] + delta_ICrit_GetICU_GetOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (1st)
update(ICrit_GetICU_GetOx_NoMV_Surv2[]) <- ICrit_GetICU_GetOx_NoMV_Surv2[i] + delta_ICrit_GetICU_GetOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (2nd)
update(ICrit_GetICU_NoOx_NoMV_Die1[]) <- ICrit_GetICU_NoOx_NoMV_Die1[i] + delta_ICrit_GetICU_NoOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (1st)
update(ICrit_GetICU_NoOx_NoMV_Die2[]) <- ICrit_GetICU_NoOx_NoMV_Die2[i] + delta_ICrit_GetICU_NoOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (2nd)
update(ICrit_GetICU_NoOx_NoMV_Surv1[]) <- ICrit_GetICU_NoOx_NoMV_Surv1[i] + delta_ICrit_GetICU_NoOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (1st)
update(ICrit_GetICU_NoOx_NoMV_Surv2[]) <- ICrit_GetICU_NoOx_NoMV_Surv2[i] + delta_ICrit_GetICU_NoOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (2nd)
update(ICrit_NoICU_NoOx_NoMV_Die1[]) <- ICrit_NoICU_NoOx_NoMV_Die1[i] + delta_ICrit_NoICU_NoOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get nothing, die (1st)
update(ICrit_NoICU_NoOx_NoMV_Die2[]) <- ICrit_NoICU_NoOx_NoMV_Die2[i] + delta_ICrit_NoICU_NoOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get nothing, die (2nd)
update(ICrit_NoICU_NoOx_NoMV_Surv1[]) <- ICrit_NoICU_NoOx_NoMV_Surv1[i] + delta_ICrit_NoICU_NoOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get nothing, survive (1st)
update(ICrit_NoICU_NoOx_NoMV_Surv2[]) <- ICrit_NoICU_NoOx_NoMV_Surv2[i] + delta_ICrit_NoICU_NoOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get nothing, survive (2nd)

# Passage Through Recovery, from Mild Infection, Requiring Oxygen or From ICU Post-Requiring Mechanical Ventilation
update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
update(R[]) <- R[i] + delta_R[i] # Recovered
update(D_Community[]) <- D_Community[i] + delta_D_Community[i] # Deaths in the community
update(D_Hospital[]) <- D_Hospital[i] + delta_D_Hospital[i] # Deaths in the community

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
current_prop_ox_hosp_beds <- interpolate(tt_prop_ox_hosp_beds, prop_ox_hosp_beds, "constant")
tt_prop_ox_hosp_beds[] <- user()
prop_ox_hosp_beds[] <- user()
dim(tt_prop_ox_hosp_beds) <- user()
dim(prop_ox_hosp_beds) <- length(tt_prop_ox_hosp_beds)

current_prop_ox_ICU_beds <- interpolate(tt_prop_ox_ICU_beds, prop_ox_ICU_beds, "constant")
tt_prop_ox_ICU_beds[] <- user()
prop_ox_ICU_beds[] <- user()
dim(tt_prop_ox_ICU_beds) <- user()
dim(prop_ox_ICU_beds) <- length(tt_prop_ox_ICU_beds)

MV_capacity <- user() # number of mechanical ventilators available

## MODEL OUTPUTS
##------------------------------------------------------------------------------
output(pop) <- TRUE

# Variables to Check for Drug 1 and Drug 2
output(n_S_PS) <- TRUE
output(n_leave_PS) <- TRUE
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
output(p_IMild_R) <- TRUE

# Variables to Check for Drug 5
output(n_E2_IMild) <- TRUE
output(n_E2_IMild_Drug_5) <- TRUE
output(n_E2_IMild_No_Drug_5) <- TRUE
output(n_PE2_IMild) <- TRUE
output(n_PE2_IMild_Drug_5) <- TRUE
output(n_PE2_IMild_No_Drug_5) <- TRUE
# output(n_E2_ICase1) <- TRUE
output(n_E2_ICase1_Drug_5) <- TRUE
output(n_E2_ICase1_No_Drug_5) <- TRUE
# output(n_PE2_ICase1) <- TRUE
output(n_PE2_ICase1_Drug_5) <- TRUE
output(n_PE2_ICase1_No_Drug_5) <- TRUE

# Variables to Check for Drug 6
output(number_req_ICU_initial) <- TRUE
output(number_req_ICU) <- TRUE

# Variables to Check for Drug 7
output(number_ICrit_GetICU_GetOx) <- TRUE
output(number_ISev_GetICU_Bed_GetOx) <- TRUE
output(number_ICrit_GetICU_NoOx_NoMV) <- TRUE
output(number_ISev_GetICU_NoOx) <- TRUE

# Variables to Check for Drug 8
output(gamma_IMod_GetHosp_GetOx_Surv) <- TRUE
output(gamma_IMod_GetHosp_GetOx_Surv_Drug_8) <- TRUE
output(gamma_IMod_GetHosp_NoOx_Surv) <- TRUE
output(gamma_IMod_GetHosp_NoOx_Surv_Drug_8) <- TRUE
output(p_IMod_GetHosp_GetOx_Surv) <- TRUE
output(p_IMod_GetHosp_NoOx_Surv) <- TRUE

# Variables to Check for Drug 9
output(gamma_ISev_GetICU_GetOx_Surv) <- TRUE
output(gamma_ISev_GetICU_GetOx_Surv_Drug_9) <- TRUE
output(gamma_ISev_GetICU_NoOx_Surv) <- TRUE
output(gamma_ISev_GetICU_NoOx_Surv_Drug_9) <- TRUE
output(p_ISev_GetICU_GetOx_Surv) <- TRUE
output(p_ISev_GetICU_NoOx_Surv) <- TRUE
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
output(p_ICrit_GetICU_GetOx_GetMV_Surv) <- TRUE
output(p_ICrit_GetICU_GetOx_NoMV_Surv) <- TRUE
output(p_ICrit_GetICU_NoOx_NoMV_Surv) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Surv1) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2) <- TRUE

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



## DEFINING INITIAL STATES
##------------------------------------------------------------------------------
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

# For the Flows Between State Variables
dim(delta_S) <- N_age
dim(delta_E1) <- N_age
dim(delta_E2) <- N_age
dim(delta_IAsymp) <- N_age
dim(delta_IMild) <- N_age
dim(delta_ICase1) <- N_age
dim(delta_ICase2) <- N_age
dim(delta_IMild_Drug_5) <- N_age
dim(delta_ICase1_Drug_5) <- N_age
dim(delta_ICase2_Drug_5) <- N_age
dim(delta_IRec1) <- N_age
dim(delta_IRec2) <- N_age
dim(delta_R) <- N_age
dim(delta_D_Community) <- N_age
dim(delta_D_Hospital) <- N_age

dim(delta_PS) <- N_age
dim(delta_PE1) <- N_age
dim(delta_PE2) <- N_age

dim(delta_IMod_GetHosp_GetOx_Surv1) <- N_age
dim(delta_IMod_GetHosp_GetOx_Surv2) <- N_age
dim(delta_IMod_GetHosp_GetOx_Die1) <- N_age
dim(delta_IMod_GetHosp_GetOx_Die2) <- N_age
dim(delta_IMod_GetHosp_NoOx_Surv1) <- N_age
dim(delta_IMod_GetHosp_NoOx_Surv2) <- N_age
dim(delta_IMod_GetHosp_NoOx_Die1) <- N_age
dim(delta_IMod_GetHosp_NoOx_Die2) <- N_age
dim(delta_IMod_NoHosp_NoOx_Surv1) <- N_age
dim(delta_IMod_NoHosp_NoOx_Surv2) <- N_age
dim(delta_IMod_NoHosp_NoOx_Die1) <- N_age
dim(delta_IMod_NoHosp_NoOx_Die2) <- N_age

dim(delta_ISev_GetICU_GetOx_Surv1) <- N_age
dim(delta_ISev_GetICU_GetOx_Surv2) <- N_age
dim(delta_ISev_GetICU_GetOx_Die1) <- N_age
dim(delta_ISev_GetICU_GetOx_Die2) <- N_age
dim(delta_ISev_GetICU_NoOx_Surv1) <- N_age
dim(delta_ISev_GetICU_NoOx_Surv2) <- N_age
dim(delta_ISev_GetICU_NoOx_Die1) <- N_age
dim(delta_ISev_GetICU_NoOx_Die2) <- N_age
dim(delta_ISev_NoICU_NoOx_Surv1) <- N_age
dim(delta_ISev_NoICU_NoOx_Surv2) <- N_age
dim(delta_ISev_NoICU_NoOx_Die1) <- N_age
dim(delta_ISev_NoICU_NoOx_Die2) <- N_age

dim(delta_ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Die2) <- N_age

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

# Additional Flows in the Hospitalisation Module
dim(number_req_Hosp) <- N_age
dim(number_IMod_GetHosp_GetOx) <- N_age
dim(number_req_hosp_remaining) <- N_age
dim(number_IMod_GetHosp_NoOx) <- N_age
dim(number_IMod_NoHosp_NoOx) <- N_age
dim(number_req_ICU_initial) <- N_age
dim(number_req_ICU) <- N_age
dim(number_GetICU_GetOx) <- N_age
dim(number_req_ICU_remaining) <- N_age
dim(number_ICrit_GetICU_GetOx_initial) <- N_age
dim(number_ICrit_GetICU_GetOx) <- N_age
dim(number_ISev_GetICU_Bed_GetOx) <- N_age
dim(number_ICrit_GetICU_GetOx_GetMV) <- N_age
dim(number_ICrit_GetICU_GetOx_NoMV) <- N_age
dim(number_GetICU_NoOx) <- N_age
dim(number_ICrit_GetICU_NoOx_NoMV_initial) <- N_age
dim(number_ICrit_GetICU_NoOx_NoMV) <- N_age
dim(number_ISev_GetICU_NoOx) <- N_age
dim(number_NoICU) <- N_age
dim(number_ICrit_NoICU_NoOx_NoMV) <- N_age
dim(number_ISev_NoICU_NoOx) <- N_age

# Related to Calculating Age-Structured Force of Infection
dim(p_S_E1) <- N_age
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

dim(drug_lambda) <- N_age
dim(n_S_PS) <- N_age
dim(n_leave_PS) <- N_age
dim(n_PS_PE1) <- N_age
dim(n_PS_S) <- N_age
dim(n_PE1_PE2) <- N_age
dim(n_PE2_I) <- N_age
dim(n_PE2_ICase1_initial) <- N_age
dim(n_PE2_ICase1_initial_part_2) <- N_age
dim(n_PE2_ICase1) <- N_age
dim(n_PE2_ICase1_Drug_5) <- N_age
dim(n_PE2_ICase1_No_Drug_5) <- N_age
dim(n_E2_IMild_No_Drug_5) <- N_age
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
dim(p_leave_PS) <- N_age

