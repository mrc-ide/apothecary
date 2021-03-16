age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80+")
ages <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 85)

# Probabilities of symptom type
prob_hosp <- probs$prob_hosp
prob_mod <- 1 - probs$prob_severe
prob_severe <- probs$prob_severe
prob_critical <- probs$prob_critical

# Probabilities of dying given symptom type and treatment
prob_mod_death_treatment <- probs$prob_moderate_death_get_hosp_get_ox_baseline
prob_mod_death_no_treatment <- probs$prob_moderate_death_get_hosp_no_ox_baseline
prob_mod_death_no_bed <- probs$prob_moderate_death_no_hosp_no_ox

prob_severe_death_treatment <- probs$prob_severe_death_get_ICU_get_ox_baseline
prob_severe_death_no_treatment <- probs$prob_severe_death_get_ICU_no_ox_baseline
prob_severe_death_no_bed <- probs$prob_severe_death_no_ICU_no_ox

prob_critical_death_treatment <- probs$prob_critical_death_get_ICU_get_ox_get_MV_baseline
prob_critical_death_no_MV <- probs$prob_critical_death_get_ICU_get_ox_no_MV_baseline
prob_critical_death_no_treatment <- probs$prob_critical_death_get_ICU_no_ox_no_MV_baseline
prob_critical_death_no_bed <- probs$prob_critical_death_no_ICU_no_ox_no_MV

# No Healthcare Quality Constraints
scen_1 <- (prob_hosp * prob_mod * prob_mod_death_treatment) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_treatment) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_treatment)
scen_1_drug <- (prob_hosp * prob_mod * prob_mod_death_treatment * 0.82) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_treatment * 0.64) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_treatment * 0.64)

# No Mechanical Ventilation
scen_2 <- (prob_hosp * prob_mod * prob_mod_death_treatment) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_treatment) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_MV)
scen_2_drug <- (prob_hosp * prob_mod * prob_mod_death_treatment * 0.82) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_treatment * 0.64) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_MV * 0.82)

# No Mechanical Ventilation & No Oxygen
scen_3 <- (prob_hosp * prob_mod * prob_mod_death_no_treatment) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_no_treatment) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_treatment)
scen_3_drug <- (prob_hosp * prob_mod * prob_mod_death_no_treatment * 0.91) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_no_treatment * 0.82) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_treatment * 1)

# No Mechanical Ventilation & No Oxygen & No Beds
scen_4 <- (prob_hosp * prob_mod * prob_mod_death_no_bed) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_no_bed) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_bed)
scen_4_drug <- (prob_hosp * prob_mod * prob_mod_death_no_bed * 0.91) +
  (prob_hosp * prob_severe * (1 - prob_critical) * prob_severe_death_no_bed * 0.82) +
  (prob_hosp * prob_severe * prob_critical * prob_critical_death_no_bed * 1)

# Plotting Different Scenarios
scen_df <- data.frame(scenario = rep(c("scen_1", "scen_2", "scen_3", "scen_4"), each = 34),
                      age_group = rep(age_groups, 8),
                      prob = c(scen_1, scen_1_drug, scen_2, scen_2_drug,
                               scen_3, scen_3_drug, scen_4, scen_4_drug),
                      drug = rep(c(rep("No", 17), rep("Yes", 17)), 4)) %>%
  mutate(age_group = factor(age_group, levels = age_groups)) %>%
  spread(scenario, prob) %>%
  mutate(scen_2 = scen_2 - scen_1,
         scen_3 = scen_3 - scen_2 - scen_1,
         scen_4 = scen_4 - scen_3 - scen_2 - scen_1) %>%
  gather(scenario, prob, -age_group, -drug) %>%
  mutate(scenario = factor(scenario, levels = c("scen_1", "scen_2", "scen_3", "scen_4")))

ggplot(scen_df, aes(x = interaction(drug, age_group), y = prob, fill = scenario)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y = "Infection Fatality Ratio", x = "Age Group (Years)") +
  scale_fill_manual(name = "Healthcare Quality",
                    values = c("#F5E3E0", "#E8B4BC", "#D282A6", "#6E4555"),
                    labels = c("No MV & No Oxygen", "No MV & Sub-Optimal Oxygen",  "No MV", "Baseline")) +
  theme(axis.text.y = element_text(size = 12),
        legend.position = "right")
