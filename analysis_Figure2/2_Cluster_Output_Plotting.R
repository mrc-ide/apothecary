# Loading required libraries
library(apothecary); library(tidyverse)

# Loading in required files and creating relevant dataframes
filenames <- list.files("analysis_Figure2/Outputs/")

drug_filenames <- filenames[!grepl("notreat", filenames)]
for (i in 1:length(drug_filenames)) {
  if (i == 1) {
    drug_temp <- readRDS(paste0("analysis_Figure2/Outputs/", drug_filenames[i]))
  } else {
    drug_temp <- rbind(drug_temp, readRDS(paste0("analysis_Figure2/Outputs/", drug_filenames[i])))
  }
}
drugs <- drug_temp %>%
  separate(scenario, c("R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  unite("drug_benefit", drug_benefit:drug_benefit2, remove = TRUE) %>%
  mutate(healthcare = factor(healthcare, levels = c("unlimHC", "limMV", "limMVox", "limMVoxbeds", "noHC"))) %>%
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull")))

no_drug_filenames <- filenames[grepl("notreat", filenames)]
for (i in 1:length(no_drug_filenames)) {
  if (i == 1) {
    no_drug_temp <- readRDS(paste0("analysis_Figure2/Outputs/", no_drug_filenames[i]))
  } else {
    no_drug_temp <- rbind(no_drug_temp, readRDS(paste0("analysis_Figure2/Outputs/", no_drug_filenames[i])))
  }
}
no_drugs <- no_drug_temp %>%
  separate(scenario, c("R0", "healthcare", "drug_benefit", "drug_benefit2")) %>%
  select(IFR, R0, healthcare) %>%
  group_by(R0, healthcare) %>%
  summarise(no_drugs_IFR = mean(IFR))

# Merging Drug and No Drug Dataframes Together and Plotting the IFR
overall <- drugs %>%
  left_join(no_drugs, by = c("R0", "healthcare")) %>%
  mutate(IFR_diff = no_drugs_IFR - IFR) %>%
  mutate(prop_IFR_red = IFR_diff/no_drugs_IFR) %>%
  mutate(healthcare = factor(healthcare, levels = c("unlimHC", "limMV", "limMVox", "limMVoxbeds", "noHC"))) %>%
  mutate(drug_benefit = factor(drug_benefit, levels = c("treatonly_benfull", "allhosp_gradbencons", "allhosp_gradbenopti", "allhosp_benfull")))

ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = healthcare), outlier.shape = NA) +
  geom_point(aes(x = healthcare, y = no_drugs_IFR, col = healthcare), shape = 8) +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

summary <- overall %>%
  group_by(R0, healthcare, drug_benefit) %>%
  summarise(med_IFR = median(IFR),
            mean_IFR = mean(IFR))

ggplot(y) +
  geom_boxplot(aes(x = healthcare, y = prop_IFR_red, fill = healthcare), outlier.shape = NA) +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)


test <- overall %>%
  filter(R0 == "highR0") %>%
  group_by(healthcare, drug_benefit) %>%
  summarise(mean = mean(IFR))

ggplot(test, aes(x = healthcare, y = mean, group = drug_benefit, col = drug_benefit)) +
  geom_path()

ggplot(overall, aes(x = healthcare, y = hosp_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)
ggplot(overall, aes(x = healthcare, y = hosp_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_full_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)

ggplot(overall, aes(x = healthcare, y = ICU_any_treat, fill = healthcare)) +
  geom_boxplot() +
  facet_wrap(R0 ~ drug_benefit, nrow = 2)
