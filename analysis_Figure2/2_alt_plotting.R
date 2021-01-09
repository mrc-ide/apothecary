overall <- overall %>%
  mutate(drug_benefit = as.character(drug_benefit)) %>%
  mutate(drug_benefit = case_when(
    drug_benefit == "treatonly_benfull" ~ "atreatonly_benfull",
    drug_benefit == "allhosp_gradbencons" ~ "ballhosp_gradbencons",
    drug_benefit == "allhosp_gradbenopti" ~ "callhosp_gradbenopti",
    TRUE ~ drug_benefit)
  )

overall$drug_benefit <- factor(overall$drug_benefit, levels = c("atreatonly_benfull", "ballhosp_gradbencons", "callhosp_gradbenopti"))

healthcare.labs <- c("Impact Only In Treated Patients",
                     "Impact In All Hospitalised Patients - Pess.",
                     "Impact In All Hospitalised Patients - Opti.")
#"Impact In All Hospitalised Patients - Full")
names(healthcare.labs) <- c("atreatonly_benfull", "ballhosp_gradbencons", "callhosp_gradbenopti")

IFRplot2 <- ggplot(overall) +
  geom_boxplot(aes(x = healthcare, y = IFR, fill = interaction(R0, healthcare)), outlier.shape = NA) +
  geom_point(data = no_drug_data, aes(x = healthcare, y = no_drugs_IFR, group = interaction(R0, healthcare)),
             shape = 19, position = position_dodge(width = 0.75)) +
  geom_hline(aes(yintercept = no_hc_IFR), no_hc, linetype = "dashed") +
  facet_grid(. ~ drug_benefit,
             labeller = labeller(R0 = R0.labs, drug_benefit = healthcare.labs)) +
  scale_fill_manual(values = c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                               "#82CA59", "#9E3813", "#549418", "purple", "pink"),
                    name = "fill") +
  scale_x_discrete(labels = c("Unlimited\nHealthcare",
                              "Limited\nARS",
                              "Limited\nARS\n& O2",
                              "Limited\nARS, O2\n& Beds",
                              "No\nHealthcare")) +
  scale_colour_manual(values = rep("black", 10)) +
  theme(legend.position = "none", axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(vjust = +2.5)) +
  labs(x = "", y = "Infection Fatality Ratio (%)") +
  lims(y = c(0, 0.55))

unlimcHC_drug_benefit <- overall %>%
  filter(healthcare == "unlimHC") %>%
  mutate(abs_unlimitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_unlimitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, no_drugs_IFR, IFR,
         abs_unlimitedHC_drug_IFR_reduction, prop_unlimitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_unlimitedHC_drug_IFR_reduction = mean(abs_unlimitedHC_drug_IFR_reduction),
            prop_unlimitedHC_drug_IFR_reduction = mean(prop_unlimitedHC_drug_IFR_reduction))

limHC_drug_benefit <- overall %>%
  mutate(abs_limitedHC_drug_IFR_reduction = no_drugs_IFR - IFR,
         prop_limitedHC_drug_IFR_reduction = (no_drugs_IFR - IFR)/no_drugs_IFR) %>%
  select(R0, drug_benefit, healthcare, no_drugs_IFR, IFR,
         abs_limitedHC_drug_IFR_reduction, prop_limitedHC_drug_IFR_reduction) %>%
  group_by(R0, drug_benefit, healthcare) %>%
  summarise(no_drugs_IFR = mean(no_drugs_IFR), IFR = mean(IFR),
            abs_limitedHC_drug_IFR_reduction = mean(abs_limitedHC_drug_IFR_reduction),
            prop_limitedHC_drug_IFR_reduction = mean(prop_limitedHC_drug_IFR_reduction))

overall_new <- limHC_drug_benefit %>%
  left_join(unlimcHC_drug_benefit, by = c("R0", "drug_benefit")) %>%
  mutate(prop_benefit_gained = prop_limitedHC_drug_IFR_reduction/prop_unlimitedHC_drug_IFR_reduction) %>%
  filter(drug_benefit != "allhosp_benfull")

treatonly_benfull <- overall_new %>%
  filter(drug_benefit == "atreatonly_benfull")
treatonly_benfull <- ggplotGrob(ggplot(treatonly_benfull) +
                  geom_bar(aes(x = R0, y = prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                           stat = "identity", position = "dodge", col = "black") +
                    scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                                                       "#82CA59", "#9E3813", "#549418"),
                                                     alpha = c(1, 1, 1, 1, 1, 1, 1, 1)),
                                      name = "fill") +
                    scale_x_discrete(labels = c("High R0", "Low R0")) +
                  #scale_fill_manual(values = c("#9E3813", "#549418")) +
                  scale_y_continuous(position="left", limits = c(0, 1)) +
                  theme(legend.position = "none", axis.title = element_blank(),
                        #axis.text.x = element_blank(),
                        #axis.ticks.x = element_blank(),
                        strip.background.y = element_blank(),
                        strip.text.y = element_blank()) +
                  theme(plot.background = element_rect(colour = "black")))

allhosp_gradbencons <- overall_new %>%
  filter(drug_benefit == "ballhosp_gradbencons")
allhosp_gradbencons <- ggplotGrob(ggplot(allhosp_gradbencons) +
                                  geom_bar(aes(x = R0, y = prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                                           stat = "identity", position = "dodge", col = "black") +
                                    scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                                                                       "#82CA59", "#9E3813", "#549418"),
                                                                     alpha = c(1, 1, 1, 1, 1, 1, 1, 1)),
                                                      name = "fill") +
                                    scale_x_discrete(labels = c("High R0", "Low R0")) +
                                  #scale_fill_manual(values = c("#9E3813", "#549418")) +
                                  scale_y_continuous(position="left", limits = c(0, 1)) +
                                  theme(legend.position = "none", axis.title = element_blank(),
                                        #axis.text.x = element_blank(),
                                        #axis.ticks.x = element_blank(),
                                        strip.background.y = element_blank(),
                                        strip.text.y = element_blank()) +
                                  theme(plot.background = element_rect(colour = "black")))

allhosp_gradbenopti <- overall_new %>%
  filter(drug_benefit == "callhosp_gradbenopti")
allhosp_gradbenopti <- ggplotGrob(ggplot(allhosp_gradbenopti) +
                                    geom_bar(aes(x = R0, y = prop_benefit_gained, fill = interaction(R0, healthcare), group = interaction(healthcare, R0)),
                                             stat = "identity", position = "dodge", col = "black") +
                                    scale_fill_manual(values = alpha(c("white", "white", "#F6A78A", "#DBF0CE", "#E4521B",
                                                                       "#82CA59", "#9E3813", "#549418"),
                                                                     alpha = c(1, 1, 1, 1, 1, 1, 1, 1)),
                                                      name = "fill") +
                                    scale_x_discrete(labels = c("High R0", "Low R0")) +
                                    # scale_fill_manual(values = alpha(c("grey", "grey", "grey",
                                    #                                    "grey", "#9E3813", "#549418"),
                                    #                                  alpha = c(1, 1, 1, 1, 1, 1)),
                                    #                   name = "fill") +
                                    #scale_fill_manual(values = c("#9E3813", "#549418")) +
                                    scale_y_continuous(position="left", limits = c(0, 1)) +
                                    theme(legend.position = "none", axis.title = element_blank(),
                                          strip.background.y = element_blank(),
                                          #axis.text.x = element_blank(),
                                          #axis.ticks.x = element_blank(),
                                          strip.text.y = element_blank()) +
                                    theme(plot.background = element_rect(colour = "black")))


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity,
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob,
                                          xmin = xmin, xmax = xmax,
                                          ymin = ymin, ymax = ymax))
}

IFRplot2 +
  annotation_custom2(grob = treatonly_benfull, data = data.frame(drug_benefit="atreatonly_benfull"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5) +
  annotation_custom2(grob = allhosp_gradbencons, data = data.frame(drug_benefit="ballhosp_gradbencons"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5) +
  annotation_custom2(grob = allhosp_gradbenopti, data = data.frame(drug_benefit="callhosp_gradbenopti"),
                     ymin = 0, ymax = 0.25, xmin = 2, xmax = 4.5)

alpha(c("#F6A78A", "#DBF0CE", "#E4521B",
        "#82CA59", "#9E3813", "#549418"), alpha = c(0.3, 0.3, 0.3, 0.3, 1, 1))
