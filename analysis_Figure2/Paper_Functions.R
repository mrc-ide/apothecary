extract_IFR <- function(model_run, age_reduce = TRUE) {
  output <- model_run$output
  index <- apothecary:::odin_index(model_run$model)
  R <- apply(output[, index$R], 2, max)
  D_Hosp <- apply(output[, index$D_Hospital], 2, max)
  D_Comm <- apply(output[, index$D_Community], 2, max)
  Total_D <- D_Hosp + D_Comm
  Total_Inf <- Total_D + R
  if (age_reduce) {
    overall_IFR <- sum(Total_D)/sum(Total_Inf)
    return(overall_IFR)
  } else {
    age_spec_IFR <- Total_D/Total_Inf
    return(unname(age_spec_IFR))
  }
}

get_hosp_occ <- function(apothecary_output) {

  index <- apothecary:::odin_index(apothecary_output$model)

  hosp_full_treat_occ_index <- c(index$IMod_GetHosp_GetOx_Surv1, index$IMod_GetHosp_GetOx_Surv2,
                                 index$IMod_GetHosp_GetOx_Die1, index$IMod_GetHosp_GetOx_Die2,
                                 index$IRec1, index$IRec2)
  hosp_any_occ_index <- c(hosp_full_treat_occ_index,
                          index$IMod_GetHosp_NoOx_Die1, index$IMod_GetHosp_NoOx_Die2,
                          index$IMod_GetHosp_NoOx_Surv1, index$IMod_GetHosp_NoOx_Surv2)
  hosp_dem_index <- c(hosp_any_occ_index,
                      index$IMod_NoHosp_NoOx_Surv2, index$IMod_NoHosp_NoOx_Die1, index$IMod_NoHosp_NoOx_Die2)
  number_need_hosp_bed <- index$number_req_hosp_bed
  number_get_hosp_full_treat <- index$number_get_hosp_full_treat
  number_get_hosp_any_treat <- index$number_get_hosp_any_treat

  hosp_full_treat_occ <- apply(apothecary_output$output[, hosp_full_treat_occ_index], 1, sum)
  hosp_any_treat_occ <- apply(apothecary_output$output[, hosp_any_occ_index], 1, sum)
  hosp_dem <- apply(apothecary_output$output[, hosp_dem_index], 1, sum)
  hosp_need <- apothecary_output$output[, number_need_hosp_bed]
  hosp_get_full_treat <- apothecary_output$output[, number_get_hosp_full_treat]
  hosp_get_any_treat <- apothecary_output$output[, number_get_hosp_any_treat]

  return(list(hosp_dem = hosp_dem, hosp_full_treat_occ = hosp_full_treat_occ,
              hosp_need = hosp_need, hosp_get_full_treat = hosp_get_full_treat, hosp_get_any_treat = hosp_get_any_treat))
}

get_ICU_occ <- function(apothecary_output) {

  index <- apothecary:::odin_index(apothecary_output$model)

  ICU_full_treat_occ_index <- c(index$ISev_GetICU_GetOx_Die1, index$ISev_GetICU_GetOx_Die2, index$ISev_GetICU_GetOx_Surv1, index$ISev_GetICU_GetOx_Surv2,
                                index$ICrit_GetICU_GetOx_GetMV_Die1, index$ICrit_GetICU_GetOx_GetMV_Die2, index$ICrit_GetICU_GetOx_GetMV_Surv1, index$ICrit_GetICU_GetOx_GetMV_Surv2)
  ICU_any_occ_index <- c(ICU_full_treat_occ_index,
                         index$ISev_GetICU_NoOx_Die1, index$ISev_GetICU_NoOx_Die2, index$ISev_GetICU_NoOx_Surv1, index$ISev_GetICU_NoOx_Surv2,
                         index$ICrit_GetICU_GetOx_NoMV_Die1, index$ICrit_GetICU_GetOx_NoMV_Die2, index$ICrit_GetICU_GetOx_NoMV_Surv1, index$ICrit_GetICU_GetOx_NoMV_Surv2,
                         index$ICrit_GetICU_NoOx_NoMV_Die1, index$ICrit_GetICU_NoOx_NoMV_Die2, index$ICrit_GetICU_NoOx_NoMV_Surv1, index$ICrit_GetICU_NoOx_NoMV_Surv2)
  ICU_dem_index <- c(ICU_any_occ_index,
                     index$ISev_NoICU_NoOx_Die1, index$ISev_NoICU_NoOx_Die2, index$ISev_NoICU_NoOx_Surv1, index$ISev_NoICU_NoOx_Surv2,
                     index$ICrit_NoICU_NoOx_NoMV_Die1, index$ICrit_NoICU_NoOx_NoMV_Die2, index$ICrit_NoICU_NoOx_NoMV_Surv1, index$ICrit_NoICU_NoOx_NoMV_Surv2)
  number_need_ICU_bed <- index$number_req_ICU_bed
  number_get_ICU_full_treat <- index$number_get_ICU_full_treat
  number_get_ICU_any_treat <- index$number_get_ICU_any_treat

  ICU_full_treat_occ <- apply(apothecary_output$output[, ICU_full_treat_occ_index], 1, sum)
  ICU_any_treat_occ <- apply(apothecary_output$output[, ICU_any_occ_index], 1, sum)
  ICU_dem <- apply(apothecary_output$output[, ICU_dem_index], 1, sum)
  ICU_need <- apothecary_output$output[, number_need_ICU_bed]
  ICU_get_full_treat <- apothecary_output$output[, number_get_ICU_full_treat]
  ICU_get_any_treat <- apothecary_output$output[, number_get_ICU_any_treat]

  return(list(ICU_dem = ICU_dem, ICU_full_treat_occ = ICU_full_treat_occ, ICU_any_treat_occ = ICU_any_treat_occ,
              ICU_need = ICU_need, ICU_get_full_treat = ICU_get_full_treat, ICU_get_any_treat = ICU_get_any_treat))
}

run_drugs_hc_combo <- function(demog_pars, hc_pars, drug_ind_pars, drug_eff_pars, scenario) {

  output <- lapply(seq_along(drug_eff_pars$dexy_mod_getox_mort), function(x) {

    temp <- run_apothecary(

      # Demographic and Epidemiological Parameter Specification
      country = demog_pars$country,
      R0 = demog_pars$R0,
      population = demog_pars$population,
      contact_matrix_set = demog_pars$matrix,
      time_period = demog_pars$time_period,
      seeding_cases = demog_pars$seeding_cases,
      day_return = TRUE,

      # Healthcare Capacity Parameter Specification
      hosp_bed_capacity = hc_pars$hosp_bed_capacity,
      ICU_bed_capacity = hc_pars$ICU_bed_capacity,
      prop_ox_hosp_beds = hc_pars$prop_ox_hosp_beds,
      prop_ox_ICU_beds = hc_pars$prop_ox_ICU_beds,
      MV_capacity = hc_pars$MV_capacity,

      # Drug Indicator and Proportion Treated Parameters
      drug_8_indic_IMod_GetHosp_GetOx = drug_ind_pars$drug_8_indic_IMod_GetHosp_GetOx,
      drug_8_indic_IMod_GetHosp_NoOx = drug_ind_pars$drug_8_indic_IMod_GetHosp_NoOx,
      drug_8_prop_treat = drug_ind_pars$drug_8_prop_treat,

      drug_11_indic_IMod_GetHosp_GetOx = drug_ind_pars$drug_11_indic_IMod_GetHosp_GetOx,
      drug_11_indic_IMod_GetHosp_NoOx = drug_ind_pars$drug_11_indic_IMod_GetHosp_NoOx,
      drug_11_prop_treat = drug_ind_pars$drug_11_prop_treat,

      drug_12_indic_ISev_GetICU_GetOx = drug_ind_pars$drug_12_indic_ISev_GetICU_GetOx,
      drug_12_indic_ISev_GetICU_NoOx = drug_ind_pars$drug_12_indic_ISev_GetICU_NoOx,
      drug_12_prop_treat = drug_ind_pars$drug_12_prop_treat,

      drug_13_indic_ICrit_GetICU_GetOx_GetMV = drug_ind_pars$drug_13_indic_ICrit_GetICU_GetOx_GetMV,
      drug_13_indic_ICrit_GetICU_GetOx_NoMV = drug_ind_pars$drug_13_indic_ICrit_GetICU_GetOx_NoMV,
      drug_13_indic_ICrit_GetICU_NoOx_NoMV = drug_ind_pars$drug_13_indic_ICrit_GetICU_NoOx_NoMV,
      drug_13_prop_treat = drug_ind_pars$drug_13_prop_treat,

      # Drug Effect Parameters
      drug_8_GetOx_effect_size = drug_eff_pars$rem_mod_getox_dur[x],
      drug_8_NoOx_effect_size = drug_eff_pars$rem_mod_noox_dur[x],

      drug_11_GetOx_effect_size = drug_eff_pars$rem_mod_getox_mort[x] * drug_eff_pars$dexy_mod_getox_mort[x],
      drug_11_NoOx_effect_size = drug_eff_pars$rem_mod_noox_mort[x] * drug_eff_pars$dexy_mod_noox_mort[x],

      drug_12_GetOx_effect_size = drug_eff_pars$dexy_sev_getox_mort[x],
      drug_12_NoOx_effect_size = drug_eff_pars$dexy_sev_noox_mort[x],

      drug_13_GetOx_GetMV_effect_size = drug_eff_pars$dexy_crit_getox_getmv_mort[x],
      drug_13_GetOx_NoMV_effect_size = drug_eff_pars$dexy_crit_getox_nomv_mort[x],
      drug_13_NoOx_NoMV_effect_size = drug_eff_pars$dexy_crit_noox_nomv_mort[x])

    # Extracting Relevant Outputs
    IFR <- 100 * extract_IFR(temp)

    hosp_outputs <- get_hosp_occ(temp)
    hosp_fully_treated_prop <- sum(hosp_outputs$hosp_get_full_treat)/sum(hosp_outputs$hosp_need)
    hosp_any_treated_prop <- sum(hosp_outputs$hosp_get_any_treat)/sum(hosp_outputs$hosp_need)

    ICU_outputs <- get_ICU_occ(temp)
    ICU_fully_treated_prop <- sum(ICU_outputs$ICU_get_full_treat)/sum(ICU_outputs$ICU_need)
    ICU_any_treated_prop <- sum(ICU_outputs$ICU_get_any_treat)/sum(ICU_outputs$ICU_need)

    return(c(demog_pars$R0, IFR, hosp_fully_treated_prop, hosp_any_treated_prop, ICU_fully_treated_prop, ICU_any_treated_prop))

  })

  output <- do.call(rbind, output)
  output <- as.data.frame(output)
  colnames(output) <- c("R0", "IFR", "hosp_full_treat", "hosp_any_treat", "ICU_full_treat", "ICU_any_treat")
  output$scenario <- scenario
  return(output)

}


