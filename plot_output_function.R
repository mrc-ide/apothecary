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
                "overall_ICU_occ", "ICU_bed_ox_occ", "ICU_bed_no_ox_occ", "MV_occ", "current_free_MV",
                "overall_hosp_occ", "hosp_bed_ox_occ", "hosp_bed_no_ox_occ", "hosp_bed_no_ox_occ_without_IRec")


plot_output <- function(stoch, det, variables) {
  par(mfrow = c(5, 13), mar = c(2, 2, 2, 2))
  for (i in 1:length(base_check)) {

    var <- base_check[i]
    stoch_index <- apothecary:::odin_index(stoch$model)
    stoch_indices <- stoch_index[[which(names(stoch_index) == var)]]
    det_index <- apothecary:::odin_index(det$model)
    det_indices <- det_index[[which(names(det_index) == var)]]

    if (nchar(var) > 10) {
      title_size <- 0.75
    } else {
      title_size <- 1.25
    }

    if (length(stoch_indices) == 1) {
      if (var == "current_free_hosp_bed_no_ox") {
        plot(stoch$output[, stoch_index$time, 1], stoch$output[, stoch_indices, 1], main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size)
        lines(det$output[, det_indices], col = "red", type = "l")
      } else {
        ymax <- max(c(max(stoch$output[, stoch_indices, 1]), max(det$output[, det_indices])))
        plot(stoch$output[, stoch_index$time, 1], stoch$output[, stoch_indices, 1], main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
             ylim = c(0, ymax))
        lines(det$output[, det_indices], col = "red", type = "l")
      }
    } else {
      ymax <- max(c(max(apply(stoch$output[, stoch_indices, 1], 1, sum)), max(apply(det$output[, det_indices], 1, sum))))
      plot(stoch$output[, stoch_index$time, 1], apply(stoch$output[, stoch_indices, 1], 1, sum), main = var,
           col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
           ylim = c(0, ymax))
      lines(apply(det$output[, det_indices], 1, sum), col = "red", type = "l")
    }
  }
}

