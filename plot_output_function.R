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
                "hosp_occ", "ICU_occ", "MV_occ", "oxygen_needed_overall", "oxygen_used")

plot_output <- function(stoch, det, variables) {
  par(mfrow = c(5, 13), mar = c(2, 2, 2, 2))
  for (i in 1:length(base_check)) {

    var <- base_check[i]
    x_indices <- x_index[[which(names(x_index) == var)]]
    y_indices <- y_index[[which(names(y_index) == var)]]

    if (nchar(var) > 10) {
      title_size <- 0.75
    } else {
      title_size <- 1.25
    }

    if (length(x_indices) == 1) {
      if (var == "oxygen_needed_overall" | var == "oxygen_used" | var == "oxygen_availability") {
        ymax <- max(c(max(x$output[, x_indices, 1]/dt), max(y$output[, y_indices])))
        plot(x$output[, x_index$time, 1], x$output[, x_indices, 1]/dt, main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
             ylim = c(0, ymax))
        lines(y$output[, y_indices], col = "red", type = "l")
      } else {
        ymax <- max(c(max(x$output[, x_indices, 1]), max(y$output[, y_indices])))
        plot(x$output[, x_index$time, 1], x$output[, x_indices, 1], main = var,
             col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
             ylim = c(0, ymax))
        lines(y$output[, y_indices], col = "red", type = "l")
      }
    } else {
      ymax <- max(c(max(apply(x$output[, x_indices, 1], 1, sum)), max(apply(y$output[, y_indices], 1, sum))))
      plot(x$output[, x_index$time, 1], apply(x$output[, x_indices, 1], 1, sum), main = var,
           col = "black", type = "l", ylab = "", xlab = "", cex.main = title_size,
           ylim = c(0, ymax))
      lines(apply(y$output[, y_indices], 1, sum), col = "red", type = "l")
    }
  }
}

