# IFR
calc_IFR <- function(model_output) {
  index <- apothecary::odin_index(model_output$model)
  IFR <- max(apply(model_output$output[, index$D], 1, sum))/ max(apply(model_output$output[, index$R], 1, sum) + apply(model_output$output[, index$D], 1, sum)) * 100
  return(IFR)
}

# Attack Rates
calc_AR <- function(model_output) {
  index <- apothecary::odin_index(model_output$model)
  infected <- max(apply(model_output$output[, index$D], 1, sum)) + max(apply(model_output$output[, index$R], 1, sum))
  population <- sum(model_output$output[1, index$S])
  return(infected/population)
}

# Proportion Individuals Receiving Full/Incomplete Healthcare
calc_receipt_healthare <- function(model_output, type) {
  index <- apothecary::odin_index(model_output$model)
  if (type == "hospital_full") {
    number_receive_full_treatment <- sum(model_output$output[, index$number_get_hosp_full_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_hosp])
    return(number_receive_full_treatment/number_need_treatment)
  } else if (type == "hospital_any") {
    number_receive_any_treatment <- sum(model_output$output[, index$number_get_hosp_any_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_hosp])
    return(number_receive_any_treatment/number_need_treatment)
  } else if (type == "ICU_full") {
    number_receive_full_treatment <- sum(model_output$output[, index$number_get_ICU_full_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_ICU])
    return(number_receive_full_treatment/number_need_treatment)
  } else if (type == "ICU_any") {
    number_receive_any_treatment <- sum(model_output$output[, index$number_get_ICU_any_treat])
    number_need_treatment <- sum(model_output$output[, index$number_need_ICU])
    return(number_receive_any_treatment/number_need_treatment)
  } else {
    stop("Error - input hospital_full, hospital_any, ICU_full or ICU_any")
  }
}

# Days Over Healthcare Capacity
days_over_capacity <- function(model_output, type) {
  index <- apothecary::odin_index(model_output$model)
  if (type == "hospital_full") {
    return(sum(model_output$output[, index$number_get_hosp_incomplete_treat] > 0.5 & model_output$output[, index$number_need_hosp] > 0.5))
  } else if (type == "hospital_any") {
    return(sum(model_output$output[, index$number_need_hosp_no_treat] > 0.5 & model_output$output[, index$number_need_hosp] > 0.5))
  } else if (type == "ICU_full") {
    return(sum(model_output$output[, index$number_get_ICU_incomplete_treat] > 0.5 & model_output$output[, index$number_need_ICU] > 0.5))
  } else if (type == "ICU_any") {
    return(sum(model_output$output[, index$number_need_ICU_no_treat] > 0.5 & model_output$output[, index$number_need_ICU] > 0.5))
  } else {
    stop("Error - input hospital_full, hospital_any, ICU_full or ICU_any")
  }
}

heatmap_plot <- function(matrix, deaths, reverse) {
  matrix <- matrix/deaths
  colnames(matrix) <- seq(0, 1, length.out = 21)
  if (reverse) {
    row.names(matrix) <- seq(1, 0, length.out = 21)
  } else {
    row.names(matrix) <- seq(0, 1, length.out = 21)
  }
  matrix <- reshape2::melt(matrix)
  cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#25A593",
                               "#38AB6F", "#4ab04a", "#A5C444", "#D2CE41", "#ffd73e"))(50),
            colorRampPalette(c("#ffd73e", "#eec73a", "#E8AE2E", "#e29421", "#E9742C",
                               "#f05336", "#ce472e", "#A62E18"))(50))

  plot <- ggplot() +
    geom_tile(data = matrix, aes(Var1, Var2, fill = 1-value)) +
    scale_x_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
    scale_fill_gradientn(colours=cols, limits=c(-0.01, 1),
                         breaks=seq(-0.1, 1, by=0.1),
                         guide=guide_colourbar(ticks=T, nbin=50,
                                               barheight=.5, label=T,
                                               barwidth=10)) +
    theme_cowplot() +
    labs(x = "", y = "") +
    theme(axis.line=element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(vjust = 8),
          axis.text.y = element_blank(), axis.title.y = element_text(vjust = -0.5),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    annotate("text", x = -0.03, y = seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
             hjust = 1, size = 4.5) +
    annotate("text", y = -0.05, x = 0.05 +seq(0, 1, 0.25), label= c("0.00", "0.25", "0.50", "0.75", "1.00"),
             hjust = 1, size = 4.5)
  return(plot)
}
