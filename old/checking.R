devtools::load_all()
library(tictoc)
source("plot_output_function.R")

dt <- 0.05
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2493,
                    input_oxygen_supply = 500, day_return = FALSE, max_leftover = 2)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, model = "deterministic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2493,
                    input_oxygen_supply = 500)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)
plot_output(x, y, base_check)

plot(x$output[, x_index$temp_leftover, 1])
plot(x$output[, x_index$leftover, 1])

# Receive Hospital Bed, Doesn't Get Oxygen - OUT BY A SUBSTANTIAL AMOUNT
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetHosp_NoOx, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetHosp_NoOx], 1, sum), type = "l")
sum(x$output[, x_index$number_GetHosp_NoOx, 1])
sum(y$output[, y_index$number_GetHosp_NoOx])

# Receives ICU Bed, Doesn't Get Oxygen - OUT BY A SUBSTANTIAL AMOUNT
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetICU_NoOx, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetICU_NoOx], 1, sum), type = "l")
sum(x$output[, x_index$number_GetICU_NoOx, 1])
sum(y$output[, y_index$number_GetICU_NoOx])

# Checking Same Numbers of People Get Beds - Appears Fine
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$n_ICase2_Hosp, 1], 1, sum)/x$raw_parameters$dt, type = "l")
lines(apply(y$output[, y_index$n_ICase2_Hosp], 1, sum), type = "l", col = "red")
sum(x$output[, x_index$n_ICase2_Hosp, 1])
sum(y$output[, y_index$n_ICase2_Hosp])

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetHosp, 1], 1, sum)/x$raw_parameters$dt, type = "l")
lines(apply(y$output[, y_index$number_GetHosp], 1, sum), type = "l", col = "red")
sum(x$output[, x_index$number_GetHosp, 1])
sum(y$output[, y_index$number_GetHosp])

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetICU, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetICU], 1, sum), type = "l", col = "red")
sum(x$output[, x_index$number_GetICU, 1])
sum(y$output[, y_index$number_GetICU])

# Receive Hospital Bed, Gets Oxygen - Appears Fine
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetHosp_Ox, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetHosp_Ox], 1, sum), type = "l")
sum(x$output[, x_index$number_GetHosp_Ox, 1])
sum(y$output[, y_index$number_GetHosp_Ox])

# Receives ICU Bed, Gets Oxygen - Appears Fine
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetICU_GetOx, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetICU_GetOx], 1, sum), type = "l")
sum(x$output[, x_index$number_GetICU_GetOx, 1])
sum(y$output[, y_index$number_GetICU_GetOx])

# Receives ICU Bed, Gets Oxygen and Gets MV - Appears Fine
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetICU_GetOx_GetMV, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetICU_GetOx_GetMV], 1, sum), type = "l")
sum(x$output[, x_index$number_GetICU_GetOx_GetMV, 1])
sum(y$output[, y_index$number_GetICU_GetOx_GetMV])

# People Not Getting Hospital/ICU Beds = Identical and Looks Fine
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_NotHosp, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_NotHosp], 1, sum), type = "l")
sum(x$output[, x_index$number_NotHosp, 1])
sum(y$output[, y_index$number_NotHosp])

plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_NotICU, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_NotICU], 1, sum), type = "l")
sum(x$output[, x_index$number_NotICU, 1])
sum(y$output[, y_index$number_NotICU])




















y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, dt = 0.1,
                    model = "stochastic", hosp_bed_capacity = 10000, ICU_bed_capacity = 300,
                    input_oxygen_supply = 1000)

y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
                    model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 300,
                    input_oxygen_supply = 1000)

tic()
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
                    model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 10,
                    input_oxygen_supply = 10)
toc()

# when input_oxygen_supply = 5, takes 4 seconds to run
# when input_oxygen_supply = 10, takes 21 seconds to run
# when input_oxygen_supply = 15, takes 4 seconds to run
# when input_oxygen_supply = 20, takes 1 second to run
tic()
for (i in 1:10) {
  y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
                      model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 0,
                      input_oxygen_supply = 15)
}
toc()


# THIS BREAKS IT AND I DON'T KNOW WHY!!!!
# y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
#                     model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
#                     input_oxygen_supply = 1375)
# RUNS WHEN INPUT_OXYGEN_SUPPLY IS SET TO 1376




# THIS BREAKS IT AND I DON'T KNOW WHY!!!!
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
                    model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 2494,
                    input_oxygen_supply = 1375)
# RUNS WHEN ICU_BED_CAPACITY IS SET TO 2493
dt <- 0.01
x <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, dt = dt, model = "stochastic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2493,
                     input_oxygen_supply = 1375, day_return = FALSE, max_leftover = 15)
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, model = "deterministic",
                    hosp_bed_capacity = 10000, ICU_bed_capacity = 2493,
                    input_oxygen_supply = 1375)
x_index <- squire:::odin_index(x$model)
y_index <- squire:::odin_index(y$model)

# Receive Hospital Bed, Doesn't Get Oxygen - OUT BY A SUBSTANTIAL AMOUNT
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetHosp_NoOx, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetHosp_NoOx], 1, sum), type = "l")
sum(x$output[, x_index$number_GetHosp_NoOx, 1])
sum(y$output[, y_index$number_GetHosp_NoOx])

# Receives ICU Bed, Doesn't Get Oxygen - OUT BY A SUBSTANTIAL AMOUNT
plot(x$output[, x_index$time, 1], apply(x$output[, x_index$number_GetICU_NoOx, 1], 1, sum)/x$raw_parameters$dt, type = "l", col = "red")
lines(apply(y$output[, y_index$number_GetICU_NoOx], 1, sum), type = "l")
sum(x$output[, x_index$number_GetICU_NoOx, 1])
sum(y$output[, y_index$number_GetICU_NoOx])






# THIS RUNS FINE AND THE ONLY DIFFERENCE IS THE ICU BED CAPACITY IS ALOT LOWER!!!!!
y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0,
                    model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 10000 * 3/100,
                    input_oxygen_supply = 1375)

y_index <- apothecary:::odin_index(y$model)

par(mfrow = c(5, 13), mar = c(2, 2, 2, 2))
for (i in 1:length(base_check)) {

  var <- base_check[i]
  y_indices <- y_index[[which(names(y_index) == var)]]

  if (nchar(var) > 10) {
    title_size <- 0.75
  } else {
    title_size <- 1.25
  }

  if (length(y_indices) == 1) {
    if (var == "oxygen_needed_overall" | var == "oxygen_used" | var == "oxygen_availability") {
      plot(y$output[, y_indices], col = "red", type = "l", main = var, ylab = "", xlab = "", cex.main = title_size)
    } else {
      plot(y$output[, y_indices], col = "red", type = "l", main = var,  ylab = "", xlab = "", cex.main = title_size)
    }
  } else {
    plot(apply(y$output[, y_indices], 1, sum), col = "red", type = "l", main = var, ylab = "", xlab = "", cex.main = title_size)
  }
}


plot(apply(y$output[, index$S], 1, sum))
plot(y$output[, y_index$oxygen_needed_overall])
plot(y$output[, y_index$oxygen_supply])
plot(y$output[, y_index$oxygen_used])
max(y$output[, y_index$oxygen_used])




y <- run_apothecary(country = "France", R0 = 1.5, tt_R0 = 0, day_return = TRUE, seeding_cases = 250, dt = 1,
                    model = "deterministic", hosp_bed_capacity = 10000, ICU_bed_capacity = 2500,
                    oxygen_availability_0 = 0, input_oxygen_supply = 3000, input_baseline_oxygen_demand = 0,
                    MV_capacity = 10000000)

index <- apothecary:::odin_index(y$model)
plot(apply(y$output[, index$S], 1, sum))
plot(y$output[, index$oxygen_needed_overall])
plot(y$output[, index$oxygen_supply])
plot(y$output[, index$oxygen_used])
