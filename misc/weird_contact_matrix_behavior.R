matrix(1, 3, 3) %*% diag(1:3)

squire::get_mixing_matrix("United Kingdom")[c(1, 2), c(1, 2)]
squire::get_population("United Kingdom")$n[1:2]


squire::get_population("United Kingdom")$n[1:2] %*% squire::get_mixing_matrix("United Kingdom")[c(1, 2), c(1, 2)]

3924490 * 0.9473684
4119566 * 0.6470588

contact_matrix <- squire::get_mixing_matrix("Uganda")
plot(rowSums(contact_matrix), type = "l", ylim = c(0, 20))
population <- squire::get_population("United Kingdom")$n
proc <- squire:::process_contact_matrix_scaled_age(contact_matrix, population)
lines(rowSums(proc), type = "l", ylim = c(0, 20), col = "red")

contact_matrix <- squire::get_mixing_matrix("Uganda")
plot(rowSums(contact_matrix), type = "l", ylim = c(0, 20))
population <- squire::get_population("Uganda")$n
proc <- squire:::process_contact_matrix_scaled_age(contact_matrix, population)
lines(rowSums(proc), type = "l", ylim = c(0, 20), col = "red")

contact_matrix <- squire::get_mixing_matrix("United Kingdom")
plot(rowSums(contact_matrix), type = "l", ylim = c(0, 20))
population <- squire::get_population("Uganda")$n
proc <- squire:::process_contact_matrix_scaled_age(contact_matrix, population)
lines(rowSums(proc), type = "l", ylim = c(0, 20), col = "red")

contact_matrix <- squire::get_mixing_matrix("United Kingdom")
plot(rowSums(contact_matrix), type = "l", ylim = c(0, 20))
population <- squire::get_population("United Kingdom")$n
proc <- squire:::process_contact_matrix_scaled_age(contact_matrix, population)
lines(rowSums(proc), type = "l", ylim = c(0, 20), col = "red")



population <- squire::get_population("United Kingdom")$n[1:2]
x <- 1
mat[x,] * pop[x]

contact_matrix <- squire::get_mixing_matrix("United Kingdom")
population <- squire::get_population("United Kingdom")$n

contact_matrix <- rbind(contact_matrix, contact_matrix[16,])
contact_matrix <- cbind(contact_matrix, contact_matrix[,16]*population[17] / sum(population[16:17]))
contact_matrix[,16] <- contact_matrix[,16]*population[16] / sum(population[16:17])


MIJ <- t(vapply(seq(population),function(x){
  contact_matrix[x,] * population[x]
}, FUN.VALUE = numeric(length(population))))

adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced


# Convert to New Per-Capita Rates By Dividing By Population
# Resulting Matrix Is Asymmetric But Balanced
# Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
# Is Balanced (so when we divide by pop at end, will be balanced)
processed_matrix <- t(vapply(seq(population), function(x) {
  adjust_mat[x, ] / population[x]
}, FUN.VALUE = numeric(length(population))))

MIJ <- t(vapply(seq(population),function(x){
  processed_matrix[x,] * population[x]
}, FUN.VALUE = numeric(length(population))))

