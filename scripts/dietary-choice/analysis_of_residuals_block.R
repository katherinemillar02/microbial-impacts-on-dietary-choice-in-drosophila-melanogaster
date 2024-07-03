#### Trying to do analysis of residuals 

glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_v)

residuals <- residuals(glm.nb_v_comb_egg, type = "pearson")


plot(residuals ~ fitted(glm.nb_v_comb_egg), by = combined_ovi_v$block, 
     main = "Residuals vs. Fitted by Block",
     xlab = "Fitted values", ylab = "Residuals")


par(mfrow = c(2, 2))  # Set up a 2x2 grid for multiple plots
for (b in unique(combined_ovi_v$block)) {
  subset_data <- combined_ovi_v[combined_ovi_v$block == b, ]
  residuals <- residuals(glm.nb_v_comb_egg, subset = which(combined_ovi_v$block == b))
  plot(residuals, main = paste("Residuals for Block", b))
}
