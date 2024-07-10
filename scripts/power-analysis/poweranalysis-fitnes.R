
############################################################################ 

### Mixed effect models

# fixed_effect_Group1 <- 0.535
# fixed_effect_Group2 <- 0.228

fixed_effect_Group1 <- 5.0417563
fixed_effect_Group2 <-    3.0596230

number_of_plates <- c(10, 15, 30, 10, 15, 30,10, 15, 30)
number <- c(60,60,60,80,80,80,100,100,100)


simulate_power <- function(plates, n){
  
  num_significant <- 0  
  
  sims <- 200
  for(i in 1:sims){
    
    # Simulate random effect with sd = 0.5
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.818))
    
    rand_eff2 <- data.frame(repeats = as.factor(seq(1:n)),
                            b1 = rnorm(n, mean = 0, sd =  1.997 ))
    
    
    rand_eff_duplicated <- rand_eff[rep(row.names(rand_eff), each = n), ]
    rownames(rand_eff_duplicated) <- NULL  # Resetting row names
    
    rand_eff_duplicated2 <- rand_eff2[rep(row.names(rand_eff2), times = plates), ]
    rownames(rand_eff_duplicated2) <- NULL  # Resetting row names
    
    rand_eff_total <- as_tibble(cbind(rand_eff_duplicated, rand_eff_duplicated2)) |> 
      mutate(rand_eff_total = b0 + b1 )
    
    Treatment1 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group1 + rand_eff_total$rand_eff_total))
    
    #Treatment1 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group1 + .)))
    Treatment1 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment1[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Condition")
    
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment1))
    
    Treatment1$id <- repeating_vector
    
    #Treatment2 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group2 + . + rnorm(n=1, mean = 0, sd = .3))))
    Treatment2 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group2 + rand_eff_total$rand_eff_total))
    
    Treatment2 <- map_df(
      seq_along(Treatment2),
      ~ tibble(Count = Treatment2[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Uncondition")
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment2))
    
    Treatment2$id <- repeating_vector
    
    
    
    data <- rbind(Treatment1, Treatment2)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + (1|Plate) +(1|id) , data = data, family  = poisson)
    
    
    if (summary(model)$coefficients$cond[2,4] < 0.05) {
      num_significant <- num_significant + 1
    }
  }
  
  return(num_significant / sims)
  
}

simulation_results <- map2_dbl(number_of_plates, number, simulate_power)


## RAN IT 200 TIMES 

#  1 1 1 1 1 1 1 1 1

## Can't be right?? 