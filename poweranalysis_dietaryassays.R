## Packages 


## Without block 
### Mixed effect models

## I 'THINK' these are the only numbers you need to change 
fixed_effect_Group1 <- 0.535
fixed_effect_Group2 <- 0.228
fixed_effect_Group3 <- 0.228

number_of_plates <- c(30, 40, 50, 30, 40, 50, 30, 40, 50)
number <- c(10,10,10,15,15,15,20,20,20) ## This is the number of of observations 


simulate_power <- function(plates, n){
  
  num_significant <- 0  
  
  sims <- 10 ## Set to run to 10 BUT for accuracy I ran it 1000x, to test it out you can run 200x but be warned both take AGES 
  for(i in 1:sims){
    
    # Simulate random effect with sd = 0.5
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.4))
    
    rand_eff2 <- data.frame(repeats = as.factor(seq(1:n)),
                            b1 = rnorm(n, mean = 0, sd = 0.3))
    
    
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
    
    
    #Treatment3 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group3 + . + rnorm(n=1, mean = 0, sd = .3))))
    
    Treatment3 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group3 + rand_eff_total$rand_eff_total))
    
    Treatment3 <- map_df(
      seq_along(Treatment3),
      ~ tibble(Count = Treatment3[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment3))
    
    Treatment3$id <- repeating_vector
    
    data <- rbind(Treatment1, Treatment2, Treatment3)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + (1|Plate) +(1|id) , data = data, family  = poisson)
    
    
    if (summary(model)$coefficients$cond[2,4] < 0.05) {
      num_significant <- num_significant + 1
    }
  }
  
  return(num_significant / sims)
  
}

simulation_results <- map2_dbl(number_of_plates, number, simulate_power)




############################################################################################################












#### This is a code if your model contains block - less confident but think it follows the same regime

## Using these values for: 
# 1:4
# At the moment
# But I need to double-check the model so these are subject to change


## Fixed effect 
fixed_effect_Group1 <- 0.8949
fixed_effect_Group2 <- 0.3812
fixed_effect_Group3 <- 0.3812



fixed_effect_Block <- 0.522
## These values are taken from the estimate when running summary() of a GLMM

## Simulating the number of different petri dishes used 
number_of_plates <- c(10,12,14,10,12,14)

## Number here represents the number of observations taken
number <- c(10,10,10,12,12,12)

number_of_blocks <- 2

simulate_power <- function(plates, n){ # I have n = 10 because there were n = 10 plates in my model
  number_of_blocks <- 2
  num_significant <- 0  # ? 
  
  sims <- 200
  for(i in 1:sims){
    
    # Simulate random effect with sd = 0.5? - 0.4 - this was the sd in my model? 
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.24)) 
    
    rand_eff2 <- data.frame(repeats = as.factor(seq(1:n)),
                            b1 = rnorm(n, mean = 0, sd = 0.32))
    
    rand_eff3 <- data.frame(blocks = as.factor(seq(1:blocks)),
                            b2 = rnorm(blocks, mean = 0, sd = 0.00003))
    
    
    rand_eff_duplicated <- rand_eff[rep(row.names(rand_eff), each = n), ]
    rownames(rand_eff_duplicated) <- NULL  # Resetting row names
    
    rand_eff_duplicated2 <- rand_eff2[rep(row.names(rand_eff2), times = plates), ]
    rownames(rand_eff_duplicated2) <- NULL  # Resetting row names
    
    
    
    rand_eff_duplicated3 <- rand_eff3[rep(row.names(rand_eff3), each = nrow(rand_eff_duplicated)/2), ]
    rownames(rand_eff_duplicated3) <- NULL  # Resetting row names
    
    rand_eff_total <- as_tibble(cbind(rand_eff_duplicated, rand_eff_duplicated2, rand_eff_duplicated3)) |> 
      mutate(rand_eff_total = b0 + b1 +b2)
    
    Treatment1 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group1 + rand_eff_total$rand_eff_total))
    
    
    ## Here, I am generating code for my new assay design "treatment" represents a diet patch within an assay 
    
    
    #Treatment1 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group1 + .)))
    Treatment1 <- map_df( 
      seq_along(Treatment1),
      ~ tibble(Count = Treatment1[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Condition") |>
      mutate(Block = rand_eff_duplicated3$blocks) |>
      mutate(Count = if_else(Block == 1, Count, as.integer(Count * fixed_effect_Block)))
    
    
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment1))
    
    Treatment1$id <- repeating_vector
    
    #Treatment2 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group2 + . + rnorm(n=1, mean = 0, sd = .3))))
    Treatment2 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group2 + rand_eff_total$rand_eff_total))
    
    Treatment2 <- map_df(
      seq_along(Treatment2),
      ~ tibble(Count = Treatment2[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Uncondition")|>
      mutate(Block = rand_eff_duplicated3$blocks) |>
      mutate(Count = if_else(Block == 1, Count, as.integer(Count * fixed_effect_Block)))
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment2))
    
    Treatment2$id <- repeating_vector
    
    
    #Treatment3 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group3 + . + rnorm(n=1, mean = 0, sd = .3))))
    
    Treatment3 <- rpois(n = nrow(rand_eff_total), lambda = exp(fixed_effect_Group3 + rand_eff_total$rand_eff_total))
    
    Treatment3 <- map_df(
      seq_along(Treatment3),
      ~ tibble(Count = Treatment3[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")|>
      mutate(Block = rand_eff_duplicated3$blocks) |>
      mutate(Count = if_else(Block == 1, Count, as.integer(Count * fixed_effect_Block)))
    
    repeating_vector <- rep(1:n, length.out = nrow(Treatment3))
    
    Treatment3$id <- repeating_vector
    
    data <- rbind(Treatment1, Treatment2, Treatment3)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + Block + (1|Block/Plate) +(1|id) , data = data, family  = poisson)
    
    if(is.na(drop1(model, test = "Chi")[2,4])){
      num_significant
    }
    else{
      if (drop1(model, test = "Chi")[2,4] < 0.05) {
        num_significant <- num_significant + 1
      }
    }
  }
  
  return(num_significant / sims)
  
}

simulation_results <- map2_dbl(number_of_plates, number, simulate_power)

