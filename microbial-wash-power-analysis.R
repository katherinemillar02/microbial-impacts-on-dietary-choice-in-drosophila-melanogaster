## Packages 
library(pwr)
library(tidyverse)
##


                         ######################## Statistical Power Analysis ######################## 

#### Environmental Computing - https://environmentalcomputing.net/statistics/power-analysis/

#### Doing it for the 4:1 Feeding Assay

set.seed(1) # This ensures everything is reproducible

## Using the 4:1 diets
N <- 15
## I had 15 plate replicates in the 4:1 Male Conditioned Assays

## READING THE DATA IN
## Just the 4:1 dataset 

## doing a pattern so it excludes 1:4 
pattern <- "1-4"
## Uaing grepl() to exclude the 1-4 rows 
exclude_rows <- grepl(pattern, df2_male$id)

## Making a new dataframe with 4:1 excluded 
df2_male_filtered <- df2_male[!exclude_rows, ]

## Viewing just the dataframe for 4:1 
View(df2_male_filtered)




## Working out the trt effect (treatment effect)

## Calculating the mean effect
conditioned_mean_fourone <- mean(df2_male_filtered$Conditioned) # 2.8
unconditioned_mean_fourone <- mean(df2_male_filtered$Unconditioned) # 0.636

## Calulcating the treatment effect mean
trt_mean_fourone <- conditioned_mean_fourone - unconditioned_mean_fourone #2.163636

trt.effect_fourone <- 2.163636 # difference between control and treatment means 
## Is this the mean of 4:1 Conditioned - the mean of 4:1 Unconditioned? 




### Working out the standard deviation

# Concatenate the values from both "Conditioned" and "Unconditioned" columns
combined_values_fourone <- c(df2_male_filtered$Conditioned, df2_male_filtered$Unconditioned)

# Calculate the standard deviation of the combined values
combined_sd_fourone <- sd(combined_values)

### the sd together?? 
sigma_fourone <- 2.027764 # standard deviation of control and treatment groups? 

# The mean of 4:1 Unconditioned
mean.con_fourone <-  0.6363636 # mean of control group (mean of 4:1 Unconditioned)

## This is the mean  
mean.trt_fourone <- mean.con_fourone + trt.effect_fourone # mean of treatment group - adding the effect back 

## Simulating control (unconditioned) data points
control_fourone <- rnorm(N, mean.con_fourone, sigma_fourone) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

## Simulated trt data points
treatment_fourone <- rnorm(N, mean.trt_fourone, sigma_fourone) # data for the treatment group

## Basic t-test of Conditioned vs. Unconditioned
t.test(control_fourone, treatment_fourone)

## Visualising Conditioned vs. Unconditioned
boxplot(cbind(control_fourone, treatment_fourone))





## DOING POWER TESTS -- 

## With the n of replicates used before
pwr.t.test(n = 15, d = trt.effect_fourone / sigma_fourone, sig.level = 0.05, power = NULL)
# using n = 15 gives a power of 0.8 


## Using the power to work out what replicates are needed
pwr.t.test(n = NULL, d = trt.effect_fourone / sigma_fourone, sig.level = 0.05, power = 0.8)
  ## says I only need an n of 14.8 (15)




##### Plotting power analysis for various sample sizes
nvals <- seq(1, 30, length.out = 30)
powvals <- sapply(nvals, function(x) pwr.2p.test(h = trt.effect_fourone / sigma_fourone, n = x, sig.level = 0.05)$power)


## Shows sample size against power!! would need a sample size of like 20?? 
plot(nvals, powvals,
     xlab = "sample size", ylab = "power",
     main = "Power curve for sample size for difference in proportions",
     lwd = 2, col = "red", type = "l"
)

abline(h = 0.8)

## Is this saying a sample size of 15 is enough?? 
## Seems coincidental











#### Doing it for the 1:4 Feeding Assay

set.seed(1) # This ensures everything is reproducible

## Using the 1:4 diets
N <- 15
## I had 15 plate replicates in the 1:4 Male Conditioned Assays

## READING THE DATA IN
## Just the 1:4 dataset 

## Assigning the pattern
pattern2 <- "4-1"
## Uaing grepl() to exclude the 4-1 rows 
exclude_rows <- grepl(pattern2, df2_male$id)

## Making a new dataframe with 4:1 excluded 
df2_male_filtered_2 <- df2_male[!exclude_rows, ]

## Viewing just the dataframe for 4:1 
View(df2_male_filtered_2)





## Working out the trt effect (treatment effect)

## Calculating the mean effect
conditioned_mean_onefour <- mean(df2_male_filtered_2$Conditioned) # 0.7987805
unconditioned_mean_onefour <- mean(df2_male_filtered_2$Unconditioned) # 0.5914634

## Calulcating the treatment effect mean
trt_mean_onefour <- conditioned_mean_onefour - unconditioned_mean_onefour # 0.2073171

trt.effect_onefour <- 0.2073171 # difference between control and treatment means 





### Working out the standard deviation

# Concatenate the values from both "Conditioned" and "Unconditioned" columns
combined_values_onefour <- c(df2_male_filtered_2$Conditioned, df2_male_filtered_2$Unconditioned)

# Calculate the standard deviation of the combined values
combined_sd_onefour <- sd(combined_values)

### the sd together?? 
sigma_onefour <- 0.9179025 # standard deviation of control and treatment groups? 

# The mean of 4:1 Unconditioned
mean.con_onefour <-  0.5914634 # mean of control group (mean of 4:1 Unconditioned)

## This is the mean  
mean.trt_onefour <- mean.con_onefour + trt.effect_onefour # mean of treatment group - adding the effect back 

## Simulating control (unconditioned) data points
control_onefour <- rnorm(N, mean.con_onefour, sigma_onefour) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

## Simulated trt data points
treatment_onefour <- rnorm(N, mean.trt_onefour, sigma_onefour) # data for the treatment group

## Basic t-test of Conditioned vs. Unconditioned
t.test(control_onefour, treatment_onefour)

## Visualising Conditioned vs. Unconditioned
boxplot(cbind(control_onefour, treatment_onefour))





## DOING POWER TESTS -- 

## With the n of replicates used before
pwr.t.test(n = 15, d = trt.effect_onefour / sigma_onefour, sig.level = 0.05, power = NULL)

## Using the power to work out what replicates are needed
pwr.t.test(n = NULL, d = trt.effect_onefour / sigma, sig.level = 0.05, power = 0.8)
    #### says I need an n of 308?? 




##### Plotting power analysis for various sample sizes
nvals <- seq(1, 350, length.out = 350)
powvals <- sapply(nvals, function(x) pwr.2p.test(h = trt.effect_onefour / sigma_onefour, n = x, sig.level = 0.05)$power)


## Shows sample size against power!! would need a sample size of like 20?? 
plot(nvals, powvals,
     xlab = "sample size", ylab = "power",
     main = "Power curve for sample size for difference in proportions",
     lwd = 2, col = "red", type = "l"
)

abline(h = 0.8)

## This one says I need an n = of 308... 
## I have set the plot for 350 




#################################################### --

## Doing both 4:1 and 1:4 at the same time 
 ## even though I don't get why we would do this 

set.seed(1)

N <- 15 # the sample size

## working out the treatment effect

# the mean of unconditioned
unconditioned_mean_both <- mean(df2_male$Unconditioned) # 0.6139818
conditioned_mean_both <- mean(df2_male$Conditioned) # 1.802432

trt_both <- conditioned_mean_both - unconditioned_mean_both # 1.18845



trt.effect_both <- 1.18845

# Standard deviation of control and treatment groups will be sigma
# Adding conditioned and unconditioned together then adding the standard deviation? 
combining_values_both <- df2_male$Conditioned + df2_male$Unconditioned 
sd_both <- sd(combined_values)


sigma_both <- 0.9179025 

mean.con_both <- 0.6139818 # mean of control group

mean.trt_both <- mean.con_both + trt.effect_both # mean of treatment group

control_both <- rnorm(N, mean.con_both, sigma_both) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

treatment_both <- rnorm(N, mean.trt_both, sigma_both) # data for the treatment group

t.test(control_both, treatment_both)

boxplot(cbind(control_both, treatment_both))


pwr.t.test(n = 15, d = trt.effect / sigma, sig.level = 0.05, power = NULL)
   ## an n of 15 only gives a power of 0.9 :) 


pwr.t.test(n = NULL, d = trt.effect / sigma, sig.level = 0.05, power = 0.8)
   ## says I only need an n of 10 when I do a power test of both? 


## A bit confused at these results...  






########################################################################### --


##### Trying LADA - power analysis - https://ladal.edu.au/pwr.html

## Basic power analysis
# One-way ANOVA

pwr.anova.test(k = 2, ## there is conditioned vs unconditioned compared?? so 2? 
               f =  2.163636, ## for this I used the trt effect generated before, is this correct?
               sig.level = 0.05,
               power = 0.8) 

## Don't know how to calculate effect size ?

## n = 2.29 in each group, seems rather low? 

pwr.anova.test(k=2,
               f=2.163636,
               sig.level = 0.05,
               n = 15)
## says power level is one if 15 replicates per patch
## doing power analysis for plates not flies per plate right? 
## these results don't seem too good 




##################################### --

### Mixed effect models ####

fixed_effect_Group1 <- 0.535
fixed_effect_Group2 <- 0.228
fixed_effect_Group3 <- 0.228

number_of_plates <- c(30, 40, 50, 30, 40, 50, 30, 40, 50)
number <- c(20,20,20,30,30,30,40,40,40)




simulate_power <- function(plates, n){
  
  num_significant <- 0  
  for(i in 1:200){
    
    # Simulate random effect with sd = 0.5
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.3))
    
    
    Treatment1 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group1 + .)))
    
    Treatment1 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment1[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Condition")
    
    Treatment2 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group2 + .)))
    
    Treatment2 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment2[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Uncondition")
    
    
    Treatment3 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group3 + .)))
    
    Treatment3 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment3[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")
    
    data <- rbind(Treatment1, Treatment2, Treatment3)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + (1|Plate), data = data, family  = poisson)
    
    
    if (summary(model)$coefficients$cond[2,4] < 0.05) {
      num_significant <- num_significant + 1
    }
  }
  
  return(num_significant / 200)
  
}

simulation_results <- map2_dbl(number_of_plates, number, simulate_power)




#### The results from these: 
number_of_platez <- c(30, 40, 50, 30, 40, 50, 30, 40, 50)
numberz <- c(20, 20, 20, 30, 30, 30, 40, 40, 40)
simulated_powerz <- c(0.520, 0.590, 0.615, 0.745, 0.755, 0.775, 0.850, 0.835, 0.865)


results_table <- tibble(
  number_of_platez = number_of_platez,
  numberz = numberz,
  simulated_powerz = simulated_powerz
)















######################################################################################## ---


#### 4:1 Results


## for this I did -0.6259 + 0.9808  then exp(0.3549) is this okay

fixed_effect_Group1 <-  1.426038
fixed_effect_Group2 <- 0.8318526
fixed_effect_Group3 <- 0.8318526

number_of_plates <- c(10, 20, 30, 10, 20, 30, 10, 20, 30)
number <- c(10,10,10,15,15,15,20,20,20)


simulate_power <- function(plates, n){
  
  num_significant <- 0  
  for(i in 1:200){
    
    # Simulate random effect with sd = 0.5
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.3))
    
    
    Treatment1 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group1 + .)))
    
    Treatment1 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment1[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Condition")
    
    Treatment2 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group2 + .)))
    
    Treatment2 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment2[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Uncondition")
    
    
    Treatment3 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group3 + .)))
    
    Treatment3 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment3[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")
    
    data <- rbind(Treatment1, Treatment2, Treatment3)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + (1|Plate), data = data, family  = poisson)
    
    
    if (summary(model)$coefficients$cond[2,4] < 0.05) {
      num_significant <- num_significant + 1
    }
  }
  
  return(num_significant / 200)
  
}

simulation_results_2 <- map2_dbl(number_of_plates, number, simulate_power)



####

fixed_effect_Group1 <- 0.535
fixed_effect_Group2 <- 0.228
fixed_effect_Group3 <- 0.228
fixed_effect_Group4 <-  1.426038
fixed_effect_Group5 <- 0.8318526
fixed_effect_Group6 <- 0.8318526

number_of_plates <- c(10, 20, 30, 10, 20, 30, 10, 20, 30)
number <- c(10,10,10,15,15,15,20,20,20)




simulate_power <- function(plates, n){
  
  num_significant <- 0  
  for(i in 1:200){
    
    # Simulate random effect with sd = 0.5
    
    rand_eff <- data.frame(group = as.factor(seq(1:plates)),
                           b0 = rnorm(plates, mean = 0, sd = 0.3))
    
    
    Treatment1 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group1 + .)))
    
    Treatment1 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment1[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Condition")
    
    Treatment2 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group2 + .)))
    
    Treatment2 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment2[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Uncondition")
    
    
    Treatment3 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group4 + .)))
    
    Treatment3 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment3[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")
    
    Treatment4 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group4 + .)))
    
    Treatment4 <- map_df(
      seq_along(Treatment4),
      ~ tibble(Count = Treatment4[[.]], Plate = .)
    ) |> mutate(Treatment = "4:1 Condition")
    
    Treatment5 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group5 + .)))
    
    Treatment5 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment5[[.]], Plate = .)
    ) |> mutate(Treatment = "4:1 Uncondition")
    
    
    Treatment6 <- map(rand_eff$b0, ~ rpois(n = n, lambda = exp(fixed_effect_Group6 + .)))
    
    Treatment6 <- map_df(
      seq_along(Treatment1),
      ~ tibble(Count = Treatment6[[.]], Plate = .)
    ) |> mutate(Treatment = "1:4 Microbe Filtered")
    
    
    
    
    data <- rbind(Treatment1, Treatment2, Treatment3, Treatment4, Treatment5, Treatment6)
    
    
    model <- glmmTMB::glmmTMB(Count ~ Treatment + (1|Plate), data = data, family  = poisson)
    
    
    if (summary(model)$coefficients$cond[2,4] < 0.05) {
      num_significant <- num_significant + 1
    }
  }
  
  return(num_significant / 200)
  
}

simulation_results_3 <- map2_dbl(number_of_plates, number, simulate_power)



number_of <- c(10, 20, 30, 10, 20, 30, 10, 20, 30)
numbe <- c(10,10,10,15,15,15,20,20,20)
simulated_powe <- c(1,1,1,1,1,1,1,1,1)

# Create a table
results_table <- tibble(
  number_of_plates = number_of,
  number = numbe,
  simulated_power = simulated_powe
)
