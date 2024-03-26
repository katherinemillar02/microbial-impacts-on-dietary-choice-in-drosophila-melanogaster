## Packages 
library(pwr)
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
conditioned_mean <- mean(df2_male_filtered$Conditioned) # 2.8
unconditioned_mean <- mean(df2_male_filtered$Unconditioned) # 0.636

## Calulcating the treatment effect mean
trt_mean <- conditioned_mean - unconditioned_mean #2.163636

trt.effect <- 2.163636 # difference between control and treatment means 
## Is this the mean of 4:1 Conditioned - the mean of 4:1 Unconditioned? 




### Working out the standard deviation

# Concatenate the values from both "Conditioned" and "Unconditioned" columns
combined_values <- c(df2_male_filtered$Conditioned, df2_male_filtered$Unconditioned)

# Calculate the standard deviation of the combined values
combined_sd <- sd(combined_values)

### the sd together?? 
sigma <- 2.027764 # standard deviation of control and treatment groups? 

# The mean of 4:1 Unconditioned
mean.con <-  0.6363636 # mean of control group (mean of 4:1 Unconditioned)

## This is the mean  
mean.trt <- mean.con + trt.effect # mean of treatment group - adding the effect back 

## Simulating control (unconditioned) data points
control <- rnorm(N, mean.con, sigma) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

## Simulated trt data points
treatment <- rnorm(N, mean.trt, sigma) # data for the treatment group

## Basic t-test of Conditioned vs. Unconditioned
t.test(control, treatment)

## Visualising Conditioned vs. Unconditioned
boxplot(cbind(control, treatment))





## DOING POWER TESTS -- 

## With the n of replicates used before
pwr.t.test(n = 15, d = trt.effect / sigma, sig.level = 0.05, power = NULL)

## Using the power to work out what replicates are needed
pwr.t.test(n = NULL, d = trt.effect / sigma, sig.level = 0.05, power = 0.8)





##### Plotting power analysis for various sample sizes
nvals <- seq(1, 30, length.out = 30)
powvals <- sapply(nvals, function(x) pwr.2p.test(h = trt.effect / sigma, n = x, sig.level = 0.05)$power)


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
conditioned_mean <- mean(df2_male_filtered_2$Conditioned) # 0.7987805
unconditioned_mean <- mean(df2_male_filtered_2$Unconditioned) # 0.5914634

## Calulcating the treatment effect mean
trt_mean <- conditioned_mean - unconditioned_mean # 0.2073171

trt.effect <- 0.2073171 # difference between control and treatment means 





### Working out the standard deviation

# Concatenate the values from both "Conditioned" and "Unconditioned" columns
combined_values <- c(df2_male_filtered_2$Conditioned, df2_male_filtered_2$Unconditioned)

# Calculate the standard deviation of the combined values
combined_sd <- sd(combined_values)

### the sd together?? 
sigma <- 0.9179025 # standard deviation of control and treatment groups? 

# The mean of 4:1 Unconditioned
mean.con <-  0.5914634 # mean of control group (mean of 4:1 Unconditioned)

## This is the mean  
mean.trt <- mean.con + trt.effect # mean of treatment group - adding the effect back 

## Simulating control (unconditioned) data points
control <- rnorm(N, mean.con, sigma) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

## Simulated trt data points
treatment <- rnorm(N, mean.trt, sigma) # data for the treatment group

## Basic t-test of Conditioned vs. Unconditioned
t.test(control, treatment)

## Visualising Conditioned vs. Unconditioned
boxplot(cbind(control, treatment))





## DOING POWER TESTS -- 

## With the n of replicates used before
pwr.t.test(n = 15, d = trt.effect / sigma, sig.level = 0.05, power = NULL)

## Using the power to work out what replicates are needed
pwr.t.test(n = NULL, d = trt.effect / sigma, sig.level = 0.05, power = 0.8)
    #### says I need an n of 308?? 




##### Plotting power analysis for various sample sizes
nvals <- seq(1, 30, length.out = 30)
powvals <- sapply(nvals, function(x) pwr.2p.test(h = trt.effect / sigma, n = x, sig.level = 0.05)$power)


## Shows sample size against power!! would need a sample size of like 20?? 
plot(nvals, powvals,
     xlab = "sample size", ylab = "power",
     main = "Power curve for sample size for difference in proportions",
     lwd = 2, col = "red", type = "l"
)

abline(h = 0.8)


## This one says I need an n = of 308... 


















########################################################################################################################



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



