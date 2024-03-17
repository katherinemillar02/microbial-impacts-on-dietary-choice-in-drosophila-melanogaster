


######################## Statistical Power Analysis ######################## 

set.seed(1) # Does this reset everything? 

## Using the 4:1 diets
N <- 15 ## Need to put the appropriate sample size here 
## I had 15 plate replicates 



## Just the 4:1 dataset 
View(df2_male_filtered)
## Working out the trt effect 

## getting the mean effect 
conditioned_mean <- mean(df2_male_filtered$Conditioned)
unconditioned_mean <- mean(df2_male_filtered$Unconditioned)


trt_mean <- conditioned_mean - unconditioned_mean



trt.effect <- 2.163636 # difference between control and treatment means - HOW DO I FIND THIS? 
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
mean.trt <- mean.con + trt.effect # mean of treatment group

control <- rnorm(N, mean.con, sigma) # 20 data points for the control group taken from a normal distribution with known sample size, mean and s.d.

treatment <- rnorm(N, mean.trt, sigma) # data for the treatment group

t.test(control, treatment)
boxplot(cbind(control, treatment))

library(pwr)

pwr.t.test(n = 15, d = trt.effect / sigma, sig.level = 0.05, power = NULL)

pwr.t.test(n = NULL, d = trt.effect / sigma, sig.level = 0.05, power = 0.8)

nvals <- seq(2, 200, length.out = 200)
powvals <- sapply(nvals, function(x) pwr.2p.test(h = trt.effect / sigma, n = x, sig.level = 0.05)$power)

plot(nvals, powvals,
     xlab = "sample size", ylab = "power",
     main = "Power curve for sample size for difference in proportions",
     lwd = 2, col = "red", type = "l"
)

abline(h = 0.8)
