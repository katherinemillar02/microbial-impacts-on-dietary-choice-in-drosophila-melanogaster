
                        #### DENSITY CONDITIONING PART 2 (FIRST ONE WAS PILOT) ####

                                       #### RELATIVE ANALYSIS ####

                        
## Packages 
                        library(tidyverse)
                        library(lmerTest)
                        library(readxl)
                        library(MASS)
                        library(performance)
                        library(pscl)
                        library(DHARMa)
                        library(glmmTMB)
                        ##################---
                        
                        
                        
                        
                        
## Reading the data in:
                        
## 90 mm 
fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")

## Adding the appropriate data variables
fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Mutating an id variable to better read the data in 
fourone_90mm_long <- fourone_90mm_long %>%  
  mutate(id = "4-1_90")

onefour_90mm_long <- onefour_90mm_long  %>%  
  mutate(id = "1-4_90")



## 35 mm 
fourone_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")

## Adding the appropriate data variables
fourone_35mm_long <- fourone_35mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_35mm_long <- onefour_35mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


## Mutating an id variable to mutate a variable 
fourone_35mm_long <- fourone_35mm_long %>%  
  mutate(id = "4-1_50")

onefour_35mm_long <- onefour_35mm_long  %>%  
  mutate(id = "1-4_50")



## Binding the data to one data frame 
two_choice_density <- rbind( fourone_35mm_long, onefour_35mm_long, fourone_90mm_long, onefour_90mm_long )


## Mutating a density variable 
two_choice_density_1 <- two_choice_density %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
    ))


## Reading the data in, and separating diet into ratio and condition
two_choice_density_df <- two_choice_density_1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, observation, plate, ratio, condition, density) %>%
  summarise(count = sum(fly_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")





#### DATA ANALYSIS 

# Binomial GLM
glm.bin.density.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * density , family = binomial, data = two_choice_density_df)

## Assumption checks 
glm.bin.density.2choice.residuals <- residuals(glm.bin.density.2choice, type = "pearson")

# Pearson residual check
plot(glm.bin.density.2choice.residuals ~ fitted(glm.bin.density.2choice), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")




## Binomial GLMM - to test for random effects 
glmm.bin.density.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * density  + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df)

## Testing for the interaction effect 
drop1(glmm.density.2choice, test = "Chisq") 
    ## Interaction effect found, keep model as it s 

## Using the model for analysis
summary(glmm.density.2choice)






