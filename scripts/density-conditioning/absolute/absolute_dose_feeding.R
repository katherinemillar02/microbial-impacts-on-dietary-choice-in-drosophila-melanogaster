## Chapter 4 - Appendix


#### Experiment - Density of Conditioning - Feeding

#### Assay: Absolute 


#### Packages ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
##################---


                        
                        
                        
#### 📖 Reading, cleaning, editing the data: ####


#### Low density - 90 mm ####


        ## 4:1 ##
# Reading data in with read_excel()
fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")

## Adding the appropriate data variables
fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Mutating an id variable to better read the data in 
fourone_90mm_long <- fourone_90mm_long %>%  
  mutate(id = "4-1_90")


         ## 1:4 ##
# Reading data in with read_excel()
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")


## Adding the appropriate data variables
onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Mutating an id variable to better read the data in 
onefour_90mm_long <- onefour_90mm_long  %>%  
  mutate(id = "1-4_90")









#### High density - 35 mm ####


           ## 4:1 ##
# Reading data in with read_excel()
fourone_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")

## Adding the appropriate data variables
fourone_35mm_long <- fourone_35mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Mutating an id variable to mutate a variable 
fourone_35mm_long <- fourone_35mm_long %>%  
  mutate(id = "4-1_50")







              ## 1:4 ##
# Reading data in with read_excel()
onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")

## Adding the appropriate data variables
onefour_35mm_long <- onefour_35mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


## Mutating an id variable to mutate a variable 
onefour_35mm_long <- onefour_35mm_long  %>%  
  mutate(id = "1-4_50")




## Binding the data to one data frame 
density_feeding_2choice <- rbind( fourone_35mm_long, onefour_35mm_long, fourone_90mm_long, onefour_90mm_long )


## Mutating a density variable 
density_feeding_2choice <- density_feeding_2choice %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
    ))


## Reading the data in, and separating diet into ratio and condition
density_feeding_2choice_df <- density_feeding_2choice %>%
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
drop1(glmm.bin.density.2choice, test = "Chisq") 
    ## Interaction effect found, keep model as it s 

two_choice_density_df$ratio <- as.factor(two_choice_density_df$ratio)
two_choice_density_df$ratio <- relevel(two_choice_density_df$ratio, ref = "4:1")

glmm.bin.density.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * density  + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df)


## Using the model for analysis
summary(glmm.bin.density.2choice)

tab_model(glmm.bin.density.2choice, CSS = list(css.table = '+font-family: Arial;'))


emmeans::emmeans(glmm.bin.density.2choice, specs = ~ ratio  * density, type = "response")


## Model assumption testing
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.density.2choice, plot = T)


