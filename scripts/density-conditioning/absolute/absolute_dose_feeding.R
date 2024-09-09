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


# Changing the intercept to 4:1 
density_feeding_2choice_df$ratio <- as.factor(density_feeding_2choice_df$ratio)
density_feeding_2choice_df$ratio <- relevel(density_feeding_2choice_df$ratio, ref = "4:1")



#### Data Analysis 📊 #### 


#### Working through models and checking ####



# Model 1
 # Binomial GLM #
glm.bin.feeding.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * density , family = binomial, data = density_feeding_2choice_df)



## Assumption checks 

## Base R 

glm.bin.feeding.2choice.residuals <- residuals(glm.bin.feeding.2choice, type = "pearson")

# Pearson residual check
plot(glm.bin.feeding.2choice.residuals  ~ fitted(glm.bin.feeding.2choice), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")





# Model 2
 # Binomial GLMM #
glmm.bin.feeding.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * density  
                                  + (1|plate) + (1|observation) , 
                                  family = binomial, data = density_feeding_2choice_df)


## Assumption checks 

# DHARma
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.feeding.2choice, plot = T)


## Good model, using it for now 

glmm.bin.feeding.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * density  
                                  + (1|plate) + (1|observation) , 
                                  family = binomial, data = density_feeding_2choice_df)



## Testing for the interaction effect 
drop1(glmm.bin.feeding.2choice, test = "Chisq") 
    ## Interaction effect found, keep model as it s 


## Using the model for analysis
summary(glmm.bin.feeding.2choice)

## Getting numbers for the write-up
emmeans::emmeans(glmm.bin.feeding.2choice, specs = ~ ratio  * density, type = "response")


## Generating a table for the write-up
tab_model(glmm.bin.feeding.2choice, CSS = list(css.table = '+font-family: Arial;'))




