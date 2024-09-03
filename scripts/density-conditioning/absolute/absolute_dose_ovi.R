## Chapter 4 - Appendix


#### Experiment - Density of Conditioning 

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
fourone_90mm_ovi <- read_excel("data/density_experiment/90mm_4-1_oviposition_2.xlsx")


## Adding the appropriate data variables
fourone_90mm_long_ovi <- fourone_90mm_ovi   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


## Mutating an id variable to better read the data in 
fourone_90mm_long_ovi <- fourone_90mm_long_ovi %>%  
  mutate(id = "4-1_90")




              ## 1:4 ##
# Reading data in with read_excel()
onefour_90mm_ovi <- read_excel("data/density_experiment/90mm_1-4_oviposition_2.xlsx")

## Adding the appropriate data variables - 1:4 
onefour_90mm_long_ovi <- onefour_90mm_ovi  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

## Mutating an id variable to better read the data in 
onefour_90mm_long_ovi <- onefour_90mm_long_ovi  %>%  
  mutate(id = "1-4_90")






#### High density - 35 mm ####

               ## 4:1 ##
# Reading data in with read_excel()
fourone_35mm_ovi <- read_excel("data/density_experiment/50mm_4-1_oviposition_2.xlsx")

## Adding the appropriate data variables
fourone_35mm_long_ovi <- fourone_35mm_ovi   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Mutating an id variable to mutate a variable 
fourone_35mm_long_ovi <- fourone_35mm_long_ovi %>%  
  mutate(id = "4-1_50")



              ## 1:4 ##
# Reading data in with read_excel()
onefour_35mm_ovi <- read_excel("data/density_experiment/50mm_1-4_oviposition_2.xlsx")

## Adding the appropriate data variables
onefour_35mm_long_ovi <- onefour_35mm_ovi  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

## Mutating an id variable to mutate a variable 
onefour_35mm_long_ovi <- onefour_35mm_long_ovi  %>%  
  mutate(id = "1-4_50")






#### Using rbind() to bind the data together:


## Binding the data to one data frame 
density_ovi_2choice <- rbind(fourone_35mm_long_ovi, onefour_35mm_long_ovi, fourone_90mm_long_ovi, onefour_90mm_long_ovi)


## Mutating a variable representing the mm at which diets were conditioned? 
density_ovi_2choice <- density_ovi_2choice %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
  ))


## Reading the data in, and separating diet into ratio and condition - cleaning the overall dataframe
density_ovi_2choice_df <- density_ovi_2choice %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, plate, ratio, condition, density) %>%
  summarise(count = sum(egg_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")


## The model with a changed intercept to 4:1 
density_ovi_2choice_df$ratio <- as.factor(density_ovi_2choice_df$ratio)
density_ovi_2choice_df$ratio <- relevel(density_ovi_2choice_df$ratio, ref = "4:1")





#### Data Analysis 📊 #### 


#### Working through models and checking ####


# Model 1 
 # Binomial GLM # 
glm.bin.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) 
                               ~ ratio * density , 
                               family = binomial, data = density_ovi_2choice_df)


## Assumption checks of model

# Base R checks? 

## Generating residuals 
glm.bin.ovi.2choice.residuals <- residuals(glm.bin.ovi.2choice, type = "pearson")

# Pearson residual check
plot(glm.bin.ovi.2choice.residuals ~ fitted(glm.bin.ovi.2choice), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")
  ## I can't actually read what this is... 



## DHARMa checks

## qq plot and residual plot
simulationOutput <- simulateResiduals(fittedModel = glm.bin.ovi.2choice, plot = T)
  ## not a great model 





# Model 2
  # Binomial GLMM #
glmm.bin.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                      ~ ratio  * density 
                                      + (1|plate) , family = binomial, data = density_ovi_2choice_df)


## Assumption checks of model


# Base R checks? 

## Generating residuals 
glmm.bin.ovi.2choice.residuals <- residuals(glm.bin.ovi.2choice, type = "pearson")

# Pearson residual check
plot(glmm.bin.ovi.2choice.residuals ~ fitted(glmm.bin.ovi.2choice), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")
  ## I can't actually read what this is... 




## DHARMa checks

## qq plot and residual plot
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.ovi.2choice, plot = T)
    # Better than model 1, but still not great




## Using this model, FOR NOW 

# Binomial GLMM #
glmm.bin.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                              ~ ratio  * density 
                              + (1|plate) , family = binomial, data = density_ovi_2choice_df)

## Testing interaction of ratio and density
drop1(glmm.bin.ovi.2choice, test = "Chisq")
 # Significant interaction effect


#### Final Data Analysis ####

# Final model analysis
summary(glmm.bin.ovi.2choice)

## Getting numbers for the write-up
emmeans::emmeans(glmm.bin.ovi.2choice, specs =  ~ ratio *  density , type = "response")

## Generating a table for the write-up
tab_model(glmm.bin.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))








