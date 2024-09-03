#### 4-1 AND 1-4 ANALYSIS 



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



## Reading the data in


## 35 mm
fourone_onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")

## Making the data long 
fourone_onefour_35mm_long <- fourone_onefour_35mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Mutating a density variable 
fourone_onefour_35mm_long <- fourone_onefour_35mm_long %>%
  mutate(density = "35mm")


## 90 mm
fourone_onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1_1-4.xlsx")

## Making the data long 
fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Mutating a density variable 
fourone_onefour_90mm_long <- fourone_onefour_90mm_long %>%
  mutate(density = "90mm")

## Binding the two densities 
combined_assays <- rbind(fourone_onefour_35mm_long, fourone_onefour_90mm_long)



combined_assays %>%
  group_by(diet, density) %>%         # Group by diet and density
  summarise(mean_fly_numbers = mean(fly_numbers, na.rm = TRUE)) # Calculate the mean of fly_numbers



#### DATA ANALYSIS 

#### Splitting "diet" up into ratio and treatment. 
combined_assays <- combined_assays %>% 
  separate(diet, into = c("ratio", "treatment"), sep = " ")


## Binomial GLMM
## Testing for a 3-way interaction
glmm.density.4choice <- glmmTMB(fly_numbers ~ ratio * treatment * density + (1|plate) + (1|observation), family = poisson, data = combined_assays)

## Testing for a 3-way interaction effect
drop1(glmm.density.4choice, test = "Chisq") 
   ## No 3-way interaction effect found

glmm.density.4choice.2 <- glmmTMB(fly_numbers ~
                                    ratio * treatment 
                                  + ratio * density 
                                  + treatment * density 
                                  
                                  + (1|plate) + (1|observation), family = poisson, data = combined_assays)

## Testing for 2-way interacation effects
drop1(glmm.density.4choice.2, test = "Chisq") 
  ## No interaction effect found between treatment and density
  ## Interaction effects found between ratio and treatment, and ratio and density

## Final model, contains significant interaction effects
glmm.density.4choice.3 <- glmmTMB(fly_numbers ~  ratio * treatment +  density  + (1|plate) + (1|observation), family = poisson, data = combined_assays)

## Shows thwe remaining interaction effects.
drop1(glmm.density.4choice.3, test = "Chisq") 

simulationOutput <- simulateResiduals(fittedModel = glmm.density.4choice.3, plot = T)


combined_assays$ratio <- as.factor(combined_assays$ratio)
combined_assays$ratio <- relevel(combined_assays$ratio, ref = "4:1")

glmm.density.4choice.3 <- glmmTMB(fly_numbers ~  ratio * treatment +  density  + (1|plate) + (1|observation), family = poisson, data = combined_assays)

## Overall analysis
summary(glmm.density.4choice.3)

tab_model(glmm.density.4choice.3, CSS = list(css.table = '+font-family: Arial;'))



emmeans::emmeans(glmm.density.4choice.3, specs =  ~ ratio * treatment +  density , type = "response")

#### I don't know how to interpret this exactly, but is the point in this model maily just looking for an interaction between density and ratio/  treatment? 

