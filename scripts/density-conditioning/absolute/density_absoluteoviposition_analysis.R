#### 4-1 AND 1-4 ANALYSIS oviposition 



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
fourone_onefour_35mm_ovi <- read_excel("data/density_experiment/50mm_combined_oviposition_2.xlsx")

## Making the data long 
fourone_onefour_35mm_long_ovi <- fourone_onefour_35mm_ovi  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Mutating a density variable 
fourone_onefour_35mm_long_ovi <- fourone_onefour_35mm_long_ovi %>%
  mutate(density = "35mm")


## 90 mm
fourone_onefour_90mm_ovi <- read_excel("data/density_experiment/90mm_combined_oviposition_2.xlsx")

## Making the data long 
fourone_onefour_90mm_long_ovi <- fourone_onefour_90mm_ovi  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Mutating a density variable 
fourone_onefour_90mm_long_ovi <- fourone_onefour_90mm_long_ovi %>%
  mutate(density = "90mm")

## Binding the two densities 
combined_assays_ovi <- rbind(fourone_onefour_35mm_long_ovi, fourone_onefour_90mm_long_ovi)



#### DATA ANALYSIS 

#### Splitting "diet" up into ratio and treatment. 
combined_assays_ovi <- combined_assays_ovi %>% 
  separate(diet, into = c("ratio", "treatment"), sep = " ")


## Binomial GLMM
## Testing for a 3-way interaction
glmm.density.4choice.ovi <- glmmTMB(egg_numbers ~ 
                                  ratio * treatment * density 
                                + (1|plate), family = poisson, data = combined_assays_ovi)


## assumption checks
simulationOutput <- simulateResiduals(fittedModel = glmm.density.4choice.ovi, plot = T)
   ## not a great model 


check_overdispersion(glmm.density.4choice.ovi)
 # OD

check_zeroinflation(glmm.density.4choice.ovi)
 # no zero inflation 

combined_assays_ovi$ratio <- as.factor(combined_assays_ovi$ratio)
combined_assays_ovi$ratio <- relevel(combined_assays_ovi$ratio, ref = "4:1")
## new model
glm.nb.density.4choice.ovi <- glm.nb(egg_numbers ~ 
                                       ratio * treatment * density, 
                                     data = combined_assays_ovi)


## assumption checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb.density.4choice.ovi, plot = T)
 # a lot better

check_overdispersion(glm.nb.density.4choice.ovi)
 # no overdispersion 


AIC(glmm.density.4choice.ovi, glm.nb.density.4choice.ovi)
# AIC for nb a lot lower 



## Testing for a 3-way interaction effect
drop1(glm.nb.density.4choice.ovi, test = "Chisq") 
## A 3-way interaction effect is found



summary(glm.nb.density.4choice.ovi)

tab_model(glm.nb.density.4choice.ovi, CSS = list(css.table = '+font-family: Arial;'))

