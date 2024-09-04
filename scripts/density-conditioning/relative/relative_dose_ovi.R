## Chapter 4 - Appendix


#### Experiment - Density of Conditioning 

#### Assay: Relative


 
#### Packages ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
##################---


#### 📖 Reading, cleaning, editing the data: ####


#### Low density - 90 mm ####

# Reading data in with read_excel()
fourone_onefour_90mm_ovi <- read_excel("data/density_experiment/90mm_combined_oviposition_2.xlsx")

## Adding the appropriate data variables
fourone_onefour_90mm_long_ovi <- fourone_onefour_90mm_ovi  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Mutating a density variable 
fourone_onefour_90mm_long_ovi <- fourone_onefour_90mm_long_ovi %>%
  mutate(density = "90mm")






#### High density - 35 mm ####

# Reading data in with read_excel()
fourone_onefour_35mm_ovi <- read_excel("data/density_experiment/50mm_combined_oviposition_2.xlsx")

## Adding the appropriate data variables
fourone_onefour_35mm_long_ovi <- fourone_onefour_35mm_ovi  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Mutating a density variable 
fourone_onefour_35mm_long_ovi <- fourone_onefour_35mm_long_ovi %>%
  mutate(density = "35mm")





## Binding the two data-frames for data analysis 
density_ovi_4choice <- rbind(fourone_onefour_35mm_long_ovi, fourone_onefour_90mm_long_ovi)



#### Splitting "diet" up into ratio and treatment. 
density_ovi_4choice <- density_ovi_4choice %>% 
  separate(diet, into = c("ratio", "treatment"), sep = " ")

# Changing the intercept to 4:1 
density_ovi_4choice$ratio <- as.factor(density_ovi_4choice$ratio)
density_ovi_4choice$ratio <- relevel(density_ovi_4choice$ratio, ref = "4:1")


#### Data Analysis 📊 ####


# Model 1
 # Binomial GLMM # 
glmm.bin.ovi.4choice.dose <- glmmTMB(egg_numbers ~ 
                                  ratio * treatment * density 
                                + (1|plate), family = poisson, data = density_ovi_4choice)


## Assumption checks: 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.ovi.4choice.dose, plot = T)
   ## not a great model 

# easystats 

check_overdispersion(glmm.bin.ovi.4choice.dose)
 # overdispersion

check_zeroinflation(glmm.bin.ovi.4choice.dose)
 # no zero inflation 




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

