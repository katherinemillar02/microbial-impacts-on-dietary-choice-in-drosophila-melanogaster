## Chapter 4 - Appendix

#### Experiment - Density of Conditioning - Oviposition

#### Assay: Relative


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


#### 📖 Reading, cleaning, editing the data: ####


#### Low density - 90 mm ####

# Reading data in with read_excel()
fourone_onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1_1-4.xlsx")

## Adding the appropriate data variables
fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Mutating a density variable 
fourone_onefour_90mm_long <- fourone_onefour_90mm_long %>%
  mutate(density = "90mm")


#### High density - 35 mm ####

# Reading data in with read_excel()
fourone_onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")

## Adding the appropriate data variables
fourone_onefour_35mm_long <- fourone_onefour_35mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Mutating a density variable 
fourone_onefour_35mm_long <- fourone_onefour_35mm_long %>%
  mutate(density = "35mm")


## Binding dataframes
density_feeding_4choice <- rbind(fourone_onefour_35mm_long , fourone_onefour_90mm_long)


## Splitting "diet" up into ratio and treatment 
density_feeding_4choice <- density_feeding_4choice %>% 
  separate(diet, into = c("ratio", "treatment"), sep = " ")


## Changing the intercept to 4:1 
density_feeding_4choice$ratio <- as.factor(density_feeding_4choice$ratio)
density_feeding_4choice$ratio <- relevel(density_feeding_4choice$ratio, ref = "4:1")


#### Data Analysis 📊 ####


# Model 1
# Binomial GLMM #
glmm.bin.feeding.4choice.dose <- glmmTMB(fly_numbers 
                                         ~ ratio * treatment * density 
                                         + (1|plate) + (1|observation), family = poisson, data = density_feeding_4choice)


## Assumption checks

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.feeding.4choice.dose, plot = T)
# Looks pretty good

## Using this model for now:

# Binomial GLMM #
glmm.bin.feeding.4choice.dose <- glmmTMB(fly_numbers 
                                         ~ ratio * treatment * density 
                                         + (1|plate) + (1|observation), family = poisson, data = density_feeding_4choice)


## Testing for a 3-way interaction effect
drop1(glmm.bin.feeding.4choice.dose, test = "Chisq") 
## No 3-way interaction effect found


# Looking for a 2-way interaction effects 
glmm.bin.feeding.4choice.dose.2 <- glmmTMB(fly_numbers ~
                                             ratio * treatment 
                                           + ratio * density 
                                           + treatment * density 
                                           + (1|plate) + (1|observation), family = poisson, data = density_feeding_4choice)

## Testing for 2-way interacation effects
drop1(glmm.bin.feeding.4choice.dose.2, test = "Chisq") 
## No interaction effect found between treatment and density
## Interaction effects found between ratio and treatment, and ratio and density



#### Final model analysis ####
glmm.bin.feeding.4choice.dose.2 <- glmmTMB(fly_numbers ~
                                             ratio * treatment 
                                           + ratio * density 
                                           + treatment * density 
                                           + (1|plate) + (1|observation), family = poisson, data = density_feeding_4choice)


summary()


