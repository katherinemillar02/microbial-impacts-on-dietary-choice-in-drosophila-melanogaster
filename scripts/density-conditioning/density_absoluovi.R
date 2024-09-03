
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
library(sjPlot)
##################---





## Reading the data in:

## 90 mm 
fourone_90mm_ovi <- read_excel("data/density_experiment/90mm_4-1_oviposition_2.xlsx")
onefour_90mm_ovi <- read_excel("data/density_experiment/90mm_1-4_oviposition_2.xlsx")

## Adding the appropriate data variables
fourone_90mm_long_ovi <- fourone_90mm_ovi   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

onefour_90mm_long_ovi <- onefour_90mm_ovi  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

## Mutating an id variable to better read the data in 
fourone_90mm_long_ovi <- fourone_90mm_long_ovi %>%  
  mutate(id = "4-1_90")

onefour_90mm_long_ovi <- onefour_90mm_long_ovi  %>%  
  mutate(id = "1-4_90")



## 35 mm 
fourone_35mm_ovi <- read_excel("data/density_experiment/50mm_4-1_oviposition_2.xlsx")
onefour_35mm_ovi <- read_excel("data/density_experiment/50mm_1-4_oviposition_2.xlsx")

## Adding the appropriate data variables
fourone_35mm_long_ovi <- fourone_35mm_ovi   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

onefour_35mm_long_ovi <- onefour_35mm_ovi  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


## Mutating an id variable to mutate a variable 
fourone_35mm_long_ovi <- fourone_35mm_long_ovi %>%  
  mutate(id = "4-1_50")

onefour_35mm_long_ovi <- onefour_35mm_long_ovi  %>%  
  mutate(id = "1-4_50")



## Binding the data to one data frame 
two_choice_density_ovi <- rbind(fourone_35mm_long_ovi, onefour_35mm_long_ovi, fourone_90mm_long_ovi, onefour_90mm_long_ovi)


## Mutating a density variable 
two_choice_density_1_ovi <- two_choice_density_ovi %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
  ))


## Reading the data in, and separating diet into ratio and condition
two_choice_density_df_ovi <- two_choice_density_1_ovi %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, plate, ratio, condition, density) %>%
  summarise(count = sum(egg_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")





#### DATA ANALYSIS 

# Binomial GLM
glm.bin.density.2choice.ovi <- glm(cbind(Conditioned, Unconditioned) 
                               ~ ratio * density , 
                               family = binomial, data = two_choice_density_df_ovi)

## Assumption checks 
glm.bin.density.2choice.residuals <- residuals(glm.bin.density.2choice.ovi, type = "pearson")

# Pearson residual check
plot(glm.bin.density.2choice.residuals ~ fitted(glm.bin.density.2choice.ovi), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")



simulationOutput <- simulateResiduals(fittedModel = glm.bin.density.2choice.ovi, plot = T)
## not a great model 




## Binomial GLMM - to test for random effects 
glmm.bin.density.2choice.ovi <- glmer(cbind(Conditioned, Unconditioned)
                                      ~ ratio  * density 
                                      + (1|plate) , family = binomial, data = two_choice_density_df_ovi)


drop1(glmm.bin.density.2choice.ovi, test = "Chisq")


two_choice_density_df_ovi$ratio <- as.factor(two_choice_density_df_ovi$ratio)
two_choice_density_df_ovi$ratio <- relevel(two_choice_density_df_ovi$ratio, ref = "4:1")



glmm.bin.density.2choice.ovi <- glmer(cbind(Conditioned, Unconditioned)
                                      ~ ratio  * density 
                                      + (1|plate) , family = binomial, data = two_choice_density_df_ovi)


summary(glmm.bin.density.2choice.ovi)
tab_model(glmm.bin.density.2choice.ovi, CSS = list(css.table = '+font-family: Arial;'))


simulationOutput <- simulateResiduals(fittedModel = glmm.bin.density.2choice.ovi, plot = T)
## still not a great model 

testZeroInflation(simulation_output)

check_overdispersion(glmm.bin.density.2choice.ovi)
 # no overdispersion 

check_zeroinflation(glmm.bin.density.2choice.ovi)
 # cannot do 


AIC(glm.bin.density.2choice.ovi, glmm.bin.density.2choice.ovi)


 # GLMM ZI
two_choice_density_df_ovi$ratio <- as.factor(two_choice_density_df_ovi$ratio)
two_choice_density_df_ovi$ratio <- relevel(two_choice_density_df_ovi$ratio, ref = "4:1")
## Binomial GLMM - to test for random effects 
glmm.bin.density.2choice.ovi <- glmer(cbind(Conditioned, Unconditioned)
                                      ~ ratio  * density 
                                      + (1|plate) , family = binomial, data = two_choice_density_df_ovi)


drop1(glmm.bin.density.2choice.ovi, test = "Chisq")

summary(glmm.bin.density.2choice.ovi)

emmeans::emmeans(glmm.bin.density.2choice.ovi, specs =  ~ ratio *  density , type = "response")

tab_model(glmm.bin.density.2choice.ovi, CSS = list(css.table = '+font-family: Arial;'))


