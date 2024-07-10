## Packages ## 📦📦📦📦
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
############### 📦📦📦📦


################################################ Data Analysis ###################################
########################################  4:1 and 1:4 Four-Choice Assays ###################################
################################################################################################### ---




################################### --
####### OvoD1 Conditioning ####### 
################################### ---


## Reading the data in 
combined_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
combined_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

# Mutating a block variable 
combined_ovod1_b1 <- combined_ovod1_b1  %>% mutate(block = "one")
combined_ovod1_b2 <- combined_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
combined_ovod1 <- rbind(combined_ovod1_b1, combined_ovod1_b2)

# Making the data long
combined_of_egg  <- combined_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Model 1 
glm.pois.ovi.4choice <- glm(egg_numbers ~ diet * block, family = poisson, combined_of_egg)

## Model assumption checking 

## easystats currently doesn't work 

## DHARMa
testDispersion(glm.pois.ovi.4choice) ## Does this mean very overdispersed? 

simulationOutput <- simulateResiduals(fittedModel = glm.pois.ovi.4choice, plot = T) ## ? bad 


## Checking model dispersion values
summary(combined_od1_glm.p) ## Very overdispersed 

# Checking for zero inflation
check_zeroinflation(glm.pois.ovi.4choice) ## No zero inflation?? 

# Looking at qq
  ## Generating residuals 
residuals_p <- residuals(glm.pois.ovi.4choice, type = "pearson")
qnorm(residuals_p)
## Will generate a qq 
qqnorm(residuals_p) ## looks pretty low 

# But model is still very overdispersed 
# Because of this, first trying a quassipoisson 

## quasipoisson model
glm.quasipois.ovi.4choice <- glm(egg_numbers ~ diet * block, family = quasipoisson, combined_of_egg)


## can't do DHARma checks on a quasipoisson model? 
# Looking at qq
## Generating residuals 
residuals_qp <- residuals(glm.quasipois.ovi.4choice, type = "pearson")
qnorm(residuals_qp)
## Will generate a qq 
qqnorm(residuals_qp) ## looks the same as the poisson qq ?? 


## Don't really know how to interpret the quasipoisson model, doing a mixed model 
# trying a mixed GLM 
glmm.pois.ovi.4choice <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_of_egg)

# Looking at qq
## Generating residuals 
residuals_glm_mm <- residuals(glmm.pois.ovi.4choice , type = "pearson")
qnorm(residuals_glm_mm )
## Will generate a qq 
qqnorm(residuals_glm_mm ) ## looks pretty low -- all qq looks the same?? 



## DHARMa checks 
testDispersion(glmm.pois.ovi.4choice) ## looks a lot better, still odd 

simulation0utput <- simulateResiduals(fittedModel = glmm.pois.ovi.4choice, plot = T) ## Looking slightly better 





## Comparing the AIC of the models 
AIC(glm.pois.ovi.4choice, glm.quasipois.ovi.4choice, glmm.pois.ovi.4choice)
 ## Mixed model has the lowest AIC 



## Using this model for now 
glmm.pois.ovi.4choice <- glmmTMB(egg_numbers ~ diet + block + (1|plate/block) , family = poisson, data = combined_of_egg)

emmeans::emmeans(glmm.pois.ovi.4choice, ~ diet, type = "response")

emmeans::emmeans(glmm.pois.ovi.4choice, pairwise ~ diet)


## Testing the significance of block
drop1(glmm.pois.ovi.4choice , test = "Chisq") # block is very signficiant 

summary(glmm.pois.ovi.4choice)
   ## everything is very significant - dodgy data? 

glmm.pois.ovi.4choice <- glmmTMB(egg_numbers ~ diet  + block + (1|plate/block) , family = poisson, data = combined_of_egg)




#### Splitting up diet in the data 





#### Trying the model 
glmm.pois.ovi.4choice.2 <- glmmTMB(egg_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block  + (1|plate/block) , family = poisson, data = combined_of_egg_split )

drop1(glmm.pois.ovi.4choice.2, test = "Chisq") ## No 3-way interaction effect


glmm.pois.ovi.4choice.3 <- glmmTMB(egg_numbers ~ ratio * condition + ratio * block + condition * block  + (1|plate/block) , family = poisson, data = combined_of_egg_split )

drop1(glmm.pois.ovi.4choice.3, test = "Chisq") 
  ## No interaction effect between ratio and condition
  ## Interaction effect between ratio and block 
  ## Interaction effect between condition and block 


glmm.pois.ovi.4choice.4 <- glmmTMB(egg_numbers ~ ratio * block + condition * block  + (1|plate/block) , family = poisson, data = combined_of_egg_split )

drop1(glmm.pois.ovi.4choice.4, test = "Chisq") 

summary(glmm.pois.ovi.4choice.4)

emmeans::emmeans(combined_glm_mm_od1_egg, pairwise ~ diet)









glm.pois.ovi.4choice.2 <- glm(egg_numbers ~ ratio * condition * block, family = poisson, combined_of_egg_split)
glm.quasipois.ovi.4choice.2 <- glm(egg_numbers ~ ratio * condition * block, family = quasipoisson, combined_of_egg_split)
glmm.pois.ovi.4choice.2 <- glmmTMB(egg_numbers ~ ratio * condition * block + (1|factor(block)/plate) , family = poisson, data = combined_of_egg_split)


AIC(glm.pois.ovi.4choice.2, glm.quasipois.ovi.4choice.2, glmm.pois.ovi.4choice.2)



combined_of_egg_split <- combined_of_egg %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")






glmm.pois.ovi.4choice.2 <- glmmTMB(egg_numbers ~ ratio * condition * block + (1|factor(block)/plate) , family = poisson, data = combined_of_egg_split)


drop1(glmm.pois.ovi.4choice.2, test = "Chisq")






glmm.pois.ovi.4choice.3 <- glmmTMB(egg_numbers ~ 
                                     ratio + condition + block 
                                   + ratio : condition 
                                   + ratio : block 
                                   + condition : block +  (1|factor(block)/plate) , family = poisson, data = combined_of_egg_split)


drop1(glmm.pois.ovi.4choice.3, test = "Chisq")


  #### interactiomn r block and b block 




glmm.pois.ovi.4choice.4 <- glmmTMB(egg_numbers ~ 
                                     ratio + condition + block 
                             
                                   + ratio : block 
                                   + condition : block +
                                     
                                     (1|factor(block)/plate) , family = poisson, data = combined_of_egg_split)


drop1(glmm.pois.ovi.4choice.4, test = "Chisq")


summary(glmm.pois.ovi.4choice.4)

################################# --
####### Male Conditioning #######
################################# --

# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")
# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")





## Model 1 
## Beginning with a glm with poisson
comb_m_egg_glm.p <- glm(egg_numbers ~ diet * block, family = poisson,  combined_ovi_m_split)

## Assumption checking 

## DHARMa
testDispersion(comb_m_egg_glm.p) ## Really really overdispersed 

simulationOutput <- simulateResiduals(fittedModel = comb_m_egg_glm.p, plot = T) ## not great


## Looking at a qq plot 
## Generating residuals 
residuals_glm_p_m <- residuals(combined_glm_mm_od1_egg , type = "pearson")
qnorm(residuals_glm_p_m)
## Will generate a qq 
qqnorm(residuals_glm_p_m) ## ?? 


## Checking for overdispersion
summary(comb_m_egg_glm.p) ## very overdispersed by the looks of it 

## Checking for zeroinflation 
check_zeroinflation(comb_m_egg_glm.p) ## There is zero inflation 









## Trying a negative binomial model
glm.nb_m_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_m)


## DHARMa checks 
testDispersion(glm.nb_m_comb_egg) ## model has changed a lot - underdispersed now? 


## Looking at a qq plot 
## Generating residuals 
residuals_glm_nb_m <- residuals(glm.nb_m_comb_egg , type = "pearson")
qnorm(residuals_glm_nb_m)
## Will generate a qq 
qqnorm(residuals_glm_nb_m) ## points go down compared to previous model



## Trying a mixed model
combined_glm_mm_m_egg <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_ovi_m)

summary(combined_glm_mm_m_egg)

## DHARMa checks 
testDispersion(combined_glm_mm_m_egg) ## overdispersed 

## Looking at a qq plot 
## Generating residuals 
residuals_glm_mm_m <- residuals(combined_glm_mm_m_egg , type = "pearson")
qnorm(combined_glm_mm_m_egg)
## Will generate a qq 
qqnorm(combined_glm_mm_m_egg) ## points go down compared to previous model
   ## Can't do a qqplot with this code 


## Trying some zero inflation models as there is zero-inflation 

## zero inflated poisson
zi.p_m_comb_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_ovi_m)


## checks?? 

## zero inflated negative binomial 
zi.nb_m_comb_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_ovi_m)


AIC(comb_m_egg_glm.p, combined_glm_mm_m_egg, glm.nb_of_comb_egg, zi.p_m_comb_egg, zi.nb_m_comb_egg)
   ## the negative binomial glm has the lowest AIC by a while so is probably the best 


## checking the model out 
glm.nb_of_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_m)













## checking significance of block 
drop1(glm.nb_of_comb_egg, test = "F") ## says block is not significant!! 
summary(glm.nb_of_comb_egg)

## dropping block from the model
glm.nb_of_comb_egg_2 <- glm.nb(egg_numbers ~ diet , data =  combined_ovi_m)




glm.nb_of_comb_egg_2 <- glm.nb(egg_numbers ~ ratio * condition + block , data =  combined_ovi_m)

drop1(glm.nb_of_comb_egg_2, test = "F") ## not sig


## analysing results 
summary(glm.nb_of_comb_egg_2)

## Tukey Pairwise test
emmeans::emmeans(glm.nb_of_comb_egg_2, ~ diet, type = "response")

## Getting the response values for written analysis
emmeans(glm_mm_m_3, specs = ~ diet, type = "response" )





## Splitting "diet" up

combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")




## GLMM with poisson 
comb_m_egg_glm.p <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_ovi_m_split)

comb_m_egg_glm.nb <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_m_split)

comb_m_egg_glmm.p <- glmmTMB(egg_numbers ~ ratio * condition * block + (1|block/plate) , family = poisson, data = combined_ovi_m_split)


AIC(comb_m_egg_glm.p, comb_m_egg_glm.nb, comb_m_egg_glmm.p)

## GLM NB smallest by "a lot" ? 


# three way interaction
comb_m_egg_glm.nb <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_m_split)


drop1(comb_m_egg_glm.nb, test = "F")

comb_m_egg_glm.nb.2 <- glm.nb(egg_numbers ~
                                + ratio + condition + block
                               + ratio : condition 
                              + ratio : block
                              + condition : block, data =  combined_ovi_m_split)


drop1(comb_m_egg_glm.nb.2, test = "F")

## no two way  interactions 


comb_m_egg_glm.nb.3 <- glm.nb(egg_numbers ~
                                ratio + condition + block, data =  combined_ovi_m_split)


summary(comb_m_egg_glm.nb.3)

emmeans::emmeans(comb_m_egg_glm.nb.3, pairwise ~ ratio + condition + block )
############################## --
#### Virgin Conditioning ####
############################# --


## Reading in the different data-sets
fourone_onefour_oviposition_virgin_b2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
fourone_onefour_oviposition_virgin_b3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
fourone_onefour_oviposition_virgin_b4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
## Mutating a block variable to the data-sets
fourone_onefour_oviposition_virgin_b2 <- fourone_onefour_oviposition_virgin_b2 %>% mutate(block = "two")
fourone_onefour_oviposition_virgin_b3 <- fourone_onefour_oviposition_virgin_b3 %>% mutate(block = "three")
fourone_onefour_oviposition_virgin_b4 <- fourone_onefour_oviposition_virgin_b4 %>% mutate(block = "four")

## Binding the different data-sets
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_b2, fourone_onefour_oviposition_virgin_b3, fourone_onefour_oviposition_virgin_b4)


## adding some data names
combined_ovi_v <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




##### Data Analysis

# Model 1
# glm with poisson
comb_v_egg_glm.p <- glm(egg_numbers ~ diet * block, family = poisson,  combined_ovi_v)

## assumption checking 

## easystats
performance::check_model(comb_v_egg_glm.p, check = c("qq")) ## bit weird 
performance::check_model(comb_v_egg_glm.p, check = c("homogeneity")) ## slopey 
    ## Doesn't work for some reason - was "insight" 


## DHARMa
testDispersion(comb_v_egg_glm.p) # really overdispersed data

simulation_Output <- simulateResiduals(fittedModel = comb_v_egg_glm.p, plot = T)
   ## qq plot doesn't really work
   ## need to understand residuals vs predicted plot a bit better 


## generating a qqplot 
## Generating residuals 
residuals_glm_v_egg <- residuals(comb_v_egg_glm.p , type = "pearson")
qnorm(residuals_glm_v_egg)
## Will generate a qq 
qqnorm(residuals_glm_v_egg) ## looks okay? 


## Now checking for overdispersion values
summary(comb_v_egg_glm.p) ## very overdispersed 

## Checking for zero inflation 
check_zeroinflation(comb_v_egg_glm.p) 
  ## shows model is underfitting zeros 



# Model 2 
## Doing a negative binomial model 
glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_v)

## Assumption checks for this 
## easystats
performance::check_model(glm.nb_v_comb_egg, check = c("qq")) ## pretty much the same as poisson
performance::check_model(glm.nb_v_comb_egg, check = c("homogeneity")) ## less slopey than poisson


## DHARMa checks
testDispersion(glm.nb_v_comb_egg) ## nor overdispersed or underdispersed now 

simulation_Output <- simulateResiduals(fittedModel = glm.nb_v_comb_egg, plot = T) ## this looks a lot better


# Model 3

## Trying a mixed glm 
glm_mm_v_egg <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_ovi_v)

## Assumption checks 


## easystats
## glmmTMB not supported 


## DHARMa checks
testDispersion(glm_mm_v_egg) ## looking overdispersed now 

simulateOutput <- simulateResiduals(fittedModel = glm_mm_v_egg, plot = T) # ? 

## trying a qq plot 
## Generating residuals 
residuals_glm_mm_v_egg <- residuals(glm_mm_v_egg , type = "pearson")
qnorm(residuals_glm_mm_v_egg)
## Will generate a qq 
qqnorm(residuals_glm_mm_v_egg) ## doesn't look that different to previous 


## trying zero inflation models:: 

# poisson
zif.p_v_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_ovi_v)

# negative binomial 
zif.nb_v_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_ovi_v)


## Comparing the models 
AIC(comb_v_egg_glm.p , glm.nb_v_comb_egg, glm_mm_v_egg, zif.p_v_egg, zif.nb_v_egg)

## the negative binomial models have the lowest AIC 
## go with negative binomial glm


## Using the negative binomial glm
glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_v)

## tesing the signifiance of block 
drop1(glm.nb_v_comb_egg , test = "F") # block is significant 

## using the model 
summary(glm.nb_v_comb_egg)

emmeans::emmeans(glm.nb_v_comb_egg, ~ diet, type = "response")

## with block
glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_v)

## without block
glm.nb_v_comb_egg_2 <- glm.nb(egg_numbers ~ diet, data =  combined_ovi_v)

combined_ovi_v <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")

glm.nb_v_comb_egg_2 <- glm.nb(egg_numbers ~ ratio * condition + block, data =  combined_ovi_v)

drop1(glm.nb_v_comb_egg_2, test = "F") ## not sig

summary(glm.nb_v_comb_egg_2)


## viewing the data results 
summary(glm.nb_v_comb_egg_2) 

emmeans::emmeans(glm.nb_v_comb_egg, pairwise ~ diet)


