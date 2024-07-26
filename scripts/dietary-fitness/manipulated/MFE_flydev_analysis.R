# Overall Fly Emergence 
# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### DATA ANALYSIS



#### Reading data in ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Separating the data into female and male columns 
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 


# as df
fly_fitness_tidy_MFE <- as.data.frame(fly_fitness_tidy_MFE)







#### Testing Models ####

# Model 1 
#### Poisson GLM ####
glm.p.MFE.fly <- glm(count ~
                        treatment * sex * time_hours , 
                      
                      family = poisson, data = fly_fitness_tidy_MFE)


#### ASSUMPTION CHECKS 
performance::check_model(glm.p.MFE.fly, check = c("qq")) 
# doesn't show 
performance::check_model(glm.p.MFE.fly, check = c("outliers")) 
# weird? 
performance::check_model(glm.p.MFE.fly, check = c("homogeneity")) 
# really quite bad 

## Checking for overdispersion 
check_overdispersion(glm.p.UMFE.fly) 
## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm.p.UMFE.fly) 
## there is zero inflation 









# Model 2 
#### Negative Binomial GLM ####
glm.nb.MFE.fly <- glm.nb(count ~
                            
                            treatment * sex * time_hours,
                          
                          data = fly_fitness_tidy_MFE)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb.MFE.fly, check = c("qq")) 
# does not show. 
performance::check_model(glm.nb.MFE.fly, check = c("outliers")) 

performance::check_model(glm.nb.MFE.fly, check = c("homogeneity"))
# looks a bit weird


## Checking for overdispersion 
check_overdispersion(glm.nb.MFE.fly)
## No overdispersion detected 









# Model 3
#### Poisson GLMM ####
glmm.p.MFE.fly <- glmmTMB(count ~ 
                             
                             treatment * sex * time_hours
                           
                           + (1|sex/vial) + (1|time_hours),
                           
                           family = poisson, data = fly_fitness_tidy_MFE)


## qq plot from the model
residuals <- residuals(glmm.p.MFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)
  ## qq residuals do not look great. 

## Performance
check_zeroinflation(glmm.p.MFE.fly)
# there is zero inflation
check_overdispersion(glmm.p.MFE.fly)
# overdispersion detected




# Model 4 
#### Zero-Inflated Poisson ####
zi.p.MFE.fly <- zeroinfl(count ~ 
                            
                            treatment * time_hours * sex  
                          
                          , data = fly_fitness_tidy_MFE)







# Comparing models
AIC(glm.p.MFE.fly,glm.nb.MFE.fly, glmm.p.MFE.fly, zi.p.MFE.fly)






#### Chosen model: Poisson GLMM ####
glmm.p.MFE.fly <- glmmTMB(count ~ 
                             
                             treatment * sex * time_hours
                           
                           + (1|vial/sex) + (1|time_hours),
                           
                           family = poisson, data = fly_fitness_tidy_MFE)





# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.MFE.fly, test = "Chisq")
# No 3-way interaction effect


#### Chosen model: Poisson GLMM ####
glmm.p.MFE.fly.2 <- glmmTMB(count ~ 
                            
                            treatment * sex +
                            treatment * time_hours +
                            sex * time_hours +
                          
                          + (1|vial/sex) + (1|time_hours),
                          
                          family = poisson, data = fly_fitness_tidy_MFE)





# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.MFE.fly.2, test = "Chisq")
# 2 - way interaction between sex and time of emergence. 
# 2 - way interaction between treatment and time of emergence. 





glmm.p.MFE.fly.3 <- glmmTMB(count ~ 
                              
                              sex * time_hours +
                              treatment * time_hours +
                           
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)



drop1(glmm.p.MFE.fly.3, test = "Chisq")

summary(glmm.p.MFE.fly.3)


####
fly_fitness_tidy_MFE$sex <- as.factor(fly_fitness_tidy_MFE$sex)
fly_fitness_tidy_MFE$sex <- relevel(fly_fitness_tidy_MFE$sex, ref = "males")

glmm.p.MFE.fly.3 <- glmmTMB(count ~ 
                              
                              sex * time_hours +
                              treatment * time_hours +
                              
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)



drop1(glmm.p.MFE.fly.31, test = "Chisq")

tab_model(glmm.p.MFE.fly.3)

summary(glmm.p.MFE.fly.3)


####
fly_fitness_tidy_MFE$sex <- as.factor(fly_fitness_tidy_MFE$sex)
fly_fitness_tidy_MFE$sex <- relevel(fly_fitness_tidy_MFE$sex, ref = "females")

glmm.p.MFE.fly.31 <- glmmTMB(count ~ 
                              
                              sex * time_hours +
                              treatment * time_hours +
                              
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)

summary(glmm.p.MFE.fly.31)
