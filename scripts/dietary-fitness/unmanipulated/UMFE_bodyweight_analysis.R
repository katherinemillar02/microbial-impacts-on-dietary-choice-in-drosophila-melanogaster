#### Packages ####
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
##################

#### Reading data in: ####
bodyweight_2 <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")




bodyweight_2$weight_mg <- bodyweight_2$weight_mg * 1000


# Model 1 
#### Poisson GLMM ###
glmm.p.UMFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1| sample_number), family = poisson, data = bodyweight_2)

## qq plot from the model
residuals <- residuals(glmm.p.UMFE.weight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty good

# Model
simulationOutput <- simulateResiduals(fittedModel = glmm.p.UMFE.weight, plot = T)
# pretty bad 

# performance checks 
check_overdispersion(glmm.p.UMFE.weight)
 # No overdispersion
check_zeroinflation(glmm.p.UMFE.weight) 
 # No zero-inflation 


# Model 2 
glm.nb.UMFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_2)






## qq plot from the model
residuals <- residuals(glm.zi.nb.UMFE.bodyweight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos

simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.UMFE.bodyweight, plot = T)



AIC(glmm.p.UMFE.weight, glm.nb.UMFE.weight)

# if bodyweight is * 1000, assumptions are a lot better? 

# Model 2 chosen
glm.nb.UMFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                             
                             , data = bodyweight_2)




drop1(glm.nb.UMFE.weight, test = "Chisq")


glm.nb.UMFE.weight.2  <- glm.nb(weight_mg ~ treatment + sex  
                             
                             , data = bodyweight_2)



emmeans::emmeans(glm.nb.UMFE.weight, specs = ~ sex + treatment, type = "response")

summary(glm.nb.UMFE.weight.2)





## Table for model. 
tab_model(glm.nb.UMFE.weight.2, CSS = list(css.table = '+font-family: Arial;'))





#### Chosen model: Poisson GLMM ####

drop1(glmm.p.UMFE.weight, test = "Chisq")
 # No interaction effect



# Final model: 
glmm.p.UMFE.weight.2 <- glmmTMB(weight_mg ~ treatment + sex, family = poisson, data = bodyweight_2)







#### DATA ANALYSIS ####
summary(glmm.p.UMFE.weight.2)

exp(confint(glmm.p.UMFE.weight.2))


 
## Table for model. 
tab_model(glmm.p.UMFE.weight.2, CSS = list(css.table = '+font-family: Arial;'))


