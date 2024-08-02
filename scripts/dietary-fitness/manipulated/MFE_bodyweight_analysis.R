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
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")

bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000

# Model 1 
#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## qq plot from the model
residuals <- residuals(glmm.p.MFE.weight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos

# performance checks 
check_overdispersion(glmm.p.MFE.weight)
# overdispersion
check_zeroinflation(glmm.p.MFE.weight) 
# No zero-inflation 

simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.weight, plot = T)





glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                               
                               , data = bodyweight_MFE)



simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.weight, plot = T)

drop1(glm.nb.MFE.weight, test = "Chisq")


glm.nb.MFE.weight.2 <- glm.nb(weight_mg ~ treatment + sex  
                            
                            , data = bodyweight_MFE)

summary(glm.nb.MFE.wei