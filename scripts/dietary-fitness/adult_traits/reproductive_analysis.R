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



#### FIRST REPRODUCTIVE COUNT ##### 22/10 -25/10
## Reading the data in
reproductive_adultstraits <- read_excel("data/fitness_development/treatment_reproductive.xlsx")
reproductive_adultstraits <- as.data.frame(reproductive_adultstraits)

## adding a sex section 
reproductive_adultstraits$Conditioning <- ifelse(grepl("Conditioned", reproductive_adultstraits$treatment), "Conditioned", "Unconditioned")
reproductive_adultstraits$Sex <- ifelse(grepl("female", reproductive_adultstraits$treatment), "Focal female", "Focal male")

## ANALYSIS 

# Model 1 
#### Poisson GLMM ###
glmm.p.adulttraits.reproductive <- glmmTMB(offspring ~ Conditioning * Sex + (1|vial), family = poisson, data = reproductive_adultstraits)
 


## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.adulttraits.reproductive, plot = T)
# some errors with the plot, negative tests


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.adulttraits.reproductive)
# nO overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.adulttraits.reproductive) 
# There is no zero-inflation 


## zero inflation models

glm.zi.p.MFE.flies <- glmmTMB(
  offspring ~ Conditioning * Sex + (1 | vial),  
  ziformula = ~ Conditioning * Sex,               
  family = poisson(),                          
  data = reproductive_adultstraits
)



simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.MFE.flies, plot = T)
 ## model looks good 

AIC(glmm.p.adulttraits.reproductive, glm.zi.p.MFE.flies)
 # zeroinflated poisson  lower 



# analysis 
glm.zi.p.MFE.flies <- glmmTMB(
  offspring ~ Conditioning * Sex + (1 | vial),  
  ziformula = ~ Conditioning * Sex,               
  family = poisson(),                          
  data = reproductive_adultstraits
)


drop1(glm.zi.p.MFE.flies, test = "Chisq")

glm.zi.p.MFE.flies.2 <- glmmTMB(
  offspring ~ Conditioning + Sex + (1 | vial),  
  ziformula = ~ Conditioning + Sex,               
  family = poisson(),                          
  data = reproductive_adultstraits
)

summary(glm.zi.p.MFE.flies.2)
 ## sig diff between treatment, higher reproductive in unconditioned






#### SECOND #### 29/10 - 01/11 
##################
## Reading the data in

## Reading pupae data in
reproductive_adultstraits.2 <- read_excel("data/fitness_development/reproductive.count.2.xlsx")
reproductive_adultstraits.2 <- as.data.frame(reproductive_adultstraits.2)

## adding a sex section 
reproductive_adultstraits.2$Conditioning <- ifelse(grepl("Conditioned", reproductive_adultstraits.2$treatment), "Conditioned", "Unconditioned")
reproductive_adultstraits.2$Sex <- ifelse(grepl("female", reproductive_adultstraits.2$treatment), "Focal female", "Focal male")


## ANALYSIS 

# Model 1 
#### Poisson GLMM ###
glmm.p.adulttraits.reproductive.2 <- glmmTMB(offspring ~ Conditioning * Sex + (1|vial), family = poisson, data = reproductive_adultstraits.2)



## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.adulttraits.reproductive.2, plot = T)
# some errors with the plot, negative tests


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.adulttraits.reproductive.2)
# nO overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.adulttraits.reproductive.2) 
# There is no zero-inflation 


## zero inflation models

glm.zi.p.MFE.flies.2 <- glmmTMB(
  offspring ~ Conditioning * Sex + (1 | vial),  
  ziformula = ~ Conditioning * Sex,               
  family = poisson(),                          
  data = reproductive_adultstraits.2
)



simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.MFE.flies.2, plot = T)
## model looks good 

AIC(glmm.p.adulttraits.reproductive.2, glm.zi.p.MFE.flies.2)
# zeroinflated poisson  lower 



# analysis 
glm.zi.p.MFE.flies.2.1 <- glmmTMB(
  offspring ~ Conditioning * Sex + (1 | vial),  
  ziformula = ~ Conditioning * Sex,               
  family = poisson(),                          
  data = reproductive_adultstraits.2
)

# no treatment difference? 


drop1(glm.zi.p.MFE.flies.2.1, test = "Chisq")


summary(glm.zi.p.MFE.flies.2.1)
## sig diff between treatment, higher reproductive in unconditioned