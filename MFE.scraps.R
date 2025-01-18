


## Reading data in using read excel 
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")

## Multiplying the values by 1000, for better analysis and to get the plots to work... for consistency too 
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000



#### 1. Preliminary Data Analysis #### 


# Model 1 
#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.weight, plot = T)
# The model is not great with either qq or homogeneity tests 


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.MFE.weight)
# There is overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.MFE.weight) 
# There is no zero-inflation 



## As there is overdispersion, trying a Negative Binomial GLM 

# Model 2
#### Negative Binomial GLM #### 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_MFE)


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.weight, plot = T)
# Model is better, but still get significant result for homogeneity


## Using this model for now 

#### 2. Data analysis with chosen model ####
