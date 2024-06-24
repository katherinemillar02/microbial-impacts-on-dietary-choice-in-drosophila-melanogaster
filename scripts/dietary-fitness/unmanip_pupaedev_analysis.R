## Pupae 

pupae_model <- glmmTMB(pupae ~ treatment * `time (hours)` + (1| vial), family = poisson, data = pupae_fitness)


# DHARMa checks 
plot(simulateResiduals(pupae_model)) ## doesn't look too bad? 

check_zeroinflation(pupae_model) ## There is zero inflation

check_overdispersion(pupae_model) ## There is over dispersion 


glm.nb_pupae <- glm.nb(pupae ~ treatment * `time (hours)` + (1| vial), data = pupae_fitness)

drop1(glm.nb_pupae, tets = "F")


summary(glm.nb_pupae)

# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) ## doesn't look too bad? 

check_zeroinflation(glm.nb_pupae) ## There is zero inflation

check_overdispersion(glm.nb_pupae) ## There is NO over dispersion 

# There is still zero inflation 

## zero inflation model 
zi.p_pupae <- zeroinfl(pupae ~ treatment | treatment, dist = "poisson", link = "logit", data = pupae_fitness)

## don't know how to do checks 

drop1(glm.nb_pupae, test = "F")

summary(glm.nb_pupae)


# trying a zero inflated negative binomial model 
zi.nb_pupae <- zeroinfl(pupae ~ treatment | treatment, dist = "negbin", link = "logit", data = pupae_fitness)


## AIC check 
AIC(pupae_model, glm.nb_pupae, zi.p_pupae, zi.nb_pupae)