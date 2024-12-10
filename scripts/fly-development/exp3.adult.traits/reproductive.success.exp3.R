


################################################ Packages ############################################
source("packages.R")
source("scripts/fly-development/exp3.adult.traits/fly.development.dataread.R")
######################################################################################################

## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)
## Reading pupae data in


#### FEMALE FOCAL REPRODUCTIVE SUCCESS #### 
reproductive_boxplot_adultstraits_f <- ggplot(reproductive_adultstraits_f, 
                                              aes(x = day, y = os, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Day in experiment when eggs were laid", 
       y = "Offspring",
       fill = "Treatment",
       title = "Female focal") 

## Run plot:
reproductive_boxplot_adultstraits_f



#### MALE FOCAL REPRODUCTIVE SUCCESS #### 
reproductive_boxplot_adultstraits_m <- ggplot(reproductive_adultstraits_m, 
                                              aes(x = day, y = os, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Day in experiment when eggs were laid", 
       y = "Offspring per day",
       fill = "Treatment",
       title = "Male focal") 

## Run plot:
reproductive_boxplot_adultstraits_m



#################################### DATA ANALYSIS ####################################

# Beginning a basic linear model 
rs.lm.1 <- lm(os ~ treatment * day, data = reproductive_adultstraits_m)

# Checks 
# performance checks using easystats 
performance::check_model(rs_lm_1)


## Doing more complex models - generalised linear mixed model 
rs.glmm.p.1 <- glmmTMB(os ~ treatment * day
                            
                            
                            + (1 | vial), 
                            
                            family = poisson, data = reproductive_adultstraits_m)



## Checks 

# performance checks 
performance::check_model(rs.glmm.p.1)

check_overdispersion(rs.glmm.p.1)
 # Overdispersion detected

check_zeroinflation(rs.glmm.p.1)
 # ZI detected 


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = rs.glmm.p.1, plot = T)
 # not an awfl mode, but deviation is significant 



#### Trying a negative binomial mixed model 
rs.glmm.nb.1 <- glmmTMB(os ~ treatment * day
                       
                       
                       + (1 | vial), 
                       
                       family = nbinom1(), data = reproductive_adultstraits_m)


## Checks 

# performance checks 
performance::check_model(rs.glmm.nb.1)

check_overdispersion(rs.glmm.nb.1)
# Undersipersion detected

check_zeroinflation(rs.glmm.nb.1)
# ZI is detected still 


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = rs.glmm.nb.1, plot = T)
 # DHARMA checks show model to be even worse 




#### Trying a zeroinflation model

rs.glmm.zi.p.1 <- glmmTMB(os ~ treatment * day
                        
                        
                        + (1 | vial), 
                        
                        family = poisson, 
                        
                        zi = ~ treatment * day,
                        
                        data = reproductive_adultstraits_m)


## Checks 


check_overdispersion(rs.glmm.zi.p.1)
# No overdispersion detected

check_zeroinflation(rs.glmm.zi.p.1)
# No zero inflation detected 


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = rs.glmm.zi.p.1, plot = T)
 # Actually looks okay, small significant deviation 


#### Doing AIC test to confirm best fitting model: 
AIC(rs.lm.1, rs.glmm.p.1, rs.glmm.nb.1, rs.glmm.zi.p.1)
 # Poisson zero inflated is the best 


#### Using this model: 
summary(rs.glmm.zi.p.1)

