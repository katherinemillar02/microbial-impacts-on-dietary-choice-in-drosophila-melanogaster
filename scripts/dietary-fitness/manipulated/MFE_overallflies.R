
## more conditioned than unconditioned have come out now? 
#### Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### Reading data in: ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                             cols = c( females, males),
                                             names_to = "sex",
                                             values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment, id) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned females", "unconditioned females",
                                           "conditioned males", "unconditioned males")))



#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.total.MFE <- glmmTMB(total_count ~ 
                               
                               sex * treatment +
                             
                              (1|id) + (1|vial) ,
                             
                             family = poisson, data = overallflies_MFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.MFE , plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.total.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.total.MFE)
## zero inflation 


# GLM NB 
glm.nb.MFE.flies <- glm.nb(total_count ~
                              
                              sex * treatment,
                            
                            data = overallflies_MFE)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.flies, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.MFE.flies)
# Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.flies)
## zero inflation 


AIC(glmm.p.total.MFE, glm.nb.MFE.flies)

#### Chosen model; Poisson GLMM ####
glmm.p.total.MFE <- glmmTMB(total_count ~ 
                              
                              sex * treatment +
                              
                              (1|id) + (1|vial) ,
                            
                            family = poisson, data = overallflies_MFE)



# Interaction effect
drop1(glmm.p.total.MFE, test = "Chisq")





glmm.p.total.MFE.2 <- glmmTMB(total_count ~ 
                              
                              sex + treatment +
                              
                              (1|id) + (1|vial) ,
                            
                            family = poisson, data = overallflies_MFE)






#### DATA ANALYSIS ####
summary(glmm.p.total.MFE.2)

# Table
tab_model(glmm.p.total.MFE.2)





#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

viridis_colors <- viridis(10)


overall_emergence_treatment <- ggplot(overallflies_MFE, aes(x = sex, y = total_count, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.8)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Number of Flies Emerged", 
       fill = "Treatment") +
  ylim(0, 30)


## Saving a plot
ggsave(filename = "overall_emergence_treatment.png", 
       plot = overall_emergence_treatment, 
       width = 10, 
       height = 6, 
       dpi = 300)
