#### MFE Pupae Analysis ####


source("packages.R")

#### Reading, cleaning and organising the data ####

## Reading pupae data in with read excel
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")

## This code does something with 0s - not sure if it needed... 
pupae_fitness_MFE <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))

## The uncount code will simply clean the data
pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE, pupae)


## I now use this uncounted dataset, which works best for time. 

#### Preliminary Data Analysis #### 


#### 1. Testing Models ####


# Model 1 
#### Poisson GLMM #### 
glmm.p.pupae <- glmmTMB(time_hours ~ treatment  + (1| vial), family = poisson, 
                        data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS:  
# DHARMa checks 
plot(simulateResiduals(glmm.p.pupae)) 
  ## Does not look great
  # DHARMa assumptions shows this model is poor for both qq and homogeneity 
  # tests are significant for both 


## Performance checks - easystats 

# Looking for zeroinflation
check_zeroinflation(glmm.p.pupae) 
## There NO is zero inflation, indicating issues may be coming from overdispersion 

# Looking for overdispersion 
check_overdispersion(glmm.p.pupae) 
 ## There is overdispersion, indicating a negative binomial model might be good to test 






# Model 2
#### Negative Binomial GLM ####
glm.nb_pupae <- glm.nb(time_hours ~ 
                         treatment   
                       
                       , data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) 
## qq looks a lot better, BUT there are still significant P values

## Performance checks
check_zeroinflation(glm.nb_pupae)
## No zeroinflation 
check_overdispersion(glm.nb_pupae) 
## No overdispersion 




## In this situation, I do not know what to do? 




## Comparing models:
AIC(glmm.p.pupae,glm.nb_pupae)
 # Negative Binomial GLM is lower, but only slightly
 # It is still not a great model... need to research into what models could be better






## Using Negative Binomial GLM (FOR NOW)... 
glm.nb_pupae <- glm.nb(time_hours  ~ 
                         treatment  
                       
                       , data = pupae_fitness_MFE_2)



#### 2. Data analysis with chosen model ####

# Basic analysis 
summary(glm.nb_pupae)

# Confidence intervals
exp(confint(glm.nb_pupae))

# Getting real values for write-up 
emmeans::emmeans(glm.nb_pupae, specs =  ~ treatment, type = "response")


# Generating a table 
tab_model(glm.nb_pupae, CSS = list(css.table = '+font-family: Arial;'))


## Boxplot ##

viridis_colors <- viridis(10)

###

pupae_boxplot_MFE <- ggplot(pupae_fitness_MFE, aes(x = factor(time_hours), y = pupae, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(pupae_fitness_MFE$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of pupae emerged",
       fill = "Treatment")


## Display the plot 
pupae_boxplot_MFE

## Saving a plot
ggsave(filename = "pupae_boxplot_MFE.png", 
       plot = pupae_boxplot_MFE, 
       width = 10, 
       height = 6, 
       dpi = 300)






