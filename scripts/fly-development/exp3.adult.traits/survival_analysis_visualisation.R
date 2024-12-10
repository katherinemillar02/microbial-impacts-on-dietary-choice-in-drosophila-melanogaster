############################################# Packages #################################################### 
library(ggpubr)
source("packages.R")
####################################################################################################################################### 

# Running Surv: 
Surv(lifespan_adultstraits$days_alive, lifespan_adultstraits$censor)
 # Using Surv() to run time-to-event data, censor represents the days alive not being definitive.
 # days alive indicates how long each individual survived. 
 # censor: 1 indicates there has been a death,  indicates the event was not observed. 

## Creating a new survival dataset 
## This dataset will run the days alive to still running vials to whatever today's date is
survival <- lifespan_adultstraits %>% 
  mutate(censor = replace_na(censor, 0)) %>%  # replacing NA (not yet dead) with 0 
  mutate(`date_of _death` = as.Date(`date_of _death`)) %>% # reading date as date
  mutate(`date_of _death` = replace_na(`date_of _death`, Sys.Date())) %>% # replacing missing death dates with the system date (whatever today's date is)
  mutate(days_alive = as.numeric(as.duration(`date_of _death` - dmy("19-10-2024")), "days")) # calculating the days alive for NA (where they are still alive)



# Running a plot with the lifespan dataset
survival_plot <- survfit2(Surv(days_alive, censor) ~ treatment, data = survival) %>% 
  ggsurvfit()+
  add_confidence_interval()+
  theme_classic()+
  theme(
    legend.position = c(0.9, 0.9),  
    legend.justification = c(1, 1),  
    legend.title = element_blank()) +
  labs(x = "Days alive since experiment began", 
       y = "Survival Probability")

# Running plot
survival_plot





## Data Analysis 

cox_model_1 <- coxph(Surv(days_alive, censor) ~ Conditioning * Sex, data = survival)
 # Adding cox proportional hazards model to the data 
 # Using surv with days alive, as well as censor (death or censered data)
 # looks at the independant variables of Conditioning and Sex as an interaction effect 


# Testing for the intteraction between Conditioning and Sex 
drop1(cox_model_1, test = "Chisq")
 # No significant interaction effect found

# Doing the cox model without an interaction effect 
cox_model_2 <- coxph(Surv(days_alive, censor) ~ Conditioning + Sex, data = lifespan)
 # Final model? 


 # Looking for assumptions of Cox
cox.assumptions <- cox.zph(cox_model_2)

# Running code 
cox.assumptions

 # cox.zph tests the proportional hazards assumption for the fitted Cox model
 # the proportional hazards assumption assumed that the hazard ratio are constant over time (in this case, death) 
 # it assumes that the effect of a covariate (death / days alive) should not change over the duration of the study
 
 # a p value > 0.05: proportional hazards hold for that covariate 
 # a p value < 0.05: assumption is violated for that covariate (effect not constant over time)
 # the importance of doing this is to see if the result of the cox model are valid

# Visualisation fo the Cox assumptions
ggcoxzph(cox.assumptions)




summary(coxph(Surv(days_alive, censor) ~ Conditioning + strata(Sex), data = lifespan))

lifespan_female <- lifespan %>% filter(Sex == "Focal female")

lifespan_male <- lifespan %>% filter(Sex == "Focal male")

summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_female))

summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_male))




